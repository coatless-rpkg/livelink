skip_if_not_installed("knitr")

# Render a document containing a ```{livelink} chunk and hand back the rendered
# markdown. This has to be a real knit: the whole point of the engine is that it
# behaves differently from expression input *under knitr*.
knit_livelink <- function(chunk_header, code_lines) {
  rmd <- c(
    "```{r}",
    "#| include: false",
    "library(livelink)",
    "```",
    "",
    chunk_header,
    code_lines,
    "```"
  )
  input <- withr::local_tempfile(fileext = ".Rmd")
  output <- withr::local_tempfile(fileext = ".md")
  writeLines(rmd, input)
  # Knit from a temp directory so figures and Rplots.pdf land there rather
  # than in tests/testthat: CRAN only allows writes to tempdir() during check.
  withr::local_dir(withr::local_tempdir())
  suppressMessages(knitr::knit(input, output = output, quiet = TRUE))
  readLines(output, warn = FALSE)
}

extract_url <- function(rendered) {
  line <- grep("\\]\\(https://", rendered, value = TRUE)[1]
  sub(".*\\]\\((https://[^)]+)\\).*", "\\1", line)
}

test_that("the engine is registered when livelink is loaded", {
  expect_true("livelink" %in% names(knitr::knit_engines$get()))
})

test_that("the chunk hook is registered when livelink is loaded", {
  expect_true("livelink" %in% names(knitr::knit_hooks$get()))
})

test_that("use_livelink_hook registers the hook", {
  expect_true(use_livelink_hook())
  expect_true("livelink" %in% names(knitr::knit_hooks$get()))
})

# The reason the hook exists: the engine replaces execution, so a ```{livelink}
# chunk shows a link but never any R output. The hook leaves the chunk running.
test_that("a hooked chunk still runs, and gains a link", {
  rendered <- knit_livelink("```{r}\n#| livelink: true", "mean(c(1, 2, 3))")
  text <- paste(rendered, collapse = "\n")

  # the chunk's own output is there ...
  expect_match(text, "[1] 2", fixed = TRUE)
  # ... and so is the link
  expect_match(text, "[Open in webR](https://webr.r-wasm.org/", fixed = TRUE)
})

test_that("comments survive into a hooked chunk's link", {
  rendered <- knit_livelink(
    "```{r}\n#| livelink: true",
    c("# a leading comment", "x <- 1  # a trailing comment")
  )

  code <- preview_webr_link(extract_url(rendered))$files_data[[1]]$text

  expect_match(code, "# a leading comment", fixed = TRUE)
  expect_match(code, "# a trailing comment", fixed = TRUE)
})

test_that("the hook honors autorun and panels", {
  rendered <- knit_livelink(
    "```{r}\n#| livelink: true\n#| autorun: true\n#| panels: [\"editor\", \"plot\"]",
    "plot(1:10)"
  )
  url <- extract_url(rendered)

  expect_match(url, "mode='editor-plot'", fixed = TRUE)
  expect_equal(preview_webr_link(url)$autorun_files, "script.R")
})

test_that("the hook can target Shinylive", {
  rendered <- knit_livelink(
    "```{r}\n#| livelink: shinylive-r\n#| eval: false",
    "library(shiny)"
  )

  expect_match(extract_url(rendered), "^https://shinylive\\.io/r/")
})

test_that("livelink: false opts a chunk out", {
  rendered <- knit_livelink("```{r}\n#| livelink: false", "mean(c(1, 2, 3))")
  text <- paste(rendered, collapse = "\n")

  expect_match(text, "[1] 2", fixed = TRUE)
  expect_false(grepl("Open in webR", text, fixed = TRUE))
})

test_that("an invalid hook target is rejected", {
  expect_error(
    knit_livelink("```{r}\n#| livelink: julia", "x <- 1"),
    "livelink chunk target"
  )
})

test_that("use_livelink_engine registers the engine", {
  expect_true(use_livelink_engine())
  expect_true("livelink" %in% names(knitr::knit_engines$get()))
})

# knitr's chunk syntax forbids a hyphen in the engine name (the chunk-detection
# regex is `[a-zA-Z0-9_]+`), so a `{shinylive-r}` chunk cannot dispatch to a knitr
# engine at all. Shinylive is named through engine.target instead. Guard against a
# well-meaning future re-introduction of a hyphenated engine that would silently
# never fire.
test_that("no hyphenated shinylive engine is registered", {
  engines <- names(knitr::knit_engines$get())
  expect_false(any(grepl("^shinylive", engines)))
})

# The reason this feature exists. knitr runs chunks through evaluate::evaluate(),
# which discards source references, so comments inside a `{ }` expression are lost
# when a document is rendered. The engine receives the chunk's verbatim source, so
# they survive.
test_that("comments in a livelink chunk survive into the generated link", {
  rendered <- knit_livelink(
    "```{livelink}",
    c(
      "# a leading comment",
      "data(mtcars)",
      "plot(mtcars$mpg, mtcars$wt)  # a trailing comment"
    )
  )

  code <- preview_webr_link(extract_url(rendered))$files_data[[1]]$text

  expect_match(code, "# a leading comment", fixed = TRUE)
  expect_match(code, "# a trailing comment", fixed = TRUE)
  expect_match(code, "plot(mtcars$mpg, mtcars$wt)", fixed = TRUE)
})

test_that("the engine emits both the source and a link by default", {
  rendered <- knit_livelink("```{livelink}", "plot(1:10)")
  text <- paste(rendered, collapse = "\n")

  expect_match(text, "plot(1:10)", fixed = TRUE)
  expect_match(text, "[Open in webR](https://webr.r-wasm.org/", fixed = TRUE)
})

# Regression: the link used to travel to engine_output() as `out`, which routes
# it through knitr's output hook. That prefixed it with the chunk's `comment`
# string and wrapped it in a code block, so the reader got a literal
# `#> [Open in webR](https://...)` rather than a link they could click.
test_that("the link is rendered as markdown, not as commented chunk output", {
  rendered <- knit_livelink("```{livelink}", "plot(1:10)")

  expect_false(any(grepl("#>\\s*\\[Open in webR\\]", rendered)))
  expect_true(any(grepl("^\\[Open in webR\\]\\(https://", rendered)))
})

# The fenced block used to be tagged with the engine name, `livelink`, which no
# highlighter knows -- so the code rendered unhighlighted.
test_that("the source block is fenced as the language it actually is", {
  r_chunk <- knit_livelink("```{livelink}", "plot(1:10)")
  expect_true(any(grepl("^```+\\s*r\\s*$", r_chunk)))
  expect_false(any(grepl("^```+\\s*livelink", r_chunk)))

  py_chunk <- knit_livelink(
    "```{livelink, engine.target='shinylive-py'}",
    "from shiny import App"
  )
  expect_true(any(grepl("^```+\\s*python\\s*$", py_chunk)))
})

test_that("link.only suppresses the source chunk", {
  rendered <- knit_livelink(
    "```{livelink, link.only=TRUE}",
    "secret_marker <- 1"
  )
  text <- paste(rendered, collapse = "\n")

  expect_match(text, "https://webr.r-wasm.org/")
  expect_false(grepl("secret_marker", text, fixed = TRUE))
})

test_that("autorun and panels chunk options reach the link", {
  rendered <- knit_livelink(
    "```{livelink, autorun=TRUE, panels=c('editor','plot')}",
    "plot(1:10)"
  )
  url <- extract_url(rendered)

  expect_match(url, "mode='editor-plot'", fixed = TRUE)

  preview <- preview_webr_link(url)
  expect_equal(preview$autorun_files, "script.R")
})

test_that("engine.target routes to Shinylive", {
  rendered <- knit_livelink(
    "```{livelink, engine.target='shinylive-r'}",
    "library(shiny)"
  )
  url <- extract_url(rendered)

  expect_match(url, "^https://shinylive\\.io/r/")
  expect_match(
    paste(rendered, collapse = "\n"),
    "Open in Shinylive",
    fixed = TRUE
  )
})

test_that("engine.target routes to Python Shinylive", {
  rendered <- knit_livelink(
    "```{livelink, engine.target='shinylive-py'}",
    "from shiny import App"
  )

  expect_match(extract_url(rendered), "^https://shinylive\\.io/py/")
})

test_that("link.text overrides the hyperlink label", {
  rendered <- knit_livelink(
    "```{livelink, link.text='Run this yourself'}",
    "plot(1:10)"
  )

  expect_match(
    paste(rendered, collapse = "\n"),
    "[Run this yourself](http",
    fixed = TRUE
  )
})

test_that("an invalid engine.target is rejected", {
  expect_error(
    knit_livelink("```{livelink, engine.target='julia'}", "x <- 1"),
    "livelink chunk target"
  )
})

test_that("filename chunk option names the file in the link", {
  rendered <- knit_livelink(
    "```{livelink, filename='analysis.R'}",
    "plot(1:10)"
  )

  preview <- preview_webr_link(extract_url(rendered))
  expect_equal(preview$files_data[[1]]$name, "analysis.R")
})
