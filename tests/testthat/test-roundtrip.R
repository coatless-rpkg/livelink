# The core invariant of this package: whatever we encode into a sharelink must
# come back out byte-for-byte.

decode_to_temp <- function(url, decoder) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  suppressMessages(decoder(url, output_dir = dir, overwrite = TRUE))

  paths <- list.files(dir, recursive = TRUE, full.names = TRUE)
  contents <- lapply(paths, function(p) paste(readLines(p, warn = FALSE), collapse = "\n"))
  stats::setNames(contents, basename(paths))
}

test_that("webR single-file links round-trip", {
  code <- "x <- rnorm(100)\nhist(x)"

  files <- decode_to_temp(as.character(webr_repl_link(code)), decode_webr_link)

  expect_named(files, "script.R")
  expect_equal(files[["script.R"]], code)
})

test_that("webR multi-file projects round-trip", {
  project <- list(
    "main.R" = "source('utils.R')\nrun()",
    "utils.R" = "run <- function() 42",
    "README.md" = "# Title\n\nSome prose."
  )

  files <- decode_to_temp(as.character(webr_repl_project(project)), decode_webr_link)

  expect_setequal(names(files), names(project))
  for (nm in names(project)) {
    expect_equal(files[[nm]], project[[nm]])
  }
})

test_that("Shinylive R apps round-trip", {
  app <- "library(shiny)\nui <- fluidPage()\nserver <- function(input, output) {}\nshinyApp(ui, server)"

  files <- decode_to_temp(as.character(shinylive_r_link(app)), decode_shinylive_link)

  expect_named(files, "app.R")
  expect_equal(files[["app.R"]], app)
})

test_that("Shinylive Python apps round-trip", {
  app <- "from shiny import App, ui\napp_ui = ui.page_fluid()\napp = App(app_ui, None)"

  files <- decode_to_temp(as.character(shinylive_py_link(app)), decode_shinylive_link)

  expect_named(files, "app.py")
  expect_equal(files[["app.py"]], app)
})

test_that("unicode survives a round-trip", {
  code <- "# héllo 你好 — emoji: \U0001F600\nx <- \"naïve café\""

  files <- decode_to_temp(as.character(webr_repl_link(code)), decode_webr_link)

  expect_equal(files[["script.R"]], code)
})

# Regression: simple_hash() overflowed on long input, so decoding a link built
# from a realistically-sized script failed with `invalid format '%x'`.
test_that("a large script round-trips", {
  set.seed(42)
  # Deliberately incompressible, so the URL stays long after gzip.
  lines <- paste0(
    "v", 1:600, " <- \"",
    replicate(600, paste(sample(c(letters, LETTERS, 0:9), 40, TRUE), collapse = "")),
    "\""
  )
  code <- paste(lines, collapse = "\n")
  url <- as.character(webr_repl_link(code))

  expect_gt(nchar(url), 8000)

  files <- decode_to_temp(url, decode_webr_link)

  expect_equal(files[["script.R"]], code)
})

test_that("webR link metadata round-trips", {
  url <- as.character(
    webr_repl_link("plot(1:10)", autorun = TRUE, version = "v0.5.4")
  )

  preview <- preview_webr_link(url)

  expect_equal(preview$version, "v0.5.4")
  expect_equal(preview$autorun_files, "script.R")
  expect_match(preview$flags, "a")
})

test_that("panels round-trip into the URL", {
  url <- as.character(webr_repl_link("plot(1:10)", panels = c("plot", "editor")))

  expect_match(url, "mode='plot-editor'", fixed = TRUE)
  expect_equal(preview_webr_link(url)$mode, c("plot", "editor"))
})
