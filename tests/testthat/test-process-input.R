test_that("is_likely_file_path only accepts paths that actually exist", {
  tmp <- withr::local_tempdir()
  real <- file.path(tmp, "real.R")
  writeLines("plot(1:10)", real)

  expect_true(is_likely_file_path(real))

  # Code, not paths -- these must never be treated as files.
  expect_false(is_likely_file_path("x <- 1/2"))
  expect_false(is_likely_file_path("mean(x)/n"))
  expect_false(is_likely_file_path("df$col.name"))
  expect_false(is_likely_file_path("plot(1:10)"))

  # A plausible-but-missing filename stays a path, so the user gets a
  # "not found" error instead of silently sharing the literal string as code.
  expect_true(is_likely_file_path("nonexistent_file.R"))
})

# Regression: any single-line string containing "/" or a trailing ".ext" was
# classified as a file path, so ordinary code aborted instead of encoding.
test_that("code containing / or a dot-suffix is treated as code, not a path", {
  code <- c(
    "x <- 1/2",
    "mean(x)/n",
    "df$col.name",
    "ratio <- hits/total",
    "p <- ggplot(d) + facet_wrap(~a/b)"
  )

  for (snippet in code) {
    link <- webr_repl_link(snippet)

    expect_s3_class(link, "webr_link")
  }
})

test_that("a missing file path produces a readable error", {
  # The abort message must actually render -- it previously crashed cli with
  # "Cannot pluralize without a quantity".
  expect_error(
    locate_input(c("no_such_file.R", "also_missing.R")),
    "not found"
  )
  expect_error(locate_input("no_such_file.R"), "not found")
})

test_that("locate_input classifies each input type", {
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "script.R")
  writeLines("plot(1:10)", f)

  expect_equal(locate_input(f), "path")
  expect_equal(locate_input("plot(1:10)"), "input")
  expect_equal(locate_input(list("a.R" = "x <- 1")), "input")
  expect_equal(locate_input(x_expr = quote({ plot(1:10) })), "expr")
  expect_equal(locate_input(NULL), "clipboard")
})

test_that("process_input reads a single file path", {
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "script.R")
  writeLines(c("x <- 1", "y <- 2"), f)

  expect_equal(process_input(f), "x <- 1\ny <- 2")
})

test_that("process_input reads multiple file paths into a named list", {
  tmp <- withr::local_tempdir()
  a <- file.path(tmp, "a.R")
  b <- file.path(tmp, "b.R")
  writeLines("x <- 1", a)
  writeLines("y <- 2", b)

  res <- process_input(c(a, b))

  expect_named(res, c("a.R", "b.R"))
  expect_equal(res$`a.R`, "x <- 1")
})

test_that("expression input is converted to source code", {
  link <- webr_repl_link({
    x <- 1
    plot(x)
  })

  expect_s3_class(link, "webr_link")

  code <- preview_webr_link(as.character(link))$files_data[[1]]$text

  expect_match(code, "x <- 1")
  expect_match(code, "plot(x)", fixed = TRUE)
  # The wrapping braces must not survive into the shared source.
  expect_false(startsWith(trimws(code), "{"))
})

test_that("expression input preserves comments when source refs are available", {
  # Comments only survive parsing when keep.source = TRUE, so parse explicitly
  # rather than depending on the session default.
  expr <- parse(
    text = "{\n  # a leading comment\n  x <- 1 # a trailing comment\n}",
    keep.source = TRUE
  )[[1]]

  code <- paste(stringify_expression(expr), collapse = "\n")

  expect_match(code, "# a leading comment")
  expect_match(code, "# a trailing comment")
  expect_false(startsWith(trimws(code), "{"))
  # Common leading indentation is trimmed.
  expect_false(grepl("^\\s+#", strsplit(code, "\n")[[1]][1]))
})

# Regression: the trailing-comment rescue scanned forward to Inf looking for a
# line starting with `}`. A one-line brace nested in a call -- whose own `}` is
# not on a line of its own -- therefore swallowed every source line down to the
# next such line, shipping unrelated code into the link.
test_that("a one-line brace does not swallow the lines after it", {
  expr <- parse(
    text = paste(
      'f(list(',
      '  "main.R" = { plot(1:10) },',
      '  "other"  = "not code"',
      '))',
      sep = "\n"
    ),
    keep.source = TRUE
  )[[1]]

  brace <- expr[[2]][[2]]
  code <- paste(stringify_expression(brace), collapse = "\n")

  expect_equal(code, "plot(1:10)")
  expect_false(grepl("not code", code, fixed = TRUE))
  expect_false(grepl("main.R", code, fixed = TRUE))
})

# Regression: with keep.source = FALSE (the default under Rscript and R CMD
# check) there are no srcrefs, and the deparse fallback shipped the wrapping
# braces into the shared script.
test_that("expression input strips braces even without source refs", {
  expr <- parse(text = "{\n  x <- 1\n  plot(x)\n}", keep.source = FALSE)[[1]]

  code <- paste(stringify_expression(expr), collapse = "\n")

  expect_false(startsWith(trimws(code), "{"))
  expect_false(grepl("}", code, fixed = TRUE))
  expect_match(code, "x <- 1")
  expect_match(code, "plot(x)", fixed = TRUE)
})

# Regression: only a `{ ... }` block means "this argument is source code".
# is.call() was true for ANY call, so a computed value like paste0(...) or
# readLines(f) got deparsed and the call itself was shared as the script.
test_that("a computed value is evaluated, not deparsed as source", {
  link <- webr_repl_link(paste0("plot(", "1:10", ")"))
  code <- preview_webr_link(as.character(link))$files_data[[1]]$text

  expect_equal(code, "plot(1:10)")
  expect_false(grepl("paste0", code, fixed = TRUE))
})

test_that("a computed value is evaluated for Shinylive too", {
  link <- shinylive_r_link(paste0("library(", "shiny", ")"))
  code <- preview_shinylive_link(as.character(link))$files_data[[1]]$content

  expect_equal(code, "library(shiny)")
  expect_false(grepl("paste0", code, fixed = TRUE))
})

# Regression: named-list input was documented for the Shinylive link functions
# but process_input() aborted on any multi-element input, so it never worked.
test_that("shinylive link functions accept a named list of files", {
  files <- list(
    "app.R" = "library(shiny)",
    "utils.R" = "helper <- function() 42"
  )

  link <- shinylive_r_link(files)

  expect_s3_class(link, "shinylive_link")
  expect_setequal(link$files, c("app.R", "utils.R"))

  preview <- preview_shinylive_link(as.character(link))
  expect_equal(preview$total_files, 2)
})

test_that("shinylive_py_link accepts a named list of files", {
  link <- shinylive_py_link(list(
    "app.py" = "from shiny import App",
    "utils.py" = "def helper(): return 42"
  ))

  expect_setequal(link$files, c("app.py", "utils.py"))
})

test_that("shinylive link functions accept several file paths", {
  tmp <- withr::local_tempdir()
  app <- file.path(tmp, "app.R")
  helper <- file.path(tmp, "helper.R")
  writeLines("library(shiny)", app)
  writeLines("# helper", helper)

  link <- shinylive_r_link(c(app, helper))

  expect_setequal(link$files, c("app.R", "helper.R"))
})

test_that("process_project_input rejects unsupported input", {
  expect_error(
    process_project_input(x_expr = quote({ plot(1:10) })),
    "not supported"
  )
  expect_error(process_project_input(list("x <- 1")), "named list")
})

test_that("process_input rejects multiple bare code strings", {
  expect_error(process_input(c("x <- 1", "y <- 2")), "Multiple")
})

# Regression: shinylive_r_link()/shinylive_py_link() documented `NULL: read from
# clipboard` but their input argument had no default, so calling them with no
# arguments failed with `argument "files" is missing, with no default`.
test_that("link functions read from the clipboard when called with no input", {
  local_mocked_bindings(ingest_clipboard = function() "library(shiny)")

  expect_s3_class(webr_repl_link(), "webr_link")
  expect_s3_class(shinylive_r_link(), "shinylive_link")
  expect_s3_class(shinylive_py_link(), "shinylive_link")
})

test_that("clipboard content becomes the app body", {
  local_mocked_bindings(ingest_clipboard = function() "marker <- 1")

  link <- shinylive_r_link()

  expect_equal(link$files, "app.R")

  contents <- preview_shinylive_link(as.character(link))$files_data[[1]]$content
  expect_equal(contents, "marker <- 1")
})
