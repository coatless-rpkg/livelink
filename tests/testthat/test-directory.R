make_shiny_app <- function(parent, name, engine = "r") {
  dir <- file.path(parent, name)
  dir.create(dir, recursive = TRUE)
  app_file <- if (engine == "python") "app.py" else "app.R"
  writeLines("# app", file.path(dir, app_file))
  dir
}

# Regression: sapply() returns list() when given zero subdirectories, and
# character(0)[list()] is an error -- so the friendly "no apps found" warning
# written for exactly this case was unreachable.
test_that("shinylive_directory warns rather than erroring on a directory with no apps", {
  empty <- withr::local_tempdir()

  expect_warning(result <- shinylive_directory(empty, engine = "r"), "No Shiny apps found")
  expect_s3_class(result, "shinylive_directory")
  expect_length(result$urls, 0)
})

test_that("shinylive_directory warns when subdirectories hold no app file", {
  parent <- withr::local_tempdir()
  dir.create(file.path(parent, "not_an_app"))
  writeLines("# nope", file.path(parent, "not_an_app", "helper.R"))

  expect_warning(result <- shinylive_directory(parent, engine = "r"), "No Shiny apps found")
  expect_length(result$urls, 0)
})

test_that("shinylive_directory builds a link per app subdirectory", {
  parent <- withr::local_tempdir()
  make_shiny_app(parent, "app_one")
  make_shiny_app(parent, "app_two")

  result <- suppressMessages(shinylive_directory(parent, engine = "r"))

  expect_s3_class(result, "shinylive_directory")
  expect_setequal(names(result$urls), c("app_one", "app_two"))
  expect_true(all(grepl("^https://shinylive\\.io/r/", result$urls)))
})

test_that("shinylive_directory finds Python apps", {
  parent <- withr::local_tempdir()
  make_shiny_app(parent, "py_app", engine = "python")

  result <- suppressMessages(shinylive_directory(parent, engine = "python"))

  expect_named(result$urls, "py_app")
  expect_match(result$urls[["py_app"]], "^https://shinylive\\.io/py/")
})

test_that("app files are named by their app-relative path in the link", {
  parent <- withr::local_tempdir()
  app <- make_shiny_app(parent, "app_one")
  dir.create(file.path(app, "data"))
  writeLines("x,y", file.path(app, "data", "values.csv"))

  result <- suppressMessages(shinylive_directory(parent, engine = "r"))
  files <- preview_shinylive_link(result$urls[["app_one"]])$files_data
  file_names <- vapply(files, function(f) f$name, character(1))

  expect_setequal(file_names, c("app.R", "data/values.csv"))
})

# Regression for CRAN's r-devel-windows failure: every Windows path contains
# backslash sequences (like \R, \U, \t) that are invalid TRE escapes, so an app
# path must never reach the regex engine as a pattern. Backslashes are legal in
# Unix file names, so a Windows-shaped directory name reproduces the Windows
# failure on every platform.
test_that("shinylive_directory survives regex-hostile directory names", {
  skip_on_os("windows") # backslash and colon are illegal in Windows file names

  parent <- withr::local_tempdir()
  hostile <- file.path(parent, "d:\\temp\\Rtmp+1")
  dir.create(hostile)
  make_shiny_app(hostile, "app_one")

  result <- suppressMessages(shinylive_directory(hostile, engine = "r"))

  expect_named(result$urls, "app_one")
  files <- preview_shinylive_link(result$urls[["app_one"]])$files_data
  expect_equal(vapply(files, function(f) f$name, character(1)), "app.R")
})

test_that("webr_repl_directory builds a link per R script", {
  dir <- withr::local_tempdir()
  writeLines("plot(1:10)", file.path(dir, "one.R"))
  writeLines("hist(rnorm(10))", file.path(dir, "two.R"))

  result <- suppressMessages(webr_repl_directory(dir))

  expect_s3_class(result, "webr_directory")
  expect_setequal(names(result$urls), c("one.R", "two.R"))
})

test_that("webr_repl_directory returns an empty object for a directory with no scripts", {
  empty <- withr::local_tempdir()

  expect_warning(result <- webr_repl_directory(empty), "No files found")
  # Must stay type-stable: an object, not character(0).
  expect_s3_class(result, "webr_directory")
  expect_length(result$urls, 0)
})

test_that("webr_repl_directory honors the pattern argument", {
  dir <- withr::local_tempdir()
  writeLines("plot(1:10)", file.path(dir, "keep.R"))
  writeLines("x <- 1", file.path(dir, "skip.R"))

  result <- suppressMessages(webr_repl_directory(dir, pattern = "^keep"))

  expect_named(result$urls, "keep.R")
})

test_that("webr_repl_directory(single_link = TRUE) bundles the whole directory", {
  dir <- withr::local_tempdir()
  writeLines("source('utils.R')", file.path(dir, "main.R"))
  writeLines("f <- function() 42", file.path(dir, "utils.R"))

  result <- suppressMessages(webr_repl_directory(dir, single_link = TRUE))

  # One link, not a directory of links.
  expect_s3_class(result, "webr_project")

  # The one link carries every matched file, named by basename.
  preview <- preview_webr_link(as.character(result))
  names <- vapply(preview$files_data, function(f) f$name, character(1))
  expect_setequal(names, c("main.R", "utils.R"))
})

test_that("single_link honors panels and autorun", {
  dir <- withr::local_tempdir()
  writeLines("plot(1:10)", file.path(dir, "one.R"))
  writeLines("hist(rnorm(10))", file.path(dir, "two.R"))

  result <- suppressMessages(webr_repl_directory(
    dir, single_link = TRUE, autorun = TRUE, panels = c("editor", "plot")
  ))
  url <- as.character(result)

  expect_match(url, "mode='editor-plot'", fixed = TRUE)
  # autorun bundles the a flag and marks every R file to run.
  expect_match(url, "&jza", fixed = TRUE)
  expect_setequal(preview_webr_link(url)$autorun_files, c("one.R", "two.R"))
})

test_that("single_link produces the same link as webr_repl_project on the same files", {
  dir <- withr::local_tempdir()
  writeLines("source('utils.R')", file.path(dir, "main.R"))
  writeLines("f <- function() 42", file.path(dir, "utils.R"))

  from_dir <- suppressMessages(webr_repl_directory(dir, single_link = TRUE))
  files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  from_project <- webr_repl_project(files)

  expect_identical(as.character(from_dir), as.character(from_project))
})
