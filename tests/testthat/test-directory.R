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
