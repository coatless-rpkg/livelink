# Tests for shinylive link functions

test_that("shinylive_r_link creates valid shinylive_link object", {
  app_code <- "library(shiny)\nshinyApp(ui = fluidPage(), server = function(input, output) {})"
  result <- shinylive_r_link(app_code)

  expect_s3_class(result, "shinylive_link")
  expect_true(grepl("shinylive.io/r/", result$url))
  expect_true(grepl("#code=", result$url))
  expect_equal(result$engine, "r")
  expect_equal(result$mode, "editor")
  expect_true("app.R" %in% result$files)
})

test_that("shinylive_r_link respects mode parameter", {
  app_code <- "shinyApp(ui = fluidPage(), server = function(input, output) {})"

  editor_result <- shinylive_r_link(app_code, mode = "editor")
  expect_true(grepl("/editor/", editor_result$url))
  expect_equal(editor_result$mode, "editor")

  app_result <- shinylive_r_link(app_code, mode = "app")
  expect_true(grepl("/app/", app_result$url))
  expect_equal(app_result$mode, "app")
})

test_that("shinylive_r_link validates mode parameter", {
  expect_error(
    shinylive_r_link("code", mode = "invalid"),
    "must be either"
  )
})

test_that("shinylive_py_link creates valid shinylive_link object", {
  py_code <- "from shiny import App, ui\napp = App(ui.page_fluid(), None)"
  result <- shinylive_py_link(py_code)

  expect_s3_class(result, "shinylive_link")
  expect_true(grepl("shinylive.io/py/", result$url))
  expect_equal(result$engine, "python")
  expect_true("app.py" %in% result$files)
})

test_that("shinylive_py_link respects mode parameter", {
  py_code <- "from shiny import App, ui\napp = App(ui.page_fluid(), None)"

  editor_result <- shinylive_py_link(py_code, mode = "editor")
  expect_true(grepl("/editor/", editor_result$url))

  app_result <- shinylive_py_link(py_code, mode = "app")
  expect_true(grepl("/app/", app_result$url))
})

test_that("shinylive_project creates valid shinylive_project object", {
  skip_on_cran()
  temp_dir <- tempfile("shinylive_proj")
  dir.create(temp_dir)
  writeLines("library(shiny)\nshinyApp(fluidPage(), function(input, output) {})", file.path(temp_dir, "app.R"))
  writeLines("helper <- function() 1", file.path(temp_dir, "utils.R"))

  files <- c(file.path(temp_dir, "app.R"), file.path(temp_dir, "utils.R"))
  result <- shinylive_project(files, engine = "r")

  expect_s3_class(result, "shinylive_project")
  expect_true(grepl("shinylive.io/r/", result$url))
  expect_equal(result$engine, "r")
  expect_equal(length(result$files), 2)

  unlink(temp_dir, recursive = TRUE)
})

test_that("shinylive_project validates engine parameter", {
  skip_on_cran()
  temp_dir <- tempfile("shinylive_validate")
  dir.create(temp_dir)
  writeLines("code", file.path(temp_dir, "app.R"))

  expect_error(
    shinylive_project(file.path(temp_dir, "app.R"), engine = "julia"),
    "must be either"
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("repl_urls extracts URL from shinylive_link", {
  result <- shinylive_r_link("shinyApp(fluidPage(), function(i, o) {})")
  url <- repl_urls(result)

  expect_type(url, "character")
  expect_equal(url, result$url)
})

test_that("as.character works for shinylive_link", {
  result <- shinylive_r_link("shinyApp(fluidPage(), function(i, o) {})")
  url <- as.character(result)

  expect_type(url, "character")
  expect_equal(url, result$url)
})
