# Tests for webr_repl_link functions

test_that("webr_repl_link creates valid webr_link object", {
  result <- webr_repl_link("1 + 1")

  expect_s3_class(result, "webr_link")
  expect_true(grepl("webr.r-wasm.org", result$url))
  expect_true(grepl("#code=", result$url))
  expect_equal(result$filename, "script.R")
  expect_equal(result$path, "/home/web_user/script.R")
  expect_equal(result$version, "latest")
  expect_false(result$autorun)
})

test_that("webr_repl_link respects custom filename", {
  result <- webr_repl_link("x <- 1", filename = "analysis.R")

  expect_equal(result$filename, "analysis.R")
  expect_equal(result$path, "/home/web_user/analysis.R")
})

test_that("webr_repl_link respects custom path", {
  result <- webr_repl_link("x <- 1", path = "/custom/path/script.R")

  expect_equal(result$path, "/custom/path/script.R")
})

test_that("webr_repl_link respects autorun", {
  result <- webr_repl_link("print('hello')", autorun = TRUE)

  expect_true(result$autorun)
  expect_true(grepl("&mza", result$url)) # 'a' flag for autorun
})

test_that("webr_repl_link respects mode parameter", {
  result <- webr_repl_link("x <- 1", panels = "editor")

  expect_equal(result$mode, "editor")
  expect_true(grepl("mode='editor'", result$url))
})

test_that("webr_repl_link respects version parameter", {
  result <- webr_repl_link("x <- 1", version = "v0.5.4")

  expect_equal(result$version, "v0.5.4")
  expect_true(grepl("v0.5.4", result$url))
})

test_that("webr_repl_link validates inputs", {
  expect_error(webr_repl_link("x", filename = 123))
  expect_error(webr_repl_link("x", autorun = "yes"))
  expect_error(webr_repl_link("x", panels = "invalid"))
  expect_error(webr_repl_link("x", version = "v0.4.0"))
})

test_that("webr_repl_project creates valid webr_project object", {
  skip_on_cran()
  temp_dir <- tempfile("webr_proj")
  dir.create(temp_dir)
  writeLines("source('utils.R')", file.path(temp_dir, "main.R"))
  writeLines("helper <- function() 1", file.path(temp_dir, "utils.R"))

  files <- c(file.path(temp_dir, "main.R"), file.path(temp_dir, "utils.R"))
  result <- webr_repl_project(files)

  expect_s3_class(result, "webr_project")
  expect_true(grepl("webr.r-wasm.org", result$url))
  expect_equal(length(result$files), 2)
  expect_true("main.R" %in% result$files)
  expect_true("utils.R" %in% result$files)

  unlink(temp_dir, recursive = TRUE)
})

test_that("webr_repl_project respects autorun_files", {
  skip_on_cran()
  temp_dir <- tempfile("webr_autorun")
  dir.create(temp_dir)
  writeLines("print('main')", file.path(temp_dir, "main.R"))
  writeLines("print('utils')", file.path(temp_dir, "utils.R"))

  files <- c(file.path(temp_dir, "main.R"), file.path(temp_dir, "utils.R"))
  result <- webr_repl_project(files, autorun_files = "main.R")

  expect_equal(result$autorun_files, "main.R")

  unlink(temp_dir, recursive = TRUE)
})

test_that("webr_repl_project validates autorun_files exist in files", {
  skip_on_cran()
  temp_dir <- tempfile("webr_validate")
  dir.create(temp_dir)
  writeLines("code", file.path(temp_dir, "main.R"))

  expect_error(
    webr_repl_project(file.path(temp_dir, "main.R"), autorun_files = "nonexistent.R"),
    "not found"
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("repl_urls extracts URL from webr_link", {
  result <- webr_repl_link("1 + 1")
  url <- repl_urls(result)

  expect_type(url, "character")
  expect_equal(url, result$url)
})

test_that("as.character works for webr_link", {
  result <- webr_repl_link("1 + 1")
  url <- as.character(result)

  expect_type(url, "character")
  expect_equal(url, result$url)
})
