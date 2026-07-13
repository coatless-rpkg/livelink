# Tests for validator functions

test_that("is_single_string works correctly", {
  expect_true(livelink:::is_single_string("hello"))
  expect_true(livelink:::is_single_string("a"))

  expect_false(livelink:::is_single_string(""))

expect_false(livelink:::is_single_string(c("a", "b")))
  expect_false(livelink:::is_single_string(123))
  expect_false(livelink:::is_single_string(NULL))
  expect_false(livelink:::is_single_string(NA_character_))
})

test_that("check_single_string aborts on invalid input", {
  expect_error(livelink:::check_single_string("", "arg"))
  expect_error(livelink:::check_single_string(123, "arg"))
  expect_error(livelink:::check_single_string(NULL, "arg"))

  expect_invisible(livelink:::check_single_string("valid", "arg"))
})

test_that("is_single_logical works correctly", {
  expect_true(livelink:::is_single_logical(TRUE))
  expect_true(livelink:::is_single_logical(FALSE))

  expect_false(livelink:::is_single_logical(c(TRUE, FALSE)))
  expect_false(livelink:::is_single_logical("TRUE"))
  expect_false(livelink:::is_single_logical(1))
  expect_false(livelink:::is_single_logical(NA))
})

test_that("check_single_logical aborts on invalid input", {
  expect_error(livelink:::check_single_logical("TRUE", "arg"))
  expect_error(livelink:::check_single_logical(NA, "arg"))

  expect_invisible(livelink:::check_single_logical(TRUE, "arg"))
})

test_that("is_valid_version works correctly", {
  expect_true(livelink:::is_valid_version("latest"))
  expect_true(livelink:::is_valid_version("v0.5.4"))
  expect_true(livelink:::is_valid_version("v0.5.5"))
  expect_true(livelink:::is_valid_version("v1.0.0"))

  expect_false(livelink:::is_valid_version("v0.5.3"))
  expect_false(livelink:::is_valid_version("v0.4.0"))
  expect_false(livelink:::is_valid_version("0.5.4"))
  expect_false(livelink:::is_valid_version("invalid"))
})

test_that("is_valid_mode works correctly", {
  expect_true(livelink:::is_valid_mode(NULL))
  expect_true(livelink:::is_valid_mode("editor"))
  expect_true(livelink:::is_valid_mode("plot"))
  expect_true(livelink:::is_valid_mode("editor-plot"))
  expect_true(livelink:::is_valid_mode(c("editor", "plot")))
  expect_true(livelink:::is_valid_mode(c("plot", "files", "terminal", "editor")))

  expect_false(livelink:::is_valid_mode("invalid"))
  expect_false(livelink:::is_valid_mode(""))
  expect_false(livelink:::is_valid_mode(c("editor", "editor"))) # duplicates
})

test_that("is_named_list works correctly", {
  expect_true(livelink:::is_named_list(list(a = 1, b = 2)))
  expect_true(livelink:::is_named_list(list("file.R" = "code")))

  expect_false(livelink:::is_named_list(list(1, 2)))
  expect_false(livelink:::is_named_list(list(a = 1, 2)))
  expect_false(livelink:::is_named_list(c(a = 1, b = 2)))
  expect_false(livelink:::is_named_list(NULL))
})

test_that("is_valid_shinylive_engine works correctly", {
  expect_true(livelink:::is_valid_shinylive_engine("r"))
  expect_true(livelink:::is_valid_shinylive_engine("python"))

  expect_false(livelink:::is_valid_shinylive_engine("R"))
  expect_false(livelink:::is_valid_shinylive_engine("Python"))
  expect_false(livelink:::is_valid_shinylive_engine("julia"))
})

test_that("is_valid_shinylive_mode works correctly", {
  expect_true(livelink:::is_valid_shinylive_mode("editor"))
  expect_true(livelink:::is_valid_shinylive_mode("app"))

  expect_false(livelink:::is_valid_shinylive_mode("Editor"))
  expect_false(livelink:::is_valid_shinylive_mode("both"))
})

test_that("is_valid_webr_url works correctly", {
  valid_url <- "https://webr.r-wasm.org/latest/#code=abc123"
  expect_true(livelink:::is_valid_webr_url(valid_url))

  expect_false(livelink:::is_valid_webr_url("https://example.com"))
  expect_false(livelink:::is_valid_webr_url("https://webr.r-wasm.org/latest/"))
  expect_false(livelink:::is_valid_webr_url(""))
  expect_false(livelink:::is_valid_webr_url(NULL))
})

test_that("is_valid_shinylive_url works correctly", {
  valid_r_url <- "https://shinylive.io/r/editor/#code=abc"
  valid_py_url <- "https://shinylive.io/py/app/#code=xyz"

  expect_true(livelink:::is_valid_shinylive_url(valid_r_url))
  expect_true(livelink:::is_valid_shinylive_url(valid_py_url))

  expect_false(livelink:::is_valid_shinylive_url("https://example.com"))
  expect_false(livelink:::is_valid_shinylive_url("https://shinylive.io/r/"))
})
