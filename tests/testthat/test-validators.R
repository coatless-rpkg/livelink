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

# Regression: NA reached the comparisons inside the predicates, where `NA ==
# "latest"` is NA and `if (NA)` aborts with "missing value where TRUE/FALSE
# needed" -- naming neither the argument nor the value.
test_that("NA is reported as an invalid value, not a control-flow error", {
  expect_error(webr_repl_link("1", version = NA_character_), "version")
  expect_error(webr_repl_link("1", panels = NA_character_), "panels")
  expect_false(is_valid_version(NA_character_))
  expect_false(is_valid_mode(NA_character_))
})

# Regression: the message listed the valid components and nothing else, so
# rejecting c("plot", "plot") read as a contradiction -- "plot" was named valid
# in the same breath. Say which component is at fault, and why.
test_that("an invalid panels value names the offending component", {
  expect_error(webr_repl_link("1", panels = c("plot", "plot")), "more than once")
  expect_error(webr_repl_link("1", panels = "plot-editorr"), "Unknown panel")
  expect_error(webr_repl_link("1", panels = "plot-editorr"), "editorr")
})

# Regression: `{.val {x}}` renders nothing for NULL and for a zero-length
# vector, leaving a dangling "You provided:" in exactly the cases where the
# value is the whole problem.
test_that("the reported value is never blank", {
  expect_match(
    tryCatch(webr_repl_link("1", version = NULL), error = conditionMessage),
    "You provided: NULL"
  )
  expect_match(
    tryCatch(webr_repl_link("1", version = character(0)), error = conditionMessage),
    "empty character vector"
  )
  expect_equal(describe_value(NA), "NA")
})

# Regression: the URL validators required webr.r-wasm.org, so a link built
# against a self-hosted webR -- a documented workflow, via base_url or
# set_webr_base_url() -- could not be read back by the package that wrote it.
# Decoding never contacts the server; the fragment is what matters.
test_that("a self-hosted link can be read back", {
  url <- as.character(
    webr_repl_link("plot(1:10)", base_url = "https://my-webr.example.com/")
  )

  expect_true(is_valid_webr_url(url))
  expect_equal(
    preview_webr_link(url)$files_data[[1]]$text,
    "plot(1:10)"
  )

  # The sibling format must still be turned away: its fragment is LZ-string.
  expect_false(is_valid_webr_url(as.character(shinylive_r_link("1"))))
})
