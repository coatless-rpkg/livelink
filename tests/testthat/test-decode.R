# Tests for decode functions

test_that("preview_webr_link returns webr_preview object", {
  # Create a link and then preview it
  link <- webr_repl_link("x <- 1 + 1\nprint(x)")
  preview <- preview_webr_link(link$url)

  expect_s3_class(preview, "webr_preview")
  expect_equal(preview$total_files, 1)
  expect_true(preview$total_size > 0)
  expect_equal(preview$version, "latest")
})

test_that("preview_webr_link extracts correct file content", {
  original_code <- "# Test comment\nresult <- 42"
  link <- webr_repl_link(original_code)
  preview <- preview_webr_link(link$url)

  # Check file data structure
  expect_equal(length(preview$files_data), 1)
  file_info <- preview$files_data[[1]]
  expect_equal(file_info$name, "script.R")
})

test_that("preview_webr_link handles autorun files", {
  link <- webr_repl_link("print('auto')", autorun = TRUE)
  preview <- preview_webr_link(link$url)

  expect_equal(length(preview$autorun_files), 1)
})

test_that("preview_webr_link validates URL", {
  expect_error(
    preview_webr_link("https://example.com"),
    "valid webR"
  )
  expect_error(
    preview_webr_link(""),
    "valid webR"
  )
})

test_that("decode_webr_link creates output directory and files", {
  skip_on_cran() # Skip file I/O tests on CRAN

  link <- webr_repl_link("test_code <- 123")
  temp_dir <- tempfile("webr_decode_test")

  result <- decode_webr_link(link$url, output_dir = temp_dir, create_subdir = FALSE)

  expect_s3_class(result, "webr_decoded")
  expect_true(dir.exists(temp_dir))
  expect_true(file.exists(file.path(temp_dir, "script.R")))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("decode_webr_link respects overwrite parameter", {
  skip_on_cran()

  link <- webr_repl_link("original <- 1")
  temp_dir <- tempfile("webr_overwrite_test")
  dir.create(temp_dir, recursive = TRUE)

  # First decode
  decode_webr_link(link$url, output_dir = temp_dir, create_subdir = FALSE)

  # Second decode without overwrite should skip (suppress warning about existing file)
  link2 <- webr_repl_link("modified <- 2")
  result <- suppressWarnings(
    decode_webr_link(link2$url, output_dir = temp_dir, create_subdir = FALSE, overwrite = FALSE)
  )

  # File should not be overwritten (files_info will show skipped)
  expect_equal(nrow(result$files_info), 0) # Skipped due to existing file

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("preview_shinylive_link returns shinylive_preview object", {
  link <- shinylive_r_link("shinyApp(fluidPage(), function(i, o) {})")
  preview <- preview_shinylive_link(link$url)

  expect_s3_class(preview, "shinylive_preview")
  expect_equal(preview$total_files, 1)
  expect_equal(preview$engine, "r")
  expect_equal(preview$mode, "editor")
})

test_that("preview_shinylive_link validates URL", {
  expect_error(
    preview_shinylive_link("https://example.com"),
    "valid Shinylive"
  )
})

test_that("decode_shinylive_link creates output directory and files", {
  skip_on_cran()

  link <- shinylive_r_link("shinyApp(fluidPage('test'), function(i, o) {})")
  temp_dir <- tempfile("shinylive_decode_test")

  result <- decode_shinylive_link(link$url, output_dir = temp_dir, create_subdir = FALSE)

  expect_s3_class(result, "shinylive_decoded")
  expect_true(dir.exists(temp_dir))
  expect_true(file.exists(file.path(temp_dir, "app.R")))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("round-trip webr encoding and decoding preserves content", {
  skip_on_cran()

  original_code <- "# Round trip test\nresult <- sum(1:10)\nprint(result)"
  link <- webr_repl_link(original_code, filename = "roundtrip.R")
  temp_dir <- tempfile("webr_roundtrip")

  result <- decode_webr_link(link$url, output_dir = temp_dir, create_subdir = FALSE)
  decoded_content <- readLines(file.path(temp_dir, "roundtrip.R"), warn = FALSE)

  expect_equal(paste(decoded_content, collapse = "\n"), original_code)

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("decode_shinylive_link creates files from URL", {
  skip_on_cran()

  link <- shinylive_r_link("shinyApp(fluidPage('test'), function(i, o) {})")
  temp_dir <- tempfile("shinylive_roundtrip")

  result <- decode_shinylive_link(link$url, output_dir = temp_dir, create_subdir = FALSE)

  # Verify file was created
  expect_true(file.exists(file.path(temp_dir, "app.R")))
  expect_s3_class(result, "shinylive_decoded")

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})
