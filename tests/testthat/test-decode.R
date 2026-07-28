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

# Regression: the file names in a payload are attacker-controlled, since
# decoding a link is how you open one a stranger sent you. A name like
# "../../.Rprofile" resolved outside output_dir and overwrote a file that runs
# at R startup, while the summary still reported success against a directory
# nothing had been written to.
test_that("decoding refuses a file name that escapes output_dir", {
  payload <- jsonlite::toJSON(
    list(list(name = "../escaped.R", path = "/escaped.R", text = "pwned <- TRUE")),
    auto_unbox = TRUE
  )
  url <- paste0(
    "https://webr.r-wasm.org/latest/#code=",
    utils::URLencode(
      base64enc::base64encode(memCompress(charToRaw(payload), "gzip")),
      reserved = TRUE
    ),
    "&jz"
  )

  parent <- withr::local_tempdir()
  output_dir <- file.path(parent, "decoded")
  dir.create(output_dir)

  expect_warning(
    decode_webr_link(url, output_dir = output_dir, create_subdir = FALSE),
    "Unsafe file name"
  )

  expect_false(file.exists(file.path(parent, "escaped.R")))
})

test_that("decoding still writes names holding a subdirectory", {
  payload <- jsonlite::toJSON(
    list(
      list(name = "main.R", path = "/main.R", text = "1"),
      list(name = "R/helpers.R", path = "/R/helpers.R", text = "2")
    ),
    auto_unbox = TRUE
  )
  url <- paste0(
    "https://webr.r-wasm.org/latest/#code=",
    utils::URLencode(
      base64enc::base64encode(memCompress(charToRaw(payload), "gzip")),
      reserved = TRUE
    ),
    "&jz"
  )

  output_dir <- withr::local_tempdir()
  decode_webr_link(url, output_dir = output_dir, create_subdir = FALSE)

  expect_true(file.exists(file.path(output_dir, "main.R")))
  expect_true(file.exists(file.path(output_dir, "R", "helpers.R")))
})

# Regression: msgpack carries text and binary alike as raw. Binary was rendered
# as the hex string "89 50 4e 47", which then failed the is-this-binary test
# and was written out as a text dump of the file, while decode reported
# success. Bytes now stay bytes.
test_that("a binary file in a link is written back byte for byte", {
  skip_if_not_installed("RcppMsgPack")

  png <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x01))
  packed <- RcppMsgPack::msgpack_pack(
    list(list(name = "logo.png", path = "/home/web_user/logo.png", data = png))
  )
  url <- paste0(
    "https://webr.r-wasm.org/latest/#code=",
    utils::URLencode(
      base64enc::base64encode(memCompress(packed, "gzip")),
      reserved = TRUE
    ),
    "&mz"
  )

  output_dir <- withr::local_tempdir()
  suppressMessages(
    decode_webr_link(url, output_dir = output_dir, create_subdir = FALSE)
  )

  expect_equal(readBin(file.path(output_dir, "logo.png"), "raw", 10), png)
})

test_that("a msgpack text link still decodes as text", {
  skip_if_not_installed("RcppMsgPack")

  packed <- RcppMsgPack::msgpack_pack(
    list(list(name = "script.R", path = "/home/web_user/script.R",
              text = "plot(1:10) # kept"))
  )
  url <- paste0(
    "https://webr.r-wasm.org/latest/#code=",
    utils::URLencode(
      base64enc::base64encode(memCompress(packed, "gzip")),
      reserved = TRUE
    ),
    "&mz"
  )

  expect_equal(
    preview_webr_link(url)$files_data[[1]]$text,
    "plot(1:10) # kept"
  )
})

# Regression: a payload that parsed but was not a list of file entries -- what
# a link whose flags no longer describe its contents decodes to -- reached `$`
# on an atomic vector and surfaced "$ operator is invalid for atomic vectors".
test_that("a link whose flags do not match its payload says so", {
  # Built rather than derived from webr_repl_link(), so the test does not
  # depend on which serialization the encoder happens to write today: msgpack
  # bytes, deliberately labelled as JSON.
  packed <- RcppMsgPack::msgpack_pack(
    list(list(name = "script.R", path = "/home/web_user/script.R",
              text = "plot(1:10)"))
  )
  mislabelled <- paste0(
    "https://webr.r-wasm.org/latest/#code=",
    utils::URLencode(
      base64enc::base64encode(memCompress(packed, "gzip")),
      reserved = TRUE
    ),
    "&jz"
  )

  expect_error(preview_webr_link(mislabelled), "preview webR link")

  # The cause is chained rather than pasted in, so cli never has to render the
  # raw bytes that failed to parse -- which are not valid UTF-8.
  expect_no_error(
    conditionMessage(tryCatch(preview_webr_link(mislabelled), error = function(e) e))
  )
})

test_that("a payload that is not a list of files is reported as malformed", {
  packed <- RcppMsgPack::msgpack_pack(list("not", "a", "file", "list"))
  url <- paste0(
    "https://webr.r-wasm.org/latest/#code=",
    utils::URLencode(
      base64enc::base64encode(memCompress(packed, "gzip")),
      reserved = TRUE
    ),
    "&mz"
  )

  expect_error(preview_webr_link(url), "Malformed webR link")
})
