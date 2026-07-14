# Coverage for the decode paths -- the least-tested half of the package, and where
# the simple_hash() overflow bug lived.

webr_urls <- function(n = 2) {
  vapply(
    seq_len(n),
    function(i) as.character(webr_repl_link(sprintf("x <- %d", i), filename = sprintf("s%d.R", i))),
    character(1)
  )
}

shiny_urls <- function(n = 2) {
  vapply(
    seq_len(n),
    function(i) as.character(shinylive_r_link(sprintf("library(shiny) # %d", i))),
    character(1)
  )
}

# ---- batch decoding -------------------------------------------------------

test_that("decoding several webR URLs returns a batch object", {
  dir <- withr::local_tempdir()

  batch <- suppressMessages(decode_webr_link(webr_urls(3), output_dir = dir))

  expect_s3_class(batch, "webr_decoded_batch")
  expect_equal(batch$total_urls, 3)
  expect_equal(batch$successful_urls, 3)
  expect_equal(batch$total_files, 3)
})

test_that("each URL in a batch lands in its own directory", {
  dir <- withr::local_tempdir()

  suppressMessages(decode_webr_link(webr_urls(2), output_dir = dir))

  # name_dirs = TRUE (default) -> script_01, script_02
  expect_setequal(list.dirs(dir, recursive = FALSE, full.names = FALSE),
                  c("script_01", "script_02"))
  expect_true(file.exists(file.path(dir, "script_01", "s1.R")))
  expect_true(file.exists(file.path(dir, "script_02", "s2.R")))
})

test_that("name_dirs = FALSE uses hash-named directories", {
  dir <- withr::local_tempdir()

  suppressMessages(decode_webr_link(webr_urls(2), output_dir = dir, name_dirs = FALSE))

  dirs <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  expect_length(dirs, 2)
  expect_true(all(grepl("^webr_[0-9a-f]{8}$", dirs)))
})

test_that("create_subdir = FALSE decodes a batch into one flat directory", {
  dir <- withr::local_tempdir()

  suppressMessages(
    decode_webr_link(webr_urls(2), output_dir = dir, create_subdir = FALSE)
  )

  expect_length(list.dirs(dir, recursive = FALSE), 0)
  expect_setequal(list.files(dir), c("s1.R", "s2.R"))
})

test_that("decoding several Shinylive URLs returns a batch object", {
  dir <- withr::local_tempdir()

  batch <- suppressMessages(decode_shinylive_link(shiny_urls(2), output_dir = dir))

  expect_s3_class(batch, "shinylive_decoded_batch")
  expect_equal(batch$total_urls, 2)
  expect_equal(batch$successful_urls, 2)
})

test_that("an invalid URL in a batch is reported, not silently dropped", {
  dir <- withr::local_tempdir()
  urls <- c(webr_urls(1), "https://example.com/not-a-webr-link")

  expect_error(
    suppressMessages(decode_webr_link(urls, output_dir = dir)),
    "Invalid URL"
  )
})

test_that("an empty URL vector warns and returns an empty batch", {
  dir <- withr::local_tempdir()

  expect_warning(
    batch <- suppressMessages(decode_webr_link(character(0), output_dir = dir)),
    "No URLs"
  )
  expect_s3_class(batch, "webr_decoded_batch")
  expect_equal(batch$total_urls, 0)
})

# ---- single-URL layout ----------------------------------------------------

test_that("create_subdir = FALSE writes straight into output_dir", {
  dir <- withr::local_tempdir()
  url <- as.character(webr_repl_link("plot(1:10)", filename = "flat.R"))

  suppressMessages(decode_webr_link(url, output_dir = dir, create_subdir = FALSE))

  expect_true(file.exists(file.path(dir, "flat.R")))
  expect_length(list.dirs(dir, recursive = FALSE), 0)
})

test_that("create_subdir = TRUE nests under a hash-named directory", {
  dir <- withr::local_tempdir()

  suppressMessages(decode_webr_link(as.character(webr_repl_link("x <- 1")), output_dir = dir))

  subdirs <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  expect_length(subdirs, 1)
  expect_match(subdirs, "^webr_[0-9a-f]{8}$")
})

# ---- overwrite -----------------------------------------------------------

test_that("existing files are skipped unless overwrite = TRUE", {
  dir <- withr::local_tempdir()
  url <- as.character(webr_repl_link("new_content <- 1", filename = "f.R"))

  writeLines("old_content <- 0", file.path(dir, "f.R"))

  # overwrite = FALSE: the file on disk must survive untouched
  suppressMessages(
    decode_webr_link(url, output_dir = dir, create_subdir = FALSE, overwrite = FALSE)
  )
  expect_equal(readLines(file.path(dir, "f.R"), warn = FALSE), "old_content <- 0")

  # overwrite = TRUE: it must be replaced
  suppressMessages(
    decode_webr_link(url, output_dir = dir, create_subdir = FALSE, overwrite = TRUE)
  )
  expect_equal(readLines(file.path(dir, "f.R"), warn = FALSE), "new_content <- 1")
})

test_that("a skipped file is reported without leaking cli templates", {
  dir <- withr::local_tempdir()
  url <- as.character(webr_repl_link("x <- 1", filename = "f.R"))
  writeLines("existing", file.path(dir, "f.R"))

  result <- suppressMessages(
    decode_webr_link(url, output_dir = dir, create_subdir = FALSE, overwrite = FALSE)
  )

  expect_equal(result$total_files, 0)

  out <- paste(capture.output(print(result), type = "message"), collapse = "\n")

  expect_match(out, "already exist")
  expect_match(out, "f.R", fixed = TRUE)
  expect_match(out, "overwrite = TRUE", fixed = TRUE)
  # An unevaluated {length(...)} in the output means cli globbed the template.
  expect_false(grepl("{length(", out, fixed = TRUE))
})

test_that("many skipped files are truncated in the report", {
  dir <- withr::local_tempdir()
  files <- setNames(as.list(paste0("x <- ", 1:5)), paste0("f", 1:5, ".R"))
  url <- as.character(webr_repl_project(files))

  for (nm in names(files)) writeLines("existing", file.path(dir, nm))

  result <- suppressMessages(
    decode_webr_link(url, output_dir = dir, create_subdir = FALSE, overwrite = FALSE)
  )
  out <- paste(capture.output(print(result), type = "message"), collapse = "\n")

  expect_match(out, "and 2 more", fixed = TRUE)
  expect_false(grepl("{length(", out, fixed = TRUE))
})

# ---- binary content -------------------------------------------------------

test_that("detect_binary_content spots binary payloads", {
  expect_false(detect_binary_content("plain text\nwith newlines"))
  expect_false(detect_binary_content(""))

  # Control characters are the real signal.
  expect_true(detect_binary_content(rawToChar(as.raw(c(1, 2, 3, 4, 5, 6, 7, 8)))))
})

# Regression: "printable" was judged byte by byte, so every byte above 126 counted
# as non-printable -- which made ordinary UTF-8 text look like binary data.
test_that("UTF-8 text is not mistaken for binary", {
  expect_false(detect_binary_content("caf\u00e9"))
  expect_false(detect_binary_content("h\u00e9llo w\u00f6rld"))
  expect_false(detect_binary_content("\u4f60\u597d\u4e16\u754c"))
})

test_that("a Shinylive binary file is decoded from base64", {
  dir <- withr::local_tempdir()
  payload <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))  # PNG magic

  files <- list(
    list(name = "app.R", content = "library(shiny)", type = "text"),
    list(name = "logo.png", content = base64enc::base64encode(payload), type = "binary")
  )
  json <- jsonlite::toJSON(files, auto_unbox = TRUE)
  url <- paste0(
    "https://shinylive.io/r/editor/#code=",
    gsub("/", "-", lzstring::compressToEncodedURIComponent(as.character(json)))
  )

  suppressMessages(decode_shinylive_link(url, output_dir = dir, create_subdir = FALSE))

  written <- readBin(file.path(dir, "logo.png"), "raw", n = 8)
  expect_identical(written, payload)
})

test_that("a Shinylive preview reports file types", {
  url <- as.character(shinylive_r_link("library(shiny)"))

  preview <- preview_shinylive_link(url)

  expect_equal(preview$file_types, "text")
  expect_gt(preview$total_size, 0)
})

# ---- Python Shinylive -----------------------------------------------------

test_that("a Python Shinylive link previews and decodes", {
  dir <- withr::local_tempdir()
  url <- as.character(shinylive_py_link("from shiny import App\napp = App()"))

  preview <- preview_shinylive_link(url)
  expect_equal(preview$engine, "python")

  suppressMessages(decode_shinylive_link(url, output_dir = dir, create_subdir = FALSE))
  expect_equal(
    paste(readLines(file.path(dir, "app.py"), warn = FALSE), collapse = "\n"),
    "from shiny import App\napp = App()"
  )
})

test_that("a multi-file Shinylive app decodes every file", {
  dir <- withr::local_tempdir()
  url <- as.character(shinylive_r_link(list(
    "app.R"   = "library(shiny)",
    "utils.R" = "f <- function() 42",
    "data.csv" = "a,b\n1,2"
  )))

  suppressMessages(decode_shinylive_link(url, output_dir = dir, create_subdir = FALSE))

  expect_setequal(list.files(dir), c("app.R", "utils.R", "data.csv"))
  expect_equal(
    paste(readLines(file.path(dir, "data.csv"), warn = FALSE), collapse = "\n"),
    "a,b\n1,2"
  )
})

# ---- URL parsing error branches -------------------------------------------

test_that("malformed webR URLs are rejected with useful errors", {
  expect_error(preview_webr_link("not a url"), "webR")
  expect_error(preview_webr_link("https://example.com/#code=abc&jz"), "webR")
  expect_error(decode_webr_link(42), class = "rlang_error")
})

test_that("malformed Shinylive URLs are rejected", {
  expect_error(preview_shinylive_link("https://example.com/#code=abc"), "Shinylive")
  expect_error(preview_shinylive_link("not a url"), "Shinylive")
})

test_that("a webR URL with no code fragment is rejected", {
  expect_error(preview_webr_link("https://webr.r-wasm.org/latest/#flags=jz"))
})

# ---- URL parsing happy paths ----------------------------------------------

test_that("version and panels are parsed back out of a URL", {
  url <- as.character(
    webr_repl_link("x <- 1", version = "v0.5.4", panels = c("editor", "plot"))
  )

  preview <- preview_webr_link(url)

  expect_equal(preview$version, "v0.5.4")
  expect_equal(preview$mode, c("editor", "plot"))
})

test_that("Shinylive mode and engine are parsed back out of a URL", {
  preview <- preview_shinylive_link(as.character(shinylive_r_link("library(shiny)", mode = "app")))

  expect_equal(preview$mode, "app")
  expect_equal(preview$engine, "r")
})
