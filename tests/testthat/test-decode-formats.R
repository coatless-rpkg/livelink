# livelink always WRITES json+gzip ("jz"), so every round-trip test only proves we
# can read links we made ourselves. webR's own native wire format is msgpack+zlib
# ("mz"). These tests build links in the formats webR emits and prove we can read
# them -- i.e. that livelink can decode a link produced by webR itself.

webr_url <- function(payload, flags) {
  encoded <- utils::URLencode(base64enc::base64encode(payload), reserved = TRUE)
  paste0("https://webr.r-wasm.org/latest/#code=", encoded, "&", flags)
}

msgpack_webr_url <- function(files, flags = "mz") {
  packed <- RcppMsgPack::msgpack_pack(files)
  payload <- if (grepl("z", flags)) memCompress(packed, type = "gzip") else packed
  webr_url(payload, flags)
}

json_webr_url <- function(files, flags = "jz") {
  json <- jsonlite::toJSON(files, auto_unbox = TRUE)
  payload <- if (grepl("z", flags)) {
    memCompress(charToRaw(json), type = "gzip")
  } else {
    charToRaw(json)
  }
  webr_url(payload, flags)
}

one_file <- list(list(
  name = "script.R",
  path = "/home/web_user/script.R",
  text = "# a comment\nplot(1:10)"
))

test_that("a msgpack link (webR's native 'mz' format) can be previewed", {
  preview <- preview_webr_link(msgpack_webr_url(one_file))

  expect_s3_class(preview, "webr_preview")
  expect_equal(preview$total_files, 1)
  expect_equal(preview$files_data[[1]]$name, "script.R")
  expect_equal(preview$files_data[[1]]$text, "# a comment\nplot(1:10)")
})

test_that("a msgpack link can be decoded to disk", {
  dir <- withr::local_tempdir()

  suppressMessages(
    decode_webr_link(msgpack_webr_url(one_file), output_dir = dir, overwrite = TRUE)
  )

  path <- list.files(dir, recursive = TRUE, full.names = TRUE)
  expect_length(path, 1)
  expect_equal(
    paste(readLines(path, warn = FALSE), collapse = "\n"),
    "# a comment\nplot(1:10)"
  )
})

test_that("a multi-file msgpack link round-trips", {
  files <- list(
    list(name = "main.R", path = "/home/web_user/main.R", text = "source('utils.R')"),
    list(name = "utils.R", path = "/home/web_user/utils.R", text = "f <- function() 42")
  )
  dir <- withr::local_tempdir()

  suppressMessages(
    decode_webr_link(msgpack_webr_url(files), output_dir = dir, overwrite = TRUE)
  )

  written <- list.files(dir, recursive = TRUE)
  expect_setequal(basename(written), c("main.R", "utils.R"))
})

test_that("uncompressed links are decoded", {
  # 'ju' = json, uncompressed
  preview <- preview_webr_link(json_webr_url(one_file, flags = "ju"))

  expect_equal(preview$files_data[[1]]$text, "# a comment\nplot(1:10)")
})

test_that("the autorun flag is recovered from the URL", {
  autorun_file <- list(list(
    name = "script.R",
    path = "/home/web_user/script.R",
    text = "plot(1:10)",
    autorun = TRUE
  ))

  preview <- preview_webr_link(json_webr_url(autorun_file, flags = "jza"))

  expect_equal(preview$autorun_files, "script.R")
})

test_that("a corrupt payload produces a readable error, not an internal one", {
  bad <- paste0(
    "https://webr.r-wasm.org/latest/#code=",
    utils::URLencode(base64enc::base64encode(charToRaw("not gzipped at all")), reserved = TRUE),
    "&jz"
  )

  err <- tryCatch(
    suppressWarnings(preview_webr_link(bad)),
    error = function(e) conditionMessage(e)
  )

  # Must not surface `object 'raw_data' not found` -- the decompression fallback
  # used to be assigned inside a tryCatch error handler, so it was discarded and
  # the graceful path died with an internal error instead.
  expect_false(grepl("raw_data", err))
  expect_false(grepl("not found", err))
  expect_match(err, "webR link|decompress|corrupt|JSON")
})

test_that("a URL with no code fragment is rejected", {
  expect_error(preview_webr_link("https://webr.r-wasm.org/latest/"), class = "rlang_error")
})

test_that("a non-webR URL is rejected", {
  expect_error(preview_webr_link("https://example.com/nope"))
})

# The decoder must hand back the same shape whichever serialization a link
# used, so nothing downstream has to know or ask. msgpack arrives as
# RcppMsgPack's key/value structure and JSON as a plain list;
# normalize_msgpack_data() reconciles them at the boundary. This matters more
# since livelink began writing msgpack: its own links now travel that path.
test_that("msgpack and JSON links decode to the same shape", {
  skip_if_not_installed("RcppMsgPack")

  items <- list(list(
    name = "script.R",
    path = "/home/web_user/script.R",
    text = "x <- 1 # hi",
    autorun = TRUE
  ))

  as_url <- function(payload, flags) {
    paste0(
      "https://webr.r-wasm.org/latest/#code=",
      utils::URLencode(
        base64enc::base64encode(memCompress(payload, "gzip")),
        reserved = TRUE
      ),
      "&", flags
    )
  }

  from_json <- preview_webr_link(as_url(
    charToRaw(jsonlite::toJSON(items, auto_unbox = TRUE)), "jza"
  ))
  from_msgpack <- preview_webr_link(as_url(
    RcppMsgPack::msgpack_pack(items), "mza"
  ))

  # Field access must work the same way for both: a plain list, reachable with
  # `$`, not a named vector that would need [[ ]].
  expect_type(from_json$files_data[[1]], "list")
  expect_type(from_msgpack$files_data[[1]], "list")

  for (field in c("name", "path", "text")) {
    expect_equal(from_msgpack$files_data[[1]][[field]],
                 from_json$files_data[[1]][[field]])
  }

  expect_equal(from_msgpack$autorun_files, from_json$autorun_files)
})
