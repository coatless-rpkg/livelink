# Conformance, not round-tripping.
#
# Every other decode test reads a link back with livelink's own decoder, so an
# encoder bug that the decoder mirrors passes them all. These tests take a link
# apart with base R only -- URLdecode(), base64enc, memDecompress(), jsonlite --
# the way webR's `decodeShareData` does, so what actually travels on the wire is
# checked against webR's format rather than against ourselves.
#
# webR's decoder (src/repl/components/Share.tsx) does:
#   decodeURIComponent(data) -> base64ToBuffer -> pako inflate -> msgpack/JSON
# and reads the flags after "&": u uncompressed, z compressed, m msgpack,
# j JSON, a autorun.

# Pull a link apart exactly as webR does, using nothing from livelink.
dissect_webr_url <- function(url) {
  fragment <- sub("^.*#code=", "", url)
  parts <- strsplit(fragment, "&", fixed = TRUE)[[1]]

  encoded <- parts[1]
  flags <- if (length(parts) > 1) parts[2] else ""

  compressed <- base64enc::base64decode(utils::URLdecode(encoded))

  payload <- if (grepl("z", flags, fixed = TRUE)) {
    memDecompress(compressed, type = "gzip")
  } else {
    compressed
  }

  list(flags = flags, compressed = compressed, payload = payload)
}

test_that("the fragment is percent-encoded base64, as webR expects", {
  url <- as.character(webr_repl_link("plot(1:10)"))
  encoded <- sub("&.*$", "", sub("^.*#code=", "", url))

  # webR runs decodeURIComponent() on this, so the reserved characters base64
  # produces must be escaped or the fragment is corrupted in transit.
  expect_false(grepl("[+/]", encoded))
  expect_no_error(base64enc::base64decode(utils::URLdecode(encoded)))
})

test_that("the payload is zlib, the container pako inflate() reads", {
  parts <- dissect_webr_url(as.character(webr_repl_link("plot(1:10)")))

  # R's memCompress(type = "gzip") emits RFC 1950 (zlib), not RFC 1952 (gzip),
  # which is the same container pako's deflate() writes. The header says which:
  # zlib starts 0x78, gzip starts 0x1f 0x8b.
  expect_equal(parts$compressed[[1]], as.raw(0x78))
  expect_false(identical(parts$compressed[1:2], as.raw(c(0x1f, 0x8b))))
})

test_that("the flags describe what the payload actually is", {
  parts <- dissect_webr_url(as.character(webr_repl_link("plot(1:10)")))

  expect_match(parts$flags, "m") # msgpack, as webR's own share button writes
  expect_match(parts$flags, "z") # compressed

  # Unpacked without going through livelink's decoder. R has no scalar type, so
  # the thing worth checking is that each field arrived as a single value rather
  # than a one-element array, which webR would not read.
  items <- RcppMsgPack::msgpack_unpack(parts$payload, simplify = TRUE)

  expect_true(is.list(items))
  expect_true(all(c("name", "path", "text") %in% names(items[[1]])))
  expect_equal(items[[1]][["name"]], "script.R")
  expect_equal(items[[1]][["text"]], "plot(1:10)")
})

test_that("the autorun flag is present only when something autoruns", {
  off <- dissect_webr_url(as.character(webr_repl_link("plot(1:10)")))
  on <- dissect_webr_url(as.character(webr_repl_link("plot(1:10)", autorun = TRUE)))

  expect_false(grepl("a", off$flags, fixed = TRUE))
  expect_true(grepl("a", on$flags, fixed = TRUE))
})

test_that("a multi-file project travels as an array of file objects", {
  url <- as.character(webr_repl_project(
    list("main.R" = "source('utils.R')", "utils.R" = "f <- function() 42")
  ))

  items <- RcppMsgPack::msgpack_unpack(dissect_webr_url(url)$payload,
                                       simplify = TRUE)

  expect_length(items, 2)
  expect_equal(
    vapply(items, function(x) x[["name"]], character(1)),
    c("main.R", "utils.R")
  )
  # webR writes each file to this path in its virtual filesystem.
  expect_match(items[[1]][["path"]], "^/home/web_user/")
})

test_that("non-ASCII source survives the wire intact", {
  code <- "x <- 'café 你好'"
  url <- as.character(webr_repl_link(code))

  items <- RcppMsgPack::msgpack_unpack(dissect_webr_url(url)$payload,
                                       simplify = TRUE)

  expect_equal(items[[1]][["text"]], code)
})
