test_that("simple_hash returns a fixed-width hex string for short input", {
  h <- simple_hash("hello world")

  expect_type(h, "character")
  expect_length(h, 1)
  expect_equal(nchar(h), 8)
  expect_match(h, "^[0-9a-f]+$")
})

test_that("simple_hash honors the length argument", {
  expect_equal(nchar(simple_hash("hello world", length = 4)), 4)
  expect_equal(nchar(simple_hash("hello world", length = 16)), 16)
})

test_that("simple_hash is deterministic", {
  expect_identical(simple_hash("abc"), simple_hash("abc"))
  expect_false(identical(simple_hash("abc"), simple_hash("abd")))
})

# Regression: the weighted sum overflowed .Machine$integer.max and was silently
# promoted to double, which sprintf("%x", .) rejects. This broke decoding for
# any sharelink built from a non-trivial script.
test_that("simple_hash does not overflow on long input", {
  for (n in c(8000, 20000, 100000)) {
    big <- strrep("x", n)

    expect_match(simple_hash(big), "^[0-9a-f]{8}$")
  }
})

test_that("simple_hash handles empty and unicode input", {
  expect_match(simple_hash(""), "^[0-9a-f]{8}$")
  expect_match(simple_hash("héllo 你好"), "^[0-9a-f]{8}$")
})

test_that("format_file_size renders bytes, KB, and MB", {
  expect_equal(format_file_size(0), "0 bytes")
  expect_equal(format_file_size(512), "512 bytes")
  expect_equal(format_file_size(2048), "2 KB")
  expect_equal(format_file_size(2 * 1024^2), "2 MB")
})
