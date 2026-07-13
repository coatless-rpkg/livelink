test_that("set_webr_base_url sets and resets the global option", {
  withr::local_options(livelink.base_url = NULL)

  set_webr_base_url("https://example.org/webr/")
  expect_equal(getOption("livelink.base_url"), "https://example.org/webr/")

  set_webr_base_url(NULL)
  expect_null(getOption("livelink.base_url"))
})

test_that("set_webr_base_url returns its value invisibly", {
  withr::local_options(livelink.base_url = NULL)

  expect_invisible(set_webr_base_url("https://example.org/webr/"))
  expect_equal(withVisible(set_webr_base_url("https://example.org/webr/"))$value,
               "https://example.org/webr/")
})

test_that("set_webr_base_url validates its input", {
  withr::local_options(livelink.base_url = NULL)

  expect_error(set_webr_base_url(42))
  expect_error(set_webr_base_url(c("a", "b")))
})

test_that("link functions honor the global base URL option", {
  withr::local_options(livelink.base_url = "https://example.org/webr/")

  expect_match(as.character(webr_repl_link("plot(1:10)")),
               "^https://example\\.org/webr/")
  expect_match(as.character(webr_repl_project(list("a.R" = "x <- 1"))),
               "^https://example\\.org/webr/")
})

test_that("the default base URL is used when no option is set", {
  withr::local_options(livelink.base_url = NULL)

  expect_match(as.character(webr_repl_link("plot(1:10)")),
               "^https://webr\\.r-wasm\\.org/")
})

test_that("an explicit base_url argument beats the global option", {
  withr::local_options(livelink.base_url = "https://option.example/webr/")

  url <- as.character(webr_repl_link("x <- 1", base_url = "https://arg.example/webr/"))

  expect_match(url, "^https://arg\\.example/webr/")
})
