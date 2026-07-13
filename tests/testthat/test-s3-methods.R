sample_objects <- function() {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines("plot(1:10)", file.path(tmp, "one.R"))
  app <- file.path(tmp, "app_one")
  dir.create(app)
  writeLines("# app", file.path(app, "app.R"))

  webr_url <- as.character(webr_repl_link("plot(1:10)"))
  shiny_url <- as.character(shinylive_r_link("library(shiny)"))
  out <- withr::local_tempdir(.local_envir = parent.frame())

  list(
    webr_link       = webr_repl_link("plot(1:10)"),
    webr_project    = webr_repl_project(list("a.R" = "x <- 1")),
    webr_exercise   = webr_repl_exercise("# todo", "x <- 1", "ex"),
    webr_directory  = suppressMessages(webr_repl_directory(tmp)),
    webr_preview    = preview_webr_link(webr_url),
    webr_decoded    = suppressMessages(decode_webr_link(webr_url, output_dir = out, overwrite = TRUE)),
    shinylive_link      = shinylive_r_link("library(shiny)"),
    shinylive_project   = shinylive_project(list("app.R" = "library(shiny)"), engine = "r"),
    shinylive_directory = suppressMessages(shinylive_directory(tmp, engine = "r")),
    shinylive_preview   = preview_shinylive_link(shiny_url),
    shinylive_decoded   = suppressMessages(
      decode_shinylive_link(shiny_url, output_dir = out, overwrite = TRUE)
    )
  )
}

test_that("every class has a print method that returns its input invisibly", {
  objs <- sample_objects()

  for (nm in names(objs)) {
    x <- objs[[nm]]

    # print() must dispatch to a real method, not print.default.
    expect_true(
      !is.null(utils::getS3method("print", class(x)[1], optional = TRUE)),
      info = paste("no print method for class", nm)
    )

    expect_invisible(suppressMessages(print(x)))
    expect_identical(suppressMessages(print(x)), x, info = paste("print method for", nm))
  }
})

test_that("repl_urls works for every link-bearing class", {
  objs <- sample_objects()

  for (nm in names(objs)) {
    urls <- repl_urls(objs[[nm]])

    expect_type(urls, "character")
    expect_true(all(grepl("^https://", urls)), info = paste("repl_urls for", nm))
  }
})

test_that("as.character works for every class", {
  objs <- sample_objects()

  for (nm in names(objs)) {
    txt <- as.character(objs[[nm]])

    expect_type(txt, "character")
    expect_true(all(grepl("^https://", txt)), info = paste("as.character for", nm))
  }
})

test_that("repl_urls rejects unsupported objects", {
  expect_error(repl_urls(1:10), "Cannot extract URLs")
  expect_error(repl_urls("https://example.org"), "Cannot extract URLs")
})

test_that("print.webr_preview honors show_content", {
  url <- as.character(webr_repl_link("secret_marker <- 42"))
  preview <- preview_webr_link(url)

  expect_snapshot(print(preview))
  expect_snapshot(print(preview, show_content = TRUE))
})

test_that("print.shinylive_preview honors show_content", {
  url <- as.character(shinylive_r_link("secret_marker <- 42"))
  preview <- preview_shinylive_link(url)

  expect_snapshot(print(preview))
  expect_snapshot(print(preview, show_content = TRUE))
})

test_that("print methods render without leaking glue templates", {
  objs <- sample_objects()

  for (nm in names(objs)) {
    out <- paste(
      utils::capture.output(suppressMessages(print(objs[[nm]]))),
      collapse = "\n"
    )
    # An unevaluated {expr} in the output means cli globbed a template.
    expect_false(grepl("\\{[^}]*\\(", out), info = paste("glue template leaked in", nm))
  }
})
