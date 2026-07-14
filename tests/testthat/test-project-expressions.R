# Multi-file projects can name each file's contents as a braced expression rather
# than a string full of escaped newlines.

contents_of <- function(link) {
  preview <- preview_webr_link(as.character(link))
  stats::setNames(
    lapply(preview$files_data, function(f) f$text),
    vapply(preview$files_data, function(f) f$name, character(1))
  )
}

test_that("a project accepts braced expressions as file contents", {
  link <- webr_repl_project(list(
    "main.R"  = { source("utils.R"); summarise(mtcars) },
    "utils.R" = { summarise <- function(d) summary(d) }
  ))

  files <- contents_of(link)

  expect_setequal(names(files), c("main.R", "utils.R"))
  expect_match(files[["main.R"]], 'source("utils.R")', fixed = TRUE)
  expect_match(files[["main.R"]], "summarise(mtcars)", fixed = TRUE)
  expect_match(files[["utils.R"]], "summarise <- function(d) summary(d)",
               fixed = TRUE)
  # The braces themselves must not travel into the shared file.
  expect_false(startsWith(trimws(files[["main.R"]]), "{"))
})

test_that("expressions and strings can be mixed in one project", {
  link <- webr_repl_project(list(
    "main.R"    = { plot(1:10) },
    "README.md" = "# Analysis\n\nRun main.R to start."
  ))

  files <- contents_of(link)

  expect_equal(files[["main.R"]], "plot(1:10)")
  expect_equal(files[["README.md"]], "# Analysis\n\nRun main.R to start.")
})

# The braces must never be evaluated: `{ x <- 1 }` inside list() would otherwise
# run in the caller's environment and leak the binding.
test_that("braced file contents are not evaluated", {
  marker_env <- environment()

  webr_repl_project(list(
    "a.R" = { leaked_marker <- 99 }
  ))

  expect_false(exists("leaked_marker", envir = marker_env, inherits = FALSE))
})

test_that("values are still evaluated when they are not braces", {
  code <- "x <- 1"
  helper <- "y <- 2"

  link <- webr_repl_project(list("a.R" = code, "b.R" = helper))
  files <- contents_of(link)

  expect_equal(files[["a.R"]], "x <- 1")
  expect_equal(files[["b.R"]], "y <- 2")
})

test_that("a list held in a variable still works", {
  project <- list("a.R" = "x <- 1", "b.R" = "y <- 2")

  link <- webr_repl_project(project)

  expect_setequal(names(contents_of(link)), c("a.R", "b.R"))
})

test_that("file paths still work", {
  dir <- withr::local_tempdir()
  writeLines("x <- 1", file.path(dir, "a.R"))
  writeLines("y <- 2", file.path(dir, "b.R"))

  link <- webr_repl_project(c(file.path(dir, "a.R"), file.path(dir, "b.R")))

  expect_setequal(names(contents_of(link)), c("a.R", "b.R"))
})

test_that("autorun_files still matches a braced file", {
  link <- webr_repl_project(
    list("main.R" = { plot(1:10) }, "utils.R" = { f <- function() 42 }),
    autorun_files = "main.R"
  )

  expect_equal(preview_webr_link(as.character(link))$autorun_files, "main.R")
})

test_that("an unnamed list is still rejected", {
  expect_error(webr_repl_project(list({ plot(1:10) })), "named list")
})

# `list()` is an ordinary call, so assigning it to a variable first evaluates the
# blocks -- livelink never sees the source, and receives whatever they returned.
# Say so, rather than encoding a function into a link.
test_that("a list whose values are not strings is rejected with a useful error", {
  assigned <- list("a.R" = function() 42)

  expect_error(webr_repl_project(assigned), "must be a single string")
  expect_error(webr_repl_project(assigned), "inside the call")
})

test_that("the error names the offending file", {
  err <- tryCatch(
    webr_repl_project(list("ok.R" = "x <- 1", "bad.R" = 1:10)),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "bad.R", fixed = TRUE)
  expect_false(grepl("ok.R", err, fixed = TRUE))
})

# ---- Shinylive ------------------------------------------------------------

test_that("shinylive_project accepts braced expressions", {
  link <- shinylive_project(
    list(
      "app.R"   = { shinyApp(ui, server) },
      "utils.R" = { helper <- function() 42 }
    ),
    engine = "r"
  )

  expect_s3_class(link, "shinylive_project")
  expect_setequal(link$files, c("app.R", "utils.R"))

  files <- preview_shinylive_link(as.character(link))$files_data
  named <- stats::setNames(lapply(files, function(f) f$content),
                           vapply(files, function(f) f$name, character(1)))
  expect_equal(named[["utils.R"]], "helper <- function() 42")
})

test_that("shinylive_r_link accepts braced expressions in a named list", {
  link <- shinylive_r_link(list(
    "app.R"   = { shinyApp(ui, server) },
    "utils.R" = { helper <- function() 42 }
  ))

  expect_setequal(link$files, c("app.R", "utils.R"))
})
