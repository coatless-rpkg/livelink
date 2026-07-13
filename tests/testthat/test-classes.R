# Tests for S3 class methods

# webr_link class tests
test_that("webr_link has correct structure", {
  result <- webr_repl_link("x <- 1")

  expect_s3_class(result, "webr_link")
  expect_named(result, c("url", "filename", "path", "mode", "version", "autorun"))
})

test_that("print.webr_link returns invisibly", {
  result <- webr_repl_link("x <- 1")

  expect_invisible(print(result))
  expect_identical(print(result), result)
})

test_that("as.character.webr_link returns URL", {
  result <- webr_repl_link("x <- 1")

  expect_equal(as.character(result), result$url)
})

# webr_project class tests
test_that("webr_project has correct structure", {
  skip_on_cran()
  # Create temp files for testing

  temp_dir <- tempfile("project_test")
  dir.create(temp_dir)
  writeLines("# main code", file.path(temp_dir, "main.R"))
  writeLines("# utils code", file.path(temp_dir, "utils.R"))

  files <- c(file.path(temp_dir, "main.R"), file.path(temp_dir, "utils.R"))
  result <- webr_repl_project(files)

  expect_s3_class(result, "webr_project")
  expect_named(result, c("url", "files", "base_path", "mode", "version", "autorun_files"))

  unlink(temp_dir, recursive = TRUE)
})

test_that("print.webr_project returns invisibly", {
  skip_on_cran()
  temp_dir <- tempfile("project_print")
  dir.create(temp_dir)
  writeLines("# code", file.path(temp_dir, "main.R"))

  result <- webr_repl_project(file.path(temp_dir, "main.R"))

  expect_invisible(print(result))
  expect_identical(print(result), result)

  unlink(temp_dir, recursive = TRUE)
})

test_that("as.character.webr_project returns URL", {
  skip_on_cran()
  temp_dir <- tempfile("project_char")
  dir.create(temp_dir)
  writeLines("# code", file.path(temp_dir, "main.R"))

  result <- webr_repl_project(file.path(temp_dir, "main.R"))

  expect_equal(as.character(result), result$url)

  unlink(temp_dir, recursive = TRUE)
})

# webr_exercise class tests
test_that("webr_exercise has correct structure", {
  result <- livelink:::webr_repl_exercise("# Exercise", "# Solution", "test")

  expect_s3_class(result, "webr_exercise")
  expect_named(result, c("exercise", "solution"))
  expect_s3_class(result$exercise, "webr_link")
  expect_s3_class(result$solution, "webr_link")
})

test_that("webr_exercise creates correct filenames", {
  result <- livelink:::webr_repl_exercise("ex", "sol", "stats")

  expect_equal(result$exercise$filename, "stats_exercise.R")
  expect_equal(result$solution$filename, "stats_solution.R")
})

test_that("webr_exercise solution has autorun enabled", {
  result <- livelink:::webr_repl_exercise("ex", "sol", "test")

  expect_false(result$exercise$autorun)
  expect_true(result$solution$autorun)
})

test_that("print.webr_exercise returns invisibly", {
  result <- livelink:::webr_repl_exercise("ex", "sol", "test")

  expect_invisible(print(result))
  expect_identical(print(result), result)
})

# shinylive_link class tests
test_that("shinylive_link has correct structure", {
  result <- shinylive_r_link("shinyApp(fluidPage(), function(i, o) {})")

  expect_s3_class(result, "shinylive_link")
  expect_named(result, c("url", "files", "engine", "mode"))
})

test_that("print.shinylive_link returns invisibly", {
  result <- shinylive_r_link("shinyApp(fluidPage(), function(i, o) {})")

  expect_invisible(print(result))
})

test_that("as.character.shinylive_link returns URL", {
  result <- shinylive_r_link("shinyApp(fluidPage(), function(i, o) {})")

  expect_equal(as.character(result), result$url)
})

# shinylive_project class tests
test_that("shinylive_project has correct structure", {
  skip_on_cran()
  temp_dir <- tempfile("shinylive_proj")
  dir.create(temp_dir)
  writeLines("# app code", file.path(temp_dir, "app.R"))
  writeLines("# helpers", file.path(temp_dir, "utils.R"))

  files <- c(file.path(temp_dir, "app.R"), file.path(temp_dir, "utils.R"))
  result <- shinylive_project(files, engine = "r")

  expect_s3_class(result, "shinylive_project")
  expect_named(result, c("url", "files", "engine", "mode"))

  unlink(temp_dir, recursive = TRUE)
})

test_that("print.shinylive_project returns invisibly", {
  skip_on_cran()
  temp_dir <- tempfile("shinylive_print")
  dir.create(temp_dir)
  writeLines("# app code", file.path(temp_dir, "app.R"))

  result <- shinylive_project(file.path(temp_dir, "app.R"), engine = "r")

  expect_invisible(print(result))
  expect_identical(print(result), result)

  unlink(temp_dir, recursive = TRUE)
})

test_that("as.character.shinylive_project returns URL", {
  skip_on_cran()
  temp_dir <- tempfile("shinylive_char")
  dir.create(temp_dir)
  writeLines("# app code", file.path(temp_dir, "app.R"))

  result <- shinylive_project(file.path(temp_dir, "app.R"), engine = "r")

  expect_equal(as.character(result), result$url)

  unlink(temp_dir, recursive = TRUE)
})

# webr_preview class tests
test_that("webr_preview has correct structure", {
  link <- webr_repl_link("x <- 1")
  result <- preview_webr_link(link$url)

  expect_s3_class(result, "webr_preview")
  expect_true("url" %in% names(result))
  expect_true("files_data" %in% names(result))
  expect_true("total_files" %in% names(result))
  expect_true("total_size" %in% names(result))
})

test_that("print.webr_preview returns invisibly", {
  link <- webr_repl_link("x <- 1")
  result <- preview_webr_link(link$url)

  expect_invisible(print(result))
  expect_identical(print(result), result)
})

# repl_urls generic tests
test_that("repl_urls works for all link types", {
  webr_link <- webr_repl_link("x <- 1")
  shinylive_link <- shinylive_r_link("shinyApp(fluidPage(), function(i, o) {})")

  expect_type(repl_urls(webr_link), "character")
  expect_type(repl_urls(shinylive_link), "character")

  expect_equal(repl_urls(webr_link), webr_link$url)
  expect_equal(repl_urls(shinylive_link), shinylive_link$url)
})
