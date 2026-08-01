test_that("is_likely_file_path only accepts paths that actually exist", {
  tmp <- withr::local_tempdir()
  real <- file.path(tmp, "real.R")
  writeLines("plot(1:10)", real)

  expect_true(is_likely_file_path(real))

  # Code, not paths -- these must never be treated as files.
  expect_false(is_likely_file_path("x <- 1/2"))
  expect_false(is_likely_file_path("mean(x)/n"))
  expect_false(is_likely_file_path("df$col.name"))
  expect_false(is_likely_file_path("plot(1:10)"))

  # A plausible-but-missing filename stays a path, so the user gets a
  # "not found" error instead of silently sharing the literal string as code.
  expect_true(is_likely_file_path("nonexistent_file.R"))
})

# Regression: any single-line string containing "/" or a trailing ".ext" was
# classified as a file path, so ordinary code aborted instead of encoding.
test_that("code containing / or a dot-suffix is treated as code, not a path", {
  code <- c(
    "x <- 1/2",
    "mean(x)/n",
    "df$col.name",
    "ratio <- hits/total",
    "p <- ggplot(d) + facet_wrap(~a/b)"
  )

  for (snippet in code) {
    link <- webr_repl_link(snippet)

    expect_s3_class(link, "webr_link")
  }
})

test_that("a missing file path produces a readable error", {
  # The abort message must actually render -- it previously crashed cli with
  # "Cannot pluralize without a quantity".
  expect_error(
    locate_input(c("no_such_file.R", "also_missing.R")),
    "not found"
  )
  expect_error(locate_input("no_such_file.R"), "not found")
})

test_that("locate_input classifies each input type", {
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "script.R")
  writeLines("plot(1:10)", f)

  expect_equal(locate_input(f), "path")
  expect_equal(locate_input("plot(1:10)"), "input")
  expect_equal(locate_input(list("a.R" = "x <- 1")), "input")
  expect_equal(locate_input(x_expr = quote({ plot(1:10) })), "expr")
  expect_equal(locate_input(NULL), "clipboard")
})

test_that("process_input reads a single file path", {
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "script.R")
  writeLines(c("x <- 1", "y <- 2"), f)

  expect_equal(process_input(f), "x <- 1\ny <- 2")
})

# Regression: process_input() serves the single-script functions, and it used
# to answer a vector of paths with a named list. Nothing could use that: a
# project routes to process_project_input() before reaching here, so the list
# only ever reached webr_repl_link(), which dropped it into one file's `text`
# and emitted a JSON object where webR needs a string -- a link that looked
# fine and would not open. Refuse it instead, and name the function that does
# take several files.
test_that("process_input refuses multiple file paths for a single script", {
  tmp <- withr::local_tempdir()
  a <- file.path(tmp, "a.R")
  b <- file.path(tmp, "b.R")
  writeLines("x <- 1", a)
  writeLines("y <- 2", b)

  expect_error(process_input(c(a, b)), "Multiple file paths")
  expect_error(webr_repl_link(c(a, b)), "webr_repl_project")

  # One path is still the ordinary case.
  expect_equal(process_input(a), "x <- 1")
})

test_that("expression input is converted to source code", {
  link <- webr_repl_link({
    x <- 1
    plot(x)
  })

  expect_s3_class(link, "webr_link")

  code <- preview_webr_link(as.character(link))$files_data[[1]]$text

  expect_match(code, "x <- 1")
  expect_match(code, "plot(x)", fixed = TRUE)
  # The wrapping braces must not survive into the shared source.
  expect_false(startsWith(trimws(code), "{"))
})

test_that("expression input preserves comments when source refs are available", {
  # Comments only survive parsing when keep.source = TRUE, so parse explicitly
  # rather than depending on the session default.
  expr <- parse(
    text = "{\n  # a leading comment\n  x <- 1 # a trailing comment\n}",
    keep.source = TRUE
  )[[1]]

  code <- paste(stringify_expression(expr), collapse = "\n")

  expect_match(code, "# a leading comment")
  expect_match(code, "# a trailing comment")
  expect_false(startsWith(trimws(code), "{"))
  # Common leading indentation is trimmed.
  expect_false(grepl("^\\s+#", strsplit(code, "\n")[[1]][1]))
})

# Regression: the trailing-comment rescue scanned forward to Inf looking for a
# line starting with `}`. A one-line brace nested in a call -- whose own `}` is
# not on a line of its own -- therefore swallowed every source line down to the
# next such line, shipping unrelated code into the link.
test_that("a one-line brace does not swallow the lines after it", {
  expr <- parse(
    text = paste(
      'f(list(',
      '  "main.R" = { plot(1:10) },',
      '  "other"  = "not code"',
      '))',
      sep = "\n"
    ),
    keep.source = TRUE
  )[[1]]

  brace <- expr[[2]][[2]]
  code <- paste(stringify_expression(brace), collapse = "\n")

  expect_equal(code, "plot(1:10)")
  expect_false(grepl("not code", code, fixed = TRUE))
  expect_false(grepl("main.R", code, fixed = TRUE))
})

# Regression: with keep.source = FALSE (the default under Rscript and R CMD
# check) there are no srcrefs, and the deparse fallback shipped the wrapping
# braces into the shared script.
test_that("expression input strips braces even without source refs", {
  expr <- parse(text = "{\n  x <- 1\n  plot(x)\n}", keep.source = FALSE)[[1]]

  code <- paste(stringify_expression(expr), collapse = "\n")

  expect_false(startsWith(trimws(code), "{"))
  expect_false(grepl("}", code, fixed = TRUE))
  expect_match(code, "x <- 1")
  expect_match(code, "plot(x)", fixed = TRUE)
})

# Regression: srcrefs can exist while the source text they point at cannot be
# read back. Positron's Run button parses the buffer against a plain srcfile
# whose filename ("untitled:Untitled-1") is not a path and whose lines are not
# cached, so the trailing-comment rescue indexed a NULL srcfile$lines and
# aborted with "argument is of length zero". Guarding that alone is not enough:
# as.character(useSource = TRUE) yields a "<srcref: ...>" placeholder rather
# than code, so an unreadable srcfile has to fall back to deparsing.
test_that("an expression whose source cannot be read back is deparsed", {
  expr <- parse(
    text = "f({\n  data(mtcars)\n  plot(mtcars$mpg, mtcars$wt)\n})",
    keep.source = TRUE,
    srcfile = srcfile("untitled:Untitled-1")
  )[[1]]

  brace <- expr[[2]]
  code <- paste(stringify_expression(brace), collapse = "\n")

  expect_match(code, "data(mtcars)", fixed = TRUE)
  expect_match(code, "plot(mtcars$mpg, mtcars$wt)", fixed = TRUE)
  # The placeholder as.character() returns when it cannot reach the source.
  expect_false(grepl("srcref", code, fixed = TRUE))
  expect_false(startsWith(trimws(code), "{"))
})

# Regression: the local getSrcLines() read srcfile$lines directly, which is
# NULL for a srcfile backed by a real file on disk (base reads the file
# instead). Comments were then silently never rescued, and an out-of-range
# start produced a descending index, returning NAs and unrelated lines.
test_that("source lines are read from a srcfile backed by a real file", {
  path <- withr::local_tempfile(fileext = ".R")
  writeLines(c("f({", "  x <- 1 # kept", "})"), path)

  # srcfile() does not cache the lines; base reads them from the file on
  # demand, so srcfile$lines stays NULL even though the source is available.
  expr <- parse(text = readLines(path), keep.source = TRUE,
                srcfile = srcfile(path))[[1]]
  code <- paste(stringify_expression(expr[[2]]), collapse = "\n")

  expect_match(code, "x <- 1 # kept", fixed = TRUE)
})

# Regression: readability is not all-or-nothing. A srcfile whose file is
# shorter than the block it was parsed from -- a stale or truncated file --
# still reads back line 1, so the deparse fallback does not engage, and the
# trailing-comment rescue then asked for a line past EOF and aborted with
# "argument is of length zero".
test_that("a srcfile shorter than the block does not abort", {
  path <- withr::local_tempfile(fileext = ".R")
  buffer <- c("f({", "  x <- 1 # trail", "  y <- 2", "})")
  writeLines(utils::head(buffer, 2), path) # on disk: shorter than what we parse

  expr <- parse(text = buffer, keep.source = TRUE, srcfile = srcfile(path))[[1]]
  code <- paste(stringify_expression(expr[[2]]), collapse = "\n")

  expect_match(code, "x <- 1", fixed = TRUE)
})

# Regression: a block with no statements left nothing behind once the brace
# was stripped, and `lines[length(lines)]` handed regexpr() a zero-length
# pattern: "invalid 'pattern' argument". `webr_repl_link({ # TODO })` is an
# ordinary thing to type.
test_that("an empty or comment-only block does not abort", {
  empty <- parse(text = "f({})", keep.source = TRUE)[[1]]
  expect_no_error(stringify_expression(empty[[2]]))

  todo <- parse(text = "f({\n  # TODO: paste code here\n})", keep.source = TRUE)[[1]]
  expect_no_error(code <- stringify_expression(todo[[2]]))
  expect_match(paste(code, collapse = "\n"), "# TODO", fixed = TRUE)
})

# Regression: only a `{ ... }` block means "this argument is source code".
# is.call() was true for ANY call, so a computed value like paste0(...) or
# readLines(f) got deparsed and the call itself was shared as the script.
test_that("a computed value is evaluated, not deparsed as source", {
  link <- webr_repl_link(paste0("plot(", "1:10", ")"))
  code <- preview_webr_link(as.character(link))$files_data[[1]]$text

  expect_equal(code, "plot(1:10)")
  expect_false(grepl("paste0", code, fixed = TRUE))
})

test_that("a computed value is evaluated for Shinylive too", {
  link <- shinylive_r_link(paste0("library(", "shiny", ")"))
  code <- preview_shinylive_link(as.character(link))$files_data[[1]]$content

  expect_equal(code, "library(shiny)")
  expect_false(grepl("paste0", code, fixed = TRUE))
})

# Regression: named-list input was documented for the Shinylive link functions
# but process_input() aborted on any multi-element input, so it never worked.
test_that("shinylive link functions accept a named list of files", {
  files <- list(
    "app.R" = "library(shiny)",
    "utils.R" = "helper <- function() 42"
  )

  link <- shinylive_r_link(files)

  expect_s3_class(link, "shinylive_link")
  expect_setequal(link$files, c("app.R", "utils.R"))

  preview <- preview_shinylive_link(as.character(link))
  expect_equal(preview$total_files, 2)
})

test_that("shinylive_py_link accepts a named list of files", {
  link <- shinylive_py_link(list(
    "app.py" = "from shiny import App",
    "utils.py" = "def helper(): return 42"
  ))

  expect_setequal(link$files, c("app.py", "utils.py"))
})

test_that("shinylive link functions accept several file paths", {
  tmp <- withr::local_tempdir()
  app <- file.path(tmp, "app.R")
  helper <- file.path(tmp, "helper.R")
  writeLines("library(shiny)", app)
  writeLines("# helper", helper)

  link <- shinylive_r_link(c(app, helper))

  expect_setequal(link$files, c("app.R", "helper.R"))
})

test_that("process_project_input rejects unsupported input", {
  expect_error(
    process_project_input(x_expr = quote({ plot(1:10) })),
    "not supported"
  )
  expect_error(process_project_input(list("x <- 1")), "named list")
})

test_that("process_input rejects multiple bare code strings", {
  expect_error(process_input(c("x <- 1", "y <- 2")), "Multiple")
})

# Regression: shinylive_r_link()/shinylive_py_link() documented `NULL: read from
# clipboard` but their input argument had no default, so calling them with no
# arguments failed with `argument "files" is missing, with no default`.
test_that("link functions read from the clipboard when called with no input", {
  local_mocked_bindings(ingest_clipboard = function() "library(shiny)")

  expect_s3_class(webr_repl_link(), "webr_link")
  expect_s3_class(shinylive_r_link(), "shinylive_link")
  expect_s3_class(shinylive_py_link(), "shinylive_link")
})

test_that("clipboard content becomes the app body", {
  local_mocked_bindings(ingest_clipboard = function() "marker <- 1")

  link <- shinylive_r_link()

  expect_equal(link$files, "app.R")

  contents <- preview_shinylive_link(as.character(link))$files_data[[1]]$content
  expect_equal(contents, "marker <- 1")
})

# Regression: a link carries its files as UTF-8 text and is parsed back with
# rawToChar(), so a latin-1 file used to produce a link that livelink's own
# preview_webr_link() could not read ("input string 1 is invalid UTF-8"). An
# embedded NUL was worse: readLines() truncated the line and lost content
# without a word.
test_that("a script that is not UTF-8 text is refused", {
  tmp <- withr::local_tempdir()

  # Text in the wrong encoding. A script has to be UTF-8, so this is an error
  # rather than something to carry as bytes.
  latin1 <- file.path(tmp, "latin1.R")
  writeBin(c(charToRaw("x <- '"), as.raw(0xE9), charToRaw("'")), latin1)
  expect_error(webr_repl_link(latin1), "not valid UTF-8")

  # A NUL means this is not text at all. It can travel in a project as data,
  # so the message points there rather than refusing outright.
  for (nul_at in c("start", "middle", "end")) {
    body <- switch(nul_at,
      start  = c(as.raw(0), charToRaw("x <- 1")),
      middle = c(charToRaw("a <- 1"), as.raw(0), charToRaw("b <- 2")),
      end    = c(charToRaw("x <- 1"), as.raw(0))
    )
    path <- file.path(tmp, paste0("nul-", nul_at, ".R"))
    writeBin(body, path)
    expect_error(webr_repl_link(path), "webr_repl_project", label = nul_at)
  }
})

test_that("a file with no trailing newline is read without complaint", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "noeol.R")
  writeBin(charToRaw("x <- 1"), path)

  expect_no_warning(link <- webr_repl_link(path))
  expect_equal(
    preview_webr_link(as.character(link))$files_data[[1]]$text,
    "x <- 1"
  )
})

# Regression: a missing `input` reached process_project_input() as an empty
# promise, so the abort named the internal variable: `argument "x_expr" is
# missing, with no default`. The intended message existed but was unreachable.
test_that("the project functions explain a missing input", {
  expect_error(webr_repl_project(), "Clipboard input not supported")
  expect_error(shinylive_project(engine = "r"), "Clipboard input not supported")
})

# Regression: a srcref records where source was, not what it says now. When the
# file changed after parsing, the recovered "source" was whatever occupied those
# lines by then -- observed to be entirely unrelated code, silently encoded into
# the link. Structure is now compared before the recovered text is trusted.
test_that("source that no longer matches the expression is not shipped", {
  path <- withr::local_tempfile(fileext = ".R")
  writeLines(c("f({", "  secret <- Sys.getenv('AWS_KEY')", "  z <- 9", "})"), path)

  buffer <- c("f({", "  x <- 1 # mine", "  y <- 2", "})")
  expr <- parse(text = buffer, keep.source = TRUE, srcfile = srcfile(path))[[1]]

  code <- paste(stringify_expression(expr[[2]]), collapse = "\n")

  expect_false(grepl("AWS_KEY", code, fixed = TRUE))
  expect_match(code, "x <- 1", fixed = TRUE)
  expect_match(code, "y <- 2", fixed = TRUE)
})

test_that("matching source still keeps its comments", {
  path <- withr::local_tempfile(fileext = ".R")
  writeLines(c("f({", "  x <- 1 # kept", "  y <- 2", "})"), path)

  expr <- parse(text = readLines(path), keep.source = TRUE,
                srcfile = srcfile(path))[[1]]

  expect_match(
    paste(stringify_expression(expr[[2]]), collapse = "\n"),
    "x <- 1 # kept",
    fixed = TRUE
  )
})

# Regression: an empty block has nothing to deparse, and unlist() of nothing is
# NULL, which enc2utf8() rejects with "argument is not a character vector". Only
# reachable without source references, so Rscript and R CMD check hit it while an
# interactive session did not.
test_that("an empty block is handled without source references", {
  expr <- parse(text = "{}", keep.source = FALSE)[[1]]

  expect_no_error(code <- stringify_expression(expr))
  expect_equal(code, character(0))
  expect_no_error(deparse_expression(expr))

  # A block holding only a comment reduces to the same thing once parsed.
  commented <- parse(text = "{\n  # nothing but a comment\n}", keep.source = FALSE)[[1]]
  expect_no_error(stringify_expression(commented))
})
