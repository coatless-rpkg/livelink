
#' Create a webR REPL sharelink from R code
#'
#' Generates a shareable URL for R code that can be executed in the webR environment.
#' Supports expressions, file paths, character strings, and clipboard input.
#'
#' @param input Code input. Can be:
#'   - R expression (no quotes needed): `webr_repl_link({ plot(1:10) })`
#'   - Character string: R code to execute
#'   - File path: Path to R file to read
#'   - NULL: Read from clipboard (requires clipr package)
#' @param filename Name for the file (default: `"script.R"`)
#' @param path Full path where the file will be placed in webR. If NULL (default),
#'   the file is placed at `"/home/web_user/{filename}"`.
#' @param autorun Logical. Whether to auto-execute the code when link is opened (default: `FALSE`).
#'   Only R files (`.R`) can be auto-executed.
#' @param panels Character vector or string specifying which webR interface panels to show.
#'   Valid panels: `"plot"`, `"files"`, `"terminal"`, `"editor"`. Can be `c("plot", "files")` or `"plot-files"`.
#'   If NULL (default), shows all panels.
#' @param version webR version to use (`"latest"` or specific version >= "v0.5.4")
#' @param base_url webR application URL. If NULL, uses global option or builds from version
#'
#' @return webr_link object containing the webR sharelink and metadata
#'
#' @section Comments in expression input:
#' Expression input recovers your source from R's *source references*, which R only
#' attaches when `keep.source` is enabled. Comments therefore survive in an
#' interactive session, but are dropped when the calling code is parsed without
#' source references -- notably inside a knitted 'Quarto' or 'R Markdown' document,
#' because 'knitr' evaluates chunks through `evaluate::evaluate()`, which discards
#' them. No `keep.source` setting recovers them there.
#'
#' If you need comments preserved, pass the code as a string or a file path, or write
#' it as a chunk in the document -- see [livelink-knitr] and
#' `vignette("links-in-documents", package = "livelink")`.
#'
#' @seealso [webr_repl_project()] for multi-file projects and
#'   [webr_repl_exercise()] for exercise and solution pairs; [livelink-knitr] to
#'   give a document chunk its own link; `vignette("getting-started", package =
#'   "livelink")` for an introduction.
#'
#' @export
#' @examples
#' # Expression input (no quotes needed!)
#' webr_repl_link({
#'   plot(1:10)
#'   summary(mtcars)
#' })
#'
#' # Traditional string input
#' webr_repl_link("plot(1:10)")
#'
#' # Choose which panels the REPL shows
#' webr_repl_link({ hist(rnorm(100)) }, panels = c("plot", "editor"))
#'
#' # Run the code as soon as the link opens
#' webr_repl_link("plot(1:10)", autorun = TRUE)
#'
#' # File path input
#' script <- tempfile(fileext = ".R")
#' writeLines("plot(1:10)", script)
#' webr_repl_link(script)
#'
#' # Read the code from the clipboard
#' if (interactive()) {
#'   webr_repl_link()
#' }
webr_repl_link <- function(input = NULL,
                           filename = "script.R",
                           path = NULL,
                           autorun = FALSE,
                           panels = NULL,
                           version = "latest",
                           base_url = NULL) {

  # Capture expression if provided (check expression form without evaluating input)
  x_expr <- substitute(input)
  code <- if (!missing(input) && is_brace_call(x_expr)) {
    process_input(x_expr = x_expr)
  } else {
    process_input(input = input)
  }

  check_single_string(filename, "filename")
  check_single_logical(autorun, "autorun")
  check_valid_mode(panels, "panels")
  check_valid_version(version, "version")

  if (is.null(path)) {
    path <- paste0("/home/web_user/", filename)
  } else {
    check_valid_path(path, "path")
  }

  if (is.null(base_url)) {
    base_url <- get_webr_base_url(version)
  } else {
    check_single_string(base_url, "base_url")
  }

  is_r_file <- grepl("\\.R$", filename, ignore.case = TRUE)
  if (autorun && !is_r_file) {
    cli::cli_warn(c(
      "{.arg autorun} was ignored",
      "!" = "Only R files can be auto-executed, but {.file {filename}} is not one.",
      "i" = "Give {.arg filename} a {.code .R} extension to enable autorun."
    ))
  }
  autorun_enabled <- autorun && is_r_file

  share_item <- list(
    name = filename,
    path = path,
    text = code
  )

  if (autorun_enabled) {
    share_item$autorun <- TRUE
  }

  encoded_data <- encode_webr_payload(list(share_item))

  # The `a` flag must agree with the per-item autorun set above, or the link
  # claims to autorun a file that carries no autorun instruction.
  flags <- if (autorun_enabled) "jza" else "jz"

  url <- build_webr_url(base_url, encoded_data, flags, panels)

  new_webr_link(url, filename, path, panels, version, autorun_enabled)
}

#' Create WebR REPL sharelink for multiple files
#'
#' Creates a webR sharelink for projects with multiple R files, data files, or other resources.
#' Supports named lists and file path vectors as input.
#'
#' @param input Input for multiple files. Can be:
#'   - Named list of braced expressions, so each file is written as R rather than
#'     as a string full of escaped newlines:
#'     `list("main.R" = { plot(1:10) }, "utils.R" = { f <- function() 42 })`
#'   - Named list of strings: `list("main.R" = code1, "utils.R" = code2)`
#'   - Vector of file paths: `c("main.R", "utils.R", "data.csv")`
#'
#'   The two list forms mix freely, which is what you want for a project holding
#'   both code and, say, a `README.md`.
#' @param autorun_files Character vector of filenames to auto-execute when project loads, or "all" to autorun all R files (default: none)
#' @param base_path Base directory path for all files (default: `"/home/web_user/"`)
#' @param panels Character vector or string specifying which webR interface panels to show.
#'   Valid panels: `"plot"`, `"files"`, `"terminal"`, `"editor"`. Can be `c("plot", "files")` or `"plot-files"`.
#'   If NULL (default), shows all panels.
#' @param version webR version to use (`"latest"` or specific version >= "v0.5.4")
#' @param base_url webR application URL. If NULL, uses global option or builds from version
#'
#' @return webr_project object containing the webR sharelink for the multi-file project
#'
#' @section Writing a project as code:
#' A file's contents can be given as a `{ ... }` block instead of a string, which
#' spares you escaping every newline and quote:
#'
#' ```
#' webr_repl_project(list(
#'   "main.R"    = { source("utils.R"); summarise(mtcars) },
#'   "utils.R"   = { summarise <- function(d) summary(d) },
#'   "README.md" = "# Analysis"
#' ))
#' ```
#'
#' The blocks are **never evaluated** -- they are source to ship, not code to run
#' -- so an assignment inside one leaves nothing behind in your session.
#'
#' Two things to know. Comments inside `{ }` survive in an interactive session but
#' not in a knitted document (see [webr_repl_link()]). And a `library()` call
#' inside a block is visible to `R CMD check`, which will report the package as an
#' undeclared dependency of *yours*; in a vignette or an example, use a string for
#' code that loads packages.
#'
#' @seealso [webr_repl_link()] for the single-file case.
#'
#' @export
#' @examples
#' # Each file written as R, rather than as an escaped string
#' webr_repl_project(list(
#'   "main.R"  = { source("utils.R"); summarise(mtcars) },
#'   "utils.R" = { summarise <- function(d) summary(d) }
#' ))
#'
#' # Strings still work, and the two forms mix
#' files <- list(
#'   "main.R" = "source('utils.R')\nresult <- analyze_data(mtcars)",
#'   "utils.R" = "analyze_data <- function(data) { summary(data) }",
#'   "README.md" = "# My Analysis\nThis project analyzes the mtcars dataset."
#' )
#' webr_repl_project(files, autorun_files = "main.R")
#'
#' # Autorun every R file in the project
#' webr_repl_project(files, autorun_files = "all")
#'
#' # File paths input
#' project_dir <- tempfile()
#' dir.create(project_dir)
#' main <- file.path(project_dir, "main.R")
#' utils <- file.path(project_dir, "utils.R")
#' writeLines("source('utils.R')", main)
#' writeLines("# utils", utils)
#' webr_repl_project(c(main, utils))
webr_repl_project <- function(input,
                              autorun_files = character(0),
                              base_path = "/home/web_user/",
                              panels = NULL,
                              version = "latest",
                              base_url = NULL) {

  # Captured, not forced: a literal list() may name each file's contents as a
  # `{ ... }` block, and forcing it would run those blocks instead of shipping
  # them.
  x_expr <- substitute(input)
  processed_files <- process_project_input(
    input = input, x_expr = x_expr, env = parent.frame()
  )

  check_character_vector(autorun_files, "autorun_files")
  check_valid_path(base_path, "base_path")
  check_valid_mode(panels, "panels")
  check_valid_version(version, "version")

  autorun_all <- length(autorun_files) == 1 && autorun_files == "all"

  if (length(autorun_files) > 0 && !autorun_all) {
    ensure_files_in_list(autorun_files, processed_files, "input", "autorun_files")
  }

  if (is.null(base_url)) {
    base_url <- get_webr_base_url(version)
  } else {
    check_single_string(base_url, "base_url")
  }

  if (!grepl("/$", base_path)) {
    base_path <- paste0(base_path, "/")
  }

  build_webr_project(processed_files, autorun_files, base_path, panels,
                     version, base_url)
}


#' Bundle a set of named file contents into one webR project link
#'
#' The encoding core shared by [webr_repl_project()] and
#' [webr_repl_directory()]'s `single_link` mode. Inputs are already validated and
#' normalized: `processed_files` is a named list of file contents, `base_path`
#' ends in `/`, and `base_url` is resolved.
#'
#' @param processed_files Named list mapping filename to its content string
#' @param autorun_files Character vector of files to autorun, or `"all"`
#' @param base_path Normalized base directory path
#' @param panels Panels to show, or NULL
#' @param version WebR version
#' @param base_url Resolved WebR base URL
#' @return A `webr_project` object
#' @noRd
build_webr_project <- function(processed_files, autorun_files, base_path,
                               panels, version, base_url) {
  autorun_all <- length(autorun_files) == 1 && autorun_files == "all"

  share_items <- mapply(function(content, filename) {
    item <- list(
      name = filename,
      path = paste0(base_path, filename),
      text = content
    )

    should_autorun <- grepl("\\.R$", filename, ignore.case = TRUE) &&
      (autorun_all || filename %in% autorun_files)

    if (should_autorun) {
      item$autorun <- TRUE
    }

    item
  }, processed_files, names(processed_files), SIMPLIFY = FALSE, USE.NAMES = FALSE)

  encoded_data <- encode_webr_payload(share_items)

  # Set the `a` flag whenever any file carries autorun, not only for
  # `autorun_files = "all"` -- a named autorun list used to encode the per-item
  # flag but never the URL flag that activates it.
  any_autorun <- any(vapply(share_items, function(i) isTRUE(i$autorun), logical(1)))
  flags <- if (any_autorun) "jza" else "jz"

  url <- build_webr_url(base_url, encoded_data, flags, panels)

  new_webr_project(url, processed_files, base_path, panels, version, autorun_files)
}


#' Create paired exercise and solution webR REPL links
#'
#' Generates a pair of webR links for educational purposes: one for student exercises
#' (without autorun) and one for solutions (with autorun enabled).
#'
#' @param exercise_text Character string containing the exercise code with placeholders or TODOs
#' @param solution_text Character string containing the complete solution code
#' @param exercise_name Base name for the exercise (will create `"name_exercise.R"` and `"name_solution.R"`)
#' @param base_path Base directory path for files (default: `"/home/web_user/"`)
#' @param version webR version to use ("latest" or specific version >= "v0.5.4")
#' @param base_url webR application URL. If NULL, uses global option or builds from version
#'
#' @return webr_exercise object holding the paired `exercise` and `solution` links
#'
#' @seealso [webr_repl_link()], which this builds on;
#'   `vignette("teaching", package = "livelink")` for using links in a course.
#'
#' @examples
#' exercise_code <- "
#' # Exercise: Calculate mean of mtcars$mpg
#' # TODO: Complete the line below
#' mean_mpg <- 0
#' print(mean_mpg)
#' "
#'
#' solution_code <- "
#' # Solution: Calculate mean of mtcars$mpg
#' mean_mpg <- mean(mtcars$mpg)
#' print(mean_mpg)
#' "
#'
#' links <- webr_repl_exercise(exercise_code, solution_code, "basic_stats")
#' links$exercise
#' links$solution
#'
#' # Custom path and version
#' webr_repl_exercise(exercise_code, solution_code, "stats",
#'                    base_path = "/exercises/", version = "v0.5.4")
#'
#' @export
webr_repl_exercise <- function(exercise_text,
                               solution_text,
                               exercise_name,
                               base_path = "/home/web_user/",
                               version = "latest",
                               base_url = NULL) {

  check_single_string(exercise_text, "exercise_text")
  check_single_string(solution_text, "solution_text")
  check_single_string(exercise_name, "exercise_name")
  check_valid_path(base_path, "base_path")
  check_valid_version(version, "version")

  if (is.null(base_url)) {
    base_url <- get_webr_base_url(version)
  } else {
    check_single_string(base_url, "base_url")
  }

  if (!grepl("/$", base_path)) {
    base_path <- paste0(base_path, "/")
  }

  exercise_filename <- paste0(exercise_name, "_exercise.R")
  solution_filename <- paste0(exercise_name, "_solution.R")

  # Exercise link (no autorun - student works on it)
  exercise_link <- webr_repl_link(
    exercise_text,
    filename = exercise_filename,
    path = paste0(base_path, exercise_filename),
    autorun = FALSE,
    version = version,
    base_url = base_url
  )

  # Solution link (with autorun - shows complete solution)
  solution_link <- webr_repl_link(
    solution_text,
    filename = solution_filename,
    path = paste0(base_path, solution_filename),
    autorun = TRUE,
    version = version,
    base_url = base_url
  )

  new_webr_exercise(exercise_link, solution_link)
}
