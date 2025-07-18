#' Create a WebR REPL sharelink from R code
#'
#' Generates a shareable URL for R code that can be executed in the webR environment.
#' The code is compressed, base64-encoded, and embedded in the URL.
#'
#' @param code Character string containing R code to share
#' @param filename Name for the file (default: `"script.R"`)
#' @param path Full path where the file will be placed in WebR (default: `"/home/web_user/{filename}"`)
#' @param autorun Logical. Whether to auto-execute the code when link is opened (default: `FALSE`)
#' @param mode Character vector or string specifying which WebR interface components to show.
#'   Valid components: `"plot"`, `"files"`, `"terminal"`, `"editor"`. Can be `c("plot", "files")` or `"plot-files"`.
#'   If NULL (default), shows all components.
#' @param version WebR version to use (`"latest"` or specific version >= "v0.5.4")
#' @param base_url WebR application URL. If NULL, uses global option or builds from version
#'
#' @return
#' Character string containing the complete WebR sharelink URL
#'
#' @export
#' @examples
#' # Simple code sharing
#' code <- "plot(1:10)"
#' link <- webr_repl_link(code, autorun = TRUE)
#'
#' # Show only plot and editor
#' link <- webr_repl_link(code, mode = c("plot", "editor"))
#'
#' # String format for mode
#' link <- webr_repl_link(code, mode = "plot-files-terminal")
#'
#' # Custom path and version with mode
#' link <- webr_repl_link(code,
#'                       path = "/tmp/my_script.R",
#'                       mode = c("editor", "terminal"),
#'                       version = "v0.5.4")
webr_repl_link <- function(code,
                           filename = "script.R",
                           path = NULL,
                           autorun = FALSE,
                           mode = NULL,
                           version = "latest",
                           base_url = NULL) {

  # Validate inputs using helper functions
  check_single_string(code, "code")
  check_single_string(filename, "filename")
  check_single_logical(autorun, "autorun")
  check_valid_mode(mode, "mode")
  check_valid_version(version, "version")

  # Handle path
  if (is.null(path)) {
    path <- paste0("/home/web_user/", filename)
  } else {
    check_valid_path(path, "path")
  }

  # Handle base URL
  if (is.null(base_url)) {
    base_url <- get_webr_base_url(version)
  } else {
    check_single_string(base_url, "base_url")
  }

  # Create share item structure
  share_item <- list(
    name = filename,
    path = path,
    text = code
  )

  if (autorun && grepl("\\.R$", filename, ignore.case = TRUE)) {
    share_item$autorun <- TRUE
  }

  # Convert to JSON
  json_data <- jsonlite::toJSON(list(share_item), auto_unbox = TRUE)

  # Compress using gzip (closest to zlib in R)
  compressed <- memCompress(charToRaw(json_data), type = "gzip")

  # Base64 encode
  base64_data <- base64enc::base64encode(compressed)

  # URL encode for safety
  encoded_data <- utils::URLencode(base64_data, reserved = TRUE)

  # Determine flags
  flags <- "jz"  # JSON + compressed
  if (autorun) flags <- paste0(flags, "a")

  # Build complete URL with mode
  url <- build_webr_url(base_url, encoded_data, flags, mode)

  # Return webr_link object
  new_webr_link(url, filename, path, mode, version, autorun)
}

#' Create WebR REPL sharelink for multiple files
#'
#' Creates a WebR sharelink for projects with multiple R files, data files, or other resources.
#' Useful for sharing complete analyses, packages, or educational materials.
#'
#' @param files Named list where names are filenames and values are file content as character strings
#' @param autorun_files Character vector of filenames to auto-execute when project loads, or "all" to autorun all R files (default: none)
#' @param base_path Base directory path for all files (default: `"/home/web_user/"`)
#' @param mode Character vector or string specifying which WebR interface components to show.
#'   Valid components: `"plot"`, `"files"`, `"terminal"`, `"editor"`. Can be `c("plot", "files")` or `"plot-files"`.
#'   If NULL (default), shows all components.
#' @param version WebR version to use (`"latest"` or specific version >= "v0.5.4")
#' @param base_url WebR application URL. If NULL, uses global option or builds from version
#'
#' @return webr_project object containing the WebR sharelink for the multi-file project
#'
#' @examples
#' # Create a project with multiple files
#' files <- list(
#'   "main.R" = "source('utils.R')\nresult <- analyze_data(mtcars)",
#'   "utils.R" = "analyze_data <- function(data) { summary(data) }",
#'   "README.md" = "# My Analysis\nThis project analyzes the mtcars dataset."
#' )
#'
#' # Autorun specific files
#' link <- webr_repl_project(files, autorun_files = "main.R")
#'
#' # Autorun all R files
#' link <- webr_repl_project(files, autorun_files = "all")
#'
#' # Autorun multiple specific files
#' link <- webr_repl_project(files, autorun_files = c("main.R", "utils.R"))
#'
#' # No autorun (default)
#' link <- webr_repl_project(files)
#'
#' # Show only files and editor for project management
#' link <- webr_repl_project(files, mode = c("files", "editor"))
#'
#' # Custom base path, version, and interface mode
#' link <- webr_repl_project(files,
#'                          base_path = "/workspace/",
#'                          mode = "files-editor-terminal",
#'                          version = "v0.5.4")
#'
#' @export
webr_repl_project <- function(files,
                              autorun_files = character(0),
                              base_path = "/home/web_user/",
                              mode = NULL,
                              version = "latest",
                              base_url = NULL) {

  # Validate inputs using helper functions
  check_named_list(files, "files")
  check_character_vector(autorun_files, "autorun_files")
  check_valid_path(base_path, "base_path")
  check_valid_mode(mode, "mode")
  check_valid_version(version, "version")

  # Handle autorun_files validation
  if (length(autorun_files) > 0) {
    if (length(autorun_files) == 1 && autorun_files == "all") {
      # "all" is valid
    } else {
      # Check that specified files exist in the files list
      ensure_files_in_list(autorun_files, files, "files", "autorun_files")
    }
  }

  # Handle base URL
  if (is.null(base_url)) {
    base_url <- get_webr_base_url(version)
  } else {
    check_single_string(base_url, "base_url")
  }

  # Ensure base_path ends with /
  if (!grepl("/$", base_path)) {
    base_path <- paste0(base_path, "/")
  }

  # Determine which files should have autorun enabled
  autorun_all <- length(autorun_files) == 1 && autorun_files == "all"

  share_items <- mapply(function(content, filename) {
    item <- list(
      name = filename,
      path = paste0(base_path, filename),
      text = content
    )

    # Set autorun if:
    # 1. autorun_files = "all" and this is an R file, OR
    # 2. this filename is specifically listed in autorun_files and it's an R file
    should_autorun <- grepl("\\.R$", filename, ignore.case = TRUE) &&
      (autorun_all || filename %in% autorun_files)

    if (should_autorun) {
      item$autorun <- TRUE
    }

    item
  }, files, names(files), SIMPLIFY = FALSE, USE.NAMES = FALSE)

  # Convert to JSON and encode
  json_data <- jsonlite::toJSON(share_items, auto_unbox = TRUE)
  compressed <- memCompress(charToRaw(json_data), type = "gzip")
  base64_data <- base64enc::base64encode(compressed)
  encoded_data <- utils::URLencode(base64_data, reserved = TRUE)

  # Set flags
  flags <- "jz"
  # Add "a" flag only if autorun_files = "all"
  if (autorun_all) {
    flags <- paste0(flags, "a")
  }

  # Build complete URL with mode
  url <- build_webr_url(base_url, encoded_data, flags, mode)

  # Return webr_project object
  new_webr_project(url, files, base_path, mode, version, autorun_files)
}

#' Create paired exercise and solution WebR REPL links
#'
#' Generates a pair of WebR links for educational purposes: one for student exercises
#' (without autorun) and one for solutions (with autorun enabled).
#'
#' @param exercise_text Character string containing the exercise code with placeholders or TODOs
#' @param solution_text Character string containing the complete solution code
#' @param exercise_name Base name for the exercise (will create `"name_exercise.R"` and `"name_solution.R"`)
#' @param base_path Base directory path for files (default: `"/home/web_user/"`)
#' @param version WebR version to use ("latest" or specific version >= "v0.5.4")
#' @param base_url WebR application URL. If NULL, uses global option or builds from version
#'
#' @return Named list with 'exercise' and 'solution' WebR sharelinks
#'
#' @examples
#' exercise_code <- "
#' # Exercise: Calculate mean of mtcars$mpg
#' # TODO: Complete the line below
#' mean_mpg <- # YOUR CODE HERE
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
#' # Access with links$exercise and links$solution
#'
#' # Custom path and version
#' links <- webr_repl_exercise(exercise_code, solution_code, "stats",
#'                           base_path = "/exercises/", version = "v0.5.4")
#'
#' @export
webr_repl_exercise <- function(exercise_text,
                               solution_text,
                               exercise_name,
                               base_path = "/home/web_user/",
                               version = "latest",
                               base_url = NULL) {

  # Validate inputs using helper functions
  check_single_string(exercise_text, "exercise_text")
  check_single_string(solution_text, "solution_text")
  check_single_string(exercise_name, "exercise_name")
  check_valid_path(base_path, "base_path")
  check_valid_version(version, "version")

  # Handle base URL
  if (is.null(base_url)) {
    base_url <- get_webr_base_url(version)
  } else {
    check_single_string(base_url, "base_url")
  }

  # Ensure base_path ends with /
  if (!grepl("/$", base_path)) {
    base_path <- paste0(base_path, "/")
  }

  # Create filenames and paths
  exercise_filename <- paste0(exercise_name, "_exercise.R")
  solution_filename <- paste0(exercise_name, "_solution.R")
  exercise_path <- paste0(base_path, exercise_filename)
  solution_path <- paste0(base_path, solution_filename)

  # Exercise link (no autorun - student works on it)
  exercise_link <- webr_repl_link(
    exercise_text,
    filename = exercise_filename,
    path = exercise_path,
    autorun = FALSE,
    version = version,
    base_url = base_url
  )

  # Solution link (with autorun - shows complete solution)
  solution_link <- webr_repl_link(
    solution_text,
    filename = solution_filename,
    path = solution_path,
    autorun = TRUE,
    version = version,
    base_url = base_url
  )

  list(
    exercise = exercise_link,
    solution = solution_link
  )
}
