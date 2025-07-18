#' Validation helper functions
#'
#' @description
#' Internal validation functions that provide consistent checking across the package.
#' These functions follow the pattern of `is_*()` returning TRUE/FALSE and
#' `check_*()` or `ensure_*()` functions that abort with cli messages on failure.
#' @keywords internal

#' Check if version is valid
#' @param version Version string to check
#' @return Logical value
#' @keywords internal
is_valid_version <- function(version) {
  if (!is.character(version) || length(version) != 1) {
    return(FALSE)
  }

  # Allow "latest"
  if (version == "latest") {
    return(TRUE)
  }

  # Check version format (v0.5.4 or greater)
  if (grepl("^v\\d+\\.\\d+\\.\\d+$", version)) {
    # Extract version numbers
    version_clean <- gsub("^v", "", version)
    version_parts <- as.numeric(strsplit(version_clean, "\\.")[[1]])

    # Check if >= v0.5.4
    if (version_parts[1] > 0) return(TRUE)
    if (version_parts[1] == 0 && version_parts[2] > 5) return(TRUE)
    if (version_parts[1] == 0 && version_parts[2] == 5 && version_parts[3] >= 4) return(TRUE)
  }

  return(FALSE)
}

#' Check if mode is valid
#' @param mode Mode specification (character vector or string)
#' @return Logical value
#' @keywords internal
is_valid_mode <- function(mode) {
  if (is.null(mode)) {
    return(TRUE)  # NULL is valid (means no mode restriction)
  }

  valid_components <- c("plot", "files", "terminal", "editor")

  if (is.character(mode)) {
    if (length(mode) == 1) {
      # String format like "plot-files-terminal"
      if (mode == "") return(FALSE)
      components <- strsplit(mode, "-")[[1]]
    } else {
      # Vector format like c("plot", "files", "terminal")
      components <- mode
    }

    # Check all components are valid and unique
    return(all(components %in% valid_components) &&
             length(components) == length(unique(components)) &&
             length(components) > 0)
  }

  return(FALSE)
}

#' Ensure mode is valid
#' @param mode Mode specification to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_valid_mode <- function(mode, arg_name = "mode") {
  if (!is_valid_mode(mode)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be NULL, a character vector of valid components, or a string with '-' separators",
      "i" = "Valid components: {.val {c('plot', 'files', 'terminal', 'editor')}}",
      "i" = "Examples: {.code c('plot', 'files')} or {.code 'plot-files-terminal'}"
    ))
  }
  invisible(TRUE)
}


#' Check if webR version is valid
#'
#' @param version Version string to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @details
#' This function checks if the provided version is either "latest" or a valid
#' version string in the format "vX.Y.Z" where X, Y, Z are integers and the version is
#' greater than or equal to v0.5.4.
#'
#' @keywords internal
check_valid_version <- function(version, arg_name = "version") {
  if (!is_valid_version(version)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be 'latest' or a version >= v0.5.4 (e.g., 'v0.5.4', 'v1.0.0')",
      "i" = "You provided: {.val {version}}"
    ))
  }
  invisible(TRUE)
}

#' Check if input is a single character string
#' @param x Object to check
#' @return Logical value
#' @keywords internal
is_single_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' Ensure input is a single character string
#' @param x Object to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_single_string <- function(x, arg_name) {
  if (!is_single_string(x)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be a single non-empty character string",
      "i" = "You provided {.type {x}} of length {length(x)}"
    ))
  }
  invisible(TRUE)
}

#' Check if input is a single logical value
#' @param x Object to check
#' @return Logical value
#' @keywords internal
is_single_logical <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

#' Ensure input is a single logical value
#' @param x Object to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_single_logical <- function(x, arg_name) {
  if (!is_single_logical(x)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be a single logical value (TRUE or FALSE)",
      "i" = "You provided {.type {x}} of length {length(x)}"
    ))
  }
  invisible(TRUE)
}

#' Check if input is a properly named list
#' @param x Object to check
#' @return Logical value
#' @keywords internal
is_named_list <- function(x) {
  is.list(x) && !is.null(names(x)) && all(names(x) != "") && all(nzchar(names(x)))
}

#' Ensure input is a properly named list
#' @param x Object to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_named_list <- function(x, arg_name) {
  if (!is_named_list(x)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be a named list with non-empty names",
      "i" = "Each file needs a filename as the list element name"
    ))
  }
  invisible(TRUE)
}

#' Check if input is a valid file path
#' @param x Object to check
#' @return Logical value
#' @keywords internal
is_valid_path <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' Ensure input is a valid file path
#' @param x Object to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_valid_path <- function(x, arg_name) {
  if (!is_valid_path(x)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be a single non-empty character string representing a file path",
      "i" = "You provided {.type {x}} of length {length(x)}"
    ))
  }
  invisible(TRUE)
}

#' Check if input is a character vector
#'
#' @param x Object to check
#' @return
#' Logical value
#'
#' @keywords internal
is_character_vector <- function(x) {
  is.character(x)
}

#' Ensure input is a character vector
#'
#' @param x Object to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @keywords internal
check_character_vector <- function(x, arg_name) {
  if (!is_character_vector(x)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be a character vector",
      "i" = "You provided {.type {x}}"
    ))
  }
  invisible(TRUE)
}

#' Check if directory exists
#'
#' @param path Directory path to check
#'
#' @return
#' Logical value
#'
#' @keywords internal
has_directory <- function(path) {
  dir.exists(path)
}

#' Ensure directory exists
#' @param path Directory path to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
ensure_directory_exists <- function(path, arg_name = "directory_path") {
  if (!has_directory(path)) {
    cli::cli_abort(c(
      "Directory does not exist",
      "x" = "Cannot find directory {.path {path}}",
      "i" = "Please check the path and try again"
    ))
  }
  invisible(TRUE)
}

#' Check if specified files exist in a named list
#' @param files_to_check Character vector of filenames to check
#' @param files_list Named list of files
#' @return Logical value
#' @keywords internal
has_files_in_list <- function(files_to_check, files_list) {
  all(files_to_check %in% names(files_list))
}

#' Ensure specified files exist in a named list
#' @param files_to_check Character vector of filenames to check
#' @param files_list Named list of files
#' @param files_arg_name Name of the files argument (for error messages)
#' @param check_arg_name Name of the checking argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
ensure_files_in_list <- function(files_to_check, files_list,
                                 files_arg_name = "files",
                                 check_arg_name = "autorun_files") {
  missing_files <- setdiff(files_to_check, names(files_list))
  if (length(missing_files) > 0) {
    cli::cli_abort(c(
      "Files specified in {.arg {check_arg_name}} not found in {.arg {files_arg_name}}",
      "x" = "Missing files: {.file {missing_files}}",
      "i" = "Available files: {.file {names(files_list)}}"
    ))
  }
  invisible(TRUE)
}

#' Create Shinylive sharelinks for R and Python Shiny apps
#'
#' @description
#' Functions to create Shinylive links for R and Python Shiny applications.
#' Shinylive allows running Shiny apps entirely in the browser.

#' Check if engine is valid for Shinylive
#' @param engine Engine specification ("r" or "python")
#' @return Logical value
#' @keywords internal
is_valid_shinylive_engine <- function(engine) {
  is.character(engine) && length(engine) == 1 && engine %in% c("r", "python")
}

#' Ensure engine is valid for Shinylive
#' @param engine Engine specification to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_valid_shinylive_engine <- function(engine, arg_name = "engine") {
  if (!is_valid_shinylive_engine(engine)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be either 'r' or 'python'",
      "i" = "You provided: {.val {engine}}"
    ))
  }
  invisible(TRUE)
}

#' Check if Shinylive mode is valid
#' @param mode Mode specification ("editor" or "app")
#' @return Logical value
#' @keywords internal
is_valid_shinylive_mode <- function(mode) {
  is.character(mode) && length(mode) == 1 && mode %in% c("editor", "app")
}

#' Ensure Shinylive mode is valid
#' @param mode Mode specification to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_valid_shinylive_mode <- function(mode, arg_name = "mode") {
  if (!is_valid_shinylive_mode(mode)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be either 'editor' or 'app'",
      "i" = "You provided: {.val {mode}}"
    ))
  }
  invisible(TRUE)
}

#' Check if URL is a valid Shinylive URL
#' @param url URL string to check
#' @return Logical value
#' @keywords internal
is_valid_shinylive_url <- function(url) {
  if (!is_single_string(url)) {
    return(FALSE)
  }

  grepl("^https://shinylive\\.io/(r|py)/(editor|app)/#", url)
}

#' Ensure URL is a valid Shinylive URL
#' @param url URL to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_valid_shinylive_url <- function(url, arg_name = "url") {
  if (!is_valid_shinylive_url(url)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be a valid Shinylive URL",
      "i" = "Expected format: https://shinylive.io/[r|py]/[editor|app]/#...",
      "i" = "You provided: {.val {url}}"
    ))
  }
  invisible(TRUE)
}

#' Check if URL is a valid webR URL
#' @param url URL string to check
#' @return Logical value
#' @keywords internal
is_valid_webr_url <- function(url) {
  if (!is.character(url) || length(url) != 1 || is.na(url) || !nzchar(url)) {
    return(FALSE)
  }

  # Check if URL contains webR domain and has a fragment with code parameter
  has_webr_domain <- grepl("webr\\.r-wasm\\.org", url, ignore.case = TRUE)
  has_fragment <- grepl("#", url)
  has_code_param <- grepl("#.*code=", url)

  return(has_webr_domain && has_fragment && has_code_param)
}

#' Ensure URL is a valid webR URL
#' @param url URL string to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_valid_webr_url <- function(url, arg_name = "url") {
  if (!is_valid_webr_url(url)) {
    cli::cli_abort(c(
      "Invalid webR URL",
      "x" = "{.arg {arg_name}} must be a valid webR REPL sharelink",
      "i" = "URL should be from webr.r-wasm.org and contain encoded data",
      "i" = "Example: https://webr.r-wasm.org/latest/#code=..."
    ))
  }
  invisible(TRUE)
}

#' Check if webR URL has valid flags
#' @param flags Flags string to check
#' @return Logical value
#' @keywords internal
is_valid_webr_flags <- function(flags) {
  if (!is.character(flags) || length(flags) != 1) {
    return(FALSE)
  }

  # Valid webR flags: u (uncompressed), z (zlib), m (msgpack), j (json), a (autorun)
  valid_chars <- c("u", "z", "m", "j", "a")
  flag_chars <- strsplit(flags, "")[[1]]

  # All characters must be valid flags
  all(flag_chars %in% valid_chars)
}

#' Ensure webR flags are valid
#' @param flags Flags string to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_valid_webr_flags <- function(flags, arg_name = "flags") {
  if (!is_valid_webr_flags(flags)) {
    cli::cli_abort(c(
      "Invalid webR flags",
      "x" = "{.arg {arg_name}} must contain only valid webR flag characters",
      "i" = "Valid flags: {.val {c('u', 'z', 'm', 'j', 'a')}}",
      "i" = "u = uncompressed, z = zlib, m = msgpack, j = json, a = autorun",
      "i" = "Default webR format is 'mz' (msgpack + zlib compressed)",
      "!" = "You provided: {.val {flags}}"
    ))
  }
  invisible(TRUE)
}

#' Check if webR version string is valid
#' @param version Version string to check
#' @return Logical value
#' @keywords internal
is_valid_webr_version_string <- function(version) {
  if (!is.character(version) || length(version) != 1) {
    return(FALSE)
  }

  # Valid formats: "latest", "v0.5.4", "unknown"
  if (version %in% c("latest", "unknown")) {
    return(TRUE)
  }

  # Check version format (v0.5.4 or greater)
  return(grepl("^v\\d+\\.\\d+\\.\\d+$", version))
}

#' Ensure webR version string is valid
#' @param version Version string to check
#' @param arg_name Name of the argument (for error messages)
#' @return Invisible TRUE if valid, aborts if not
#' @keywords internal
check_valid_webr_version_string <- function(version, arg_name = "version") {
  if (!is_valid_webr_version_string(version)) {
    cli::cli_abort(c(
      "Invalid webR version",
      "x" = "{.arg {arg_name}} must be a valid webR version string",
      "i" = "Valid formats: {.val {'latest'}}, {.val {'v0.5.4'}}, or {.val {'unknown'}}",
      "!" = "You provided: {.val {version}}"
    ))
  }
  invisible(TRUE)
}
