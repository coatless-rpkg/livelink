# Internal validation helpers.
#
#   is_*()                 -> TRUE/FALSE
#   check_*() / ensure_*() -> invisible(TRUE), or abort with a cli message.

#' Describe the value the caller actually passed
#'
#' Formats a value for a cli bullet, naming the cases that cli itself renders
#' as nothing.
#'
#' @param x The offending value
#'
#' @return
#' A single string, already formatted for a cli bullet
#'
#' @details
#' `{.val {x}}` renders nothing at all for NULL and for a zero-length vector,
#' so an "i" bullet reading "You provided: {.val {x}}" came out as a dangling
#' "You provided:". That reads worst in exactly the cases where the value is
#' the whole problem. Name those cases instead of interpolating emptiness.
#'
#' @noRd
describe_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }

  if (length(x) == 0) {
    return(paste0("an empty ", typeof(x), " vector"))
  }

  if (length(x) == 1 && is.na(x)) {
    return("NA")
  }

  cli::format_inline("{.val {x}}")
}

#' Ensure a decoded webR payload is a list of file entries
#'
#' Aborts unless the parsed payload is a list whose entries are themselves
#' lists.
#'
#' @param files_data The parsed payload
#' @param flags The URL's flag tail, for the message
#'
#' @return
#' Invisible TRUE if usable, aborts if not
#'
#' @details
#' Decompressing and parsing can both succeed and still leave something that is
#' not a file list. A link whose flags no longer describe its payload parses as
#' an atomic vector, and every consumer then hit `$` on it. Say what is wrong
#' once, here, rather than surfacing R's "$ operator is invalid for atomic
#' vectors" from wherever the first field access happened to be.
#'
#' @noRd
check_webr_files_data <- function(files_data, flags = "") {
  entries_are_lists <- is.list(files_data) &&
    all(vapply(files_data, is.list, logical(1)))

  if (!entries_are_lists) {
    cli::cli_abort(c(
      "Malformed webR link",
      "x" = "The decoded payload is not a list of files",
      "i" = "The link's flags ({.val {flags}}) may not match how its code was
             encoded, or the fragment may be truncated."
    ), call = NULL)
  }

  invisible(TRUE)
}

#' Check if version is valid
#'
#' Tests whether a version string is `"latest"` or a `vX.Y.Z` release at or
#' above v0.5.4.
#'
#' @param version Version string to check
#'
#' @return
#' Logical value
#'
#' @noRd
is_valid_version <- function(version) {
  # NA is rejected here rather than left to the comparisons below: `NA ==
  # "latest"` is NA, and `if (NA)` aborts with "missing value where TRUE/FALSE
  # needed", which names neither the argument nor the value.
  if (!is.character(version) || length(version) != 1 || is.na(version)) {
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
#'
#' Tests whether a mode names a unique set of webR panels, or is NULL.
#'
#' @param mode Mode specification (character vector or string)
#'
#' @return
#' Logical value
#'
#' @details
#' The panels are `plot`, `files`, `terminal` and `editor`. They may arrive
#' either as a character vector or as a single `-`-separated string. NULL is
#' valid and means no mode restriction.
#'
#' @noRd
is_valid_mode <- function(mode) {
  if (is.null(mode)) {
    return(TRUE)  # NULL is valid (means no mode restriction)
  }

  valid_components <- c("plot", "files", "terminal", "editor")

  # As in is_valid_version(): NA must not reach the comparisons below.
  if (anyNA(mode)) {
    return(FALSE)
  }

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
#'
#' Aborts with a cli message naming the panel at fault when a mode is not a
#' valid panel specification.
#'
#' @param mode Mode specification to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
check_valid_mode <- function(mode, arg_name = "mode") {
  if (is_valid_mode(mode)) {
    return(invisible(TRUE))
  }

  valid_components <- c("plot", "files", "terminal", "editor")

  # Say which component is at fault. Listing the valid ones and nothing else
  # reads as a contradiction when the value is a *duplicate* of a valid one:
  # `c("plot", "plot")` was rejected under a bullet stating "plot" is valid.
  reason <- if (!is.character(mode)) {
    cli::format_inline("{.arg {arg_name}} must be a character vector or a
                        {.code -}-separated string, not {.type {mode}}")
  } else if (anyNA(mode)) {
    cli::format_inline("{.arg {arg_name}} must not contain NA")
  } else {
    components <- if (length(mode) == 1) strsplit(mode, "-")[[1]] else mode
    unknown <- setdiff(components, valid_components)
    duplicated_components <- unique(components[duplicated(components)])

    if (length(components) == 0 || !any(nzchar(components))) {
      cli::format_inline("{.arg {arg_name}} names no panels")
    } else if (length(unknown) > 0) {
      cli::format_inline("Unknown panel{?s}: {.val {unknown}}")
    } else if (length(duplicated_components) > 0) {
      cli::format_inline("Panel{?s} named more than once: {.val {duplicated_components}}")
    } else {
      cli::format_inline("{.arg {arg_name}} is not a valid panel specification")
    }
  }

  cli::cli_abort(c(
    "Invalid {.arg {arg_name}} argument",
    "x" = reason,
    "i" = "Valid components: {.val {valid_components}}",
    "i" = "Examples: {.code c('plot', 'files')} or {.code 'plot-files-terminal'}"
  ))
}


#' Check if webR version is valid
#'
#' Aborts with a cli message when a version is not one webR publishes.
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
#' @noRd
check_valid_version <- function(version, arg_name = "version") {
  if (!is_valid_version(version)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be 'latest' or a version >= v0.5.4 (e.g., 'v0.5.4', 'v1.0.0')",
      "i" = "You provided: {describe_value(version)}"
    ))
  }
  invisible(TRUE)
}

#' Check if input is a single character string
#'
#' Tests whether an object is one non-NA, non-empty string.
#'
#' @param x Object to check
#'
#' @return
#' Logical value
#'
#' @noRd
is_single_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' Ensure input is a single character string
#'
#' Aborts with a cli message when an object is not one non-empty string.
#'
#' @param x Object to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
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
#'
#' Tests whether an object is a single TRUE or FALSE.
#'
#' @param x Object to check
#'
#' @return
#' Logical value
#'
#' @noRd
is_single_logical <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

#' Ensure input is a single logical value
#'
#' Aborts with a cli message when an object is not a single TRUE or FALSE.
#'
#' @param x Object to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
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
#'
#' Tests whether an object is a list whose every element carries a non-empty
#' name.
#'
#' @param x Object to check
#'
#' @return
#' Logical value
#'
#' @noRd
is_named_list <- function(x) {
  is.list(x) && !is.null(names(x)) && all(names(x) != "") && all(nzchar(names(x)))
}

#' Check if input is a valid file path
#'
#' Tests whether an object is usable as a file path.
#'
#' @param x Object to check
#'
#' @return
#' Logical value
#'
#' @details
#' A file path is, structurally, a single non-empty string. The distinct name
#' and [check_valid_path()] wrapper exist only for the path-specific error text.
#'
#' @noRd
is_valid_path <- function(x) {
  is_single_string(x)
}

#' Ensure input is a valid file path
#'
#' Aborts with a cli message when an object is not usable as a file path.
#'
#' @param x Object to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
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
#' Tests whether an object is a character vector.
#'
#' @param x Object to check
#'
#' @return
#' Logical value
#'
#' @noRd
is_character_vector <- function(x) {
  is.character(x)
}

#' Ensure input is a character vector
#'
#' Aborts with a cli message when an object is not a character vector.
#'
#' @param x Object to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
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
#' Tests whether a path points at an existing directory.
#'
#' @param path Directory path to check
#'
#' @return
#' Logical value
#'
#' @noRd
has_directory <- function(path) {
  dir.exists(path)
}

#' Ensure directory exists
#'
#' Aborts with a cli message when a path does not point at an existing
#' directory.
#'
#' @param path Directory path to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
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

#' Ensure specified files exist in a named list
#'
#' Aborts with a cli message naming the filenames that the list does not
#' carry.
#'
#' @param files_to_check Character vector of filenames to check
#' @param files_list Named list of files
#' @param files_arg_name Name of the files argument (for error messages)
#' @param check_arg_name Name of the checking argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
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

#' Check if engine is valid for Shinylive
#'
#' Tests whether an engine is the single string `"r"` or `"python"`.
#'
#' @param engine Engine specification ("r" or "python")
#'
#' @return
#' Logical value
#'
#' @noRd
is_valid_shinylive_engine <- function(engine) {
  is.character(engine) && length(engine) == 1 && engine %in% c("r", "python")
}

#' Ensure engine is valid for Shinylive
#'
#' Aborts with a cli message when an engine is neither `"r"` nor `"python"`.
#'
#' @param engine Engine specification to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
check_valid_shinylive_engine <- function(engine, arg_name = "engine") {
  if (!is_valid_shinylive_engine(engine)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be either 'r' or 'python'",
      "i" = "You provided: {describe_value(engine)}"
    ))
  }
  invisible(TRUE)
}

#' Check if Shinylive mode is valid
#'
#' Tests whether a mode is the single string `"editor"` or `"app"`.
#'
#' @param mode Mode specification ("editor" or "app")
#'
#' @return
#' Logical value
#'
#' @noRd
is_valid_shinylive_mode <- function(mode) {
  is.character(mode) && length(mode) == 1 && mode %in% c("editor", "app")
}

#' Ensure Shinylive mode is valid
#'
#' Aborts with a cli message when a mode is neither `"editor"` nor `"app"`.
#'
#' @param mode Mode specification to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
check_valid_shinylive_mode <- function(mode, arg_name = "mode") {
  if (!is_valid_shinylive_mode(mode)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be either 'editor' or 'app'",
      "i" = "You provided: {describe_value(mode)}"
    ))
  }
  invisible(TRUE)
}

#' Check if URL is a valid Shinylive URL
#'
#' Tests whether a URL matches the `https://shinylive.io/[r|py]/[editor|app]/#`
#' shape.
#'
#' @param url URL string to check
#'
#' @return
#' Logical value
#'
#' @noRd
is_valid_shinylive_url <- function(url) {
  if (!is_single_string(url)) {
    return(FALSE)
  }

  grepl("^https://shinylive\\.io/(r|py)/(editor|app)/#", url)
}

#' Ensure URL is a valid Shinylive URL
#'
#' Aborts with a cli message when a URL is not a Shinylive sharelink.
#'
#' @param url URL to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
check_valid_shinylive_url <- function(url, arg_name = "url") {
  if (!is_valid_shinylive_url(url)) {
    cli::cli_abort(c(
      "Invalid {.arg {arg_name}} argument",
      "x" = "{.arg {arg_name}} must be a valid Shinylive URL",
      "i" = "Expected format: https://shinylive.io/[r|py]/[editor|app]/#...",
      "i" = "You provided: {describe_value(url)}"
    ))
  }
  invisible(TRUE)
}

#' Check if URL is a valid webR URL
#'
#' Tests whether a URL carries its code in a `#code=` fragment, from any host.
#'
#' @param url URL string to check
#'
#' @return
#' Logical value
#'
#' @noRd
is_valid_webr_url <- function(url) {
  if (!is_single_string(url)) {
    return(FALSE)
  }

  # What makes a sharelink readable is its fragment, not its host: decoding
  # never contacts the server. Requiring webr.r-wasm.org meant a link this
  # package had just written could not be read back, because pointing at a
  # self-hosted webR -- through `base_url` or `set_webr_base_url()` -- is a
  # documented workflow. Accept any host, and only rule out the sibling format,
  # whose fragment is LZ-string rather than gzip and would fail confusingly.
  if (is_valid_shinylive_url(url)) {
    return(FALSE)
  }

  grepl("#.*code=", url)
}

#' Ensure URL is a valid webR URL
#'
#' Aborts with a cli message when a URL is not a webR REPL sharelink.
#'
#' @param url URL string to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return
#' Invisible TRUE if valid, aborts if not
#'
#' @noRd
check_valid_webr_url <- function(url, arg_name = "url") {
  if (!is_valid_webr_url(url)) {
    cli::cli_abort(c(
      "Invalid webR URL",
      "x" = "{.arg {arg_name}} must be a valid webR REPL sharelink",
      "i" = "A webR sharelink carries its code in a {.code #code=} fragment,
             from any host running webR",
      "i" = "Example: https://webr.r-wasm.org/latest/#code=..."
    ))
  }
  invisible(TRUE)
}
