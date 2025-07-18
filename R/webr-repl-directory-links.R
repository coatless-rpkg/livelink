#' Create WebR REPL sharelinks from a directory of R files
#'
#' Batch processes all R files in a directory to create individual WebR sharelinks.
#' Useful for converting collections of scripts, examples, or course materials.
#'
#' @param directory_path Character string specifying the path to the directory containing R files
#' @param autorun Logical. Whether to enable autorun for all generated links (default: FALSE)
#' @param pattern Regular expression pattern to match files (default: "\\\\.R$" for R files)
#' @param base_path Base directory path for files in WebR (default: "/home/web_user/")
#' @param mode Character vector or string specifying which WebR interface components to show.
#'   Valid components: "plot", "files", "terminal", "editor". Can be c("plot", "files") or "plot-files".
#'   If NULL (default), shows all components.
#' @param version WebR version to use ("latest" or specific version >= "v0.5.4")
#' @param base_url WebR application URL. If NULL, uses global option or builds from version
#'
#' @return
#' Named character vector where names are filenames and values are WebR sharelinks
#'
#' @examples
#' \dontrun{
#' # Process all R files in a directory
#' links <- webr_repl_directory("./examples/", autorun = TRUE)
#'
#' # Show only editor and terminal for all files
#' links <- webr_repl_directory("./examples/",
#'                             mode = c("editor", "terminal"))
#'
#' # Process with custom settings and interface mode
#' links <- webr_repl_directory("./course/",
#'                             pattern = "exercise.*\\.R$",
#'                             base_path = "/exercises/",
#'                             mode = "editor-plot",
#'                             version = "v0.5.4")
#'
#' # Save links to a file
#' writeLines(paste(names(links), links, sep = ": "), "webr_links.txt")
#' }
#'
#' @export
webr_repl_directory <- function(directory_path,
                                autorun = FALSE,
                                pattern = "\\.R$",
                                base_path = "/home/web_user/",
                                mode = NULL,
                                version = "latest",
                                base_url = NULL) {

  # Validate inputs using helper functions
  check_single_string(directory_path, "directory_path")
  ensure_directory_exists(directory_path, "directory_path")
  check_single_logical(autorun, "autorun")
  check_valid_path(base_path, "base_path")
  check_valid_mode(mode, "mode")
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

  # Find matching files
  r_files <- list.files(directory_path, pattern = pattern, full.names = TRUE)

  if (length(r_files) == 0) {
    cli::cli_warn(c(
      "No files found",
      "!" = "No files matching pattern {.val {pattern}} found in {.path {directory_path}}",
      "i" = "Try adjusting the {.arg pattern} argument"
    ))
    return(character(0))
  }

  cli::cli_inform(c(
    "v" = "Found {length(r_files)} file{?s} matching pattern {.val {pattern}}",
    "i" = "Processing files in {.path {directory_path}}..."
  ))

  # Process each file
  links <- sapply(r_files, function(file) {
    tryCatch({
      code <- readLines(file, warn = FALSE)
      code_text <- paste(code, collapse = "\n")
      filename <- basename(file)
      file_path <- paste0(base_path, filename)

      link_obj <- webr_repl_link(code_text,
                                 filename = filename,
                                 path = file_path,
                                 autorun = autorun,
                                 mode = mode,
                                 version = version,
                                 base_url = base_url)

      # Extract URL from the webr_link object
      link_obj$url
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to process file {.file {basename(file)}}",
        "x" = "{e$message}"
      ))
      NA_character_
    })
  }, USE.NAMES = FALSE)

  # Remove failed files and set names
  valid_links <- links[!is.na(links)]
  names(valid_links) <- basename(r_files[!is.na(links)])

  failed_count <- sum(is.na(links))
  if (failed_count > 0) {
    cli::cli_warn(c(
      "Some files failed to process",
      "!" = "{failed_count} file{?s} could not be processed"
    ))
  }

  cli::cli_inform(c(
    "v" = "Successfully created {length(valid_links)} WebR link{?s}"
  ))

  # Return webr_directory object
  new_webr_directory(valid_links, base_path, mode, version, directory_path)
}
