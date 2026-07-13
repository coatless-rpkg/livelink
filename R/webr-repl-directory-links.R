#' Create WebR REPL sharelinks from a directory of R files
#'
#' Batch processes all R files in a directory to create individual WebR sharelinks.
#' Useful for converting collections of scripts, examples, or course materials.
#'
#' @param directory_path Character string specifying the path to the directory containing R files
#' @param autorun Logical. Whether to enable autorun for all generated links (default: FALSE)
#' @param pattern Regular expression pattern to match files (default: "\\\\.R$" for R files)
#' @param base_path Base directory path for files in WebR (default: "/home/web_user/")
#' @param panels Character vector or string specifying which WebR interface panels to show.
#'   Valid panels: "plot", "files", "terminal", "editor". Can be c("plot", "files") or "plot-files".
#'   If NULL (default), shows all panels.
#' @param version WebR version to use ("latest" or specific version >= "v0.5.4")
#' @param base_url WebR application URL. If NULL, uses global option or builds from version
#'
#' @return
#' A `webr_directory` object. Its `urls` element is a named character vector
#' mapping each filename to its WebR sharelink.
#'
#' @examples
#' # A directory of R scripts
#' examples <- tempfile()
#' dir.create(examples)
#' writeLines("plot(1:10)", file.path(examples, "plot.R"))
#' writeLines("hist(rnorm(100))", file.path(examples, "hist.R"))
#'
#' links <- webr_repl_directory(examples, autorun = TRUE)
#' print(links)
#'
#' # Show only the editor and terminal panels
#' webr_repl_directory(examples, panels = c("editor", "terminal"))
#'
#' # Match a subset of files
#' webr_repl_directory(examples, pattern = "^plot")
#'
#' # The URLs, named by file
#' repl_urls(links)
#'
#' @export
webr_repl_directory <- function(directory_path,
                                autorun = FALSE,
                                pattern = "\\.R$",
                                base_path = "/home/web_user/",
                                panels = NULL,
                                version = "latest",
                                base_url = NULL) {

  check_single_string(directory_path, "directory_path")
  ensure_directory_exists(directory_path, "directory_path")
  check_single_logical(autorun, "autorun")
  check_valid_path(base_path, "base_path")
  check_valid_mode(panels, "panels")
  check_valid_version(version, "version")

  if (is.null(base_url)) {
    base_url <- get_webr_base_url(version)
  } else {
    check_single_string(base_url, "base_url")
  }

  if (!grepl("/$", base_path)) {
    base_path <- paste0(base_path, "/")
  }

  r_files <- list.files(directory_path, pattern = pattern, full.names = TRUE)

  if (length(r_files) == 0) {
    cli::cli_warn(c(
      "No files found",
      "!" = "No files matching pattern {.val {pattern}} found in {.path {directory_path}}",
      "i" = "Try adjusting the {.arg pattern} argument"
    ))
    # Stay type-stable: callers should get a webr_directory whether or not the
    # directory turned up any files.
    return(new_webr_directory(character(0), base_path, panels, version, directory_path))
  }

  cli::cli_inform(c(
    "v" = "Found {length(r_files)} file{?s} matching pattern {.val {pattern}}",
    "i" = "Processing files in {.path {directory_path}}..."
  ))

  links <- vapply(r_files, function(file) {
    tryCatch({
      code_text <- paste(readLines(file, warn = FALSE), collapse = "\n")
      filename <- basename(file)

      link_obj <- webr_repl_link(code_text,
                                 filename = filename,
                                 path = paste0(base_path, filename),
                                 autorun = autorun,
                                 panels = panels,
                                 version = version,
                                 base_url = base_url)

      link_obj$url
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to process file {.file {basename(file)}}",
        "x" = "{conditionMessage(e)}"
      ))
      NA_character_
    })
  }, character(1), USE.NAMES = FALSE)

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

  new_webr_directory(valid_links, base_path, panels, version, directory_path)
}
