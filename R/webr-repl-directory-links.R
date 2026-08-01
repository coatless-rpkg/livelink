#' Create webR REPL sharelinks from a directory of R files
#'
#' Batch processes all R files in a directory into webR sharelinks.
#'
#' @param directory_path Character string specifying the path to the directory containing R files
#' @param autorun Logical. Whether to enable autorun for all generated links.
#'   Defaults to `FALSE`. With `single_link = TRUE`, this runs every R file in
#'   the bundle on arrival.
#' @param single_link Logical. If `FALSE` (default), each matched file becomes its
#'   own link and the result is a `webr_directory`. If `TRUE`, all matched files
#'   are packed into one link and the result is a single `webr_project`.
#' @param pattern Regular expression matched against file names in
#'   `directory_path`. Defaults to `"\\.R$"`, i.e. files ending in `.R`.
#' @param base_path Base directory path for files in webR. Defaults to
#'   `"/home/web_user/"`.
#' @param panels Character vector or string specifying which webR interface panels to show.
#'   The valid panels are "plot", "files", "terminal", and "editor". Can be
#'   c("plot", "files") or "plot-files". If NULL (default), shows all panels.
#' @param version webR version to use ("latest" or specific version >= "v0.5.4")
#' @param base_url webR application URL. If NULL, uses global option or builds from version
#'
#' @return
#' By default, a `webr_directory` object, which is a list with these entries.
#'
#' - `urls`, the sharelinks, as a named character vector with one entry per
#'   matched file, named by file name.
#' - `base_path`, where the files are placed inside webR.
#' - `mode`, the panels the links ask for, or `NULL` for all of them.
#' - `version`, the webR version the links point at.
#' - `source_directory`, the directory the files were read from.
#'
#' With `single_link = TRUE`, a `webr_project` object instead, which is a list
#' with these entries.
#'
#' - `url`, the one sharelink that carries every matched file, as a single
#'   string.
#' - `files`, the file contents that went into the link, as a named list keyed
#'   by file name.
#' - `base_path`, where the files are placed inside webR.
#' - `mode`, the panels the link asks for, or `NULL` for all of them.
#' - `version`, the webR version the link points at.
#' - `autorun_files`, the files that run as soon as the link opens.
#'
#' Use `as.character()` on either object to get the URLs on their own.
#'
#' @details
#' By default each file becomes its own webR sharelink. With
#' `single_link = TRUE` the whole directory is bundled into one link instead,
#' exactly as [webr_repl_project()] would. Useful for converting collections of
#' scripts, examples, or course materials.
#'
#' @seealso
#' [webr_repl_project()], which bundles a named list or a vector of file paths
#' into one link.
#'
#' @export
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
#' # Bundle the whole directory into one link instead
#' webr_repl_directory(examples, single_link = TRUE, panels = c("editor", "plot"))
#'
#' # Show only the editor and terminal panels
#' webr_repl_directory(examples, panels = c("editor", "terminal"))
#'
#' # Match a subset of files
#' webr_repl_directory(examples, pattern = "^plot")
#'
#' # The URLs, named by file
#' repl_urls(links)
webr_repl_directory <- function(directory_path,
                                autorun = FALSE,
                                single_link = FALSE,
                                pattern = "\\.R$",
                                base_path = "/home/web_user/",
                                panels = NULL,
                                version = "latest",
                                base_url = NULL) {

  check_single_string(directory_path, "directory_path")
  ensure_directory_exists(directory_path, "directory_path")
  check_single_logical(autorun, "autorun")
  check_single_logical(single_link, "single_link")
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

  if (single_link) {
    return(directory_single_link(
      r_files, autorun, base_path, panels, version, base_url, directory_path
    ))
  }

  cli::cli_inform(c(
    "v" = "Found {length(r_files)} file{?s} matching pattern {.val {pattern}}",
    "i" = "Processing files in {.path {directory_path}}..."
  ))

  links <- vapply(r_files, function(file) {
    tryCatch({
      code_text <- read_file_for_link(file)
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


#' Bundle every file in a directory into one webR project link
#'
#' The `single_link = TRUE` branch of [webr_repl_directory()]. Reads each matched
#' file, names it by its basename, and hands the lot to [build_webr_project()].
#'
#' @param r_files Character vector of matched file paths
#' @param autorun Logical. `TRUE` runs every R file on arrival
#' @param base_path Normalized base directory path
#' @param panels Panels to show, or NULL
#' @param version WebR version
#' @param base_url Resolved WebR base URL
#' @param directory_path The source directory, for messages
#'
#' @return
#' A `webr_project` object, a list holding `url`, `files`, `base_path`, `mode`,
#' `version`, and `autorun_files`. See the return value of
#' [webr_repl_directory()] for what each entry holds.
#'
#' @noRd
directory_single_link <- function(r_files, autorun, base_path, panels, version,
                                  base_url, directory_path) {
  contents <- lapply(r_files, function(file) {
    tryCatch(
      read_file_for_link(file),
      error = function(e) {
        cli::cli_warn(c(
          "Failed to read file {.file {basename(file)}}",
          "x" = "{conditionMessage(e)}"
        ))
        NA_character_
      }
    )
  })

  ok <- !vapply(contents, function(x) length(x) == 1 && is.na(x), logical(1))
  processed_files <- stats::setNames(contents[ok], basename(r_files[ok]))

  if (length(processed_files) == 0) {
    cli::cli_abort(c(
      "No files could be read",
      "x" = "Every file matched in {.path {directory_path}} failed to read"
    ))
  }

  cli::cli_inform(c(
    "v" = "Bundling {length(processed_files)} file{?s} into one link"
  ))

  autorun_files <- if (autorun) "all" else character(0)
  build_webr_project(processed_files, autorun_files, base_path, panels,
                     version, base_url)
}
