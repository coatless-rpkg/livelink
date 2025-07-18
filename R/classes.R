#' Create a webr_link object
#' @param url WebR URL
#' @param filename Original filename
#' @param path Full path in WebR
#' @param mode Interface mode
#' @param version WebR version
#' @param autorun Whether autorun is enabled
#' @return webr_link object
#' @keywords internal
new_webr_link <- function(url, filename, path, mode, version, autorun) {
  structure(
    list(
      url = url,
      filename = filename,
      path = path,
      mode = mode,
      version = version,
      autorun = autorun
    ),
    class = "webr_link"
  )
}

#' Create a webr_project object
#' @param url WebR URL
#' @param files Named list of file contents
#' @param base_path Base directory path
#' @param mode Interface mode
#' @param version WebR version
#' @param autorun_files Files with autorun enabled
#' @return webr_project object
#' @keywords internal
new_webr_project <- function(url, files, base_path, mode, version, autorun_files) {
  structure(
    list(
      url = url,
      files = names(files),
      base_path = base_path,
      mode = mode,
      version = version,
      autorun_files = autorun_files
    ),
    class = "webr_project"
  )
}

#' Create a webr_exercise object
#' @param exercise webr_link object for exercise
#' @param solution webr_link object for solution
#' @return webr_exercise object
#' @keywords internal
new_webr_exercise <- function(exercise, solution) {
  structure(
    list(
      exercise = exercise,
      solution = solution
    ),
    class = "webr_exercise"
  )
}

#' Create a webr_directory object
#'
#' @param urls Named character vector of URLs
#' @param base_path Base directory path in WebR
#' @param mode Interface mode
#' @param version WebR version
#' @param source_directory Original source directory
#'
#' @return
#' webr_directory object
#'
#' @keywords internal
new_webr_directory <- function(urls, base_path, mode, version, source_directory) {
  structure(
    list(
      urls = urls,
      base_path = base_path,
      mode = mode,
      version = version,
      source_directory = source_directory
    ),
    class = "webr_directory"
  )
}

#' Create a webr_decoded object
#' @param files_info Data frame with file information
#' @param output_dir Output directory path
#' @param url Original webR URL
#' @param mode Interface mode
#' @param version WebR version
#' @param flags Encoding flags
#' @return webr_decoded object
#' @keywords internal
new_webr_decoded <- function(files_info, output_dir, url, mode, version, flags) {
  structure(
    list(
      files_info = files_info,
      output_dir = output_dir,
      url = url,
      mode = mode,
      version = version,
      flags = flags
    ),
    class = "webr_decoded"
  )
}

#' Create a webr_decoded_batch object
#' @param results List of webr_decoded objects
#' @param base_output_dir Base output directory
#' @param urls Original URLs
#' @return webr_decoded_batch object
#' @keywords internal
new_webr_decoded_batch <- function(results, base_output_dir, urls) {
  structure(
    list(
      results = results,
      base_output_dir = base_output_dir,
      urls = urls
    ),
    class = "webr_decoded_batch"
  )
}

#' Create a webr_preview object
#' @param url Original webR URL
#' @param mode Interface mode
#' @param version WebR version
#' @param flags Encoding flags
#' @param files_data Raw file data
#' @param total_size Total size in bytes
#' @param autorun_files Files with autorun enabled
#' @return webr_preview object
#' @keywords internal
new_webr_preview <- function(url, mode, version, flags, files_data, total_size, autorun_files) {
  structure(
    list(
      url = url,
      mode = mode,
      version = version,
      flags = flags,
      files_data = files_data,
      total_files = length(files_data),
      total_size = total_size,
      autorun_files = autorun_files
    ),
    class = "webr_preview"
  )
}


#' Create a shinylive_link object
#' @param url Shinylive URL
#' @param files Named list of file contents
#' @param engine Engine type ("r" or "python")
#' @param mode Shinylive mode ("editor" or "app")
#' @return shinylive_link object
#' @keywords internal
new_shinylive_link <- function(url, files, engine, mode) {
  structure(
    list(
      url = url,
      files = names(files),
      engine = engine,
      mode = mode
    ),
    class = "shinylive_link"
  )
}

#' Create a shinylive_project object
#' @param url Shinylive URL
#' @param files Named list of file contents
#' @param engine Engine type ("r" or "python")
#' @param mode Shinylive mode ("editor" or "app")
#' @return shinylive_project object
#' @keywords internal
new_shinylive_project <- function(url, files, engine, mode) {
  structure(
    list(
      url = url,
      files = names(files),
      engine = engine,
      mode = mode
    ),
    class = "shinylive_project"
  )
}

#' Create a shinylive_directory object
#' @param urls Named character vector of URLs
#' @param engine Engine type ("r" or "python")
#' @param mode Shinylive mode ("editor" or "app")
#' @param source_directory Original source directory
#' @return shinylive_directory object
#' @keywords internal
new_shinylive_directory <- function(urls, engine, mode, source_directory) {
  structure(
    list(
      urls = urls,
      engine = engine,
      mode = mode,
      source_directory = source_directory
    ),
    class = "shinylive_directory"
  )
}

#' Create a shinylive_decoded object
#' @param files_info Data frame with file information
#' @param output_dir Directory where files were saved
#' @param url Original Shinylive URL
#' @param engine Engine type
#' @param mode Shinylive mode
#' @return shinylive_decoded object
#' @keywords internal
new_shinylive_decoded <- function(files_info, output_dir, url, engine, mode) {
  structure(
    list(
      files_info = files_info,
      output_dir = output_dir,
      url = url,
      engine = engine,
      mode = mode,
      total_files = nrow(files_info),
      total_size = sum(files_info$size_bytes, na.rm = TRUE)
    ),
    class = "shinylive_decoded"
  )
}

#' Create a shinylive_decoded_batch object
#' @param results List of shinylive_decoded objects
#' @param base_dir Base output directory
#' @param urls Original URLs
#' @return shinylive_decoded_batch object
#' @keywords internal
new_shinylive_decoded_batch <- function(results, base_dir, urls) {
  total_files <- sum(sapply(results, function(x) if (!is.null(x)) x$total_files else 0))
  total_size <- sum(sapply(results, function(x) if (!is.null(x)) x$total_size else 0))

  structure(
    list(
      results = results,
      base_dir = base_dir,
      urls = urls,
      total_urls = length(urls),
      successful_urls = length(results[!sapply(results, is.null)]),
      total_files = total_files,
      total_size = total_size
    ),
    class = "shinylive_decoded_batch"
  )
}

#' Create a shinylive_preview object
#' @param url Original Shinylive URL
#' @param engine Engine type
#' @param mode Shinylive mode
#' @param files_data Raw file data
#' @param total_size Total size in bytes
#' @param file_types Unique file types found
#' @return shinylive_preview object
#' @keywords internal
new_shinylive_preview <- function(url, engine, mode, files_data, total_size, file_types) {
  structure(
    list(
      url = url,
      engine = engine,
      mode = mode,
      files_data = files_data,
      total_files = length(files_data),
      total_size = total_size,
      file_types = file_types
    ),
    class = "shinylive_preview"
  )
}

# Print methods for existing classes (webr_link, webr_project, etc.)

#' Print method for webr_link objects
#' @param x webr_link object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.webr_link <- function(x, ...) {
  cli::cli_h2("WebR Link")
  cli::cli_text("{.url {x$url}}")
  cli::cli_text("")

  cli::cli_text("File: {.file {x$filename}} \u2192 {.path {x$path}}")

  if (!is.null(x$mode)) {
    mode_str <- if (is.character(x$mode) && length(x$mode) == 1) {
      x$mode
    } else {
      paste(x$mode, collapse = "-")
    }
    cli::cli_text("Interface: {.val {mode_str}}")
  }

  cli::cli_text("Version: {.val {x$version}}")
  cli::cli_text("Autorun: {.val {x$autorun}}")

  invisible(x)
}

#' Print method for webr_project objects
#' @param x webr_project object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.webr_project <- function(x, ...) {
  cli::cli_h2("WebR Project")
  cli::cli_text("{.url {x$url}}")
  cli::cli_text("")

  cli::cli_text("Files ({length(x$files)}):")
  for (file in x$files) {
    path <- paste0(x$base_path, file)
    if (file %in% x$autorun_files) {
      cli::cli_text("  {.file {file}} \u2192 {.path {path}} {.emph (autorun)}")
    } else {
      cli::cli_text("  {.file {file}} \u2192 {.path {path}}")
    }
  }

  if (!is.null(x$mode)) {
    mode_str <- if (is.character(x$mode) && length(x$mode) == 1) {
      x$mode
    } else {
      paste(x$mode, collapse = "-")
    }
    cli::cli_text("")
    cli::cli_text("Interface: {.val {mode_str}}")
  }

  cli::cli_text("Version: {.val {x$version}}")

  invisible(x)
}

#' Print method for webr_exercise objects
#' @param x webr_exercise object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.webr_exercise <- function(x, ...) {
  cli::cli_h2("WebR Exercise")
  cli::cli_text("")

  cli::cli_h3("Exercise")
  cli::cli_text("{.url {x$exercise$url}}")
  cli::cli_text("File: {.file {x$exercise$filename}} \u2192 {.path {x$exercise$path}}")
  cli::cli_text("")

  cli::cli_h3("Solution")
  cli::cli_text("{.url {x$solution$url}}")
  cli::cli_text("File: {.file {x$solution$filename}} \u2192 {.path {x$solution$path}} {.emph (autorun)}")
  cli::cli_text("")

  if (!is.null(x$exercise$mode)) {
    mode_str <- if (is.character(x$exercise$mode) && length(x$exercise$mode) == 1) {
      x$exercise$mode
    } else {
      paste(x$exercise$mode, collapse = "-")
    }
    cli::cli_text("Interface: {.val {mode_str}}")
  }

  cli::cli_text("Version: {.val {x$exercise$version}}")

  invisible(x)
}

#' Print method for webr_directory objects
#' @param x webr_directory object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.webr_directory <- function(x, ...) {
  cli::cli_h2("WebR Directory Links")
  cli::cli_text("Source: {.path {x$source_directory}}")
  cli::cli_text("")

  cli::cli_text("Generated {length(x$urls)} link{?s}:")
  for (i in seq_along(x$urls)) {
    filename <- names(x$urls)[i]
    path <- paste0(x$base_path, filename)
    cli::cli_text("  {.file {filename}} \u2192 {.path {path}}")
    cli::cli_text("    {.url {x$urls[i]}}")
    if (i < length(x$urls)) cli::cli_text("")
  }

  if (!is.null(x$mode)) {
    mode_str <- if (is.character(x$mode) && length(x$mode) == 1) {
      x$mode
    } else {
      paste(x$mode, collapse = "-")
    }
    cli::cli_text("")
    cli::cli_text("Interface: {.val {mode_str}}")
  }

  cli::cli_text("Version: {.val {x$version}}")

  invisible(x)
}



#' Print method for webr_decoded objects
#' @param x webr_decoded object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.webr_decoded <- function(x, ...) {
  cli::cli_h2("WebR Decoded Files")
  cli::cli_text("Source: {.url {x$url}}")
  cli::cli_text("Output: {.path {x$output_dir}}")
  cli::cli_text("")

  if (nrow(x$files_info) == 0) {
    # Show detailed reasons why no files were decoded
    skip_reasons <- attr(x$files_info, "skip_reasons")

    if (!is.null(skip_reasons)) {
      reasons_found <- sum(lengths(skip_reasons)) > 0

      if (reasons_found) {
        cli::cli_text("No files were saved due to the following issues:")

        if (length(skip_reasons$invalid_structure) > 0) {
          cli::cli_text("  {cli::symbol$cross} {length(skip_reasons$invalid_structure)} file{?s} had invalid structure (missing name or path)")
        }

        if (length(skip_reasons$no_content) > 0) {
          files_text <- if (length(skip_reasons$no_content) > 3) {
            paste0(paste(skip_reasons$no_content[1:3], collapse = ", "), ", and {length(skip_reasons$no_content) - 3} more")
          } else {
            cli::cli_vec(skip_reasons$no_content)
          }
          cli::cli_text("  {cli::symbol$cross} {length(skip_reasons$no_content)} file{?s} had no content: {files_text}")
        }

        if (length(skip_reasons$already_exists) > 0) {
          files_text <- if (length(skip_reasons$already_exists) > 3) {
            paste0(paste(skip_reasons$already_exists[1:3], collapse = ", "), ", and {length(skip_reasons$already_exists) - 3} more")
          } else {
            cli::cli_vec(skip_reasons$already_exists)
          }
          cli::cli_text("  {cli::symbol$cross} {length(skip_reasons$already_exists)} file{?s} already exist{?s}: {files_text}")
          cli::cli_text("    {cli::symbol$info} Use {.code overwrite = TRUE} to replace existing files")
        }

        if (length(skip_reasons$save_failed) > 0) {
          files_text <- if (length(skip_reasons$save_failed) > 3) {
            paste0(paste(skip_reasons$save_failed[1:3], collapse = ", "), ", and {length(skip_reasons$save_failed) - 3} more")
          } else {
            cli::cli_vec(skip_reasons$save_failed)
          }
          cli::cli_text("  {cli::symbol$cross} {length(skip_reasons$save_failed)} file{?s} failed to save: {files_text}")
        }
      } else {
        cli::cli_text("No files were found in the decoded data.")
      }
    } else {
      cli::cli_text("No files were found in the decoded data.")
    }
  } else {
    cli::cli_text("Files ({nrow(x$files_info)}):")
    for (i in seq_len(nrow(x$files_info))) {
      file_info <- x$files_info[i, ]
      size_label <- format_file_size(file_info$size_bytes)
      autorun_label <- if (file_info$autorun) " {.emph (autorun)}" else ""
      cli::cli_text("  {.file {file_info$filename}} ({size_label}){autorun_label}")
    }
  }

  cli::cli_text("")

  # Always show metadata
  if (!is.null(x$mode)) {
    formatted_mode <- format_mode_for_display(x$mode)
    if (!is.null(formatted_mode)) {
      cli::cli_text("Interface: {.val {formatted_mode}}")
    }
  }

  cli::cli_text("Version: {.val {x$version}}")
  cli::cli_text("Encoding: {.val {x$flags}}")

  invisible(x)
}

#' Print method for webr_decoded_batch objects
#' @param x webr_decoded_batch object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.webr_decoded_batch <- function(x, ...) {
  cli::cli_h2("WebR Decoded Batch")
  cli::cli_text("Base directory: {.path {x$base_output_dir}}")
  cli::cli_text("Total URLs: {length(x$urls)}")
  cli::cli_text("")

  successful_results <- x$results[!sapply(x$results, is.null)]

  if (length(successful_results) == 0) {
    cli::cli_text("No URLs were successfully processed.")
    invisible(x)
    return()
  }

  cli::cli_text("Successfully processed {length(successful_results)} URL{?s}:")

  total_files_saved <- 0

  for (i in seq_along(successful_results)) {
    result_name <- names(successful_results)[i]
    result <- successful_results[[i]]
    file_count <- nrow(result$files_info)
    total_files_saved <- total_files_saved + file_count

    if (file_count > 0) {
      cli::cli_text("  {.file {result_name}}: {file_count} file{?s}")
    } else {
      # Check for skip reasons
      skip_reasons <- attr(result$files_info, "skip_reasons")
      if (!is.null(skip_reasons) && sum(lengths(skip_reasons)) > 0) {
        cli::cli_text("  {.file {result_name}}: No files saved (see individual warnings)")
      } else {
        cli::cli_text("  {.file {result_name}}: No files found")
      }
    }

    cli::cli_text("    {.path {result$output_dir}}")
    if (i < length(successful_results)) cli::cli_text("")
  }

  failed_count <- length(x$urls) - length(successful_results)
  if (failed_count > 0) {
    cli::cli_text("")
    cli::cli_text("Failed: {failed_count} URL{?s}")
  }

  cli::cli_text("")
  cli::cli_text("Summary: {total_files_saved} total file{?s} saved across {length(successful_results)} URL{?s}")

  invisible(x)
}

#' Print method for webr_preview objects
#' @param x webr_preview object
#' @param show_content Logical. Whether to show file contents (default: FALSE)
#' @param max_content_length Maximum length of content to show (default: 500)
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.webr_preview <- function(x, show_content = FALSE, max_content_length = 500, ...) {
  cli::cli_h2("WebR Link Preview")
  cli::cli_text("URL: {.url {x$url}}")
  cli::cli_text("")

  total_size_label <- format_file_size(x$total_size)
  cli::cli_text("Files: {x$total_files}")
  cli::cli_text("Total size: {total_size_label}")

  if (length(x$autorun_files) > 0) {
    cli::cli_text("Autorun files: {length(x$autorun_files)}")
  }

  if (!is.null(x$mode)) {
    formatted_mode <- format_mode_for_display(x$mode)
    if (!is.null(formatted_mode)) {
      cli::cli_text("Interface: {.val {formatted_mode}}")
    }
  }

  cli::cli_text("Version: {.val {x$version}}")
  cli::cli_text("Encoding: {.val {x$flags}}")
  cli::cli_text("")

  # Show file details
  for (i in seq_along(x$files_data)) {
    file_info <- x$files_data[[i]]
    filename <- file_info$name
    autorun_indicator <- if (isTRUE(file_info$autorun)) " {.emph (autorun)}" else ""

    # Calculate file size and get content
    if ("text" %in% names(file_info)) {
      file_size <- nchar(file_info$text, type = "bytes")
      content <- file_info$text
      is_text <- TRUE
    } else if ("data" %in% names(file_info)) {
      content <- file_info$data
      if (is.character(content)) {
        file_size <- nchar(content, type = "bytes")
        is_text <- !detect_binary_content(content)
      } else {
        file_size <- length(content)
        content <- "[Binary data]"
        is_text <- FALSE
      }
    } else {
      file_size <- 0
      content <- "[No content]"
      is_text <- FALSE
    }

    size_label <- format_file_size(file_size)
    cli::cli_text("{.file {filename}} ({size_label}){autorun_indicator}")

    if (show_content && is_text && nchar(content) > 0) {
      # Show truncated content
      display_content <- if (nchar(content) <= max_content_length) {
        content
      } else {
        paste0(substr(content, 1, max_content_length), "...")
      }

      # Add indentation to all lines and show as verbatim
      content_lines <- strsplit(display_content, "\n")[[1]]
      indented_lines <- paste0("  ", content_lines)
      indented_content <- paste(indented_lines, collapse = "\n")

      cli::cli_verbatim(indented_content)
    }

    if (i < length(x$files_data)) cli::cli_text("")
  }

  if (!show_content && any(sapply(x$files_data, function(f) {
    ("text" %in% names(f)) || ("data" %in% names(f) && is.character(f$data))
  }))) {
    cli::cli_text("")
    cli::cli_text("{.emph Use print(preview, show_content = TRUE) to see file contents}")
  }

  invisible(x)
}

#' Print method for shinylive_link objects
#' @param x shinylive_link object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.shinylive_link <- function(x, ...) {
  engine_name <- if (x$engine == "python") "Python" else "R"
  cli::cli_h2("Shinylive {engine_name} App")
  cli::cli_text("{.url {x$url}}")
  cli::cli_text("")

  cli::cli_text("Files ({length(x$files)}):")
  for (file in x$files) {
    cli::cli_text("  {.file {file}}")
  }

  cli::cli_text("")
  cli::cli_text("Engine: {.val {engine_name}}")
  cli::cli_text("Mode: {.val {x$mode}}")
}

#' Print method for shinylive_project objects
#' @param x shinylive_project object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.shinylive_project <- function(x, ...) {
  engine_name <- if (x$engine == "python") "Python" else "R"
  cli::cli_h2("Shinylive {engine_name} Project")
  cli::cli_text("{.url {x$url}}")
  cli::cli_text("")

  cli::cli_text("Files ({length(x$files)}):")
  for (file in x$files) {
    cli::cli_text("  {.file {file}}")
  }

  cli::cli_text("")
  cli::cli_text("Engine: {.val {engine_name}}")
  cli::cli_text("Mode: {.val {x$mode}}")

  invisible(x)
}

#' Print method for shinylive_directory objects
#' @param x shinylive_directory object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the object
#' @export
print.shinylive_directory <- function(x, ...) {
  engine_name <- if (x$engine == "python") "Python" else "R"
  cli::cli_h2("Shinylive {engine_name} Directory")
  cli::cli_text("Source: {.path {x$source_directory}}")
  cli::cli_text("")

  if (length(x$urls) == 0) {
    cli::cli_text("No apps found.")
    invisible(x)
    return()
  }

  cli::cli_text("Generated {length(x$urls)} app{?s}:")
  for (i in seq_along(x$urls)) {
    app_name <- names(x$urls)[i]
    cli::cli_text("  {.file {app_name}}")
    cli::cli_text("    {.url {x$urls[i]}}")
    if (i < length(x$urls)) cli::cli_text("")
  }

  cli::cli_text("")
  cli::cli_text("Engine: {.val {engine_name}}")
  cli::cli_text("Mode: {.val {x$mode}}")

  invisible(x)
}
