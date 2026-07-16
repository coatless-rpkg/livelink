#' Convert files to Shinylive JSON format
#' @param files Named list where names are filenames and values are content
#' @return List in Shinylive JSON format
#' @noRd
files_to_shinylive_json <- function(files) {
  lapply(names(files), function(filename) {
    list(
      name = filename,
      content = files[[filename]],
      type = "text"  # For now, only support text files
    )
  })
}

#' Proper LZ-string compression for Shinylive URLs
#' @param json_string JSON string to compress
#' @return LZ-string compressed and encoded string
#' @noRd
compress_for_shinylive <- function(json_string) {
  compressed <- lzstring::compressToEncodedURIComponent(json_string)

  # Defensive: lz-string's URI-safe alphabet does not emit "/", so this is a
  # no-op in practice; it guards against a path separator ever landing in the
  # URL fragment, and other R encoders for this format do the same.
  gsub("/", "-", compressed)
}

#' Build Shinylive URL with proper format
#' @param engine Engine ("r" or "python")
#' @param mode Mode ("editor" or "app")
#' @param encoded_data LZ-string compressed file data
#' @param header Whether to show header in app mode (default: TRUE)
#' @param base_url Base Shinylive URL
#' @return Complete Shinylive URL
#' @noRd
build_shinylive_url <- function(engine, mode, encoded_data, header = TRUE, base_url = NULL) {
  if (is.null(base_url)) {
    engine_short <- if (engine == "python") "py" else "r"
    base_url <- paste0("https://shinylive.io/", engine_short, "/", mode, "/")
  }

  # Add header parameter for app mode if needed
  header_param <- if (mode == "app" && !header) "h=0&" else ""

  paste0(base_url, "#", header_param, "code=", encoded_data)
}


#' Build a Shinylive link from already-processed input
#'
#' Shared body of [shinylive_r_link()] and [shinylive_py_link()]. A lone code
#' string becomes the app's entry-point file; anything else is already a named
#' list of files.
#'
#' @param processed_input Result of [process_input()]
#' @param engine Engine: `"r"` or `"python"`
#' @param mode Shinylive mode: `"editor"` or `"app"`
#' @param header Logical. Whether to show the header in app mode
#' @param base_url Custom Shinylive base URL, or NULL for the default
#' @return shinylive_link object
#' @noRd
build_shinylive_link <- function(processed_input, engine, mode, header, base_url) {
  check_valid_shinylive_mode(mode, "mode")
  check_single_logical(header, "header")
  if (!is.null(base_url)) {
    check_single_string(base_url, "base_url")
  }

  if (is.character(processed_input) && length(processed_input) == 1) {
    entry_point <- if (engine == "python") "app.py" else "app.R"
    files_list <- stats::setNames(list(processed_input), entry_point)
  } else {
    files_list <- processed_input
  }

  url <- encode_shinylive_url(files_list, engine, mode, header, base_url)

  new_shinylive_link(url, files_list, engine, mode)
}

#' Encode Shinylive files into a share URL
#'
#' The shared encoding tail for Shinylive links: reshape to the Shinylive JSON
#' form, serialize, LZ-string-compress, then build the URL. Shared by
#' [build_shinylive_link()] and [shinylive_project()].
#' @param files_list Named list of file contents
#' @param engine "r" or "python"
#' @param mode "editor" or "app"
#' @param header Logical; whether to keep the Shinylive header
#' @param base_url Base URL, or NULL for the default
#' @return A Shinylive share URL
#' @noRd
encode_shinylive_url <- function(files_list, engine, mode, header, base_url) {
  shinylive_files <- files_to_shinylive_json(files_list)
  json_data <- jsonlite::toJSON(shinylive_files, auto_unbox = TRUE)
  encoded_data <- compress_for_shinylive(json_data)
  build_shinylive_url(engine, mode, encoded_data, header = header, base_url)
}

#' Create a Shinylive sharelink for Python Shiny apps
#'
#' Generates a shareable URL for Python Shiny applications that can run in the browser
#' using Shinylive. Supports character strings, file paths, named lists, and clipboard input.
#'
#' @param input App input. Can be:
#'   - Character string: Python code for the app
#'   - File path: Path to app.py file
#'   - Vector of file paths: Multiple files for the app
#'   - Named list: `list("app.py" = code1, "utils.py" = code2)`
#'   - NULL: Read from clipboard
#' @param mode Shinylive display mode (default `"editor"`). `"editor"` shows an
#'   editable code panel beside the running app; `"app"` shows only the running app.
#' @param header Logical, whether to show the Shinylive header bar. It applies only
#'   when `mode = "app"` and is ignored in the default `"editor"` mode. Defaults to `TRUE`.
#' @param base_url Custom Shinylive base URL. If NULL (default), links point at https://shinylive.io.
#'
#' @return shinylive_link object containing the Shinylive URL and metadata
#'
#' @seealso [shinylive_project()] for multi-file apps; [decode_shinylive_link()]
#'   and [preview_shinylive_link()] to read a link back.
#'
#' @export
#' @examples
#' # String input
#' app_code <- "
#' from shiny import App, render, ui
#' app_ui = ui.page_fluid(ui.h2('Hello World'))
#' def server(input, output, session): pass
#' app = App(app_ui, server)
#' "
#' shinylive_py_link(app_code)
#'
#' # Multiple files as a named list
#' shinylive_py_link(list(
#'   "app.py" = app_code,
#'   "utils.py" = "def helper(): return 42"
#' ))
#'
#' # File path input
#' app_dir <- tempfile()
#' dir.create(app_dir)
#' app_path <- file.path(app_dir, "app.py")
#' writeLines("from shiny import App, ui", app_path)
#' shinylive_py_link(app_path)
#'
#' # Read the app from the clipboard
#' if (interactive()) {
#'   shinylive_py_link()
#' }
shinylive_py_link <- function(input = NULL, mode = "editor", header = TRUE, base_url = NULL) {
  build_shinylive_link(
    process_shinylive_input(input = input),
    engine = "python",
    mode = mode,
    header = header,
    base_url = base_url
  )
}

#' Create a Shinylive sharelink for R Shiny apps
#'
#' Generates a shareable URL for R Shiny applications that can run in the browser
#' using Shinylive. Supports expressions, character strings, file paths, named lists,
#' and clipboard input.
#'
#' @param input App input. Can be:
#'   - R expression (no quotes needed): `shinylive_r_link({ shinyApp(ui, server) })`
#'   - Character string: R code for the app
#'   - File path: Path to app.R file
#'   - Vector of file paths: Multiple files for the app
#'   - Named list: `list("app.R" = code1, "utils.R" = code2)`
#'   - NULL: Read from clipboard
#' @param mode Shinylive display mode (default `"editor"`). `"editor"` shows an
#'   editable code panel beside the running app; `"app"` shows only the running app.
#' @param header Logical, whether to show the Shinylive header bar. It applies only
#'   when `mode = "app"` and is ignored in the default `"editor"` mode. Defaults to `TRUE`.
#' @param base_url Custom Shinylive base URL. If NULL (default), links point at https://shinylive.io.
#'
#' @return shinylive_link object containing the Shinylive URL and metadata
#'
#' @section Comments in expression input:
#' Comments inside a `{ }` expression are recovered from R's source references, so
#' they survive interactively but are dropped inside a knitted 'Quarto' or
#' 'R Markdown' document. Pass a string or a file path, or use the `livelink` chunk
#' engine, if you need them preserved. See [webr_repl_link()] for the details.
#'
#' @seealso [shinylive_project()] for multi-file apps; [decode_shinylive_link()]
#'   and [preview_shinylive_link()] to read a link back; [livelink-knitr] to give
#'   a document chunk its own link;
#'   `vignette("webr-and-shinylive", package = "livelink")` for the guide.
#'
#' @export
#' @examples
#' # Expression input (no quotes needed!)
#' shinylive_r_link({
#'   ui <- fluidPage(titlePanel("Hello World"))
#'   server <- function(input, output) {}
#'   shinyApp(ui, server)
#' })
#'
#' # Multiple files as a named list
#' shinylive_r_link(list(
#'   "app.R" = "library(shiny)\nshinyApp(fluidPage(), function(i, o) {})",
#'   "utils.R" = "helper <- function() 42"
#' ))
#'
#' # File path input
#' app_dir <- tempfile()
#' dir.create(app_dir)
#' app_path <- file.path(app_dir, "app.R")
#' writeLines("library(shiny)", app_path)
#' shinylive_r_link(app_path)
#'
#' # Read the app from the clipboard
#' if (interactive()) {
#'   shinylive_r_link()
#' }
shinylive_r_link <- function(input = NULL, mode = "editor", header = TRUE, base_url = NULL) {
  # Capture the expression form without evaluating `input`.
  x_expr <- substitute(input)
  processed_input <- if (!missing(input) && is_brace_call(x_expr)) {
    process_input(x_expr = x_expr)
  } else {
    process_shinylive_input(input = input, x_expr = x_expr,
                            env = parent.frame())
  }

  build_shinylive_link(
    processed_input,
    engine = "r",
    mode = mode,
    header = header,
    base_url = base_url
  )
}

#' Create a Shinylive sharelink for multi-file projects
#'
#' Creates Shinylive projects for either R or Python from named lists or
#' file path vectors.
#'
#' @param input Input for multiple files. Can be:
#'   - Named list: `list("app.R" = code1, "utils.R" = code2)`
#'   - Vector of file paths: `c("app.R", "utils.R", "data.csv")`
#' @param engine Engine to use: "r" for R Shiny or "python" for Python Shiny
#' @param mode Shinylive display mode (default `"editor"`). `"editor"` shows an
#'   editable code panel beside the running app; `"app"` shows only the running app.
#' @param header Logical, whether to show the Shinylive header bar. It applies only
#'   when `mode = "app"` and is ignored in the default `"editor"` mode. Defaults to `TRUE`.
#' @param base_url Custom Shinylive base URL. If NULL (default), links point at https://shinylive.io.
#'
#' @return shinylive_project object containing the Shinylive URL and metadata
#'
#' @seealso [shinylive_r_link()] and [shinylive_py_link()] for single apps;
#'   [decode_shinylive_link()] and [preview_shinylive_link()] to read a link back.
#'
#' @export
#' @examples
#' # Named list input
#' files <- list(
#'   "app.R" = "library(shiny)\nshinyApp(fluidPage(), function(i, o) {})",
#'   "utils.R" = "# Utility functions"
#' )
#' shinylive_project(files, engine = "r", mode = "editor")
#'
#' # File paths input
#' project_dir <- tempfile()
#' dir.create(project_dir)
#' app <- file.path(project_dir, "app.R")
#' utils <- file.path(project_dir, "utils.R")
#' writeLines("library(shiny)", app)
#' writeLines("# utils", utils)
#' shinylive_project(c(app, utils), engine = "r")
shinylive_project <- function(input, engine, mode = "editor", header = TRUE, base_url = NULL) {

  # Captured, not forced: a literal list() may name each file's contents as a
  # `{ ... }` block.
  x_expr <- substitute(input)
  processed_files <- process_project_input(
    input = input, x_expr = x_expr, env = parent.frame()
  )

  check_valid_shinylive_engine(engine, "engine")
  check_valid_shinylive_mode(mode, "mode")
  check_single_logical(header, "header")

  if (!is.null(base_url)) {
    check_single_string(base_url, "base_url")
  }

  url <- encode_shinylive_url(processed_files, engine, mode, header, base_url)

  new_shinylive_project(url, processed_files, engine, mode)
}

#' Create Shinylive sharelinks from a directory of Shiny apps
#'
#' @description
#' Batch processes directories containing Shiny applications to create individual Shinylive links.
#' Each subdirectory is treated as a separate Shiny app project.
#'
#' Only text files with extensions .R, .py, .txt, .md, .csv, .json, .yaml, or
#' .yml are embedded in a link. Other files (for example images or binary data)
#' are skipped with a warning.
#'
#' @param directory_path Character string specifying the path to the directory containing Shiny app directories
#' @param engine Engine to use: "r" for R Shiny or "python" for Python Shiny
#' @param mode Shinylive display mode (default `"editor"`). `"editor"` shows an
#'   editable code panel beside the running app; `"app"` shows only the running app.
#' @param header Logical, whether to show the Shinylive header bar. It applies only
#'   when `mode = "app"` and is ignored in the default `"editor"` mode. Defaults to `TRUE`.
#' @param app_file Main app filename to look for (default: "app.R" for R, "app.py" for Python)
#' @param base_url Custom Shinylive base URL. If NULL (default), links point at https://shinylive.io.
#'
#' @return shinylive_directory object containing URLs and metadata for all found apps
#'
#' @examples
#' # Each app lives in its own subdirectory:
#' #   shiny_apps/
#' #     app1/app.R
#' #     app2/app.R
#' shiny_apps <- tempfile()
#' dir.create(file.path(shiny_apps, "app1"), recursive = TRUE)
#' dir.create(file.path(shiny_apps, "app2"), recursive = TRUE)
#' writeLines("library(shiny)", file.path(shiny_apps, "app1", "app.R"))
#' writeLines("library(shiny)", file.path(shiny_apps, "app2", "app.R"))
#'
#' links <- shinylive_directory(shiny_apps, engine = "r", mode = "editor")
#' print(links)
#'
#' # Extract just the URLs
#' repl_urls(links)
#'
#' @export
shinylive_directory <- function(directory_path,
                                engine,
                                mode = "editor",
                                header = TRUE,
                                app_file = NULL,
                                base_url = NULL) {
  # Validate inputs
  check_single_string(directory_path, "directory_path")
  ensure_directory_exists(directory_path, "directory_path")
  check_valid_shinylive_engine(engine, "engine")
  check_valid_shinylive_mode(mode, "mode")
  check_single_logical(header, "header")

  if (!is.null(base_url)) {
    check_single_string(base_url, "base_url")
  }

  # Determine default app file
  if (is.null(app_file)) {
    app_file <- if (engine == "python") "app.py" else "app.R"
  }

  # Find subdirectories that contain the main app file
  subdirs <- list.dirs(directory_path, recursive = FALSE, full.names = TRUE)

  # vapply, not sapply: sapply() returns list() for zero subdirectories and
  # character(0)[list()] is an error, which made the "no apps found" branch below
  # unreachable in exactly the case it was written for.
  app_dirs <- subdirs[vapply(subdirs, function(dir) {
    file.exists(file.path(dir, app_file))
  }, logical(1))]

  if (length(app_dirs) == 0) {
    cli::cli_warn(c(
      "No Shiny apps found",
      "!" = "No directories containing {.file {app_file}} found in {.path {directory_path}}",
      "i" = "Each app should be in its own subdirectory with {.file {app_file}} as the main file"
    ))
    return(new_shinylive_directory(character(0), engine, mode, directory_path))
  }

  cli::cli_inform(c(
    "v" = "Found {length(app_dirs)} Shiny app{?s} in {.path {directory_path}}",
    "i" = "Processing {engine} Shiny apps..."
  ))

  # Process each app directory
  links <- sapply(app_dirs, function(app_dir) {
    tryCatch({
      app_name <- basename(app_dir)

      # Read all files in the app directory
      all_files <- list.files(app_dir, recursive = TRUE, full.names = TRUE)

      files <- list()
      for (file_path in all_files) {
        # Skip hidden files and directories
        if (grepl("^\\.", basename(file_path)) || file.info(file_path)$isdir) {
          next
        }

        # fixed = TRUE: app_dir is a filesystem path, not a regex. Windows
        # paths contain backslash sequences (\R, \U, \t) that are invalid TRE
        # escapes -- R-devel on Windows rejects them as errors, and other
        # flavors silently fail to match, leaking absolute paths into file
        # names.
        relative_path <- sub(paste0(app_dir, "/"), "", file_path, fixed = TRUE)

        # Read file content
        if (grepl("\\.(R|py|txt|md|csv|json|yaml|yml)$", file_path, ignore.case = TRUE)) {
          content <- readLines(file_path, warn = FALSE)
          files[[relative_path]] <- paste(content, collapse = "\n")
        } else {
          cli::cli_warn("Skipping binary file: {.file {relative_path}} in {.file {app_name}}")
        }
      }

      if (length(files) == 0) {
        cli::cli_warn("No readable files found in {.file {app_name}}")
        return(NA_character_)
      }

      # Create Shinylive project
      project <- shinylive_project(files, engine, mode, header, base_url)
      project$url

    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to process app {.file {basename(app_dir)}}",
        "x" = "{e$message}"
      ))
      NA_character_
    })
  }, USE.NAMES = FALSE)

  # Remove failed apps and set names
  valid_links <- links[!is.na(links)]
  names(valid_links) <- basename(app_dirs[!is.na(links)])

  failed_count <- sum(is.na(links))
  if (failed_count > 0) {
    cli::cli_warn(c(
      "Some apps failed to process",
      "!" = "{failed_count} app{?s} could not be processed"
    ))
  }

  cli::cli_inform(c(
    "v" = "Successfully created {length(valid_links)} Shinylive link{?s}"
  ))

  # Return shinylive_directory object
  new_shinylive_directory(valid_links, engine, mode, directory_path)
}

