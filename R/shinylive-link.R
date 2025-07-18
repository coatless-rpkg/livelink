#' Convert files to Shinylive JSON format
#' @param files Named list where names are filenames and values are content
#' @return List in Shinylive JSON format
#' @keywords internal
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
#' @keywords internal
compress_for_shinylive <- function(json_string) {
  # Use proper LZ-string compression like official Shinylive
  compressed <- lzstring::compressToEncodedURIComponent(json_string)

  # Replace "/" with "-" as done in official implementation
  gsub("/", "-", compressed)
}

#' Build Shinylive URL with proper format
#' @param engine Engine ("r" or "python")
#' @param mode Mode ("editor" or "app")
#' @param encoded_data LZ-string compressed file data
#' @param header Whether to show header in app mode (default: TRUE)
#' @param base_url Base Shinylive URL
#' @return Complete Shinylive URL
#' @keywords internal
build_shinylive_url <- function(engine, mode, encoded_data, header = TRUE, base_url = NULL) {
  if (is.null(base_url)) {
    engine_short <- if (engine == "python") "py" else "r"
    base_url <- paste0("https://shinylive.io/", engine_short, "/", mode, "/")
  }

  # Add header parameter for app mode if needed
  header_param <- if (mode == "app" && !header) "h=0&" else ""

  paste0(base_url, "#", header_param, "code=", encoded_data)
}


#' Create a Shinylive sharelink for R Shiny apps
#'
#' Generates a shareable URL for R Shiny applications that can run in the browser
#' using Shinylive. The app files are encoded and embedded in the URL.
#'
#' @param files Named list where names are filenames and values are file content as character strings.
#'   For single-file apps, can also pass a character string as the app.R content.
#' @param mode Shinylive mode: "editor" (show code editor) or "app" (show app only)
#' @param header Logical. Whether to show header in app mode (default: TRUE)
#' @param base_url Custom Shinylive base URL. If NULL, uses default Shinylive URL
#'
#' @return shinylive_link object containing the Shinylive URL and metadata
#'
#' @examples
#' # Simple single-file Shiny app
#' app_code <- '
#' library(shiny)
#' ui <- fluidPage(
#'   titlePanel("Hello Shinylive!"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       sliderInput("obs", "Number of observations:", min = 1, max = 1000, value = 500)
#'     ),
#'     mainPanel(plotOutput("distPlot"))
#'   )
#' )
#' server <- function(input, output) {
#'   output$distPlot <- renderPlot({
#'     hist(rnorm(input$obs), col = "darkgray", border = "white")
#'   })
#' }
#' shinyApp(ui = ui, server = server)
#' '
#'
#' link <- shinylive_r_link(app_code, mode = "app")
#' print(link)
#'
#' # Multi-file Shiny app
#' files <- list(
#'   "app.R" = app_code,
#'   "utils.R" = "helper_function <- function(x) { x * 2 }",
#'   "data.csv" = "x,y\n1,2\n3,4\n5,6"
#' )
#'
#' link <- shinylive_r_link(files, mode = "editor")
#'
#' @export
shinylive_r_link <- function(files, mode = "editor", header = TRUE, base_url = NULL) {
  # Handle single string input (convert to app.R)
  if (is.character(files) && length(files) == 1 && is.null(names(files))) {
    files <- list("app.R" = files)
  }

  # Validate inputs
  check_named_list(files, "files")
  check_valid_shinylive_mode(mode, "mode")

  if (!is.null(base_url)) {
    check_single_string(base_url, "base_url")
  }

  # Convert files to Shinylive JSON format
  shinylive_files <- files_to_shinylive_json(files)

  # Convert to JSON string
  json_data <- jsonlite::toJSON(shinylive_files, auto_unbox = TRUE)

  # Compress and encode
  encoded_data <- compress_for_shinylive(json_data)

  # Build URL
  url <- build_shinylive_url("r", mode, encoded_data, header = header, base_url)

  # Return shinylive_link object
  new_shinylive_link(url, files, "r", mode)
}

#' Create a Shinylive sharelink for Python Shiny apps
#'
#' @description
#' Generates a shareable URL for Python Shiny applications that can run in the browser
#' using Shinylive. The app files are encoded and embedded in the URL.
#'
#' @param files Named list where names are filenames and values are file content as character strings.
#'   For single-file apps, can also pass a character string as the app.py content.
#' @param mode Shinylive mode: "editor" (show code editor) or "app" (show app only)
#' @param header Logical. Whether to show header in app mode (default: TRUE)
#' @param base_url Custom Shinylive base URL. If NULL, uses default Shinylive URL
#'
#' @return shinylive_link object containing the Shinylive URL and metadata
#'
#' @examples
#' # Simple single-file Python Shiny app
#' app_code <- '
#' from shiny import App, render, ui
#'
#' app_ui = ui.page_fluid(
#'     ui.h2("Hello Shinylive!"),
#'     ui.input_slider("n", "N", 0, 100, 20),
#'     ui.output_text_verbatim("txt"),
#' )
#'
#' def server(input, output, session):
#'     @output
#'     @render.text
#'     def txt():
#'         return f"n*2 is {input.n() * 2}"
#'
#' app = App(app_ui, server)
#' '
#'
#' link <- shinylive_py_link(app_code, mode = "app")
#' print(link)
#'
#' # Multi-file Python Shiny app
#' files <- list(
#'   "app.py" = app_code,
#'   "utils.py" = "def helper_function(x):\n    return x * 2",
#'   "data.csv" = "x,y\n1,2\n3,4\n5,6"
#' )
#'
#' link <- shinylive_py_link(files, mode = "editor")
#'
#' @export
shinylive_py_link <- function(files, mode = "editor", header = TRUE, base_url = NULL) {
  # Handle single string input (convert to app.py)
  if (is.character(files) && length(files) == 1 && is.null(names(files))) {
    files <- list("app.py" = files)
  }

  # Validate inputs
  check_named_list(files, "files")
  check_valid_shinylive_mode(mode, "mode")

  if (!is.null(base_url)) {
    check_single_string(base_url, "base_url")
  }

  # Convert files to Shinylive JSON format
  shinylive_files <- files_to_shinylive_json(files)

  # Convert to JSON string
  json_data <- jsonlite::toJSON(shinylive_files, auto_unbox = TRUE)

  # Compress and encode
  encoded_data <- compress_for_shinylive(json_data)

  # Build URL
  url <- build_shinylive_url("python", mode, encoded_data, header = header, base_url)

  # Return shinylive_link object
  new_shinylive_link(url, files, "python", mode)
}

#' Create a Shinylive sharelink for multi-language projects
#'
#' @description
#' Unified function to create Shinylive projects for either R or Python.
#' This provides a consistent interface similar to webr_repl_project().
#'
#' @param files Named list where names are filenames and values are file content as character strings
#' @param engine Engine to use: "r" for R Shiny or "python" for Python Shiny
#' @param mode Shinylive mode: "editor" (show code editor) or "app" (show app only)
#' @param header Logical. Whether to show header in app mode (default: TRUE)
#' @param base_url Custom Shinylive base URL. If NULL, uses default Shinylive URL
#'
#' @return shinylive_project object containing the Shinylive URL and metadata
#'
#' @examples
#' # R Shiny project
#' r_files <- list(
#'   "app.R" = '
#' library(shiny)
#' source("utils.R")
#' ui <- fluidPage(titlePanel("My App"))
#' server <- function(input, output) {}
#' shinyApp(ui, server)
#'   ',
#'   "utils.R" = "helper_function <- function(x) { x * 2 }"
#' )
#'
#' r_project <- shinylive_project(r_files, engine = "r", mode = "editor")
#' print(r_project)
#'
#' # Python Shiny project
#' py_files <- list(
#'   "app.py" = '
#' from shiny import App, ui
#' from utils import process_data
#' app = App(ui.page_fluid("Hello"), None)
#'   ',
#'   "utils.py" = "def process_data(x): return x"
#' )
#'
#' py_project <- shinylive_project(py_files, engine = "python", mode = "app")
#'
#' @export
shinylive_project <- function(files, engine, mode = "editor", header = TRUE, base_url = NULL) {
  # Validate inputs
  check_named_list(files, "files")
  check_valid_shinylive_engine(engine, "engine")
  check_valid_shinylive_mode(mode, "mode")
  check_single_logical(header, "header")

  if (!is.null(base_url)) {
    check_single_string(base_url, "base_url")
  }

  # Convert files to Shinylive JSON format
  shinylive_files <- files_to_shinylive_json(files)

  # Convert to JSON string
  json_data <- jsonlite::toJSON(shinylive_files, auto_unbox = TRUE)

  # Compress using proper LZ-string compression
  encoded_data <- compress_for_shinylive(json_data)

  # Build URL
  url <- build_shinylive_url(engine, mode, encoded_data, header, base_url)

  # Return shinylive_project object
  new_shinylive_project(url, files, engine, mode)
}

#' Create Shinylive sharelinks from a directory of Shiny apps
#'
#' @description
#' Batch processes directories containing Shiny applications to create individual Shinylive links.
#' Each subdirectory is treated as a separate Shiny app project.
#'
#' @param directory_path Character string specifying the path to the directory containing Shiny app directories
#' @param engine Engine to use: "r" for R Shiny or "python" for Python Shiny
#' @param mode Shinylive mode: "editor" (show code editor) or "app" (show app only)
#' @param header Logical. Whether to show header in app mode (default: TRUE)
#' @param app_file Main app filename to look for (default: "app.R" for R, "app.py" for Python)
#' @param base_url Custom Shinylive base URL. If NULL, uses default Shinylive URL
#'
#' @return shinylive_directory object containing URLs and metadata for all found apps
#'
#' @examples
#' \dontrun{
#' # Process directory of R Shiny apps
#' # Directory structure:
#' # shiny_apps/
#' #   ├── app1/
#' #   │   ├── app.R
#' #   │   └── utils.R
#' #   └── app2/
#' #       ├── app.R
#' #       └── data.csv
#'
#' links <- shinylive_directory("./shiny_apps/", engine = "r", mode = "editor")
#' print(links)
#'
#' # Process directory of Python Shiny apps without header
#' links <- shinylive_directory("./py_apps/", engine = "python", mode = "app", header = FALSE)
#' }
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

  app_dirs <- subdirs[sapply(subdirs, function(dir) {
    file.exists(file.path(dir, app_file))
  })]

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

        relative_path <- sub(paste0(app_dir, "/"), "", file_path)

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

