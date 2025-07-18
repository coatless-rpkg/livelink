#' Decode Shinylive link(s) to extract files to local directory
#'
#' @description
#' Decodes Shinylive sharelinks to extract the embedded files and save them
#' to a local directory. This is the reverse operation of creating Shinylive links.
#' Handles both single URLs and multiple URLs automatically.
#'
#' @param url Character string or vector containing Shinylive URL(s)
#' @param output_dir Character string specifying the output directory path (default: "./shinylive_files")
#' @param overwrite Logical. Whether to overwrite existing files (default: FALSE)
#' @param create_subdir Logical. Whether to create subdirectories. For single URLs, creates
#'   a subdirectory named after the URL hash. For multiple URLs, creates numbered subdirectories
#'   (default: TRUE)
#' @param name_dirs Logical. For multiple URLs only: whether to name directories by URL index
#'   rather than hash (default: TRUE). Ignored for single URLs.
#'
#' @return For single URL: shinylive_decoded object. For multiple URLs: shinylive_decoded_batch object.
#'
#' @examples
#' \dontrun{
#' # Single URL
#' url <- "https://shinylive.io/r/editor/#code=NobwRAdgh..."
#' result <- decode_shinylive_link(url)
#' print(result)
#'
#' # Multiple URLs
#' urls <- c(
#'   "https://shinylive.io/r/editor/#code=...",
#'   "https://shinylive.io/py/app/#code=..."
#' )
#' results <- decode_shinylive_link(urls)
#' print(results)
#'
#' # Custom settings for single URL
#' result <- decode_shinylive_link(url,
#'                                output_dir = "./my_app",
#'                                create_subdir = FALSE,
#'                                overwrite = TRUE)
#'
#' # Custom settings for multiple URLs
#' results <- decode_shinylive_link(urls,
#'                                 output_dir = "./my_apps",
#'                                 name_dirs = FALSE)  # Use hash-based names
#' }
#'
#' @export
decode_shinylive_link <- function(url,
                                  output_dir = "./shinylive_files",
                                  overwrite = FALSE,
                                  create_subdir = TRUE,
                                  name_dirs = TRUE) {

  # Validate basic inputs
  check_character_vector(url, "url")
  check_single_string(output_dir, "output_dir")
  check_single_logical(overwrite, "overwrite")
  check_single_logical(create_subdir, "create_subdir")
  check_single_logical(name_dirs, "name_dirs")

  # Handle single URL case
  if (length(url) == 1) {
    return(decode_single_shinylive_link(url, output_dir, overwrite, create_subdir))
  }

  # Handle multiple URLs case
  return(decode_multiple_shinylive_links(url, output_dir, overwrite, create_subdir, name_dirs))
}

#' Decode a single Shinylive link
#' @param url Single URL string
#' @param output_dir Output directory
#' @param overwrite Whether to overwrite files
#' @param create_subdir Whether to create subdirectory
#' @return shinylive_decoded object
#' @keywords internal
decode_single_shinylive_link <- function(url, output_dir, overwrite, create_subdir) {
  # Validate URL
  check_valid_shinylive_url(url, "url")

  # Decompress and parse URL data
  cli::cli_inform("Decompressing Shinylive data...")
  url_data <- decompress_shinylive_url(url)

  engine <- url_data$engine
  mode <- url_data$mode
  files_data <- url_data$files_data

  cli::cli_inform("Parsing file data...")

  # Determine output directory
  final_output_dir <- if (create_subdir) {
    # Create subdirectory based on URL hash
    url_hash <- simple_hash(url, 8)
    file.path(output_dir, paste0("shinylive_", url_hash))
  } else {
    output_dir
  }

  # Create output directory
  if (!dir.exists(final_output_dir)) {
    dir.create(final_output_dir, recursive = TRUE, showWarnings = FALSE)
    cli::cli_inform("Created directory: {.path {final_output_dir}}")
  }

  # Decode and save files
  files_info <- decode_and_save_files(files_data, final_output_dir, overwrite)

  # Summary
  cli::cli_inform(c(
    "v" = "Successfully decoded {nrow(files_info)} file{?s} to {.path {final_output_dir}}"
  ))

  # Return shinylive_decoded object
  new_shinylive_decoded(files_info, final_output_dir, url, engine, mode)
}

#' Decode multiple Shinylive links
#' @param urls Vector of URL strings
#' @param output_dir Base output directory
#' @param overwrite Whether to overwrite files
#' @param create_subdir Whether to create subdirectories
#' @param name_dirs Whether to use numbered directory names
#' @return shinylive_decoded_batch object
#' @keywords internal
decode_multiple_shinylive_links <- function(urls, output_dir, overwrite, create_subdir, name_dirs) {
  if (length(urls) == 0) {
    cli::cli_warn("No URLs provided")
    return(new_shinylive_decoded_batch(list(), output_dir, character(0)))
  }

  # Validate all URLs first
  for (i in seq_along(urls)) {
    tryCatch({
      check_valid_shinylive_url(urls[i], paste0("url[", i, "]"))
    }, error = function(e) {
      cli::cli_abort(c(
        "Invalid URL at position {i}",
        "x" = "{e$message}"
      ))
    })
  }

  cli::cli_inform("Processing {length(urls)} Shinylive URL{?s}...")

  results <- list()

  for (i in seq_along(urls)) {
    url <- urls[i]

    tryCatch({
      # Determine subdirectory name
      if (!create_subdir) {
        # Use base output directory for each URL
        url_output_dir <- output_dir
        subdir_name <- paste0("url_", i)
      } else if (name_dirs) {
        subdir_name <- sprintf("app_%02d", i)
        url_output_dir <- file.path(output_dir, subdir_name)
      } else {
        url_hash <- simple_hash(url, 8)
        subdir_name <- paste0("shinylive_", url_hash)
        url_output_dir <- file.path(output_dir, subdir_name)
      }

      cli::cli_inform("")
      cli::cli_h3("Processing URL {i}/{length(urls)}: {subdir_name}")

      # Decode files using single URL logic
      decoded_result <- decode_single_shinylive_link(
        url = url,
        output_dir = url_output_dir,
        overwrite = overwrite,
        create_subdir = FALSE  # We handle subdirectory creation here
      )

      results[[subdir_name]] <- decoded_result

    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to process URL {i}",
        "x" = "{e$message}"
      ))
      results[[paste0("failed_", i)]] <- NULL
    })
  }

  successful_results <- results[!sapply(results, is.null)]
  cli::cli_inform(c(
    "",
    "v" = "Successfully processed {length(successful_results)}/{length(urls)} URL{?s}"
  ))

  # Return shinylive_decoded_batch object
  new_shinylive_decoded_batch(results, output_dir, urls)
}

#' Decode and save files from Shinylive data
#' @param files_data Parsed file data from Shinylive
#' @param output_dir Directory to save files
#' @param overwrite Whether to overwrite existing files
#' @return Data frame with file information
#' @keywords internal
decode_and_save_files <- function(files_data, output_dir, overwrite) {
  cli::cli_inform("Decoding {length(files_data)} file{?s}...")

  files_info <- data.frame(
    filename = character(0),
    path = character(0),
    type = character(0),
    size_bytes = numeric(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(files_data)) {
    file_info <- files_data[[i]]

    # Validate file structure
    if (!all(c("name", "content") %in% names(file_info))) {
      cli::cli_warn("Skipping invalid file entry {i}: missing required fields")
      next
    }

    filename <- file_info$name
    content <- file_info$content
    file_type <- if (!is.null(file_info$type)) file_info$type else "text"

    # Determine file path
    file_path <- file.path(output_dir, filename)

    # Create subdirectories if needed
    file_dir <- dirname(file_path)
    if (!dir.exists(file_dir)) {
      dir.create(file_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Check if file exists and handle overwrite
    if (file.exists(file_path) && !overwrite) {
      cli::cli_warn("File already exists, skipping: {.file {filename}}")
      next
    }

    # Save file based on type
    tryCatch({
      if (file_type == "binary") {
        # Decode base64 and write binary data
        binary_data <- base64enc::base64decode(content)
        writeBin(binary_data, file_path)
        file_size <- length(binary_data)
      } else {
        # Write text data
        writeLines(content, file_path, useBytes = TRUE)
        file_size <- nchar(content, type = "bytes")
      }

      # Add to results
      files_info <- rbind(files_info, data.frame(
        filename = filename,
        path = file_path,
        type = file_type,
        size_bytes = file_size,
        stringsAsFactors = FALSE
      ))

      size_label <- format_file_size(file_size)
      cli::cli_inform("  {.file {filename}} ({file_type}, {size_label})")

    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to save file: {.file {filename}}",
        "x" = "{e$message}"
      ))
    })
  }

  files_info
}

#' Decompress and parse Shinylive URL data
#' @param url Shinylive URL
#' @return List containing engine, mode, and files_data
#' @keywords internal
decompress_shinylive_url <- function(url) {
  # Extract URL metadata
  url_parts <- strsplit(url, "/")[[1]]
  engine <- if ("py" %in% url_parts) "python" else "r"
  mode <- if ("app" %in% url_parts) "app" else "editor"

  # Extract the encoded data from URL
  url_fragment_parts <- strsplit(url, "#")[[1]]
  if (length(url_fragment_parts) < 2) {
    cli::cli_abort(c(
      "No encoded data found in URL",
      "x" = "URL must contain encoded data after the '#' symbol"
    ))
  }

  # Parse URL parameters
  params_string <- url_fragment_parts[2]
  encoded_data <- extract_code_parameter(params_string)

  if (is.null(encoded_data)) {
    cli::cli_abort(c(
      "No 'code' parameter found in URL",
      "x" = "URL must contain a 'code' parameter with the encoded file data"
    ))
  }

  # Decompress the data
  tryCatch({

    # Decompress using LZ-string
    json_string <- lzstring::decompressFromEncodedURIComponent(encoded_data)

  }, error = function(e) {
    cli::cli_abort(c(
      "Error during decompression",
      "x" = "Failed to decompress LZ-string data: {e$message}",
      "i" = "The URL may be corrupted or use an unsupported encoding format"
    ))
  })

  # Parse JSON
  tryCatch({
    files_data <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Error parsing JSON data",
      "x" = "Failed to parse file data: {e$message}",
      "i" = "The decoded data may not be valid JSON"
    ))
  })

  list(
    engine = engine,
    mode = mode,
    files_data = files_data
  )
}

#' Extract code parameter from Shinylive URL fragment
#' @param params_string URL fragment part after '#'
#' @return Encoded data string or NULL if not found
#' @keywords internal
extract_code_parameter <- function(params_string) {
  # Handle both formats:
  # 1. Direct code parameter: code=...
  # 2. With other parameters: h=0&code=...

  # Split by & to handle multiple parameters
  params <- strsplit(params_string, "&")[[1]]

  for (param in params) {
    if (grepl("^code=", param)) {
      return(sub("^code=", "", param))
    }
  }

  return(NULL)
}

#' Preview Shinylive link contents without decoding to files
#'
#' @description
#' Decodes a Shinylive URL and returns information about the embedded files
#' without actually saving them to disk. Use print method options to control display.
#'
#' @param url Character string containing the Shinylive URL
#'
#' @return shinylive_preview object with file information and metadata
#'
#' @examples
#' \dontrun{
#' url <- "https://shinylive.io/r/editor/#code=..."
#'
#' # Create preview object
#' preview <- preview_shinylive_link(url)
#'
#' # Default print (no content)
#' print(preview)
#'
#' # Show file contents
#' print(preview, show_content = TRUE)
#'
#' # Show file contents with custom length limit
#' print(preview, show_content = TRUE, max_content_length = 200)
#'
#' # Access the preview data
#' preview$files_data
#' preview$total_files
#' }
#'
#' @export
preview_shinylive_link <- function(url) {

  # Validate inputs
  check_valid_shinylive_url(url, "url")

  # Decompress and parse URL data
  tryCatch({
    url_data <- decompress_shinylive_url(url)

    engine <- url_data$engine
    mode <- url_data$mode
    files_data <- url_data$files_data

    # Calculate metadata
    total_size <- 0
    file_types <- character(0)

    for (i in seq_along(files_data)) {
      file_info <- files_data[[i]]
      content <- file_info$content
      file_type <- if (!is.null(file_info$type)) file_info$type else "text"

      # Calculate size efficiently
      if (file_type == "binary") {
        size_bytes <- calculate_base64_size(content)
      } else {
        size_bytes <- nchar(content, type = "bytes")
      }

      total_size <- total_size + size_bytes
      file_types <- c(file_types, file_type)
    }

    # Return shinylive_preview object
    new_shinylive_preview(url, engine, mode, files_data, total_size, unique(file_types))

  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to preview Shinylive link",
      "x" = "{e$message}"
    ))
  })
}
