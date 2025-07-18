#' Decode webR REPL link(s) to extract files to local directory
#'
#' Decodes webR REPL sharelinks to extract the embedded files and save them
#' to a local directory. This is the reverse operation of creating webR links.
#' Handles both single URLs and multiple URLs automatically.
#'
#' @param url Character string or vector containing webR REPL URL(s)
#' @param output_dir Character string specifying the output directory path (default: "./webr_files")
#' @param overwrite Logical. Whether to overwrite existing files (default: FALSE)
#' @param create_subdir Logical. Whether to create subdirectories. For single URLs, creates
#'   a subdirectory named after the URL hash. For multiple URLs, creates numbered subdirectories
#'   (default: TRUE)
#' @param name_dirs Logical. For multiple URLs only: whether to name directories by URL index
#'   rather than hash (default: TRUE). Ignored for single URLs.
#'
#' @return
#' For single URL: webr_decoded object. For multiple URLs: webr_decoded_batch object.
#'
#' @include utils.R
#' @export
#' @examples
#' \dontrun{
#' # Single URL
#' url <- "https://webr.r-wasm.org/latest/#code=eJy1..."
#' result <- decode_webr_link(url)
#' print(result)
#'
#' # Multiple URLs
#' urls <- c(
#'   "https://webr.r-wasm.org/latest/#code=...",
#'   "https://webr.r-wasm.org/v0.5.4/#code=..."
#' )
#' results <- decode_webr_link(urls)
#' print(results)
#'
#' # Custom settings for single URL
#' result <- decode_webr_link(url,
#'                           output_dir = "./my_code",
#'                           create_subdir = FALSE,
#'                           overwrite = TRUE)
#'
#' # Custom settings for multiple URLs
#' results <- decode_webr_link(urls,
#'                            output_dir = "./my_scripts",
#'                            name_dirs = FALSE)  # Use hash-based names
#' }
decode_webr_link <- function(url,
                             output_dir = "./webr_files",
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
    return(decode_single_webr_link(url, output_dir, overwrite, create_subdir))
  }

  # Handle multiple URLs case
  return(decode_multiple_webr_links(url, output_dir, overwrite, create_subdir, name_dirs))
}

#' Decode a single webR REPL link
#' @param url Single URL string
#' @param output_dir Output directory
#' @param overwrite Whether to overwrite files
#' @param create_subdir Whether to create subdirectory
#' @return webr_decoded object
#' @keywords internal
decode_single_webr_link <- function(url, output_dir, overwrite, create_subdir) {
  # Validate URL
  check_valid_webr_url(url, "url")

  # Decompress and parse URL data
  cli::cli_inform("Decompressing webR data...")
  url_data <- decompress_webr_url(url)

  mode <- url_data$mode
  version <- url_data$version
  flags <- url_data$flags
  files_data <- url_data$files_data

  cli::cli_inform("Parsing file data...")

  # Determine output directory
  final_output_dir <- if (create_subdir) {
    # Create subdirectory based on URL hash
    url_hash <- simple_hash(url, 8)
    file.path(output_dir, paste0("webr_", url_hash))
  } else {
    output_dir
  }

  # Create output directory
  if (!dir.exists(final_output_dir)) {
    dir.create(final_output_dir, recursive = TRUE, showWarnings = FALSE)
    cli::cli_inform("Created directory: {.path {final_output_dir}}")
  }

  # Decode and save files
  files_info <- decode_and_save_webr_files(files_data, final_output_dir, overwrite)

  # Summary
  cli::cli_inform(c(
    "v" = "Successfully decoded {nrow(files_info)} file{?s} to {.path {final_output_dir}}"
  ))

  # Return webr_decoded object
  new_webr_decoded(files_info, final_output_dir, url, mode, version, flags)
}

#' Decode multiple webR REPL links
#' @param urls Vector of URL strings
#' @param output_dir Base output directory
#' @param overwrite Whether to overwrite files
#' @param create_subdir Whether to create subdirectories
#' @param name_dirs Whether to use numbered directory names
#' @return webr_decoded_batch object
#' @keywords internal
decode_multiple_webr_links <- function(urls, output_dir, overwrite, create_subdir, name_dirs) {
  if (length(urls) == 0) {
    cli::cli_warn("No URLs provided")
    return(new_webr_decoded_batch(list(), output_dir, character(0)))
  }

  # Validate all URLs first
  for (i in seq_along(urls)) {
    tryCatch({
      check_valid_webr_url(urls[i], paste0("url[", i, "]"))
    }, error = function(e) {
      cli::cli_abort(c(
        "Invalid URL at position {i}",
        "x" = "{e$message}"
      ))
    })
  }

  cli::cli_inform("Processing {length(urls)} webR URL{?s}...")

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
        subdir_name <- sprintf("script_%02d", i)
        url_output_dir <- file.path(output_dir, subdir_name)
      } else {
        url_hash <- simple_hash(url, 8)
        subdir_name <- paste0("webr_", url_hash)
        url_output_dir <- file.path(output_dir, subdir_name)
      }

      cli::cli_inform("")
      cli::cli_h3("Processing URL {i}/{length(urls)}: {subdir_name}")

      # Decode files using single URL logic
      decoded_result <- decode_single_webr_link(
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

  # Return webr_decoded_batch object
  new_webr_decoded_batch(results, output_dir, urls)
}

#' Decode and save files from webR data
#' @param files_data Parsed file data from webR
#' @param output_dir Directory to save files
#' @param overwrite Whether to overwrite existing files
#' @return Data frame with file information
#' @keywords internal
decode_and_save_webr_files <- function(files_data, output_dir, overwrite) {
  cli::cli_inform("Decoding {length(files_data)} file{?s}...")

  files_info <- data.frame(
    filename = character(0),
    path = character(0),
    autorun = logical(0),
    size_bytes = numeric(0),
    stringsAsFactors = FALSE
  )

  # Track reasons for skipped files
  skip_reasons <- list(
    invalid_structure = character(0),
    no_content = character(0),
    already_exists = character(0),
    save_failed = character(0)
  )

  for (i in seq_along(files_data)) {
    file_info <- files_data[[i]]

    # Validate file structure
    if (!all(c("name", "path") %in% names(file_info))) {
      cli::cli_warn("Skipping invalid file entry {i}: missing required fields")
      skip_reasons$invalid_structure <- c(skip_reasons$invalid_structure, paste0("entry_", i))
      next
    }

    # Check for content in either "text" or "data" field
    has_content <- ("text" %in% names(file_info)) || ("data" %in% names(file_info))
    if (!has_content) {
      filename <- if ("name" %in% names(file_info)) file_info$name else paste0("entry_", i)
      cli::cli_warn("Skipping file entry {i}: no content found")
      skip_reasons$no_content <- c(skip_reasons$no_content, filename)
      next
    }

    filename <- file_info$name
    file_path <- file.path(output_dir, filename)
    autorun <- isTRUE(file_info$autorun)

    # Create subdirectories if needed
    file_dir <- dirname(file_path)
    if (!dir.exists(file_dir)) {
      dir.create(file_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Check if file exists and handle overwrite
    if (file.exists(file_path) && !overwrite) {
      cli::cli_warn("File already exists, skipping: {.file {filename}}")
      skip_reasons$already_exists <- c(skip_reasons$already_exists, filename)
      next
    }

    # Save file based on content type
    tryCatch({
      # Determine content and type
      if ("text" %in% names(file_info)) {
        # Text content in "text" field
        content <- file_info$text
        is_binary <- FALSE
      } else if ("data" %in% names(file_info)) {
        # Content in "data" field - could be text or binary
        content <- file_info$data

        # For webR, "data" field typically contains text content (converted from raw)
        # We can detect if it's likely binary by checking for non-printable characters
        is_binary <- detect_binary_content(content)
      }

      if (is_binary) {
        # Handle as binary data
        if (is.character(content) && grepl("^[0-9a-f ]+$", content)) {
          # Hex string format - convert back to binary
          hex_chars <- strsplit(gsub(" ", "", content), "")[[1]]
          if (length(hex_chars) %% 2 == 0) {
            hex_pairs <- paste0(hex_chars[seq(1, length(hex_chars), 2)],
                                hex_chars[seq(2, length(hex_chars), 2)])
            binary_data <- as.raw(strtoi(hex_pairs, 16L))
            writeBin(binary_data, file_path)
            file_size <- length(binary_data)
          } else {
            # Odd number of hex chars, treat as text
            writeLines(content, file_path, useBytes = TRUE)
            file_size <- nchar(content, type = "bytes")
          }
        } else {
          # Treat as text
          writeLines(content, file_path, useBytes = TRUE)
          file_size <- nchar(content, type = "bytes")
        }
      } else {
        # Handle as text data
        writeLines(content, file_path, useBytes = TRUE)
        file_size <- nchar(content, type = "bytes")
      }

      # Add to results
      files_info <- rbind(files_info, data.frame(
        filename = filename,
        path = file_path,
        autorun = autorun,
        size_bytes = file_size,
        stringsAsFactors = FALSE
      ))

      size_label <- format_file_size(file_size)
      autorun_label <- if (autorun) " (autorun)" else ""
      cli::cli_inform("  {.file {filename}} ({size_label}){autorun_label}")

    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to save file: {.file {filename}}",
        "x" = "{e$message}"
      ))
      skip_reasons$save_failed <- c(skip_reasons$save_failed, filename)
    })
  }

  # Add skip reasons as attribute for better error reporting
  attr(files_info, "skip_reasons") <- skip_reasons

  files_info
}

#' Detect if content is likely binary
#' @param content Character string to check
#' @return Logical indicating if content is likely binary
#' @keywords internal
detect_binary_content <- function(content) {
  if (!is.character(content) || length(content) == 0 || nchar(content) == 0) {
    return(FALSE)
  }

  # Check for null bytes or other non-printable characters that suggest binary
  # For now, assume most webR content is text unless it's clearly hex-encoded
  raw_chars <- charToRaw(content)

  # Check for null bytes
  has_nulls <- any(raw_chars == 0)

  # Check percentage of non-printable characters (excluding common whitespace)
  printable <- raw_chars >= 32 & raw_chars <= 126  # Basic ASCII printable
  whitespace <- raw_chars %in% c(9, 10, 13)        # Tab, LF, CR
  valid_chars <- printable | whitespace

  # If more than 10% non-printable, consider binary
  non_printable_ratio <- sum(!valid_chars) / length(raw_chars)

  return(has_nulls || non_printable_ratio > 0.1)
}

#' Normalize msgpack data from RcppMsgPack format to list format
#' @param msgpack_data Raw msgpack data from RcppMsgPack
#' @return Normalized list structure
#' @keywords internal
normalize_msgpack_data <- function(msgpack_data) {
  if (!is.list(msgpack_data)) {
    return(msgpack_data)
  }

  # Handle direct key-value structure at the root level
  if (all(c("key", "value") %in% names(msgpack_data))) {
    return(list(convert_keyvalue_to_list(msgpack_data)))
  }

  # Process each item in the list
  normalized <- lapply(msgpack_data, function(item) {
    # Check if this item has the RcppMsgPack key-value structure
    if (is.list(item) && all(c("key", "value") %in% names(item))) {
      return(convert_keyvalue_to_list(item))
    } else {
      # If not the expected structure, return as-is
      return(item)
    }
  })

  return(normalized)
}

#' Convert key-value structure to named list
#' @param kv_item Item with key and value lists
#' @return Named list
#' @keywords internal
convert_keyvalue_to_list <- function(kv_item) {
  keys <- kv_item$key
  values <- kv_item$value

  if (length(keys) != length(values)) {
    cli::cli_warn("Mismatched key-value lengths in msgpack data")
    return(kv_item)
  }

  result <- list()

  for (i in seq_along(keys)) {
    key <- as.character(keys[[i]])
    value <- values[[i]]

    # Handle different value types
    if (is.raw(value)) {
      # Convert raw bytes to character string
      tryCatch({
        result[[key]] <- rawToChar(value)
      }, error = function(e) {
        # If rawToChar fails, keep as raw or convert to hex
        cli::cli_warn("Failed to convert raw data for key '{key}' to character: {e$message}")
        result[[key]] <- paste(sprintf("%02x", as.integer(value)), collapse = " ")
      })
    } else if (is.logical(value) && key == "autorun") {
      # Keep autorun as logical
      result[[key]] <- as.logical(value)
    } else {
      # Keep other values as-is
      result[[key]] <- value
    }
  }

  return(result)
}

#' Decompress and parse webR URL data
#' @param url webR URL
#' @return List containing mode, version, flags, and files_data
#' @keywords internal
decompress_webr_url <- function(url) {
  # Extract URL metadata
  url_parts <- parse_webr_url(url)

  mode <- url_parts$mode
  version <- url_parts$version
  encoded_data <- url_parts$encoded_data
  flags <- url_parts$flags

  if (is.null(encoded_data)) {
    cli::cli_abort(c(
      "No 'code' parameter found in URL",
      "x" = "URL must contain a 'code' parameter with the encoded file data"
    ))
  }

  # Decode URL encoding
  decoded_data <- utils::URLdecode(encoded_data)

  # Base64 decode
  tryCatch({
    binary_data <- base64enc::base64decode(decoded_data)
  }, error = function(e) {
    cli::cli_abort(c(
      "Error decoding base64 data",
      "x" = "Failed to decode base64: {e$message}",
      "i" = "The URL may be corrupted"
    ))
  })

  # Decompress if needed
  if (grepl("u", flags)) {
    # Uncompressed
    raw_data <- binary_data
  } else if (grepl("z", flags)) {
    # zlib/gzip compressed - try both formats
    tryCatch({
      raw_data <- memDecompress(binary_data, type = "gzip")
    }, error = function(e1) {
      tryCatch({
        # Try without gzip headers (raw zlib)
        raw_data <- memDecompress(binary_data, type = "unknown")
      }, error = function(e2) {
        cli::cli_abort(c(
          "Error during decompression",
          "x" = "Failed to decompress data: {e1$message}",
          "i" = "The URL may be corrupted or use an unsupported compression format"
        ))
      })
    })
  } else {
    cli::cli_abort(c(
      "Unknown compression flags",
      "x" = "Flags '{flags}' contain unknown compression options",
      "i" = "Supported flags: 'u' (uncompressed), 'z' (compressed)"
    ))
  }

  # Parse format
  if (grepl("m", flags)) {
    # msgpack format
    tryCatch({
      if (!requireNamespace("RcppMsgPack", quietly = TRUE)) {
        cli::cli_abort(c(
          "Package RcppMsgPack is required for msgpack format",
          "i" = "Install with: install.packages('RcppMsgPack')"
        ))
      }
      files_data_raw <- RcppMsgPack::msgpack_unpack(raw_data)

      # Normalize the msgpack data structure
      files_data <- normalize_msgpack_data(files_data_raw)

    }, error = function(e) {
      cli::cli_abort(c(
        "Error parsing msgpack data",
        "x" = "Failed to parse msgpack format: {e$message}",
        "i" = "The decoded data may not be valid msgpack"
      ))
    })
  } else if (grepl("j", flags)) {
    # JSON format
    tryCatch({
      json_string <- rawToChar(raw_data)
      files_data <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
    }, error = function(e) {
      cli::cli_abort(c(
        "Error parsing JSON data",
        "x" = "Failed to parse JSON format: {e$message}",
        "i" = "The decoded data may not be valid JSON"
      ))
    })
  } else {
    cli::cli_abort(c(
      "Unknown format flags",
      "x" = "Flags '{flags}' contain unknown format options",
      "i" = "Supported flags: 'm' (msgpack), 'j' (JSON)"
    ))
  }

  list(
    mode = mode,
    version = version,
    flags = flags,
    files_data = files_data
  )
}

#' Parse webR URL structure
#' @param url webR URL to parse
#' @return List with URL components
#' @keywords internal
parse_webr_url <- function(url) {
  # Extract version from URL path using a more robust approach
  version <- extract_version_from_url(url)

  # Split URL at fragment (#)
  url_parts <- strsplit(url, "#")[[1]]
  if (length(url_parts) < 2) {
    cli::cli_abort(c(
      "No encoded data found in URL",
      "x" = "URL must contain encoded data after the '#' symbol"
    ))
  }

  base_url <- url_parts[1]
  fragment <- url_parts[2]

  # Extract mode from base URL if present
  mode <- extract_mode_from_url(base_url)

  # Parse fragment parameters
  fragment_parts <- extract_webr_parameters(fragment)

  list(
    mode = mode,
    version = version,
    encoded_data = fragment_parts$code,
    flags = fragment_parts$flags
  )
}

#' Extract version from webR URL
#' @param url webR URL
#' @return Version string
#' @keywords internal
extract_version_from_url <- function(url) {
  # Use regmatches and regexec for more reliable capture group extraction
  pattern <- "webr\\.r-wasm\\.org/([^/]+)/"
  matches <- regexec(pattern, url)

  if (matches[[1]][1] == -1) {
    return("unknown")
  }

  # Extract the captured group (version part)
  match_result <- regmatches(url, matches)[[1]]

  if (length(match_result) >= 2) {
    version <- match_result[2]  # Second element is the captured group
  } else {
    version <- "unknown"
  }

  return(version)
}

#' Extract mode from webR URL
#' @param base_url Base part of URL before fragment
#' @return Mode string or NULL
#' @keywords internal
extract_mode_from_url <- function(base_url) {
  # Look for mode parameter in query string - simplified approach
  # Handle both quoted and unquoted mode values
  mode_patterns <- c(
    "mode=(['\"])([^'\"&]+)\\1",  # Quoted mode values
    "mode=([^&'\"]+)"             # Unquoted mode values
  )

  for (pattern in mode_patterns) {
    matches <- regexec(pattern, base_url)

    if (matches[[1]][1] != -1) {
      match_result <- regmatches(base_url, matches)[[1]]

      if (length(match_result) >= 3) {
        # Quoted pattern - mode is in third element
        mode <- match_result[3]
        return(mode)
      } else if (length(match_result) >= 2) {
        # Unquoted pattern - mode is in second element
        mode <- match_result[2]
        return(mode)
      }
    }
  }

  return(NULL)
}

#' Format mode string for display using cli
#' @param mode Mode string (like "editor-plot-terminal")
#' @return Formatted string for display
#' @keywords internal
format_mode_for_display <- function(mode) {
  if (is.null(mode) || !is.character(mode) || length(mode) != 1) {
    return(NULL)
  }

  # Split by hyphens and capitalize each component
  components <- strsplit(mode, "-")[[1]]
  components <- tools::toTitleCase(components)

  # Use cli to format with proper conjunctions (Oxford comma included by default)
  cli::cli_vec(components)
}

#' Extract parameters from webR URL fragment
#' @param fragment URL fragment part after '#'
#' @return List with code and flags
#' @keywords internal
extract_webr_parameters <- function(fragment) {
  # Split by & to handle multiple parameters
  params <- strsplit(fragment, "&")[[1]]

  code <- NULL
  flags <- NULL

  for (param in params) {
    if (grepl("^code=", param)) {
      code <- sub("^code=", "", param)
    } else if (nchar(param) > 0 && !grepl("=", param)) {
      # Flags are typically at the end without = sign
      flags <- param
    }
  }

  # Default flags if not specified
  if (is.null(flags)) {
    flags <- "mz"  # msgpack + compressed
  }

  list(code = code, flags = flags)
}


#' Preview webR REPL link contents without decoding to files
#'
#' @description
#' Decodes a webR URL and returns information about the embedded files
#' without actually saving them to disk. Use print method options to control display.
#'
#' @param url Character string containing the webR URL
#'
#' @return webr_preview object with file information and metadata
#'
#' @examples
#' \dontrun{
#' url <- "https://webr.r-wasm.org/latest/#code=..."
#'
#' # Create preview object
#' preview <- preview_webr_link(url)
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
preview_webr_link <- function(url) {

  # Validate inputs
  check_valid_webr_url(url, "url")

  # Decompress and parse URL data
  tryCatch({
    url_data <- decompress_webr_url(url)

    mode <- url_data$mode
    version <- url_data$version
    flags <- url_data$flags
    files_data <- url_data$files_data

    # Calculate metadata
    total_size <- 0
    autorun_files <- character(0)

    for (i in seq_along(files_data)) {
      file_info <- files_data[[i]]

      # Calculate size
      if ("text" %in% names(file_info)) {
        size_bytes <- nchar(file_info$text, type = "bytes")
      } else if ("data" %in% names(file_info)) {
        if (is.character(file_info$data)) {
          size_bytes <- nchar(file_info$data, type = "bytes")
        } else {
          size_bytes <- length(file_info$data)
        }
      } else {
        size_bytes <- 0
      }

      total_size <- total_size + size_bytes

      # Track autorun files
      if (isTRUE(file_info$autorun)) {
        autorun_files <- c(autorun_files, file_info$name)
      }
    }

    # Return webr_preview object
    new_webr_preview(url, mode, version, flags, files_data, total_size, autorun_files)

  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to preview webR link",
      "x" = "{e$message}"
    ))
  })
}
