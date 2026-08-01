#' Decode webR REPL link(s) to extract files to local directory
#'
#' Decodes webR REPL sharelinks to extract the embedded files and save them
#' to a local directory.
#'
#' @param url Character string or vector containing webR REPL URL(s)
#' @param output_dir Character string specifying the output directory path.
#'   Defaults to a `webr_files` directory inside the session temporary
#'   directory. Pass an explicit path to extract somewhere permanent.
#' @param overwrite Logical. Whether to overwrite existing files. Defaults to
#'   `FALSE`.
#' @param create_subdir Logical. If `TRUE` (default), each decoded link is
#'   extracted into its own subdirectory under `output_dir` rather than directly
#'   into it. For a single URL the subdirectory is named `webr_<hash>`, where
#'   `<hash>` is a short fingerprint of the URL. For multiple URLs, see
#'   `name_dirs`. Set `FALSE` to extract straight into `output_dir`.
#' @param name_dirs Logical. For multiple URLs, controls how the per-link
#'   subdirectories are named.
#'
#'   - `TRUE` (default) numbers them `script_01`, `script_02`, and so on.
#'   - `FALSE` names each one `webr_<hash>` from the URL fingerprint.
#'
#'   Ignored for a single URL, and ignored when `create_subdir = FALSE` (all
#'   files then extract into `output_dir`).
#'
#' @param binary Logical. Whether to write binary files held in the link
#'   (default: `FALSE`). A preview can show you text but not bytes, so a binary
#'   is left alone unless you ask for it.
#' @return
#' What comes back depends on how many URLs you pass.
#'
#' For one URL, a `webr_decoded` object, which is a list with these entries.
#'
#' - `files_info`, a data frame of the files written, one row per file, with
#'   the columns `filename`, `path`, `autorun`, and `size_bytes`.
#' - `output_dir`, the directory the files were written to.
#' - `url`, the link that was decoded.
#' - `mode`, the panels the link asks for, or `NULL` for all of them.
#' - `version`, the webR version the link points at.
#' - `flags`, the encoding flags read off the link.
#' - `total_files`, how many files were written.
#' - `total_size`, the size of those files in bytes.
#'
#' For several URLs, a `webr_decoded_batch` object, which is a list with these
#' entries.
#'
#' - `results`, a list of `webr_decoded` objects, one for each URL that decoded.
#' - `base_dir`, the directory the per-link subdirectories sit under.
#' - `urls`, the URLs you passed in, one per entry.
#' - `total_urls`, how many URLs were handed in.
#' - `successful_urls`, how many of them decoded.
#' - `total_files`, how many files were written across every URL.
#' - `total_size`, the size of those files in bytes.
#'
#' @details
#' This is the reverse operation of creating webR links. Handles both single
#' URLs and multiple URLs automatically.
#'
#' @seealso
#' [preview_webr_link()] to inspect a link without writing files.
#'
#' [webr_repl_link()], [webr_repl_project()], and [webr_repl_exercise()] create
#' the links this function decodes.
#'
#' @include utils.R
#' @export
#' @examples
#' # Round-trip: build a link, then decode it back to files
#' url <- as.character(webr_repl_link("plot(1:10)"))
#'
#' result <- decode_webr_link(url)
#' print(result)
#'
#' # Extract to a directory of your choosing
#' out <- file.path(tempdir(), "my_code")
#' decode_webr_link(url, output_dir = out, create_subdir = FALSE, overwrite = TRUE)
#' list.files(out)
#'
#' # Several links at once
#' urls <- c(url, as.character(webr_repl_link("hist(rnorm(100))")))
#' decode_webr_link(urls, output_dir = file.path(tempdir(), "my_scripts"))
decode_webr_link <- function(url,
                             output_dir = file.path(tempdir(), "webr_files"),
                             overwrite = FALSE,
                             create_subdir = TRUE,
                             name_dirs = TRUE,
                             binary = FALSE) {

  # Validate basic inputs
  check_character_vector(url, "url")
  check_single_logical(binary, "binary")
  check_single_string(output_dir, "output_dir")
  check_single_logical(overwrite, "overwrite")
  check_single_logical(create_subdir, "create_subdir")
  check_single_logical(name_dirs, "name_dirs")

  # Handle single URL case
  if (length(url) == 1) {
    return(decode_single_webr_link(url, output_dir, overwrite, create_subdir, binary))
  }

  # Handle multiple URLs case
  return(decode_multiple_webr_links(url, output_dir, overwrite, create_subdir, name_dirs, binary))
}

#' Decode a single webR REPL link
#'
#' @param url Single URL string
#' @param output_dir Output directory
#' @param overwrite Whether to overwrite files
#' @param create_subdir Whether to create subdirectory
#'
#' @return
#' A `webr_decoded` object, the list of `files_info`, `output_dir`, `url`,
#' `mode`, `version`, `flags`, `total_files`, and `total_size` described under
#' [decode_webr_link()].
#'
#' @noRd
decode_single_webr_link <- function(url, output_dir, overwrite, create_subdir,
                                    binary = FALSE) {
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
  files_info <- decode_and_save_webr_files(files_data, final_output_dir, overwrite, binary)

  # Summary
  cli::cli_inform(c(
    "v" = "Successfully decoded {nrow(files_info)} file{?s} to {.path {final_output_dir}}"
  ))

  # Return webr_decoded object
  new_webr_decoded(files_info, final_output_dir, url, mode, version, flags)
}

#' Decode multiple webR REPL links
#'
#' @param urls Vector of URL strings
#' @param output_dir Base output directory
#' @param overwrite Whether to overwrite files
#' @param create_subdir Whether to create subdirectories
#' @param name_dirs Whether to use numbered directory names
#'
#' @return
#' A `webr_decoded_batch` object, the list of `results`, `base_dir`, `urls`,
#' `total_urls`, `successful_urls`, `total_files`, and `total_size` described
#' under [decode_webr_link()].
#'
#' @noRd
decode_multiple_webr_links <- function(urls, output_dir, overwrite, create_subdir,
                                       name_dirs, binary = FALSE) {
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
        create_subdir = FALSE,  # We handle subdirectory creation here
        binary = binary
      )

      results[[subdir_name]] <- decoded_result

    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to process URL {i}",
        "x" = "{e$message}"
      ))
    })
  }

  # A failed URL is warned about above and simply leaves no entry in `results`.
  successful_results <- results
  cli::cli_inform(c(
    "",
    "v" = "Successfully processed {length(successful_results)}/{length(urls)} URL{?s}"
  ))

  # Return webr_decoded_batch object
  new_webr_decoded_batch(results, output_dir, urls)
}

#' Decode and save files from webR data
#'
#' @param files_data Parsed file data from webR
#' @param output_dir Directory to save files
#' @param overwrite Whether to overwrite existing files
#'
#' @return
#' A data frame with one row per file written, holding the columns `filename`,
#' `path`, `autorun`, and `size_bytes`. A `skip_reasons` attribute lists the
#' files that were passed over and why.
#'
#' @noRd
decode_and_save_webr_files <- function(files_data, output_dir, overwrite,
                                       binary = FALSE) {
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
    save_failed = character(0),
    binary = character(0)
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
      # "name" is guaranteed present: the structure check above already skipped
      # any entry missing it.
      filename <- file_info$name
      cli::cli_warn("Skipping file entry {i}: no content found")
      skip_reasons$no_content <- c(skip_reasons$no_content, filename)
      next
    }

    filename <- file_info$name

    # Refuse a name that would write outside the directory the caller named.
    if (!is_safe_output_name(output_dir, filename)) {
      cli::cli_warn("Unsafe file name, skipping: {.file {filename}}")
      skip_reasons$unsafe_name <- c(skip_reasons$unsafe_name, filename)
      next
    }

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

      if (is_binary && !binary) {
        # A link is written by whoever sent it, and preview_webr_link() can
        # show you text but not bytes. Leave a binary alone unless it is asked
        # for, so nothing lands on disk that could not be looked at first.
        cli::cli_warn(c(
          "Skipping binary file: {.file {filename}}",
          "i" = "Pass {.code binary = TRUE} to write it."
        ))
        skip_reasons$binary <- c(skip_reasons$binary, filename)
        next
      }

      if (is_binary) {
        # Already bytes: write them, rather than round-tripping through a hex
        # rendering that only ever lost fidelity.
        if (is.raw(content)) {
          writeBin(content, file_path)
          file_size <- length(content)
        } else if (is.character(content) && grepl("^[0-9a-f ]+$", content)) {
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
        "x" = "{conditionMessage(e)}"
      ))
      # `<<-`, not `<-`: a plain assignment here lands in the handler's frame and
      # is discarded, which left the save_failed reporting block unreachable.
      skip_reasons$save_failed <<- c(skip_reasons$save_failed, filename)
    })
  }

  # Add skip reasons as attribute for better error reporting
  attr(files_info, "skip_reasons") <- skip_reasons

  files_info
}

#' Detect if content is likely binary
#'
#' @param content Character string to check
#'
#' @return
#' Logical indicating if content is likely binary.
#'
#' @noRd
detect_binary_content <- function(content) {
  # Raw is binary by construction, and saying otherwise sent genuine bytes --
  # what a msgpack `data` field unpacks to -- down the text branch, where
  # writeLines() coerced them to the strings "89" "50" "4e" and wrote a hex
  # dump of the file instead of the file.
  if (is.raw(content)) {
    return(TRUE)
  }

  if (!is.character(content) || length(content) == 0 || nchar(content) == 0) {
    return(FALSE)
  }

  raw_chars <- charToRaw(content)

  if (any(raw_chars == 0)) {
    return(TRUE)
  }

  # Well-formed UTF-8 is text, even though its bytes run above 126. Judging
  # "printable" byte by byte would call "café" and "你好" binary.
  if (!validUTF8(content)) {
    return(TRUE)
  }

  # Control characters (other than tab, newline, carriage return) are the real
  # signal that this is not text.
  control <- raw_chars < as.raw(32) & !(raw_chars %in% as.raw(c(9, 10, 13)))

  sum(control) / length(raw_chars) > 0.1
}

#' Normalize msgpack data from RcppMsgPack format to list format
#'
#' @param msgpack_data Raw msgpack data from RcppMsgPack
#'
#' @return
#' Normalized list structure.
#'
#' @noRd
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
#'
#' @param kv_item Item with key and value lists
#'
#' @return
#' Named list.
#'
#' @noRd
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
      # msgpack carries both text and binary as raw. Text is worth converting;
      # bytes are not, and rendering them as the hex string "89 50 4e 47" wrote
      # a dump of the file in place of the file. Keep anything that is not text
      # as the bytes it already is, so it can be written back verbatim.
      #
      # Take the fallback as the value of tryCatch; assigning to `result` inside
      # the handler would write into the handler's frame and be discarded.
      as_text <- tryCatch(rawToChar(value), error = function(e) NULL)

      result[[key]] <- if (!is.null(as_text) && all(validUTF8(as_text))) {
        as_text
      } else {
        value
      }
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
#'
#' @param url webR URL
#'
#' @return
#' List containing mode, version, flags, and files_data.
#'
#' @noRd
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
    # zlib/gzip compressed - try both formats.
    # The fallback must be the *value* of the tryCatch: assigning inside the
    # error handler writes into the handler's own frame, so `raw_data` would
    # never reach this one and the graceful path would die with
    # `object 'raw_data' not found`.
    raw_data <- tryCatch(
      memDecompress(binary_data, type = "gzip"),
      error = function(e1) {
        tryCatch(
          # Try without gzip headers (raw zlib)
          memDecompress(binary_data, type = "unknown"),
          error = function(e2) {
            cli::cli_abort(c(
              "Error during decompression",
              "x" = "Failed to decompress data: {conditionMessage(e1)}",
              "i" = "The URL may be corrupted or use an unsupported compression format"
            ))
          }
        )
      }
    )
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
      # Chained, not interpolated: the cause's message can carry the raw bytes
      # that failed to parse, and cli aborts while rendering anything that is
      # not valid UTF-8 -- reporting that instead of the real problem.
      cli::cli_abort(c(
        "Error parsing msgpack data",
        "i" = "The decoded data may not be valid msgpack"
      ), parent = e)
    })
  } else if (grepl("j", flags)) {
    # JSON format
    tryCatch({
      json_string <- rawToChar(raw_data)
      files_data <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
    }, error = function(e) {
      # See above: chain the cause rather than pasting its text.
      cli::cli_abort(c(
        "Error parsing JSON data",
        "i" = "The decoded data may not be valid JSON"
      ), parent = e)
    })
  } else {
    cli::cli_abort(c(
      "Unknown format flags",
      "x" = "Flags '{flags}' contain unknown format options",
      "i" = "Supported flags: 'm' (msgpack), 'j' (JSON)"
    ))
  }

  # Check the shape once, here, rather than letting each consumer discover it.
  # A payload that parses but is not a list of file entries -- what a link whose
  # flags do not describe its contents decodes to -- otherwise reached `$` on an
  # atomic vector and surfaced "$ operator is invalid for atomic vectors".
  check_webr_files_data(files_data, flags)

  list(
    mode = mode,
    version = version,
    flags = flags,
    files_data = files_data
  )
}

#' Parse webR URL structure
#'
#' @param url webR URL to parse
#'
#' @return
#' A list with the URL components `mode`, `version`, `encoded_data`, and
#' `flags`.
#'
#' @noRd
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
#'
#' @param url webR URL
#'
#' @return
#' Version string.
#'
#' @noRd
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
#'
#' @param base_url Base part of URL before fragment
#'
#' @return
#' Mode string or NULL.
#'
#' @noRd
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
      } else if (length(match_result) >= 2) {
        # Unquoted pattern - mode is in second element
        mode <- match_result[2]
      } else {
        next
      }

      # Split back into the panel vector the caller passed to `panels`, so a
      # decoded link reports what was encoded rather than the wire format.
      return(strsplit(mode, "-", fixed = TRUE)[[1]])
    }
  }

  return(NULL)
}

#' Format panel vector for display using cli
#'
#' @param mode Panel vector (like c("editor", "plot")) or string ("editor-plot")
#'
#' @return
#' Formatted string for display.
#'
#' @noRd
format_mode_for_display <- function(mode) {
  if (is.null(mode) || !is.character(mode) || length(mode) == 0) {
    return(NULL)
  }

  # Accept either the panel vector or the hyphen-joined wire format.
  components <- unlist(strsplit(mode, "-", fixed = TRUE))
  components <- tools::toTitleCase(components)

  # Use cli to format with proper conjunctions (Oxford comma included by default)
  cli::cli_vec(components)
}

#' Extract parameters from webR URL fragment
#'
#' @param fragment URL fragment part after '#'
#'
#' @return
#' List with code and flags.
#'
#' @noRd
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


#' Preview webR REPL link contents without writing files to disk
#'
#' Decodes a webR URL and returns information about the embedded files
#' without actually saving them to disk.
#'
#' @param url Character string containing the webR URL
#'
#' @return
#' A `webr_preview` object, which is a list with these entries.
#'
#' - `url`, the link that was read.
#' - `mode`, the panels the link asks for, or `NULL` for all of them.
#' - `version`, the webR version the link points at.
#' - `flags`, the encoding flags read off the link.
#' - `files_data`, a list with one entry per embedded file, each holding the
#'   file's `name`, its `path` inside webR, and its `text`.
#' - `total_files`, how many files the link carries.
#' - `total_size`, the size of those files in bytes.
#' - `autorun_files`, the names of the files that run as soon as the link opens.
#'
#' Nothing is written to disk.
#'
#' @details
#' Use print method options to control the display. The returned object's
#' `print()` method accepts `show_content` and `max_content_length` to display
#' file bodies.
#'
#' @seealso
#' [decode_webr_link()] to extract the embedded files to disk once you are happy
#' with the preview.
#'
#' @export
#' @examples
#' url <- as.character(webr_repl_link("plot(1:10)"))
#'
#' # Inspect a link without writing anything to disk
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
    # Chained, not interpolated. Pasting `e$message` into a bullet threw away
    # every diagnostic the inner error carried, so previewing a broken link
    # said strictly less than decoding it -- and when the payload was not valid
    # UTF-8, cli failed while rendering the message and reported that instead
    # of the actual problem.
    cli::cli_abort("Failed to preview webR link", parent = e)
  })
}
