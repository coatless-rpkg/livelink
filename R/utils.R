#' Format file size in human readable format
#' @param size_bytes Size in bytes
#' @return Formatted string
#' @noRd
format_file_size <- function(size_bytes) {
  if (is.na(size_bytes) || size_bytes == 0) {
    return("0 bytes")
  } else if (size_bytes < 1024) {
    paste(size_bytes, "bytes")
  } else if (size_bytes < 1024^2) {
    paste(round(size_bytes/1024, 1), "KB")
  } else {
    paste(round(size_bytes/1024^2, 1), "MB")
  }
}

#' Render a file list for display, truncating long ones
#'
#' The count is interpolated here rather than in the cli template, because cli
#' globs the template string it is handed -- an unevaluated `{length(x) - 3}`
#' would reach the user verbatim.
#'
#' @param files Character vector of filenames
#' @param max Number of names to show before truncating (default: 3)
#' @return A single string
#' @noRd
truncate_file_list <- function(files, max = 3) {
  if (length(files) > max) {
    paste0(
      paste(files[seq_len(max)], collapse = ", "),
      ", and ", length(files) - max, " more"
    )
  } else {
    paste(files, collapse = ", ")
  }
}

#' Calculate the decoded size of base64 data without decoding
#' @param base64_string Base64 encoded string
#' @return Integer size in bytes
#' @noRd
calculate_base64_size <- function(base64_string) {
  # Remove any whitespace that might be present
  base64_string <- gsub("\\s", "", base64_string)

  # Count padding characters ('=')
  padding_count <- sum(strsplit(base64_string, "")[[1]] == "=")

  # Base64 encoding: every 4 characters represent 3 bytes of original data
  # Formula: (length * 3 / 4) - padding_count
  decoded_size <- (nchar(base64_string) * 3) / 4 - padding_count

  return(as.integer(decoded_size))
}

#' Create a simple hash from a string using base R
#'
#' Polynomial rolling hash reduced modulo 2^32 on every step. The intermediate
#' value therefore never leaves the range where a double represents integers
#' exactly, which matters because R silently promotes an integer sum past
#' `.Machine$integer.max` to double -- and `sprintf("%x", <double>)` is an error,
#' not a fallback.
#'
#' @param x String to hash
#' @param length Length of hash to return (default: 8)
#' @return Character string hash of `length` hex digits
#' @noRd
simple_hash <- function(x, length = 8) {
  bytes <- utf8ToInt(enc2utf8(x))
  bytes <- bytes[!is.na(bytes)]

  modulus <- 2^32
  h <- 0
  for (b in bytes) {
    h <- (h * 31 + b) %% modulus
  }

  # Split into 16-bit halves so both fit in an integer before formatting.
  hex_string <- sprintf("%04x%04x", as.integer(h %/% 65536), as.integer(h %% 65536))

  if (nchar(hex_string) < length) {
    hex_string <- strrep(hex_string, ceiling(length / nchar(hex_string)))
  }

  substr(hex_string, 1, length)
}
