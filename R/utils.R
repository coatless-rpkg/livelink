#' Format file size in human readable format
#' @param size_bytes Size in bytes
#' @return Formatted string
#' @keywords internal
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

#' Calculate the decoded size of base64 data without decoding
#' @param base64_string Base64 encoded string
#' @return Integer size in bytes
#' @keywords internal
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
#' @param x String to hash
#' @param length Length of hash to return (default: 8)
#' @return Character string hash
#' @keywords internal
simple_hash <- function(x, length = 8) {
  # Convert string to raw bytes and sum them with position weights
  raw_bytes <- utf8ToInt(x)

  # Create a weighted sum using position and character values
  weighted_sum <- sum(raw_bytes * seq_along(raw_bytes), na.rm = TRUE)

  # Convert to hex and take desired length
  hex_string <- sprintf("%x", weighted_sum)

  # Ensure we have enough characters, repeat if necessary
  if (nchar(hex_string) < length) {
    hex_string <- paste0(rep(hex_string, ceiling(length / nchar(hex_string))), collapse = "")
  }

  # Return first 'length' characters
  substr(hex_string, 1, length)
}
