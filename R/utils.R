#' Which files will actually run when the link opens?
#'
#' The single answer to that question. The payload builder and `print()` used
#' to work it out separately, and disagreed in both directions: `"all"` never
#' matched a filename, so a project that did autorun printed no marker, while a
#' named non-R file matched and printed `(autorun)` for a file webR will not
#' run. Both now ask here.
#'
#' webR only autoruns R scripts, so a name that is not `.R` cannot autorun
#' whatever the caller asked for.
#'
#' @param filenames The project's file names
#' @param autorun_files What the caller asked to autorun, or `"all"`
#'
#' @return
#' The subset of `filenames` that will autorun
#'
#' @noRd
effective_autorun_files <- function(filenames, autorun_files) {
  if (length(filenames) == 0 || length(autorun_files) == 0) {
    return(character(0))
  }

  autorun_all <- length(autorun_files) == 1 && identical(autorun_files, "all")
  is_r_file <- grepl("\\.R$", filenames, ignore.case = TRUE)

  filenames[is_r_file & (autorun_all | filenames %in% autorun_files)]
}

#' Is a decoded file name safe to write under output_dir?
#'
#' The names in a payload are attacker-controlled: decoding a link is how you
#' open one a stranger sent you. A name like `../../.Rprofile` would otherwise
#' resolve outside `output_dir` and overwrite a file that runs at R startup,
#' while the summary still reported success against a directory nothing was
#' written to. Nothing this package encodes can contain a traversal, because
#' every encoder runs names through `basename()`, so refusing them costs nothing.
#'
#' The check is lexical on purpose: the target does not exist yet, so it cannot
#' be resolved, and `normalizePath()` would follow symlinks in the parts that do.
#'
#' @param output_dir The directory the caller asked to write into
#' @param filename A file name from the decoded payload
#'
#' @return
#' TRUE if the name stays inside `output_dir`
#'
#' @noRd
is_safe_output_name <- function(output_dir, filename) {
  if (!is.character(filename) || length(filename) != 1 || is.na(filename) ||
      !nzchar(filename)) {
    return(FALSE)
  }

  # Split on either separator: "..\\evil" traverses on Windows.
  parts <- strsplit(filename, "[/\\\\]")[[1]]

  if (any(parts == "..")) {
    return(FALSE)
  }

  # An absolute name cannot escape, because file.path() concatenates rather
  # than resolving, but it still writes somewhere the caller did not name.
  !grepl("^(/|~|[A-Za-z]:)", filename)
}

#' Format file size in human readable format
#'
#' @param size_bytes Size in bytes
#'
#' @return
#' Formatted string
#'
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
#' globs the template string it is handed. An unevaluated `{length(x) - 3}`
#' would reach the user verbatim.
#'
#' @param files Character vector of filenames
#' @param max Number of names to show before truncating (default: 3)
#'
#' @return
#' A single string
#'
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
#'
#' @param base64_string Base64 encoded string
#'
#' @return
#' Integer size in bytes
#'
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
#' `.Machine$integer.max` to double, and `sprintf("%x", <double>)` is an error
#' rather than a fallback.
#'
#' @param x String to hash
#' @param length Length of hash to return (default: 8)
#'
#' @return
#' Character string hash of `length` hex digits
#'
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
