#' Convert mode to string format
#' @param mode Mode specification (character vector, string, or NULL)
#' @return String in format "component1-component2-..." or NULL
#' @keywords internal
format_mode_string <- function(mode) {
  if (is.null(mode)) {
    return(NULL)
  }

  if (length(mode) == 1) {
    # Already a string, return as-is
    return(mode)
  } else {
    # Convert vector to string
    return(paste(mode, collapse = "-"))
  }
}

#' Build WebR URL with mode and code
#' @param base_url Base WebR URL
#' @param encoded_data Base64 encoded data
#' @param flags URL flags
#' @param mode Mode string or NULL
#' @return Complete WebR URL
#' @keywords internal
build_webr_url <- function(base_url, encoded_data, flags, mode = NULL) {
  if (is.null(mode)) {
    # Original format without mode
    paste0(base_url, "#code=", encoded_data, "&", flags)
  } else {
    # New format with mode
    mode_string <- format_mode_string(mode)
    paste0(base_url, "?mode='", mode_string, "'#code=", encoded_data, "&", flags)
  }
}
