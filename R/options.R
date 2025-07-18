#' Get the base WebR URL from options or default
#' @param version WebR version to use ("latest" or specific version like "v0.5.4")
#' @return Character string with the base URL
#' @keywords internal
get_webr_base_url <- function(version = "latest") {
  # Check if user has set a custom base URL
  custom_url <- getOption("livelink.base_url", NULL)

  if (!is.null(custom_url)) {
    return(custom_url)
  }

  # Build default URL with version
  paste0("https://webr.r-wasm.org/", version, "/")
}

#' Set global base URL for WebR links
#' @param base_url Custom base URL to use for all WebR links
#' @export
#' @examples
#' # Set custom base URL
#' set_webr_base_url("https://my-custom-webr.com/")
#'
#' # Reset to default (removes custom setting)
#' set_webr_base_url(NULL)
set_webr_base_url <- function(base_url = NULL) {
  if (is.null(base_url)) {
    # Remove custom setting
    options(livelink.base_url = NULL)
    cli::cli_inform("WebR base URL reset to default")
  } else {
    check_single_string(base_url, "base_url")
    options(livelink.base_url = base_url)
    cli::cli_inform("WebR base URL set to: {.url {base_url}}")
  }
  invisible(base_url)
}
