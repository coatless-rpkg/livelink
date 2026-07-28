#' Convert mode to string format
#'
#' @param mode Mode specification (character vector, string, or NULL)
#'
#' @return
#' String in format "component1-component2-..." or NULL
#'
#' @noRd
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
#'
#' @param base_url Base WebR URL
#' @param encoded_data Base64 encoded data
#' @param flags URL flags
#' @param mode Mode string or NULL
#'
#' @return
#' Complete WebR URL
#'
#' @noRd
build_webr_url <- function(base_url, encoded_data, flags, mode = NULL) {
  if (is.null(mode)) {
    # No mode: a bare "#code=..." fragment
    paste0(base_url, "#code=", encoded_data, "&", flags)
  } else {
    # With mode: a "?mode='...'" query ahead of the "#code=" fragment
    mode_string <- format_mode_string(mode)
    paste0(base_url, "?mode='", mode_string, "'#code=", encoded_data, "&", flags)
  }
}

#' Encode webR share items into a URL-fragment payload
#'
#' The shared encoding tail for webR links: serialize the files to msgpack,
#' compress, and base64. This is the format the webR REPL's own share button
#' writes, so a link livelink produces is byte-for-byte the kind webR emits.
#'
#' `memCompress(type = "gzip")` is a misnomer worth knowing: it emits RFC 1950,
#' the zlib container, which is what pako's `inflate()` on the webR side reads.
#' `URLencode(reserved = TRUE)` is the other non-obvious step: it percent-escapes
#' base64's `+` and `/`, which are URL-reserved and would otherwise corrupt the
#' `#code=` fragment.
#'
#' @param share_items List of file items (each `name`/`path`/`text`, plus an
#'   optional `autorun`)
#'
#' @return
#' A URL-safe encoded string
#'
#' @noRd
encode_webr_payload <- function(share_items) {
  packed <- RcppMsgPack::msgpack_pack(share_items)
  compressed <- memCompress(packed, type = "gzip")
  base64_data <- base64enc::base64encode(compressed)
  utils::URLencode(base64_data, reserved = TRUE)
}
