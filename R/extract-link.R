#' Extract URLs as character vector
#'
#' @param x Link object
#' @param ... Additional arguments
#'
#' @return
#' Character vector of URLs
#'
#' @export
as.character.webr_link <- function(x, ...) {
  x$url
}

#' @rdname as.character.webr_link
#' @export
as.character.shinylive_link <- function(x, ...) {
  x$url
}

#' @rdname as.character.webr_link
#' @export
as.character.webr_project <- function(x, ...) {
  x$url
}

#' @rdname as.character.webr_link
#' @export
as.character.webr_exercise <- function(x, ...) {
  c(exercise = x$exercise$url, solution = x$solution$url)
}

#' @rdname as.character.webr_link
#' @export
as.character.webr_directory <- function(x, ...) {
  x$urls
}

#' @rdname as.character.webr_link
#' @export
as.character.webr_decoded <- function(x, ...) {
  x$url
}

#' @rdname as.character.webr_link
#' @export
as.character.webr_decoded_batch <- function(x, ...) {
  x$urls
}

#' @rdname as.character.webr_link
#' @export
as.character.webr_preview <- function(x, ...) {
  x$url
}

#' @rdname as.character.webr_link
#' @export
as.character.shinylive_project <- function(x, ...) {
  x$url
}

#' @rdname as.character.webr_link
#' @export
as.character.shinylive_directory <- function(x, ...) {
  x$urls
}

#' @rdname as.character.webr_link
#' @export
as.character.shinylive_decoded <- function(x, ...) {
  x$url
}

#' @rdname as.character.webr_link
#' @export
as.character.shinylive_decoded_batch <- function(x, ...) {
  x$urls
}

#' Get WebR URLs from objects
#'
#' @description
#' Generic function to extract WebR URLs from different object types.
#' Provides a clear way to get just the URLs for sharing or further processing.
#'
#' @param x WebR object (webr_link, webr_project, webr_exercise, or webr_directory)
#' @param ... Additional arguments passed to methods
#' @return Character vector of URLs
#'
#' @examples
#' \dontrun{
#' # Single link
#' link <- webr_repl_link("plot(1:10)")
#' repl_urls(link)
#'
#' # Exercise (returns named vector)
#' exercise <- webr_repl_exercise("# TODO", "plot(1:10)", "test")
#' repl_urls(exercise)
#'
#' # Directory (returns named vector)
#' links <- webr_repl_directory("./examples/")
#' repl_urls(links)
#'
#' # Decoded files (returns original URL)
#' decoded <- decode_webr_link("https://webr.r-wasm.org/latest/#code=...")
#' repl_urls(decoded)
#' }
#'
#' @export
repl_urls <- function(x, ...) {
  UseMethod("repl_urls")
}

#' @rdname repl_urls
#' @export
repl_urls.webr_link <- function(x, ...) {
  x$url
}

#' @rdname repl_urls
#' @export
repl_urls.webr_project <- function(x, ...) {
  x$url
}

#' @rdname repl_urls
#' @export
repl_urls.webr_exercise <- function(x, ...) {
  c(exercise = x$exercise$url, solution = x$solution$url)
}

#' @rdname repl_urls
#' @export
repl_urls.webr_directory <- function(x, ...) {
  x$urls
}

#' @rdname repl_urls
#' @export
repl_urls.webr_decoded <- function(x, ...) {
  x$url
}

#' @rdname repl_urls
#' @export
repl_urls.webr_decoded_batch <- function(x, ...) {
  x$urls
}

#' @rdname repl_urls
#' @export
repl_urls.webr_preview <- function(x, ...) {
  x$url
}


#' @rdname repl_urls
#' @export
repl_urls.shinylive_link <- function(x, ...) {
  x$url
}

#' @rdname repl_urls
#' @export
repl_urls.shinylive_project <- function(x, ...) {
  x$url
}

#' @rdname repl_urls
#' @export
repl_urls.shinylive_directory <- function(x, ...) {
  x$urls
}

#' @rdname repl_urls
#' @export
repl_urls.shinylive_decoded <- function(x, ...) {
  x$url
}

#' @rdname repl_urls
#' @export
repl_urls.shinylive_decoded_batch <- function(x, ...) {
  x$urls
}

#' @rdname repl_urls
#' @export
repl_urls.default <- function(x, ...) {
  cli::cli_abort(c(
    "Cannot extract URLs from object of class {.cls {class(x)}}",
    "i" = "Supported classes: {.cls webr_link}, {.cls webr_project}, {.cls webr_exercise}, {.cls webr_directory}, {.cls webr_decoded}, {.cls webr_decoded_batch}, {.cls shinylive_link}, {.cls shinylive_project}, {.cls shinylive_directory}, {.cls shinylive_decoded}, {.cls shinylive_decoded_batch}"
  ))
}
