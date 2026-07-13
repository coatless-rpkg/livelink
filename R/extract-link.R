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

#' @rdname as.character.webr_link
#' @export
as.character.shinylive_preview <- function(x, ...) {
  x$url
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
#' # Single link
#' link <- webr_repl_link("plot(1:10)")
#' repl_urls(link)
#'
#' # Exercise (returns named vector)
#' exercise <- webr_repl_exercise("# TODO", "plot(1:10)", "test")
#' repl_urls(exercise)
#'
#' # Shinylive links work the same way
#' repl_urls(shinylive_r_link("library(shiny)"))
#'
#' # Decoded files (returns the original URL)
#' decoded <- decode_webr_link(as.character(link))
#' repl_urls(decoded)
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
repl_urls.shinylive_preview <- function(x, ...) {
  x$url
}

#' @rdname repl_urls
#' @export
repl_urls.default <- function(x, ...) {
  cli::cli_abort(c(
    "Cannot extract URLs from object of class {.cls {class(x)}}",
    "i" = "Supported classes: {.cls webr_link}, {.cls webr_project}, {.cls webr_exercise}, {.cls webr_directory}, {.cls webr_decoded}, {.cls webr_decoded_batch}, {.cls webr_preview}, {.cls shinylive_link}, {.cls shinylive_project}, {.cls shinylive_directory}, {.cls shinylive_decoded}, {.cls shinylive_decoded_batch}, {.cls shinylive_preview}"
  ))
}
