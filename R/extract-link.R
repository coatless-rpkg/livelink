#' Extract URLs as character vector
#'
#' @param x Link object
#' @param ... Additional arguments
#'
#' @return
#' A character vector of URLs. Most objects yield a single URL; exercise objects
#' return a length-2 named vector (`exercise`, `solution`); directory and batch
#' objects return one URL per file.
#'
#' @seealso [repl_urls()] for the same extraction as a named generic you can call
#'   explicitly.
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

#' Extract shareable URLs from livelink objects
#'
#' @description
#' Generic function to extract the shareable URL(s) from any livelink object,
#' covering both webR REPL and Shinylive results. Provides a clear way to get
#' just the URLs for sharing or further processing.
#'
#' @param x A livelink object. Supported classes are `webr_link`, `webr_project`,
#'   `webr_exercise`, `webr_directory`, `webr_decoded`, `webr_decoded_batch`,
#'   `webr_preview`, `shinylive_link`, `shinylive_project`, `shinylive_directory`,
#'   `shinylive_decoded`, `shinylive_decoded_batch`, and `shinylive_preview`.
#' @param ... Additional arguments passed to methods
#' @return A character vector of URLs. Most objects yield a single URL; exercise
#'   objects return a length-2 named vector (`exercise`, `solution`); directory
#'   and batch objects return one URL per file.
#'
#' @seealso The `as.character()` methods (for example [as.character.webr_link()]),
#'   which `repl_urls()` delegates to.
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

# The classes repl_urls understands. Every one carries an as.character() method
# returning its URL(s), and repl_urls is a named alias for exactly that, so the
# default method delegates rather than repeating thirteen identical bodies.
livelink_url_classes <- c(
  "webr_link", "webr_project", "webr_exercise", "webr_directory",
  "webr_decoded", "webr_decoded_batch", "webr_preview",
  "shinylive_link", "shinylive_project", "shinylive_directory",
  "shinylive_decoded", "shinylive_decoded_batch", "shinylive_preview"
)

#' @rdname repl_urls
#' @export
repl_urls.default <- function(x, ...) {
  if (inherits(x, livelink_url_classes)) {
    return(as.character(x, ...))
  }
  cli::cli_abort(c(
    "Cannot extract URLs from object of class {.cls {class(x)}}",
    "i" = "Supported classes: {.cls {livelink_url_classes}}"
  ))
}
