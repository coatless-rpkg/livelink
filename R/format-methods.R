#' Format a livelink object as a character vector
#'
#' Returns an object's printed representation as a character vector, one element
#' per line, so it can be captured, logged, pasted into a report, or otherwise
#' reused. [print()] renders the very same content to the console; `format()`
#' hands it back to you instead.
#'
#' @param x A livelink object (a link, project, exercise, directory, decoded
#'   result, batch, or preview).
#' @param ... Passed to the object's [print()] method, so preview-specific
#'   options such as `show_content` work here too.
#'
#' @return A character vector of formatted lines.
#'
#' @examples
#' link <- webr_repl_link("plot(1:10)")
#' format(link)
#' writeLines(format(link))
#'
#' @name format.livelink
NULL

# Each method captures its print() output with cli::cli_fmt(), so a single source
# of truth (the print method) drives both console output and this character form.

#' @rdname format.livelink
#' @export
format.webr_link <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.webr_project <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.webr_exercise <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.webr_directory <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.webr_decoded <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.webr_decoded_batch <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.webr_preview <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.shinylive_link <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.shinylive_project <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.shinylive_directory <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.shinylive_decoded <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.shinylive_decoded_batch <- function(x, ...) cli::cli_fmt(print(x, ...))

#' @rdname format.livelink
#' @export
format.shinylive_preview <- function(x, ...) cli::cli_fmt(print(x, ...))
