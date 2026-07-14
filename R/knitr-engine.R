#' Use livelink as a 'knitr' chunk engine
#'
#' @description
#' Registers a `livelink` engine with 'knitr', letting you turn a code chunk in a
#' 'Quarto' or 'R Markdown' document into a shareable link. The chunk is shown as
#' code and *not* executed locally; instead its source is encoded into a webR or
#' Shinylive link.
#'
#' ````
#' ```{livelink}
#' # Load the data
#' data(mtcars)
#' plot(mtcars$mpg, mtcars$wt)
#' ```
#' ````
#'
#' @details
#' Use this rather than expression input (`webr_repl_link({ ... })`) inside a
#' knitted document. 'knitr' evaluates chunks through `evaluate::evaluate()`,
#' which discards source references, so comments inside a `{ }` expression are
#' lost when a document is rendered -- no `keep.source` setting recovers them.
#' The engine is handed the chunk's verbatim source, so comments survive intact.
#'
#' The engine is registered automatically when livelink is loaded, provided
#' 'knitr' is installed. You only need to call this yourself if you have reset
#' `knitr::knit_engines`.
#'
#' # Chunk options
#'
#' \describe{
#'   \item{`engine.target`}{`"webr"` (default), `"shinylive-r"`, or `"shinylive-py"`.}
#'   \item{`autorun`}{Logical. Run the code as soon as the link opens. webR only.}
#'   \item{`panels`}{Character vector of webR panels to show, e.g. `c("editor", "plot")`.}
#'   \item{`mode`}{Shinylive display mode, `"editor"` or `"app"`.}
#'   \item{`filename`}{Name for the file inside the environment.}
#'   \item{`link.text`}{Text for the emitted hyperlink. Defaults to `"Open in webR"`
#'     or `"Open in Shinylive"`.}
#'   \item{`link.only`}{Logical. If `TRUE`, emit only the link and not the source
#'     chunk. Defaults to `FALSE`.}
#' }
#'
#' Standard chunk options such as `echo` and `eval` are honored by 'knitr' as usual.
#'
#' @return Called for its side effect. Invisibly returns `TRUE` if the engine was
#'   registered, and `FALSE` if 'knitr' is not installed.
#'
#' @examplesIf requireNamespace("knitr", quietly = TRUE)
#' # Normally automatic on load; call directly only after resetting knit_engines.
#' use_livelink_engine()
#'
#' @export
use_livelink_engine <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  knitr::knit_engines$set(livelink = livelink_engine)
  invisible(TRUE)
}

#' The knitr engine callback
#'
#' knitr hands us `options$code`: the chunk's verbatim source lines, comments and
#' all. That is the whole point of the engine -- it is the only route by which
#' comments reach us from a knitted document.
#'
#' @param options knitr chunk options
#' @return A string of knitr output
#' @noRd
livelink_engine <- function(options) {
  code <- paste(options$code, collapse = "\n")

  target <- options[["engine.target"]] %||% "webr"
  if (!target %in% c("webr", "shinylive-r", "shinylive-py")) {
    cli::cli_abort(c(
      "Invalid {.code engine.target} chunk option",
      "x" = "Got {.val {target}}",
      "i" = "Valid targets: {.val webr}, {.val shinylive-r}, {.val shinylive-py}"
    ))
  }

  link <- switch(target,
    "webr" = webr_repl_link(
      code,
      filename = options[["filename"]] %||% "script.R",
      autorun  = isTRUE(options[["autorun"]]),
      panels   = options[["panels"]]
    ),
    "shinylive-r" = shinylive_r_link(
      code,
      mode = options[["mode"]] %||% "editor"
    ),
    "shinylive-py" = shinylive_py_link(
      code,
      mode = options[["mode"]] %||% "editor"
    )
  )

  url <- as.character(link)
  default_text <- if (target == "webr") "Open in webR" else "Open in Shinylive"
  link_text <- options[["link.text"]] %||% default_text
  markdown_link <- paste0("[", link_text, "](", url, ")")

  # engine_output() takes `code` positionally with no default; NULL drops the
  # source block and emits only the link.
  if (isTRUE(options[["link.only"]])) {
    return(knitr::engine_output(options, code = NULL, out = markdown_link))
  }

  knitr::engine_output(options, code = options$code, out = markdown_link)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.onLoad <- function(libname, pkgname) {
  # Registering here (rather than making users call it) means a document can use
  # a ```{livelink} chunk after nothing more than library(livelink).
  use_livelink_engine()
}
