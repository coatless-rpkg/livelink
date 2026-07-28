#' Turn document chunks into shareable links
#'
#' livelink plugs into 'knitr' two ways, and which you want depends on one
#' question. **Should the code also run in your document?**
#'
#' @return
#' Called for their side effect. The value is returned invisibly.
#'
#' - `TRUE` if registration happened.
#' - `FALSE` if 'knitr' is not installed.
#'
#' @details
#' Reach for either of these rather than expression input
#' (`webr_repl_link({ ... })`) inside a knitted document. 'knitr' evaluates
#' chunks through `evaluate::evaluate()`, which discards the source R kept, and
#' comments live in that. Comments inside a `{ }` expression are therefore
#' silently dropped from the link when the document renders, and no `keep.source`
#' setting brings them back. Both the hook and the engine are handed the chunk's
#' verbatim source, so nothing is lost.
#'
#' Both are registered automatically when livelink is loaded, provided 'knitr' is
#' installed. Call these yourself only if you have reset `knitr::knit_hooks` or
#' `knitr::knit_engines`.
#'
#' @section A chunk hook:
#' Set on an ordinary `r` chunk. The chunk runs as usual (its output, plots and
#' all, appear in the rendered page) and a link is added underneath. Use this
#' for code you want your reader to *see the result of* and *also* be able to open
#' and play with.
#'
#' ````
#' ```{r}
#' #| livelink: true
#' #| autorun: true
#' # Load the data
#' data(mtcars)
#' plot(mtcars$mpg, mtcars$wt)
#' ```
#' ````
#'
#' @section An engine:
#' Written as ```` ```{livelink} ````. The chunk is displayed but **not** run, so
#' only the link is produced. Use this for code your session cannot or should not
#' execute, such as a Shiny app, something needing a package you have not
#' installed, or anything slow.
#'
#' ````
#' ```{livelink}
#' #| engine.target: shinylive-r
#' library(shiny)
#' shinyApp(fluidPage(), function(input, output) {})
#' ```
#' ````
#'
#' There is deliberately no `{shinylive-r}` or `{shinylive-py}` engine. 'knitr'
#' will not accept a chunk whose engine name contains a hyphen (its chunk syntax
#' forbids it), and in Quarto such a cell is handed to the Shinylive extension
#' rather than to 'knitr'. Name Shinylive through `engine.target` instead.
#'
#' @section Chunk options:
#' \describe{
#'   \item{`livelink`}{Hook only. Use `true` for a webR link, or name the target
#'     directly with `"webr"`, `"shinylive-r"`, or `"shinylive-py"`.}
#'   \item{`engine.target`}{Engine only. `"webr"` (default), `"shinylive-r"`, or
#'     `"shinylive-py"`.}
#'   \item{`autorun`}{Logical. Run the code as soon as the link opens. webR only.}
#'   \item{`panels`}{Character vector of webR panels, e.g. `c("editor", "plot")`.}
#'   \item{`mode`}{Shinylive only. Display mode, `"editor"` (default) or `"app"`.}
#'   \item{`filename`}{webR only. Name for the script file webR creates in the
#'     browser (default `"script.R"`). It must end in `.R` for `autorun` to work.}
#'   \item{`link.text`}{Text for the hyperlink. Defaults to `"Open in webR"` or
#'     `"Open in Shinylive"`.}
#'   \item{`link.only`}{Engine only. If `TRUE`, show the link without the source.}
#' }
#'
#' @section Setting options once:
#' These are ordinary 'knitr' chunk options, so `opts_chunk` sets them for a whole
#' document, and a single chunk opts out with `livelink: false`:
#'
#' ```
#' knitr::opts_chunk$set(livelink = TRUE, autorun = TRUE)
#' ```
#'
#' @section `echo` does not gate the link:
#' It is natural to assume the code must be visible for a link to be made. It need
#' not be. `echo` controls whether the **source is shown in your page**. The link
#' is built from the chunk's source, which 'knitr' hands over either way. So
#' `echo: false` gives a working link whose code the reader simply cannot see.
#'
#' `eval: false` is the other half. The chunk is displayed but not run, which
#' makes an `r` chunk behave rather like the engine.
#'
#' @seealso
#' `vignette("links-in-documents", package = "livelink")` for the whole picture.
#'
#' [webr_repl_link()] for why a braced expression loses comments in a knitted
#' document.
#'
#' @examplesIf requireNamespace("knitr", quietly = TRUE)
#' # Both are registered on load. Call directly only after resetting knitr's hooks.
#' use_livelink_hook()
#' use_livelink_engine()
#'
#' @name livelink-knitr
NULL

#' @rdname livelink-knitr
#' @export
use_livelink_hook <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  knitr::knit_hooks$set(livelink = livelink_hook)
  invisible(TRUE)
}

#' @rdname livelink-knitr
#' @export
use_livelink_engine <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  # A single engine, `livelink`, that targets webR by default. Shinylive is
  # reached with the `engine.target` chunk option, not a separate engine name:
  # 'knitr' cannot dispatch a `{shinylive-r}` chunk (its chunk syntax forbids a
  # hyphen in the engine name), and an underscore near-miss like `{shinylive_r}`
  # only misleads.
  knitr::knit_engines$set(livelink = livelink_engine)
  invisible(TRUE)
}

#' Resolve the link target from a chunk option
#'
#' @param x Value of the `livelink` or `engine.target` chunk option
#'
#' @return
#' One of "webr", "shinylive-r", "shinylive-py"
#'
#' @noRd
livelink_target <- function(x) {
  target <- if (isTRUE(x)) {
    "webr"
  } else if (is.character(x) && length(x) == 1) {
    x
  } else {
    "webr"
  }

  if (!target %in% c("webr", "shinylive-r", "shinylive-py")) {
    cli::cli_abort(c(
      "Invalid livelink chunk target",
      "x" = "Got {.val {target}}",
      "i" = "Valid targets: {.val webr}, {.val shinylive-r}, {.val shinylive-py}"
    ))
  }

  target
}

#' Build the link a chunk asks for
#'
#' @param code The chunk's source, as one string
#' @param options knitr chunk options
#' @param target Resolved target
#'
#' @return
#' A link object
#'
#' @noRd
livelink_from_chunk <- function(code, options, target) {
  switch(target,
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
}

#' The markdown for a chunk's link
#'
#' A blank line either side keeps it a paragraph of its own, so the renderer sees
#' a link rather than a run-on of the preceding output.
#'
#' @param link A link object
#' @param options knitr chunk options
#' @param target Resolved target
#'
#' @return
#' A markdown string
#'
#' @noRd
livelink_markdown <- function(link, options, target) {
  default_text <- if (target == "webr") "Open in webR" else "Open in Shinylive"
  link_text <- options[["link.text"]] %||% default_text

  paste0("\n\n[", link_text, "](", as.character(link), ")\n")
}

#' The knitr chunk hook
#'
#' Fires after an ordinary R chunk has run. The chunk's own output is already
#' in the document, so we only append the link. `options$code` is the chunk's
#' verbatim source, comments and all. That is the only route by which comments
#' reach us from a knitted document.
#'
#' @param before TRUE before the chunk runs, FALSE after
#' @param options knitr chunk options
#' @param envir The chunk's evaluation environment (unused)
#'
#' @return
#' A markdown string appended after the chunk, or NULL
#'
#' @noRd
livelink_hook <- function(before, options, envir) {
  # Nothing to add before the chunk runs, and `livelink: false` opts out.
  if (before || isFALSE(options[["livelink"]])) {
    return(NULL)
  }

  target <- livelink_target(options[["livelink"]])
  code <- paste(options$code, collapse = "\n")
  link <- livelink_from_chunk(code, options, target)

  livelink_markdown(link, options, target)
}

#' The knitr engine callback
#'
#' The chunk is shown but never run, so this is the tool for code the session
#' cannot execute. As with the hook, `options$code` is the verbatim source.
#'
#' @param options knitr chunk options
#'
#' @return
#' A string of knitr output
#'
#' @noRd
livelink_engine <- function(options) {
  target <- livelink_target(options[["engine.target"]] %||% "webr")
  code <- paste(options$code, collapse = "\n")
  link <- livelink_from_chunk(code, options, target)

  # The link must travel as `extra`, which engine_output() appends verbatim.
  # Passing it as `out` routes it through the output hook, which prefixes it with
  # the chunk's `comment` string and wraps it in a code block -- so the reader
  # gets a literal `#> [Open in webR](https://...)` instead of a link they can
  # click.
  markdown_link <- livelink_markdown(link, options, target)

  if (isTRUE(options[["link.only"]])) {
    # `code = NULL` alone still emitted an empty fenced block: engine_output()
    # writes the source whenever `echo` is on, and NULL reaches the source hook
    # as an empty string rather than as nothing. Turning `echo` off is what
    # "without the source" means, and it is knitr's own switch for it.
    options$echo <- FALSE
    return(knitr::engine_output(options, code = NULL, out = NULL,
                                extra = markdown_link))
  }

  # The fenced source block takes its language from `options$engine`, which is
  # "livelink" here -- a language no highlighter knows. Name the real one so the
  # code is highlighted as the R or Python it is.
  options$engine <- if (target == "shinylive-py") "python" else "r"

  knitr::engine_output(options, code = options$code, out = NULL,
                       extra = markdown_link)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.onLoad <- function(libname, pkgname) {
  # Registering here (rather than making users call them) means a document can use
  # either mechanism after nothing more than library(livelink).
  use_livelink_hook()
  use_livelink_engine()
}
