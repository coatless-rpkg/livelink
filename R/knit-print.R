#' Render livelink objects as links in knitted documents
#'
#' @description
#' 'knitr' calls `knit_print()` on the last value of a chunk. Without these
#' methods a link object would fall back to [print()], dumping cli console output
#' (a header, box glyphs, metadata) into the rendered page. These methods instead
#' emit a clickable Markdown link, which is almost always what you want when a
#' link object is the visible result of a chunk.
#'
#' A single link becomes `[Open in webR](url)` (or Shinylive); a project the
#' same. A directory or an exercise, which carry several named URLs, become a
#' bulleted list, one titled link per entry.
#'
#' These fire only inside 'knitr'. Call [print()] explicitly for the full cli
#' description, or [as.character()] for the bare URL.
#'
#' @param x A livelink object (a link, project, exercise, or directory).
#' @param ... Ignored.
#'
#' @return A `knit_asis` object (via [knitr::asis_output()]).
#'
#' @seealso [livelink-knitr] for the chunk hook and engine, [format.livelink()]
#'   and [as.character.webr_link()] for other renderings, and
#'   `vignette("links-in-documents", package = "livelink")`.
#'
#' @name knit_print.livelink
NULL

# Build the Markdown. A lone unnamed URL renders as one labelled link; a named
# vector (a directory, an exercise) renders as a titled bullet per link.
livelink_knit_markdown <- function(urls, label) {
  if (length(urls) == 0) {
    return(knitr::asis_output("\n_(no links)_\n"))
  }

  if (length(urls) == 1 && is.null(names(urls))) {
    md <- paste0("[", label, "](", urls, ")")
  } else {
    titles <- names(urls)
    if (is.null(titles)) titles <- rep(label, length(urls))
    md <- paste(paste0("- [", titles, "](", urls, ")"), collapse = "\n")
  }

  knitr::asis_output(paste0("\n", md, "\n"))
}

#' @rdname knit_print.livelink
#' @exportS3Method knitr::knit_print
knit_print.webr_link <- function(x, ...) {
  livelink_knit_markdown(as.character(x), "Open in webR")
}

#' @rdname knit_print.livelink
#' @exportS3Method knitr::knit_print
knit_print.webr_project <- function(x, ...) {
  livelink_knit_markdown(as.character(x), "Open in webR")
}

#' @rdname knit_print.livelink
#' @exportS3Method knitr::knit_print
knit_print.webr_exercise <- function(x, ...) {
  livelink_knit_markdown(as.character(x), "Open in webR")
}

#' @rdname knit_print.livelink
#' @exportS3Method knitr::knit_print
knit_print.webr_directory <- function(x, ...) {
  livelink_knit_markdown(as.character(x), "Open in webR")
}

#' @rdname knit_print.livelink
#' @exportS3Method knitr::knit_print
knit_print.shinylive_link <- function(x, ...) {
  livelink_knit_markdown(as.character(x), "Open in Shinylive")
}

#' @rdname knit_print.livelink
#' @exportS3Method knitr::knit_print
knit_print.shinylive_project <- function(x, ...) {
  livelink_knit_markdown(as.character(x), "Open in Shinylive")
}

#' @rdname knit_print.livelink
#' @exportS3Method knitr::knit_print
knit_print.shinylive_directory <- function(x, ...) {
  livelink_knit_markdown(as.character(x), "Open in Shinylive")
}
