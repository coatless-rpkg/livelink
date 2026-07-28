#' Turn a livelink container into a data frame
#'
#' The container classes convert to a tidy data frame.
#'
#' @param x A `webr_directory`, `shinylive_directory`, `webr_decoded_batch`, or
#'   `shinylive_decoded_batch` object.
#' @param row.names A character vector of row names, or `NULL`.
#' @param optional Ignored. It is here so the arguments match those of
#'   `as.data.frame()`.
#' @param ... Ignored.
#'
#' @return
#' A data frame.
#'
#' - A directory gives one row for each generated link, with the columns
#'   `filename` and `url`.
#' - A decoded batch gives one row for each URL that decoded successfully, with
#'   the columns `name`, `url`, `total_files`, `total_size`, and `output_dir`.
#'   A URL that failed to decode carries no result and is left out. The counts
#'   are held in the object's `total_urls` and `successful_urls` fields.
#'
#' @details
#' A folder of links or a batch of decoded results can be tabulated, filtered,
#' joined, or written to CSV with the tools you already use. For a tibble, wrap
#' the result with `tibble::as_tibble(as.data.frame(x))`.
#'
#' @seealso
#' [webr_repl_directory()] for the webR directory objects tabulated here.
#'
#' [shinylive_directory()] for the Shinylive directory objects tabulated here.
#'
#' [decode_webr_link()] and [decode_shinylive_link()] for the decoded batch
#' objects tabulated here.
#'
#' @examples
#' dir <- tempfile()
#' dir.create(dir)
#' writeLines("plot(1:10)",       file.path(dir, "one.R"))
#' writeLines("hist(rnorm(100))", file.path(dir, "two.R"))
#'
#' links <- webr_repl_directory(dir)
#' as.data.frame(links)
#'
#' @name as.data.frame.livelink
NULL

# A directory holds a named character vector: filename -> URL.
directory_to_data_frame <- function(x, row.names) {
  urls <- x$urls
  filename <- if (is.null(names(urls))) {
    rep(NA_character_, length(urls))
  } else {
    names(urls)
  }
  data.frame(
    filename = filename,
    url = unname(urls),
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}

# A batch holds a named list of the decoded results (only the successes; a
# failed URL leaves no entry). Each result is itself a decoded object carrying
# its own url, counts, and output directory.
batch_to_data_frame <- function(x, row.names) {
  results <- x$results

  get_num <- function(field) {
    vapply(results, function(r) {
      v <- r[[field]]
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1))
  }
  get_chr <- function(field) {
    vapply(results, function(r) {
      v <- r[[field]]
      if (is.null(v)) NA_character_ else as.character(v)
    }, character(1))
  }

  name <- if (is.null(names(results))) seq_along(results) else names(results)

  data.frame(
    name = name,
    url = get_chr("url"),
    total_files = get_num("total_files"),
    total_size = get_num("total_size"),
    output_dir = get_chr("output_dir"),
    stringsAsFactors = FALSE,
    row.names = row.names
  )
}

#' @rdname as.data.frame.livelink
#' @export
as.data.frame.webr_directory <- function(x, row.names = NULL, optional = FALSE, ...) {
  directory_to_data_frame(x, row.names)
}

#' @rdname as.data.frame.livelink
#' @export
as.data.frame.shinylive_directory <- function(x, row.names = NULL, optional = FALSE, ...) {
  directory_to_data_frame(x, row.names)
}

#' @rdname as.data.frame.livelink
#' @export
as.data.frame.webr_decoded_batch <- function(x, row.names = NULL, optional = FALSE, ...) {
  batch_to_data_frame(x, row.names)
}

#' @rdname as.data.frame.livelink
#' @export
as.data.frame.shinylive_decoded_batch <- function(x, row.names = NULL, optional = FALSE, ...) {
  batch_to_data_frame(x, row.names)
}
