#' Stringify R expressions to source code
#'
#' Adapted from `stringify_expression()` in the reprex package
#' (<https://github.com/tidyverse/reprex/blob/main/R/stringify_expression.R>),
#' Copyright (c) 2024 reprex authors, MIT licensed -- see LICENSE.note. The
#' srcref reconstruction, trailing-comment rescue, and common-indentation trim
#' come from reprex; the braced-body deparse fallback, the wholeSrcref bound on
#' the tail scan, and the clamped-range `getSrcLines()` shim below are local
#' changes.
#'
#' @param x Expression to stringify
#' @return Character string of source code
#' @noRd
stringify_expression <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  .srcref <- utils::getSrcref(x)

  if (is.null(.srcref)) {
    # No srcrefs: R was parsed with keep.source = FALSE (the default under
    # Rscript and R CMD check), so comments are already gone and only the
    # abstract syntax survives. Deparse the braced body statement by statement
    # so the wrapping `{` and `}` do not end up in the shared script.
    if (is.call(x) && identical(x[[1]], as.name("{"))) {
      body_exprs <- as.list(x)[-1]
      lines <- unlist(lapply(body_exprs, deparse))
      return(enc2utf8(lines))
    }

    return(enc2utf8(deparse(x)))
  }

  ## Construct a new srcref with the first_line, first_byte, etc. from the
  ## first expression and the last_line, last_byte, etc. from the last one.
  first_src <- .srcref[[1]]
  last_src <- .srcref[[length(.srcref)]]

  .srcfile <- attr(first_src, "srcfile")

  src <- srcref(
    .srcfile,
    c(
      first_src[[1]], first_src[[2]],
      last_src[[3]], last_src[[4]],
      first_src[[5]], last_src[[6]],
      first_src[[7]], last_src[[8]]
    )
  )

  lines <- enc2utf8(as.character(src, useSource = TRUE))

  ## remove the first brace and line if the brace is the only thing on the line
  lines[[1L]] <- sub("^[{]", "", lines[[1L]])
  if (!nzchar(lines[[1L]])) {
    lines <- lines[-1L]
  }

  ## identify the last source line affiliated with an expression
  n <- utils::getSrcLocation(last_src, which = "line", first = FALSE)

  ## rescue trailing comment on (current) last surviving line
  last_source_line <- getSrcLines(.srcfile, n, n) ## "raw"
  last_line <- lines[length(lines)] ## srcref'd
  m <- regexpr(last_line, last_source_line, fixed = TRUE)
  rescue_me <- substring(last_source_line, m + attr(m, "match.length"))
  if (grepl("^\\s*#", rescue_me)) {
    lines[length(lines)] <- paste0(last_line, rescue_me)
  }

  ## rescue trailing comment lines
  tail_lines <- getSrcLines(.srcfile, n + 1, Inf)
  closing_bracket_line <- max(grep("^\\s*[}]", tail_lines), 0)
  tail_lines <- utils::head(tail_lines, closing_bracket_line - 1)

  trim_common_leading_ws(c(lines, tail_lines))
}

#' Trim common leading whitespace from lines
#'
#' Adapted from the reprex package's helper of the same name (R/utils.R),
#' Copyright (c) 2024 reprex authors, MIT licensed -- see LICENSE.note.
#' The guards for empty and whitespace-only input are local additions.
#'
#' @param lines Character vector of lines
#' @return Character vector with common leading whitespace removed
#' @noRd
trim_common_leading_ws <- function(lines) {
  if (length(lines) == 0) return(lines)

  # Find common leading whitespace
  non_empty <- lines[nzchar(trimws(lines, which = "right"))]
  if (length(non_empty) == 0) return(lines)

  leading_ws <- regexpr("^\\s*", non_empty)
  ws_lengths <- attr(leading_ws, "match.length")
  min_ws <- min(ws_lengths)

  if (min_ws > 0) {
    lines <- sub(paste0("^\\s{", min_ws, "}"), "", lines)
  }

  lines
}

#' Get source lines from srcfile
#'
#' Deliberately shadows base::getSrcLines(), which the reprex original calls,
#' with clamped-range semantics: out-of-range requests return character(0)
#' rather than erroring.
#'
#' @param srcfile Source file object
#' @param start Start line
#' @param end End line
#' @return Character vector of source lines
#' @noRd
getSrcLines <- function(srcfile, start, end = start) {
  if (is.infinite(end)) {
    all_lines <- srcfile$lines
    end <- length(all_lines)
  }

  if (start > end || start < 1) {
    return(character(0))
  }

  srcfile$lines[start:min(end, length(srcfile$lines))]
}

#' Read clipboard content
#' @return Character vector of clipboard content
#' @noRd
ingest_clipboard <- function() {
  if (!requireNamespace("clipr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg clipr} is required for clipboard input",
      "i" = "Install with: {.code install.packages('clipr')}"
    ))
  }

  if (!clipr::clipr_available()) {
    cli::cli_abort(c(
      "Clipboard is not available",
      "i" = "This may happen in non-interactive sessions or some server environments"
    ))
  }

  content <- clipr::read_clip()
  if (length(content) == 0) {
    cli::cli_abort("Clipboard is empty")
  }

  content
}

#' Locate and categorize input type
#'
#' The locate-then-switch dispatch design, and the helper names locate_input()
#' and ingest_clipboard(), follow the reprex package's input handling
#' (R/utils-io.R, R/utils-clipboard.R, R/reprex_impl.R); the implementations
#' here are livelink's own.
#'
#' @param input Input provided by user
#' @param x_expr Expression provided by user
#' @return Character string indicating input type
#' @noRd
locate_input <- function(input = NULL, x_expr = NULL) {
  if (!is.null(x_expr)) {
    return("expr")
  }

  if (is.null(input)) {
    return("clipboard")
  }

  if (is.character(input)) {
    # Check if all elements look like file paths
    if (all(is_likely_file_path(input))) {
      # Verify files exist
      if (all(file.exists(input))) {
        return("path")
      } else {
        missing_files <- input[!file.exists(input)]
        # The quantity has to appear in the same string as the {?s} or cli
        # cannot pluralize and aborts on its own error message.
        cli::cli_abort(c(
          "{length(missing_files)} file{?s} not found",
          "x" = "Cannot find: {.file {missing_files}}",
          "i" = "To share this as code rather than a file path, pass it as a multi-line string."
        ))
      }
    } else {
      return("input")
    }
  }

  if (is.list(input)) {
    return("input")
  }

  cli::cli_abort(c(
    "Invalid input type",
    "x" = "Input must be a character vector (code or file paths), a named list, or an expression"
  ))
}

#' Check if string looks like a file path
#'
#' A file on disk is always a path. For anything else we have to guess, and the
#' guess must be conservative: R code routinely contains `/` (division) and
#' trailing dot-suffixes (`df$col.name`), so treating those as paths would
#' reject ordinary code. We therefore only call a non-existent string a path
#' when it carries no code syntax *and* has a file extension -- which keeps
#' mistyped filenames reporting "not found" instead of being silently encoded.
#'
#' @param x Character vector to check
#' @return Logical vector, one element per input
#' @noRd
is_likely_file_path <- function(x) {
  if (!is.character(x)) return(FALSE)

  vapply(x, function(path) {
    if (!nzchar(path) || grepl("\n", path, fixed = TRUE)) {
      return(FALSE)
    }

    if (file.exists(path)) {
      return(TRUE)
    }

    # Syntax that cannot appear in a filename anyone meant to write.
    if (grepl("[()<>={};\"'$]", path)) {
      return(FALSE)
    }

    grepl("\\.[A-Za-z0-9]{1,10}$", path)
  }, logical(1), USE.NAMES = FALSE)
}

#' Process input based on its type
#' @param input Input provided by user
#' @param x_expr Expression provided by user
#' @return Character string or named list of processed content
#' @noRd
process_input <- function(input = NULL, x_expr = NULL) {
  where <- locate_input(input, x_expr)

  switch(where,
         expr = {
           code <- stringify_expression(x_expr)
           if (is.null(code)) {
             cli::cli_abort("Failed to convert expression to source code")
           }
           paste(code, collapse = "\n")
         },
         clipboard = {
           content <- ingest_clipboard()
           paste(content, collapse = "\n")
         },
         path = {
           if (length(input) == 1) {
             # Single file - return content as string
             content <- readLines(input, warn = FALSE)
             paste(content, collapse = "\n")
           } else {
             # Multiple files - return named list
             files <- lapply(input, function(file) {
               content <- readLines(file, warn = FALSE)
               paste(content, collapse = "\n")
             })
             names(files) <- basename(input)
             files
           }
         },
         input = {
           if (length(input) == 1) {
             # Single string - return as is
             input
           } else {
             # Multiple strings - treat as separate files
             cli::cli_abort(c(
               "Multiple character strings not supported for single code input",
               "i" = "Use a single string, file paths, or an expression",
               "i" = "For multiple files, use the project functions"
             ))
           }
         }
  )
}

#' Is this captured expression a `{ ... }` block?
#'
#' Only a brace block means "treat my argument as literal source code". Any
#' other call -- `list(...)`, `paste0(...)`, `readLines(f)` -- is a value the
#' user wants evaluated, and deparsing it would ship the call itself as the
#' shared script.
#'
#' @param x_expr Result of [substitute()] on the input argument
#' @return TRUE if `x_expr` is a brace block
#' @noRd
is_brace_call <- function(x_expr) {
  is.call(x_expr) && identical(x_expr[[1]], as.name("{"))
}

#' Process input for a Shinylive app
#'
#' Shinylive apps are single-file or multi-file, so the input can be a lone code
#' string (or expression) as well as the project-shaped forms -- a named list of
#' files, or several file paths. Route the project-shaped ones accordingly;
#' [process_input()] deliberately rejects them.
#'
#' @param input Input provided by the user
#' @param x_expr Expression provided by the user
#' @return A single code string, or a named list of file contents
#' @noRd
process_shinylive_input <- function(input = NULL, x_expr = NULL) {
  is_multi_file <- is.list(input) || (is.character(input) && length(input) > 1)

  if (is_multi_file) {
    return(process_project_input(input = input))
  }

  process_input(input = input, x_expr = x_expr)
}

#' Process input for multi-file projects
#' @param input Input provided by user
#' @param x_expr Expression provided by user
#' @return Named list of file contents
#' @noRd
process_project_input <- function(input = NULL, x_expr = NULL) {
  where <- locate_input(input, x_expr)

  switch(where,
         expr = {
           cli::cli_abort(c(
             "Expressions not supported for multi-file projects",
             "i" = "Use a named list, file paths, or clipboard input"
           ))
         },
         clipboard = {
           cli::cli_abort(c(
             "Clipboard input not supported for multi-file projects",
             "i" = "Use a named list or file paths for multiple files"
           ))
         },
         path = {
           # Read all files and create named list
           files <- lapply(input, function(file) {
             content <- readLines(file, warn = FALSE)
             paste(content, collapse = "\n")
           })
           names(files) <- basename(input)
           files
         },
         input = {
           if (!is_named_list(input)) {
             cli::cli_abort(c(
               "Multi-file input must be a named list or vector of file paths",
               "i" = "Example: {.code list('main.R' = code1, 'utils.R' = code2)}"
             ))
           }
           input
         }
  )
}
