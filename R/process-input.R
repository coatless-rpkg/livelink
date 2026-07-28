#' Stringify R expressions to source code
#'
#' Recovers the source text a captured expression was written from.
#'
#' @param x Expression to stringify
#'
#' @return
#' Character string of source code
#'
#' @section Provenance:
#' Adapted from `stringify_expression()` in the reprex package
#' (<https://github.com/tidyverse/reprex/blob/main/R/stringify_expression.R>),
#' Copyright (c) 2024 reprex authors, MIT licensed (see inst/COPYRIGHTS).
#' The srcref reconstruction, trailing-comment rescue, and common-indentation
#' trim come from reprex. The braced-body deparse fallback, the wholeSrcref
#' bound on the tail scan, and the clamped-range `getSrcLines()` shim below are
#' local changes.
#'
#' @noRd
stringify_expression <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  .srcref <- utils::getSrcref(x)

  # Srcrefs can exist while the source they point at cannot be read back: an
  # editor's unsaved buffer parses against a srcfile whose "filename" is not a
  # path (Positron's Run button sends "untitled:Untitled-1") and whose lines
  # are not cached. as.character(useSource = TRUE) then hands back a
  # "<srcref: ...>" placeholder rather than code, so treat an unreadable
  # srcfile as if there were no srcrefs at all and deparse instead.
  if (!is.null(.srcref) && !srcref_is_readable(.srcref[[1L]])) {
    .srcref <- NULL
  }

  if (is.null(.srcref)) {
    return(deparse_expression(x))
  }

  # Construct a new srcref with the first_line, first_byte, etc. from the
  # first expression and the last_line, last_byte, etc. from the last one.
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

  # remove the first brace and line if the brace is the only thing on the line
  if (length(lines) > 0) {
    lines[[1L]] <- sub("^[{]", "", lines[[1L]])
    if (!nzchar(lines[[1L]])) {
      lines <- lines[-1L]
    }
  }

  # identify the last source line affiliated with an expression
  n <- utils::getSrcLocation(last_src, which = "line", first = FALSE)

  # rescue trailing comment on (current) last surviving line.
  #
  # Both sides can legitimately be empty, and neither may be indexed blind:
  # `{ }` or a comment-only block leaves nothing once the brace is stripped,
  # and a srcfile that no longer reaches line n -- a file truncated or shortened
  # since it was parsed -- yields no raw line. Handing character(0) to regexpr()
  # or grepl() aborts with "invalid 'pattern' argument" or "argument is of
  # length zero". There is simply no line to hang a rescued comment on, so skip.
  last_source_line <- getSrcLines(.srcfile, n, n) # "raw"

  if (length(lines) > 0 && length(last_source_line) > 0) {
    last_line <- lines[length(lines)] # srcref'd
    m <- regexpr(last_line, last_source_line, fixed = TRUE)
    rescue_me <- substring(last_source_line, m + attr(m, "match.length"))
    if (grepl("^\\s*#", rescue_me)) {
      lines[length(lines)] <- paste0(last_line, rescue_me)
    }
  }

  # rescue trailing comment lines, but only those still INSIDE the block.
  # Scanning to Inf for a closing brace swallows unrelated source whenever the
  # block's own `}` is not on a line of its own: a one-line `{ plot(1:10) }`
  # nested in a call would take everything down to the next line starting with
  # `}`. The `{` call carries a wholeSrcref spanning itself; that is the bound.
  whole <- attr(x, "wholeSrcref")
  block_end <- if (!is.null(whole)) whole[[3L]] else n

  tail_lines <- getSrcLines(.srcfile, n + 1, block_end)
  closing_bracket_line <- max(grep("^\\s*[}]", tail_lines), 0)
  tail_lines <- utils::head(tail_lines, closing_bracket_line - 1)

  recovered <- trim_common_leading_ws(c(lines, tail_lines))

  # A srcref records where the source was, not what it says now. If the file has
  # changed since it was parsed -- an editor buffer saved over, a script
  # regenerated -- those line and column numbers address whatever occupies them
  # today, and the "recovered" text is some other code entirely. That is worse
  # than losing comments: it silently ships source the caller never wrote, and
  # has been seen to pick up neighbouring lines wholesale. Deparsing the
  # expression we were actually handed cannot do that.
  if (!recovered_matches(recovered, x)) {
    return(deparse_expression(x))
  }

  recovered
}

#' Deparse an expression to shareable source
#'
#' The fallback whenever source references cannot be trusted.
#'
#' @param x Expression to deparse
#'
#' @return
#' Character vector of source lines, without the wrapping braces
#'
#' @details
#' Source references cannot be trusted when there are no srcrefs at all, when
#' the source cannot be read back, or when it no longer matches. Comments are
#' lost (they live only in srcrefs), but the code is right.
#'
#' @noRd
deparse_expression <- function(x) {
  # Deparse a braced body statement by statement, so the wrapping `{` and `}`
  # do not end up in the shared script.
  if (is_brace_call(x)) {
    body_exprs <- as.list(x)[-1]

    # An empty block has nothing to deparse, and unlist() of nothing is NULL,
    # which enc2utf8() rejects. Say so directly instead.
    if (length(body_exprs) == 0) {
      return(character(0))
    }

    return(enc2utf8(unlist(lapply(body_exprs, deparse))))
  }

  enc2utf8(deparse(x))
}

#' Does recovered source actually say what the expression says?
#'
#' Compares structure, not text.
#'
#' @param lines Source lines recovered from the srcrefs
#' @param x The expression they are supposed to describe
#'
#' @return
#' TRUE if the two agree
#'
#' @details
#' The recovered lines are parsed and deparsed alongside the expression itself,
#' so comments, indentation and line breaks are free to differ (which is the
#' whole point of reading source back), while a genuine mismatch is caught.
#'
#' @noRd
recovered_matches <- function(lines, x) {
  if (length(lines) == 0) {
    # Nothing recovered is only right when there was nothing to recover.
    return(!is_brace_call(x) || length(as.list(x)) == 1L)
  }

  parsed <- tryCatch(
    parse(text = lines, keep.source = FALSE),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(FALSE)
  }

  expected <- if (is_brace_call(x)) as.list(x)[-1] else list(x)

  if (length(parsed) != length(expected)) {
    return(FALSE)
  }

  normalise <- function(exprs) {
    vapply(exprs, function(e) paste(deparse(e), collapse = "\n"), character(1))
  }

  identical(normalise(as.list(parsed)), normalise(expected))
}

#' Is the source text behind a srcref still readable?
#'
#' A srcref only locates source. It does not carry it.
#'
#' @param src A single srcref
#'
#' @return
#' TRUE if the srcref's first line can be read back
#'
#' @details
#' Whether the text can be recovered depends on the srcfile still being able to
#' produce its lines.
#'
#' @noRd
srcref_is_readable <- function(src) {
  .srcfile <- attr(src, "srcfile")

  if (is.null(.srcfile)) {
    return(FALSE)
  }

  first_line <- src[[1L]]

  length(getSrcLines(.srcfile, first_line, first_line)) > 0
}

#' Trim common leading whitespace from lines
#'
#' Removes the leading whitespace that every line has in common.
#'
#' @param lines Character vector of lines
#'
#' @return
#' Character vector with common leading whitespace removed
#'
#' @section Provenance:
#' Adapted from the reprex package's helper of the same name (R/utils.R),
#' Copyright (c) 2024 reprex authors, MIT licensed (see inst/COPYRIGHTS).
#' The guards for empty and whitespace-only input are local additions.
#'
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
#' with total semantics. Anything unreadable returns character(0) rather than
#' erroring.
#'
#' @param srcfile Source file object
#' @param start Start line
#' @param end End line
#'
#' @return
#' Character vector of source lines, empty if they cannot be read
#'
#' @details
#' Reading `srcfile$lines` directly is not enough, because only a srcfilecopy
#' caches its lines. A plain srcfile leaves them NULL and base reads the file on
#' demand.
#'
#' @noRd
getSrcLines <- function(srcfile, start, end = start) {
  if (start > end || start < 1) {
    return(character(0))
  }

  # base::getSrcLines() clamps an end past the last line, but errors outright
  # when the file behind the srcfile cannot be opened.
  tryCatch(
    suppressWarnings(base::getSrcLines(srcfile, start, end)),
    error = function(e) character(0)
  )
}

#' Read a file that has to survive being embedded in a link
#'
#' Reads a text file and refuses content that could not be read back out of a
#' link.
#'
#' @param path Path to a single file
#'
#' @return
#' The file's contents as one UTF-8 string
#'
#' @details
#' The payload is serialized to JSON and later parsed back with
#' `rawToChar()`, so anything that is not valid UTF-8 produces a link that
#' cannot be decoded. Not even this package's own `preview_webr_link()` can
#' read it back, and it fails with "input string 1 is invalid UTF-8".
#' A latin-1 script or a stray binary file is the realistic case, and it used
#' to be accepted silently. An embedded NUL is worse than invalid. `readLines()`
#' truncates the line at it, so content disappears without a word.
#'
#' @noRd
read_text_file <- function(path) {
  # `warn = TRUE`, because `warn = FALSE` silences the embedded-NUL warning
  # along with the harmless missing-final-EOL one. The handler below keeps the
  # distinction: abort on the first, muffle the second.
  lines <- withCallingHandlers(
    readLines(path, warn = TRUE),
    warning = function(w) {
      if (grepl("nul|embedded", conditionMessage(w), ignore.case = TRUE)) {
        cli::cli_abort(c(
          "Cannot embed {.file {path}}: it contains an embedded NUL",
          "i" = "Only text files can travel in a link.",
          "i" = "Have the code fetch a binary file instead of carrying it."
        ), call = NULL)
      }
      invokeRestart("muffleWarning")
    }
  )

  content <- enc2utf8(paste(lines, collapse = "\n"))

  if (!all(validUTF8(content))) {
    cli::cli_abort(c(
      "Cannot embed {.file {path}}: it is not valid UTF-8",
      "i" = "A link carries its files as UTF-8 text, so this one could not be
             read back.",
      "i" = "Re-save the file as UTF-8, or have the code fetch it instead."
    ), call = NULL)
  }

  content
}

#' Read clipboard content
#'
#' Reads the clipboard, aborting when it is unavailable or empty.
#'
#' @return
#' Character vector of clipboard content
#'
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
#' Works out where the code is coming from, whether an expression, the
#' clipboard, file paths, or literal input.
#'
#' @param input Input provided by user
#' @param x_expr Expression provided by user
#'
#' @return
#' Character string indicating input type
#'
#' @section Provenance:
#' The locate-then-switch dispatch design, and the helper names locate_input()
#' and ingest_clipboard(), follow the reprex package's input handling
#' (R/utils-io.R, R/utils-clipboard.R, R/reprex_impl.R). The implementations
#' here are livelink's own.
#'
#' @noRd
locate_input <- function(input = NULL, x_expr = NULL) {
  if (!is.null(x_expr)) {
    return("expr")
  }

  if (is.null(input)) {
    return("clipboard")
  }

  # Zero-length input classified as a path, because `all()` of nothing is TRUE,
  # and then failed inside readLines() with "invalid 'description' argument".
  if (length(input) == 0) {
    cli::cli_abort(c(
      "{.arg input} is empty",
      "i" = "Pass code, a file path, or an expression in braces."
    ), call = NULL)
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
#' A file on disk is always a path. For anything else we have to guess.
#'
#' @param x Character vector to check
#'
#' @return
#' Logical vector, one element per input
#'
#' @details
#' The guess must be conservative. R code routinely contains `/` (division) and
#' trailing dot-suffixes (`df$col.name`), so treating those as paths would
#' reject ordinary code. We therefore only call a non-existent string a path
#' when it carries no code syntax *and* has a file extension. That keeps
#' mistyped filenames reporting "not found" instead of being silently encoded.
#'
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
#'
#' Dispatches on the located input type and turns it into the code to share.
#'
#' @param input Input provided by user
#' @param x_expr Expression provided by user
#'
#' @return
#' The processed content.
#'
#' - A character string.
#' - A named list.
#'
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
           # A single script is one file. Returning a named list here instead
           # produced a link whose only file had a JSON object for its body,
           # which looks fine locally and fails to open. The sibling `input`
           # branch already refuses the analogous case.
           if (length(input) > 1) {
             cli::cli_abort(c(
               "Multiple file paths not supported for single code input",
               "i" = "Use one file path, a single string, or an expression",
               "i" = "For multiple files, use {.fn webr_repl_project}"
             ))
           }

           read_text_file(input)
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
#' Only a brace block means "treat my argument as literal source code".
#'
#' @param x_expr Result of [substitute()] on the input argument
#'
#' @return
#' TRUE if `x_expr` is a brace block
#'
#' @details
#' Any other call (`list(...)`, `paste0(...)`, `readLines(f)`) is a value the
#' user wants evaluated, and deparsing it would ship the call itself as the
#' shared script.
#'
#' @noRd
is_brace_call <- function(x_expr) {
  is.call(x_expr) && identical(x_expr[[1]], as.name("{"))
}

#' Is this captured expression a literal `list(...)` call?
#'
#' Only a literal call written at the call site can be taken apart without
#' evaluating it.
#'
#' @param x_expr Result of [substitute()] on the input argument
#'
#' @return
#' TRUE if `x_expr` is a call to `list`
#'
#' @details
#' A symbol (`webr_repl_project(project)`) has to be forced and handled the
#' ordinary way.
#'
#' @noRd
is_literal_list_call <- function(x_expr) {
  is.call(x_expr) && identical(x_expr[[1]], as.name("list"))
}

#' Turn a literal `list(...)` call into a named list of file contents
#'
#' Each element is either a `{ ... }` block, taken as literal source, or an
#' ordinary value, evaluated in the caller's environment.
#'
#' @param x_expr The captured `list(...)` call
#' @param env Environment in which to evaluate the non-braced elements
#'
#' @return
#' One of two things.
#'
#' - A named list of file contents.
#' - NULL if this is not the shape we want.
#'
#' @details
#' The braces must NOT be evaluated. `list("a.R" = { x <- 1 })` would otherwise
#' run the block in the caller's frame and leave `x` behind. The whole point is
#' that this is code to ship, not code to run. Because we work from
#' [substitute()] and never force the promise, nothing in a brace is executed.
#'
#' @noRd
eval_project_list <- function(x_expr, env) {
  args <- as.list(x_expr)[-1]

  if (length(args) == 0 || is.null(names(args)) || !all(nzchar(names(args)))) {
    # Not project-shaped, so the caller falls through to ordinary evaluation --
    # which would force any braces. A brace is code to ship, never code to run,
    # so refuse a malformed list that carries one before anything executes.
    if (any(vapply(args, is_brace_call, logical(1)))) {
      cli::cli_abort(c(
        "Project files must be a named list",
        "i" = 'Name every element: {.code list("app.R" = {{ ... }})}'
      ))
    }
    return(NULL)
  }

  contents <- lapply(args, function(arg) {
    if (is_brace_call(arg)) {
      code <- stringify_expression(arg)
      if (is.null(code)) {
        cli::cli_abort("Failed to convert expression to source code")
      }
      paste(code, collapse = "\n")
    } else {
      eval(arg, env)
    }
  })

  files <- stats::setNames(contents, names(args))
  check_file_contents(files)

  files
}

#' Process input for a Shinylive app
#'
#' Shinylive apps are single-file or multi-file, so the input can be a lone code
#' string (or expression) as well as the project-shaped forms.
#'
#' @param input Input provided by the user
#' @param x_expr Expression provided by the user
#' @param env Environment for evaluating list elements
#'
#' @return
#' The content to share.
#'
#' - A single code string.
#' - A named list of file contents.
#'
#' @details
#' The project-shaped forms are a named list of files, or several file paths.
#' Route those accordingly. [process_input()] deliberately rejects them.
#'
#' @noRd
process_shinylive_input <- function(input = NULL, x_expr = NULL,
                                    env = parent.frame()) {
  # A literal list() may carry braced file contents, and must be taken apart
  # before anything in it is forced.
  if (is_literal_list_call(x_expr)) {
    files <- eval_project_list(x_expr, env)
    if (!is.null(files)) {
      return(files)
    }
  }

  is_multi_file <- is.list(input) || (is.character(input) && length(input) > 1)

  if (is_multi_file) {
    return(process_project_input(input = input))
  }

  # As above: a symbol is a value, not source to deparse.
  process_input(input = input,
                x_expr = if (is_brace_call(x_expr)) x_expr else NULL)
}

#' Process input for multi-file projects
#'
#' Turns project-shaped input (a named list of files, or several file paths)
#' into the files to ship.
#'
#' @param input Input provided by user
#' @param x_expr Expression provided by user
#' @param env Environment for evaluating list elements
#'
#' @return
#' Named list of file contents
#'
#' @noRd
process_project_input <- function(input = NULL, x_expr = NULL,
                                  env = parent.frame()) {
  # A literal list() written at the call site may name each file's contents as a
  # `{ ... }` block. Take it apart before the promise is forced -- forcing would
  # execute the blocks in the caller's frame.
  if (is_literal_list_call(x_expr)) {
    files <- eval_project_list(x_expr, env)
    if (!is.null(files)) {
      return(files)
    }
  }

  # Only a brace block is an "expression" here. `webr_repl_project(project)`
  # captures a symbol, which must be forced and handled like any other value.
  where <- locate_input(input, if (is_brace_call(x_expr)) x_expr else NULL)

  switch(where,
         expr = {
           cli::cli_abort(c(
             "Expressions not supported for multi-file projects",
             "i" = "Use a named list of files, or file paths",
             "i" = "Example: {.code list('main.R' = {{ plot(1:10) }})}"
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
           files <- lapply(input, read_text_file)
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
           check_file_contents(input)
           input
         }
  )
}

#' Check that every element of a project list is file contents
#'
#' Catches the one way the braced form goes wrong.
#'
#' @param files Named list of file contents
#'
#' @return
#' Invisible TRUE, or aborts
#'
#' @details
#' `list()` is an ordinary call, so its arguments evaluate unless livelink
#' captures them, which it can only do when the list is written inside the
#' call:
#'
#' ```
#' project <- list("main.R" = { plot(1:10) })   # the block RUNS, here and now
#' webr_repl_project(project)                   # and we receive its value
#'
#' webr_repl_project(list("main.R" = { plot(1:10) }))   # captured, never run
#' ```
#'
#' Assigning first leaves non-character values in the list, so say what happened
#' rather than serializing a function into a link.
#'
#' @noRd
check_file_contents <- function(files) {
  bad <- !vapply(files, function(x) is.character(x) && length(x) == 1, logical(1))

  if (any(bad)) {
    cli::cli_abort(c(
      "Each file's contents must be a single string",
      "x" = "Not a string: {.file {names(files)[bad]}}",
      "i" = "Writing a file as {.code {{ ... }}} only works inside the call:",
      "*" = "{.code webr_repl_project(list('main.R' = {{ plot(1:10) }}))}",
      "!" = "Assigning the list to a variable first runs the block instead."
    ))
  }

  invisible(TRUE)
}
