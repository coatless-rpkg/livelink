# livelink 0.1.0

Initial CRAN release.

## Features

### WebR REPL Links

* `webr_repl_link()`: Create shareable links for single R scripts
* `webr_repl_project()`: Create links for multi-file R projects
* `webr_repl_exercise()`: Create paired exercise and solution links for education
* `webr_repl_directory()`: Batch process directories of R scripts

### Shinylive Links

* `shinylive_r_link()`: Create shareable R Shiny app links
* `shinylive_py_link()`: Create shareable Python Shiny app links
* `shinylive_project()`: Create multi-file Shiny project links
* `shinylive_directory()`: Batch process directories of Shiny apps

### Decoding and Preview

* `decode_webr_link()`: Extract files from webR URLs to a local directory
* `decode_shinylive_link()`: Extract files from Shinylive URLs
* `preview_webr_link()`: Preview webR link contents without writing files
* `preview_shinylive_link()`: Preview Shinylive link contents

### Input Flexibility

* Support for R expressions, character strings, file paths, and clipboard input
* Multi-file support via named lists or file path vectors

### Configuration

* `set_webr_base_url()`: Configure a custom webR base URL
* Support for webR interface panels (plot, files, terminal, editor)
* Autorun capability for automatic code execution

## Naming

The public API was unified before the first release:

* The primary input argument is `input` across every link-creating function.
  It was previously `input` in `webr_repl_link()` but `files` elsewhere, even
  though the argument accepts code strings, expressions, file paths, named
  lists, and clipboard input -- not just files.
* webR's interface argument is now `panels`, not `mode`. `mode` meant two
  unrelated things: which webR panels to display, and Shinylive's
  `"editor"`/`"app"` display mode. Shinylive keeps `mode`.

## Fixes made before release

* `decode_webr_link()` and `decode_shinylive_link()` failed on any sharelink
  longer than roughly 8,000 characters. The internal hash used to name the
  output subdirectory overflowed `.Machine$integer.max`, and R's promotion of
  the overflowed sum to double made `sprintf("%x", .)` an error. Realistically
  sized scripts could not be decoded.
* Code containing `/` (division) or a trailing dot-suffix -- `x <- 1/2`,
  `df$col.name` -- was misread as a file path and rejected. The resulting error
  message could not even render, aborting inside cli instead.
* `shinylive_r_link()` and `shinylive_py_link()` documented clipboard input but
  their input argument had no default, so calling them with no arguments raised
  `argument "files" is missing`.
* `shinylive_directory()` raised `invalid subscript type 'list'` on a directory
  containing no app subdirectories, instead of warning and returning an empty
  result.
* Added the missing `print()`, `repl_urls()`, and `as.character()` methods for
  `shinylive_preview`, `shinylive_decoded`, and `shinylive_decoded_batch`.
  `repl_urls()` on a Shinylive preview previously aborted, and
  `print(preview, show_content = TRUE)` silently did nothing.
* `print()` methods now consistently return their input invisibly.
* `webr_repl_project()` now sets the autorun URL flag whenever any file is
  marked for autorun, not only when `autorun_files = "all"`.
* `webr_repl_link()` now warns when `autorun = TRUE` is requested for a file
  that is not an R script, rather than silently emitting a link that claims to
  autorun.
* `webr_repl_directory()` now always returns a `webr_directory` object; it
  previously returned `character(0)` when no files matched.
* Expression input no longer leaks its wrapping `{` and `}` into the shared
  script when R is running without source references (the default under
  `Rscript` and `R CMD check`).
* `decode_*()` functions now default to writing inside the session temporary
  directory rather than the current working directory.
