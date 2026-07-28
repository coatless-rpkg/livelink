# livelink 0.1.1

## Security

* `decode_webr_link()` and `decode_shinylive_link()` now refuse file names that
  escape `output_dir`, such as `../../.Rprofile`. Names holding a subdirectory,
  like `R/helpers.R`, are unaffected.

## Encoding

* webR links now use msgpack rather than JSON, so a link ends in `mz` where it
  used to end in `jz`. This is the same form the webR share button writes. A
  one-line snippet comes out about 11% shorter.

* `decode_webr_link()` still reads the older `jz` links, and so does webR, so
  links you have already shared keep working.

## Bug fixes

* `webr_repl_link()` no longer fails with `argument is of length zero` when the
  code behind a braced expression cannot be read back, as from an unsaved buffer
  in Positron. The code is rebuilt from the expression instead, which loses
  comments but not the code itself.

* `webr_repl_link()` no longer fails the same way when the file it was reading
  from is shorter than the expression.

* `webr_repl_link({ })`, and a block holding only comments, no longer fail with
  `invalid 'pattern' argument`.

* `webr_repl_link()` no longer takes code from elsewhere in a file that has
  changed since the expression was read.

* `webr_repl_link()` keeps the comments in a braced expression read from a file
  on disk.

* `webr_repl_link()` now reports an error when given several file paths, instead
  of building a link that cannot open, and points to `webr_repl_project()`.

* `webr_repl_link()` and `webr_repl_project()` now refuse a file that is not
  valid UTF-8, or that holds an embedded NUL, instead of building a link that
  cannot be read back.

* `webr_repl_link()` now refuses `NA` and empty input.

* `webr_repl_project()` now warns when `autorun_files` names a file webR cannot
  run on opening, and `print()` marks the files that really do run.

* `decode_webr_link()` writes a binary file back exactly as it was, rather than
  as text listing its byte values.

* `decode_webr_link()` reports a malformed link when the ending of the link does
  not describe the code inside it.

* `decode_webr_link()` and `preview_webr_link()` now read links built against a
  webR site you host yourself.

* `shinylive_directory()` now includes the `.css` and `.js` files an app needs,
  and leaves out only files that cannot be read as text.

* `link.only = TRUE` no longer leaves an empty code block above the link.

## Error messages

* `webr_repl_project()` and `shinylive_project()` now explain a missing `input`
  instead of reporting the name of an internal variable.

* `version`, `panels`, and `mode` now report `NA` as an invalid value rather
  than failing with `missing value where TRUE/FALSE needed`.

* `panels` now names the component at fault, and says whether it is an unknown
  panel or one named twice.

* `shinylive_directory()` now checks `app_file`.

* `preview_webr_link()` and `preview_shinylive_link()` now keep the explanation
  from the underlying error rather than replacing it.

* Errors about an invalid argument no longer show a blank `You provided:` line
  for `NULL` and for empty values.

## Documentation

* Every help page now lists what a returned object holds, one entry at a time,
  rather than describing it only as an object with metadata.

# livelink 0.1.0

Initial CRAN release. livelink packs R code into a shareable URL that runs in the
browser, with no server and nothing to upload. Two destinations are supported:
webR, a full R REPL, and Shinylive, which runs R and Python Shiny apps.

## Features

### webR REPL links

* `webr_repl_link()`: create a shareable link for a single R script.
* `webr_repl_project()`: create a link for a multi-file R project.
* `webr_repl_exercise()`: create paired exercise and solution links, for teaching.
* `webr_repl_directory()`: turn a directory into one link per script, or into a
  single bundled link with `single_link = TRUE`.

### Shinylive links

* `shinylive_r_link()` and `shinylive_py_link()`: create a link for an R or Python
  Shiny app.
* `shinylive_project()`: create a link for a multi-file Shiny app.
* `shinylive_directory()`: create a link for every Shiny app in a directory.

### Decoding and preview

* `decode_webr_link()` and `decode_shinylive_link()`: extract the embedded files to
  a directory. They default to the session temporary directory, so a stray call
  cannot litter your project.
* `preview_webr_link()` and `preview_shinylive_link()`: inspect a link in memory
  without writing anything, which is the safe way to open a link from someone else.
* Decoded results expose the same fields whether a link came from webR or
  Shinylive, so one piece of code can handle both.

### Input flexibility

* Code can be an R expression (no quotes or escaping), a character string, a file
  path, or whatever is on the clipboard.
* Multi-file input via named lists or file path vectors. The primary argument is
  `input` across every link-creating function, whatever form the code takes.

### Configuration

* Choose which webR panels appear (plot, files, terminal, editor) with `panels`,
  and Shinylive's editor-or-app view with `mode`.
* `autorun` runs the code the moment a link opens.
* `set_webr_base_url()`: point webR links at a custom or self-hosted deployment.

### Working with the results

* Every returned object answers to `repl_urls()` and `as.character()` for the bare
  URL(s), and to `format()` for its summary as a character vector.
* `knit_print()` renders a link object as a clickable link when it is the value of
  a 'knitr' or 'Quarto' chunk.
* `as.data.frame()` turns a directory or a decoded batch into a tidy data frame.

### Documents

* `use_livelink_engine()`: registers a `livelink` chunk engine for 'knitr' and
  'Quarto', so a code chunk in a document becomes a runnable link.

  ````
  ```{livelink}
  #| autorun: true
  # Load the data
  data(mtcars)
  plot(mtcars$mpg, mtcars$wt)
  ```
  ````

  Use this rather than expression input inside a knitted document. 'knitr' runs
  chunks through `evaluate::evaluate()`, which discards source references, so
  comments inside a `{ }` expression are silently dropped when a document is
  rendered, and no `keep.source` setting recovers them. The engine receives the
  chunk's verbatim source, so comments survive. Chunk options cover `autorun`,
  `panels`, `mode`, `filename`, `link.text`, `link.only`, and `engine.target`
  (`"webr"`, `"shinylive-r"`, or `"shinylive-py"`).

* `use_livelink_hook()`: a 'knitr' chunk hook, so an ordinary `r` chunk can carry
  its own link. The chunk still runs, its output and plots appear in the rendered
  page, and a link is added underneath:

  ```
  ```{r}
  #| livelink: true
  #| autorun: true
  data(mtcars)
  plot(mtcars$mpg, mtcars$wt)   # a comment that survives
  ```
  ```

  The engine (```` ```{livelink} ````) replaces execution, so it produces a link
  but no R output. The hook is what you want when the reader should see the result
  and also be able to open it; the engine is for code the session cannot or should
  not run, such as a Shiny app or anything needing a package you have not installed.

  Both are registered on load, and both receive the chunk's verbatim source, so
  comments reach the link either way.

### Projects as code

* `webr_repl_project()`, `shinylive_project()`, and `shinylive_r_link()` accept a
  file's contents as a braced expression, so a multi-file project can be written as
  R rather than as strings full of escaped newlines:

  ```r
  webr_repl_project(list(
    "main.R"    = { source("utils.R"); summarise(mtcars) },
    "utils.R"   = { summarise <- function(d) summary(d) },
    "README.md" = "# Analysis"
  ))
  ```

  The blocks are captured, never evaluated. They are source to ship, not code to
  run, so an assignment inside one leaves nothing behind in the session. Strings
  and expressions mix freely, which is what a project holding both code and a
  `README.md` needs.

  This only works when the list is written *inside* the call. `list()` is an
  ordinary function, so assigning it to a variable first evaluates its arguments and
  the block runs there and then. livelink detects that case and errors, rather than
  encoding whatever the block returned.
