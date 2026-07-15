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
