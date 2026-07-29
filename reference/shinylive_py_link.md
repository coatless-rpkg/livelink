# Create a Shinylive sharelink for Python Shiny apps

Generates a shareable URL for Python Shiny applications that can run in
the browser using Shinylive.

## Usage

``` r
shinylive_py_link(
  input = NULL,
  mode = "editor",
  header = TRUE,
  base_url = NULL
)
```

## Arguments

- input:

  App input. Can be:

  - Character string: Python code for the app

  - File path: Path to app.py file

  - Vector of file paths: Multiple files for the app

  - Named list: `list("app.py" = code1, "utils.py" = code2)`

  - NULL: Read from clipboard

- mode:

  Shinylive display mode (default `"editor"`). `"editor"` shows an
  editable code panel beside the running app. `"app"` shows only the
  running app.

- header:

  Logical, whether to show the Shinylive header bar. It applies only
  when `mode = "app"` and is ignored in the default `"editor"` mode.
  Defaults to `TRUE`.

- base_url:

  Custom Shinylive base URL. If NULL (default), links point at
  https://shinylive.io.

## Value

A `shinylive_link` object, which is a list with these entries.

- `url`, the sharelink itself, as a single string.

- `files`, the names of the files carried in the link, as a character
  vector.

- `engine`, the Shiny flavor the link runs, `"python"` here.

- `mode`, the Shinylive display mode, `"editor"` or `"app"`.

Use [`as.character()`](https://rdrr.io/r/base/character.html) on the
object to get the URL on its own.

## Details

The whole app travels inside the URL, so opening the link runs it in the
reader's browser with no server behind it. A single string becomes
`app.R`. Pass a named list to ship several files.

## See also

[`shinylive_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_project.md)
for multi-file apps.

[`decode_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_shinylive_link.md)
and
[`preview_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/preview_shinylive_link.md)
to read a link back.

## Examples

``` r
# String input
app_code <- "
from shiny import App, render, ui
app_ui = ui.page_fluid(ui.h2('Hello World'))
def server(input, output, session): pass
app = App(app_ui, server)
"
shinylive_py_link(app_code)
#> 
#> ── Shinylive Python App ──
#> 
#> <https://shinylive.io/py/editor/#code=NobwRAdghgtgpmAXGKAHVA6VBPMAaMAYwHsIAXOcpMAHQgDMAnYmAAgGcALASwm1e4xUxRmVYBBdHlaNKAEziNpAV2500qAPqrWAXlaqsUAOZxN9ADaq5ACkOcATDYDkACTgWLxVgHURFuWcASiC6BXoORQA3RRteVGUyaWJEhKTI9nZuUiDEVlQoTPV0PQl0Gw1tbml2aMVQiHwwMmxUBGQKAA8yMABfAF0gA>
#> 
#> Files (1):
#> app.py
#> 
#> Engine: "Python"
#> Mode: "editor"

# Multiple files as a named list
shinylive_py_link(list(
  "app.py" = app_code,
  "utils.py" = "def helper(): return 42"
))
#> 
#> ── Shinylive Python App ──
#> 
#> <https://shinylive.io/py/editor/#code=NobwRAdghgtgpmAXGKAHVA6VBPMAaMAYwHsIAXOcpMAHQgDMAnYmAAgGcALASwm1e4xUxRmVYBBdHlaNKAEziNpAV2500qAPqrWAXlaqsUAOZxN9ADaq5ACkOcATDYDkACTgWLxVgHURFuWcASiC6BXoORQA3RRteVGUyaWJEhKTI9nZuUiDEVlQoTPV0PQl0Gw1tbml2aMVQiHwwMmxUBGQKAA8yMABfPHBoeGpE7gt2LFwCEnJKHuRw1k4PNsYbXJk4MmVGCFYAFgcmlrbqLp7egF0gA>
#> 
#> Files (2):
#> app.py
#> utils.py
#> 
#> Engine: "Python"
#> Mode: "editor"

# File path input
app_dir <- tempfile()
dir.create(app_dir)
app_path <- file.path(app_dir, "app.py")
writeLines("from shiny import App, ui", app_path)
shinylive_py_link(app_path)
#> 
#> ── Shinylive Python App ──
#> 
#> <https://shinylive.io/py/editor/#code=NobwRAdghgtgpmAXGKAHVA6VBPMAaMAYwHsIAXOcpMAMwCdiYACAZwAsBLCbJjmVYnTJMAgujxMArh3xgy2VAmQUAHmTABfALpA>
#> 
#> Files (1):
#> app.py
#> 
#> Engine: "Python"
#> Mode: "editor"

# Read the app from the clipboard
if (interactive()) {
  shinylive_py_link()
}
```
