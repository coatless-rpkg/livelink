# Create a Shinylive sharelink for R Shiny apps

Generates a shareable URL for R Shiny applications that can run in the
browser using Shinylive.

## Usage

``` r
shinylive_r_link(input = NULL, mode = "editor", header = TRUE, base_url = NULL)
```

## Arguments

- input:

  App input. Can be:

  - R expression (no quotes needed):
    `shinylive_r_link({ shinyApp(ui, server) })`

  - Character string: R code for the app

  - File path: Path to app.R file

  - Vector of file paths: Multiple files for the app

  - Named list: `list("app.R" = code1, "utils.R" = code2)`

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

- `engine`, the Shiny flavor the link runs, `"r"` here.

- `mode`, the Shinylive display mode, `"editor"` or `"app"`.

Use [`as.character()`](https://rdrr.io/r/base/character.html) on the
object to get the URL on its own.

## Details

The whole app travels inside the URL, so opening the link runs it in the
reader's browser with no server behind it. A single string becomes
`app.py`. Pass a named list to ship several files.

## Comments in expression input

Comments inside a [`{ }`](https://rdrr.io/r/base/Paren.html) expression
are recovered from the source R keeps, so they survive interactively but
are dropped inside a knitted 'Quarto' or 'R Markdown' document. Pass a
string or a file path, or use the `livelink` chunk engine, if you need
them preserved. See
[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md)
for the details.

## See also

[`shinylive_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_project.md)
for multi-file apps.

[`decode_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_shinylive_link.md)
and
[`preview_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/preview_shinylive_link.md)
to read a link back.

[livelink-knitr](https://r-pkg.thecoatlessprofessor.com/livelink/reference/livelink-knitr.md)
to give a document chunk its own link.

[`vignette("webr-and-shinylive", package = "livelink")`](https://r-pkg.thecoatlessprofessor.com/livelink/articles/webr-and-shinylive.md)
for the guide.

## Examples

``` r
# Expression input (no quotes needed!)
shinylive_r_link({
  ui <- fluidPage(titlePanel("Hello World"))
  server <- function(input, output) {}
  shinyApp(ui, server)
})
#> 
#> ── Shinylive R App ──
#> 
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAVwEsACAHgFp6AzAGzoBMAFKAOZwAFKVqkOcfhDgdhAHTAAJWRyL0A6kQBOHbooCUB+RADOcbQDcLTVm2oQCYksNoRU1UrnpFPH0gb0ICYAviamABZuAJ4AgujCdN7mVhYGeGCk0agIyOQAHqRgIQC6QA>
#> 
#> Files (1):
#> app.R
#> 
#> Engine: "R"
#> Mode: "editor"

# Multiple files as a named list
shinylive_r_link(list(
  "app.R" = "library(shiny)\nshinyApp(fluidPage(), function(i, o) {})",
  "utils.R" = "helper <- function() 42"
))
#> 
#> ── Shinylive R App ──
#> 
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlAB0IPPqwCC6dgDNqAV1oATAApQA5nHYDcAAhnyIBUrRLtaeogN0gAvgLxhSrVAmTkAHqTC3c4aPBU8ibUnNiOxGQU3sjccNSujLoAPAC0+obGphDaugAsAEyOzq5Unt62ALpAA>
#> 
#> Files (2):
#> app.R
#> utils.R
#> 
#> Engine: "R"
#> Mode: "editor"

# File path input
app_dir <- tempfile()
dir.create(app_dir)
app_path <- file.path(app_dir, "app.R")
writeLines("library(shiny)", app_path)
shinylive_r_link(app_path)
#> 
#> ── Shinylive R App ──
#> 
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA>
#> 
#> Files (1):
#> app.R
#> 
#> Engine: "R"
#> Mode: "editor"

# Read the app from the clipboard
if (interactive()) {
  shinylive_r_link()
}
```
