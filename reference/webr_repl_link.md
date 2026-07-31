# Create a webR REPL sharelink from R code

Turns a single R script into a URL that runs it in the webR REPL.

## Usage

``` r
webr_repl_link(
  input = NULL,
  filename = "script.R",
  path = NULL,
  autorun = FALSE,
  panels = NULL,
  version = "latest",
  base_url = NULL
)
```

## Arguments

- input:

  Code input. Can be:

  - R expression (no quotes needed): `webr_repl_link({ plot(1:10) })`

  - Character string: R code to execute

  - File path: Path to R file to read

  - NULL: Read from clipboard (requires clipr package)

- filename:

  Name for the file (default: `"script.R"`)

- path:

  Full path where the file will be placed in webR. If NULL (default),
  the file is placed at `"/home/web_user/{filename}"`.

- autorun:

  Logical. Whether to auto-execute the code when link is opened
  (default: `FALSE`). Only R files (`.R`) can be auto-executed.

- panels:

  Character vector or string specifying which webR interface panels to
  show. The panels are `"plot"`, `"files"`, `"terminal"`, and
  `"editor"`. Can be `c("plot", "files")` or `"plot-files"`. If NULL
  (default), shows all panels.

- version:

  webR version to use (`"latest"` or specific version \>= "v0.5.4")

- base_url:

  webR application URL. If NULL, uses global option or builds from
  version

## Value

A `webr_link` object, which is a list with these entries.

- `url`, the sharelink itself, as a single string.

- `filename`, the name the script is given inside webR.

- `path`, where that file is placed inside webR.

- `mode`, the panels the link asks for, or `NULL` for all of them.

- `version`, the webR version the link points at.

- `autorun`, `TRUE` when the code runs as soon as the link opens.

Use [`as.character()`](https://rdrr.io/r/base/character.html) on the
object to get the URL on its own.

## Details

The code travels inside the URL, in the fragment after the `#`, which
browsers keep local and never send to a server. Opening the link boots
webR in the reader's own tab, so nothing is installed and nothing runs
on your side.

`input` is deliberately permissive. It takes an expression in braces, a
string, a path to a file, or the clipboard when nothing is passed at
all. One script goes in one link. For several files, see
[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md).

## Comments in expression input

Expression input recovers your code from the source R kept when it read
the expression. R only keeps that source when `keep.source` is enabled,
so comments survive in an interactive session but are dropped when the
calling code is read without it. That is what happens inside a knitted
'Quarto' or 'R Markdown' document, because 'knitr' evaluates chunks
through
[`evaluate::evaluate()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/evaluate.r-lib.org/reference/evaluate.md),
which throws the source away. No `keep.source` setting recovers them
there.

If you need comments preserved, pass the code as a string or a file
path, or write it as a chunk in the document. See
[livelink-knitr](https://r-pkg.thecoatlessprofessor.com/livelink/reference/livelink-knitr.md)
and
[`vignette("links-in-documents", package = "livelink")`](https://r-pkg.thecoatlessprofessor.com/livelink/articles/links-in-documents.md).

## See also

[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md)
for multi-file projects.

[`webr_repl_exercise()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_exercise.md)
for exercise and solution pairs.

[livelink-knitr](https://r-pkg.thecoatlessprofessor.com/livelink/reference/livelink-knitr.md)
to give a document chunk its own link.

[`vignette("getting-started", package = "livelink")`](https://r-pkg.thecoatlessprofessor.com/livelink/articles/getting-started.md)
for an introduction.

## Examples

``` r
# Expression input (no quotes needed!)
webr_repl_link({
  plot(1:10)
  summary(mtcars)
})
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dUVxclFmQYle0JKCxJKM7foZ%2Bbmp%2BuWpSfGlxalF%2BnDJktSKkl0FOfklGoZWhgaaXMWlubmJRZUauSXJiUXFmgDuJR64&mz>
#> 
#> File: script.R → /home/web_user/script.R
#> Version: "latest"
#> Autorun: FALSE

# Traditional string input
webr_repl_link("plot(1:10)")
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dUVxclFmQYle0JKCxJKM7foZ%2Bbmp%2BuWpSfGlxalF%2BnDJktSKklUFOfklGoZWhgaaAC8DGLU%3D&mz>
#> 
#> File: script.R → /home/web_user/script.R
#> Version: "latest"
#> Autorun: FALSE

# Choose which panels the REPL shows
webr_repl_link({ hist(rnorm(100)) }, panels = c("plot", "editor"))
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/?mode='plot-editor'#code=eJyb2LwkLzE3dUVxclFmQYle0JKCxJKM7foZ%2Bbmp%2BuWpSfGlxalF%2BnDJktSKkg0ZmcUlGkV5%2BUW5GoYGBpqaANHPGvg%3D&mz>
#> 
#> File: script.R → /home/web_user/script.R
#> Interface: "Plot" and "Editor"
#> Version: "latest"
#> Autorun: FALSE

# Run the code as soon as the link opens
webr_repl_link("plot(1:10)", autorun = TRUE)
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJyb2LIkLzE3dUVxclFmQYle0JKCxJKM7foZ%2Bbmp%2BuWpSfGlxalF%2BnDJktSKklUFOfklGoZWhgaayxNLS%2FKLSvMOAwAjdx0u&mza>
#> 
#> File: script.R → /home/web_user/script.R
#> Version: "latest"
#> Autorun: TRUE

# File path input
script <- tempfile(fileext = ".R")
writeLines("plot(1:10)", script)
webr_repl_link(script)
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dUVxclFmQYle0JKCxJKM7foZ%2Bbmp%2BuWpSfGlxalF%2BnDJktSKklUFOfklGoZWhgaaAC8DGLU%3D&mz>
#> 
#> File: script.R → /home/web_user/script.R
#> Version: "latest"
#> Autorun: FALSE

# Read the code from the clipboard
if (interactive()) {
  webr_repl_link()
}
```
