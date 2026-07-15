# Create a webR REPL sharelink from R code

Generates a shareable URL for R code that can be executed in the webR
environment. Supports expressions, file paths, character strings, and
clipboard input.

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
  show. Valid panels: `"plot"`, `"files"`, `"terminal"`, `"editor"`. Can
  be `c("plot", "files")` or `"plot-files"`. If NULL (default), shows
  all panels.

- version:

  webR version to use (`"latest"` or specific version \>= "v0.5.4")

- base_url:

  webR application URL. If NULL, uses global option or builds from
  version

## Value

webr_link object containing the webR sharelink and metadata

## Comments in expression input

Expression input recovers your source from R's *source references*,
which R only attaches when `keep.source` is enabled. Comments therefore
survive in an interactive session, but are dropped when the calling code
is parsed without source references – notably inside a knitted 'Quarto'
or 'R Markdown' document, because 'knitr' evaluates chunks through
[`evaluate::evaluate()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/evaluate.r-lib.org/reference/evaluate.md),
which discards them. No `keep.source` setting recovers them there.

If you need comments preserved, pass the code as a string or a file
path, or write it as a chunk in the document – see
[livelink-knitr](https://r-pkg.thecoatlessprofessor.com/livelink/reference/livelink-knitr.md)
and
[`vignette("links-in-documents", package = "livelink")`](https://r-pkg.thecoatlessprofessor.com/livelink/articles/links-in-documents.md).

## See also

[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md)
for multi-file projects and
[`webr_repl_exercise()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_exercise.md)
for exercise and solution pairs;
[livelink-knitr](https://r-pkg.thecoatlessprofessor.com/livelink/reference/livelink-knitr.md)
to give a document chunk its own link;
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
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNCMySsuzc1NLKrUyC1JTiwq1lSqjQUAcSAerQ%3D%3D&jz>
#> 
#> File: script.R → /home/web_user/script.R
#> Version: "latest"
#> Autorun: FALSE

# Traditional string input
webr_repl_link("plot(1:10)")
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNBUqo0FAJecF%2Fo%3D&jz>
#> 
#> File: script.R → /home/web_user/script.R
#> Version: "latest"
#> Autorun: FALSE

# Choose which panels the REPL shows
webr_repl_link({ hist(rnorm(100)) }, panels = c("plot", "editor"))
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/?mode='plot-editor'#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKsjILC7RKMrLL8rVMDQw0NRUqo0FADZ%2BGjc%3D&jz>
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
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNAEiiWWluQXleYpWZUUlabWxgIAJsQdcg%3D%3D&jza>
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
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNBUqo0FAJecF%2Fo%3D&jz>
#> 
#> File: script.R → /home/web_user/script.R
#> Version: "latest"
#> Autorun: FALSE

# Read the code from the clipboard
if (interactive()) {
  webr_repl_link()
}
```
