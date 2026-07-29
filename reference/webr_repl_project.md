# Create webR REPL sharelink for multiple files

Creates a webR sharelink for projects with multiple R files, data files,
or other resources.

## Usage

``` r
webr_repl_project(
  input,
  autorun_files = character(0),
  base_path = "/home/web_user/",
  panels = NULL,
  version = "latest",
  base_url = NULL
)
```

## Arguments

- input:

  Input for multiple files. Can be:

  - Named list of braced expressions, so each file is written as R
    rather than as a string full of escaped newlines:
    `list("main.R" = { plot(1:10) }, "utils.R" = { f <- function() 42 })`

  - Named list of strings: `list("main.R" = code1, "utils.R" = code2)`

  - Vector of file paths: `c("main.R", "utils.R", "data.csv")`

  The two list forms mix freely, which is what you want for a project
  holding both code and, say, a `README.md`.

- autorun_files:

  Character vector of filenames to auto-execute when project loads, or
  "all" to autorun all R files (default: none)

- base_path:

  Base directory path for all files (default: `"/home/web_user/"`)

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

A `webr_project` object, which is a list with these entries.

- `url`, the sharelink itself, as a single string.

- `files`, the names of the files carried in the link.

- `base_path`, the folder the files are placed in inside webR.

- `mode`, the panels the link asks for, or `NULL` for all of them.

- `version`, the webR version the link points at.

- `autorun_files`, the names you asked to run on opening.

## Details

Every file is placed under `base_path` inside webR, so a
`source("utils.R")` in one file finds its sibling. Names are used
verbatim and may carry a subdirectory, as in `"R/helpers.R"`.

## Writing a project as code

A file's contents can be given as a `{ ... }` block instead of a string,
which spares you escaping every newline and quote:

    webr_repl_project(list(
      "main.R"    = { source("utils.R"); summarise(mtcars) },
      "utils.R"   = { summarise <- function(d) summary(d) },
      "README.md" = "# Analysis"
    ))

The blocks are **never evaluated**. They are source to ship, not code to
run, so an assignment inside one leaves nothing behind in your session.

Two things to know. Comments inside
[`{ }`](https://rdrr.io/r/base/Paren.html) survive in an interactive
session but not in a knitted document (see
[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md)).
And a [`library()`](https://rdrr.io/r/base/library.html) call inside a
block is visible to `R CMD check`, which will report the package as an
undeclared dependency of *yours*. In a vignette or an example, use a
string for code that loads packages.

## See also

[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md)
for the single-file case.

## Examples

``` r
# Each file written as R, rather than as an escaped string
webr_repl_project(list(
  "main.R"  = { source("utils.R"); summarise(mtcars) },
  "utils.R" = { summarise <- function(d) summary(d) }
))
#> 
#> ── webR Project ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJyb1LwkLzE3dVluYmaeXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipKbysX5pUXJqRpKpSWZOcV6QUqaXMWlubmJRZnFqRq5JcmJRcWaEBOXQ1WAjdyGZiRMDmomzAQFG12FtNK85JLM%2FDyNFE0FiESlRoomAD5RQiY%3D&mz>
#> 
#> Files (2):
#> main.R → /home/web_user/main.R
#> utils.R → /home/web_user/utils.R
#> Version: "latest"

# Strings still work, and the two forms mix
files <- list(
  "main.R" = "source('utils.R')\nresult <- analyze_data(mtcars)",
  "utils.R" = "analyze_data <- function(data) { summary(data) }",
  "README.md" = "# My Analysis\nThis project analyzes the mtcars dataset."
)
webr_repl_project(files, autorun_files = "main.R")
#> 
#> ── webR Project ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJxdzz%2BqwkAQBvDeUwy8wqR4yeteYyNoaRPsZYwjWcnuys4sGsVKb%2BAVRPEAir2n8DaSP6JYznzM%2FPj2u4NBTUeNykTJYY6SXeLMaooXNB55Jhc3kdBSHn9svUspaHtROUdJO2w5Yp8LdH4BDebFikYTFAy0pOg4PKEX67y5byvn1NxV0PULemW19Pmt%2FD71JhVlTVAuQlgDe63RFc28qYFz0u%2F2Bv1ITyri9kW80wr5%2F4FBAd1SYsWtYaYY5s7OKJVXGwbJCOo2UFJMEj0B03F4qQ%3D%3D&mza>
#> 
#> Files (3):
#> main.R → /home/web_user/main.R (autorun)
#> utils.R → /home/web_user/utils.R
#> README.md → /home/web_user/README.md
#> Version: "latest"

# Autorun every R file in the project
webr_repl_project(files, autorun_files = "all")
#> 
#> ── webR Project ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJxdzzuOwkAMBuCeU1jagqTYZLtttkFaSpqIHplglEGZGTT2CAKightwBQTiACB6TsFtUB48RGn%2Fsj%2F9283OoKa9RmWiZDdFyU5xZjXFMxoOPJOLm0hoLrcftt6lFLS9qJyjpB22HLHPBf6%2BAQ3mxYIGIxQMtKToODygF%2Bu8udbOobmroPMH9Mhq6f1b%2BX3sTSrKmqBchLAE9lqjK5p59ZTWlXRMup3%2FXjfSo8q6fFivtNJ%2Bv6BXQKckWXGrnymGqbMTSuVRi0EygroWlCaTRHebw30i&mza>
#> 
#> Files (3):
#> main.R → /home/web_user/main.R (autorun)
#> utils.R → /home/web_user/utils.R (autorun)
#> README.md → /home/web_user/README.md
#> Version: "latest"

# File paths input
project_dir <- tempfile()
dir.create(project_dir)
main <- file.path(project_dir, "main.R")
utils <- file.path(project_dir, "utils.R")
writeLines("source('utils.R')", main)
writeLines("# utils", utils)
webr_repl_project(c(main, utils))
#> 
#> ── webR Project ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJyb1LwkLzE3dVluYmaeXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipKNxfmlRcmpGuqlJZk5xXpB6poQ7cuhfLD%2BbWj6YXIgA5YrK4C5AJisMMw%3D&mz>
#> 
#> Files (2):
#> main.R → /home/web_user/main.R
#> utils.R → /home/web_user/utils.R
#> Version: "latest"
```
