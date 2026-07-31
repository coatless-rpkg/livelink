# Create webR REPL sharelink for multiple files

Creates a webR sharelink for projects with multiple R files, data files,
or other resources. Supports named lists and file path vectors as input.

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
  show. Valid panels: `"plot"`, `"files"`, `"terminal"`, `"editor"`. Can
  be `c("plot", "files")` or `"plot-files"`. If NULL (default), shows
  all panels.

- version:

  webR version to use (`"latest"` or specific version \>= "v0.5.4")

- base_url:

  webR application URL. If NULL, uses global option or builds from
  version

## Value

webr_project object containing the webR sharelink for the multi-file
project

## Writing a project as code

A file's contents can be given as a `{ ... }` block instead of a string,
which spares you escaping every newline and quote:

    webr_repl_project(list(
      "main.R"    = { source("utils.R"); summarise(mtcars) },
      "utils.R"   = { summarise <- function(d) summary(d) },
      "README.md" = "# Analysis"
    ))

The blocks are **never evaluated** – they are source to ship, not code
to run – so an assignment inside one leaves nothing behind in your
session.

Two things to know. Comments inside
[`{ }`](https://rdrr.io/r/base/Paren.html) survive in an interactive
session but not in a knitted document (see
[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md)).
And a [`library()`](https://rdrr.io/r/base/library.html) call inside a
block is visible to `R CMD check`, which will report the package as an
undeclared dependency of *yours*; in a vignette or an example, use a
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
#> <https://webr.r-wasm.org/latest/#code=eJx1jcEKwjAQRH8l7CmB2tzFr%2BjViMS40oCbSHaDivTfDWjbU28zvGHe8QPJE8IeyMfUD9DBw8vYuh0zoX3i5VwZi12w4Esa5lxLQO2gSrxzPzgwLnEl8iUyapLgCxuYukXwH24aVj4r5jd12KlbTUFiTvpq1A%2B8W4Tp9AW8VkGO&jz>
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
#> <https://webr.r-wasm.org/latest/#code=eJx1z8GKwkAMBuBXCbMHW3Dbu%2BxFWI9eijcViTXSkc6MTBK0St%2FdFqv24jH5k3xkfTceHZmZcWh9VpipOaNUXZ1XwVF%2Bof1OmWL%2BjoWu0sUcNJaUTFRszVkxSTc%2BEmst8PcL6LFubrQ7oGDipMTIabeKKiGqNzOJSu30TQ83vtqffMDH93vvqL4UG3zSN1K4A6tzGJuhbs0IKxbz%2F%2BUic4ev3HhiAH9g2cC8V9nyxq8qy3CO4USlvJ5lkIrg%2BSz0LpNkpt0%2BAJkud98%3D&jza>
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
#> <https://webr.r-wasm.org/latest/#code=eJx9j72OwkAMhF%2FF2itIJC7p0TVIR0kT0QFCJhhlUXYXrW1BQHl3EhF%2BGijtseebWV6NR0dmYhxanxVmbI4oVTfnVXCUn2i7UaaYP2Whs3QyB40lJSMVW3NWjNKVj8RaC%2Fz9AnqsmwttdiiYOCkxctq9okqI6s1EolI7fqIHj4%2Fslz7A3%2F173l59KTb4pF%2BkcAVW5zA2w9x%2BYRez6f98lrndR%2Fr7xcD%2FgXkD0z4EW175RWUZjjEcqJRHdwapCO7doY%2FBJJlp1zf%2BYX1X&jza>
#> 
#> Files (3):
#> main.R → /home/web_user/main.R
#> utils.R → /home/web_user/utils.R
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
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSyk3MzNMLUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlQOni%2FNKi5FQN9dKSzJxivSB1TaVaHbhZUEGchiHkoaYpK4CFlGpjAXHGL1w%3D&jz>
#> 
#> Files (2):
#> main.R → /home/web_user/main.R
#> utils.R → /home/web_user/utils.R
#> Version: "latest"
```
