# Create a Shinylive sharelink for multi-file projects

Creates Shinylive projects for either R or Python from named lists or
file path vectors.

## Usage

``` r
shinylive_project(
  input,
  engine,
  mode = "editor",
  header = TRUE,
  base_url = NULL
)
```

## Arguments

- input:

  Input for multiple files. Can be:

  - Named list: `list("app.R" = code1, "utils.R" = code2)`

  - Vector of file paths: `c("app.R", "utils.R", "data.csv")`

- engine:

  Engine to use: "r" for R Shiny or "python" for Python Shiny

- mode:

  Shinylive display mode (default `"editor"`). `"editor"` shows an
  editable code panel beside the running app; `"app"` shows only the
  running app.

- header:

  Logical, whether to show the Shinylive header bar. It applies only
  when `mode = "app"` and is ignored in the default `"editor"` mode.
  Defaults to `TRUE`.

- base_url:

  Custom Shinylive base URL. If NULL (default), links point at
  https://shinylive.io.

## Value

shinylive_project object containing the Shinylive URL and metadata

## See also

[`shinylive_r_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_r_link.md)
and
[`shinylive_py_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_py_link.md)
for single apps;
[`decode_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_shinylive_link.md)
and
[`preview_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/preview_shinylive_link.md)
to read a link back.

## Examples

``` r
# Named list input
files <- list(
  "app.R" = "library(shiny)\nshinyApp(fluidPage(), function(i, o) {})",
  "utils.R" = "# Utility functions"
)
shinylive_project(files, engine = "r", mode = "editor")
#> 
#> ── Shinylive R Project ──
#> 
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlAB0IPPqwCC6dgDNqAV1oATAApQA5nHYDcAAhnyIBUrRLtaeogN0gAvgLxhSrVAmTkAHqTC3c4aPBU8ibUnNiOxGQU3sgAxLoAqiG0zvqGxqaijs6uVJ7etgC6QA>
#> 
#> Files (2):
#> app.R
#> utils.R
#> 
#> Engine: "R"
#> Mode: "editor"

# File paths input
project_dir <- tempfile()
dir.create(project_dir)
app <- file.path(project_dir, "app.R")
utils <- file.path(project_dir, "utils.R")
writeLines("library(shiny)", app)
writeLines("# utils", utils)
shinylive_project(c(app, utils), engine = "r")
#> 
#> ── Shinylive R Project ──
#> 
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+ucNHhUArqVrVO2YcTIVZyAMQACdZs7DR4qtNlyAukA>
#> 
#> Files (2):
#> app.R
#> utils.R
#> 
#> Engine: "R"
#> Mode: "editor"
```
