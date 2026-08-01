# Create Shinylive sharelinks from a directory of Shiny apps

Batch processes directories containing Shiny applications to create
individual Shinylive links. Each subdirectory is treated as a separate
Shiny app project.

## Usage

``` r
shinylive_directory(
  directory_path,
  engine,
  mode = "editor",
  header = TRUE,
  app_file = NULL,
  base_url = NULL
)
```

## Arguments

- directory_path:

  Character string specifying the path to the directory containing Shiny
  app directories

- engine:

  Engine to use, either "r" for R Shiny or "python" for Python Shiny

- mode:

  Shinylive display mode (default `"editor"`). `"editor"` shows an
  editable code panel beside the running app. `"app"` shows only the
  running app.

- header:

  Logical, whether to show the Shinylive header bar. It applies only
  when `mode = "app"` and is ignored in the default `"editor"` mode.
  Defaults to `TRUE`.

- app_file:

  Main app filename to look for (defaults to "app.R" for R and "app.py"
  for Python)

- base_url:

  Custom Shinylive base URL. If NULL (default), links point at
  https://shinylive.io.

## Value

A `shinylive_directory` object, which is a list with these entries.

- `urls`, one sharelink per app, as a named character vector whose names
  are the app subdirectory names. It is empty when no app was found.

- `engine`, the Shiny flavor the links run, `"r"` or `"python"`.

- `mode`, the Shinylive display mode, `"editor"` or `"app"`.

- `source_directory`, the directory that was read.

Use
[`repl_urls()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/repl_urls.md)
on the object to get the URLs on their own.

## Details

Only text files with extensions .R, .py, .txt, .md, .csv, .json, .yaml,
or .yml are embedded in a link. Other files (for example images or
binary data) are skipped with a warning.

## See also

[`shinylive_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_project.md)
for a single multi-file app.

[`shinylive_r_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_r_link.md)
and
[`shinylive_py_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_py_link.md)
for single apps.

## Examples

``` r
# Each app lives in its own subdirectory:
#   shiny_apps/
#     app1/app.R
#     app2/app.R
shiny_apps <- tempfile()
dir.create(file.path(shiny_apps, "app1"), recursive = TRUE)
dir.create(file.path(shiny_apps, "app2"), recursive = TRUE)
writeLines("library(shiny)", file.path(shiny_apps, "app1", "app.R"))
writeLines("library(shiny)", file.path(shiny_apps, "app2", "app.R"))

links <- shinylive_directory(shiny_apps, engine = "r", mode = "editor")
#> ✔ Found 2 Shiny apps in /tmp/RtmpjOtcTs/file1b5a55ee7959
#> ℹ Processing r Shiny apps...
#> ✔ Successfully created 2 Shinylive links
print(links)
#> 
#> ── Shinylive R Directory ──
#> 
#> Source: /tmp/RtmpjOtcTs/file1b5a55ee7959
#> 
#> Generated 2 apps:
#> app1
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA>
#> 
#> app2
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA>
#> 
#> Engine: "R"
#> Mode: "editor"

# Extract just the URLs
repl_urls(links)
#>                                                                                                                           app1 
#> "https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA" 
#>                                                                                                                           app2 
#> "https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA" 
```
