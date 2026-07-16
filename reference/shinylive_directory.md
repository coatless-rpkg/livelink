# Create Shinylive sharelinks from a directory of Shiny apps

Batch processes directories containing Shiny applications to create
individual Shinylive links. Each subdirectory is treated as a separate
Shiny app project.

Only text files with extensions .R, .py, .txt, .md, .csv, .json, .yaml,
or .yml are embedded in a link. Other files (for example images or
binary data) are skipped with a warning.

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

  Engine to use: "r" for R Shiny or "python" for Python Shiny

- mode:

  Shinylive display mode (default `"editor"`). `"editor"` shows an
  editable code panel beside the running app; `"app"` shows only the
  running app.

- header:

  Logical, whether to show the Shinylive header bar. It applies only
  when `mode = "app"` and is ignored in the default `"editor"` mode.
  Defaults to `TRUE`.

- app_file:

  Main app filename to look for (default: "app.R" for R, "app.py" for
  Python)

- base_url:

  Custom Shinylive base URL. If NULL (default), links point at
  https://shinylive.io.

## Value

shinylive_directory object containing URLs and metadata for all found
apps

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
#> ✔ Found 2 Shiny apps in /tmp/Rtmpf31fJq/file1a5663281c21
#> ℹ Processing r Shiny apps...
#> ✔ Successfully created 2 Shinylive links
print(links)
#> 
#> ── Shinylive R Directory ──
#> 
#> Source: /tmp/Rtmpf31fJq/file1a5663281c21
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
