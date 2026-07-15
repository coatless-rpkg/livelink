# Create webR REPL sharelinks from a directory of R files

Batch processes all R files in a directory. By default each file becomes
its own webR sharelink; with `single_link = TRUE` the whole directory is
bundled into one link instead, exactly as
[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md)
would. Useful for converting collections of scripts, examples, or course
materials.

## Usage

``` r
webr_repl_directory(
  directory_path,
  autorun = FALSE,
  single_link = FALSE,
  pattern = "\\.R$",
  base_path = "/home/web_user/",
  panels = NULL,
  version = "latest",
  base_url = NULL
)
```

## Arguments

- directory_path:

  Character string specifying the path to the directory containing R
  files

- autorun:

  Logical. Whether to enable autorun for all generated links (default:
  FALSE). With `single_link = TRUE`, this runs every R file in the
  bundle on arrival.

- single_link:

  Logical. If `FALSE` (default), each matched file becomes its own link
  and the result is a `webr_directory`. If `TRUE`, all matched files are
  packed into one link and the result is a single `webr_project`.

- pattern:

  Regular expression matched against file names in `directory_path`.
  Defaults to `"\\.R$"`, i.e. files ending in `.R`.

- base_path:

  Base directory path for files in webR (default: "/home/web_user/")

- panels:

  Character vector or string specifying which webR interface panels to
  show. Valid panels: "plot", "files", "terminal", "editor". Can be
  c("plot", "files") or "plot-files". If NULL (default), shows all
  panels.

- version:

  webR version to use ("latest" or specific version \>= "v0.5.4")

- base_url:

  webR application URL. If NULL, uses global option or builds from
  version

## Value

By default a `webr_directory` object, whose `urls` element is a named
character vector mapping each filename to its webR sharelink. With
`single_link = TRUE`, a single `webr_project` object bundling every
matched file into one link.

## See also

[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md),
which bundles a named list or a vector of file paths into one link.

## Examples

``` r
# A directory of R scripts
examples <- tempfile()
dir.create(examples)
writeLines("plot(1:10)", file.path(examples, "plot.R"))
writeLines("hist(rnorm(100))", file.path(examples, "hist.R"))

links <- webr_repl_directory(examples, autorun = TRUE)
#> ✔ Found 2 files matching pattern "\\.R$"
#> ℹ Processing files in /tmp/RtmpeoCUsF/file1b27311d2408...
#> ✔ Successfully created 2 WebR links
print(links)
#> 
#> ── webR Directory Links ──
#> 
#> Source: /tmp/RtmpeoCUsF/file1b27311d2408
#> 
#> Generated 2 links:
#> hist.R → /home/web_user/hist.R
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSysgsLtELUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlUOUaRXn5RbkahgYGmppAmcTSkvyi0jwlq5Ki0tTaWABQkR31&jza>
#> 
#> plot.R → /home/web_user/plot.R
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKsjJL9ELUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlUOUahlaGBppAscTSkvyi0jwlq5Ki0tTaWACdwxvG&jza>
#> Version: "latest"

# Bundle the whole directory into one link instead
webr_repl_directory(examples, single_link = TRUE, panels = c("editor", "plot"))
#> ✔ Bundling 2 files into one link
#> 
#> ── webR Project ──
#> 
#> <https://webr.r-wasm.org/latest/?mode='editor-plot'#code=eJyLrlbKS8xNVbJSysgsLtELUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlUOUaRXn5RbkahgYGmppKtTpwowpy8vEZBZeGGgXiaxhaGRpoKtXGAgBVBi4%2B&jz>
#> 
#> Files (2):
#> hist.R → /home/web_user/hist.R
#> plot.R → /home/web_user/plot.R
#> 
#> Interface: "Editor" and "Plot"
#> Version: "latest"

# Show only the editor and terminal panels
webr_repl_directory(examples, panels = c("editor", "terminal"))
#> ✔ Found 2 files matching pattern "\\.R$"
#> ℹ Processing files in /tmp/RtmpeoCUsF/file1b27311d2408...
#> ✔ Successfully created 2 WebR links
#> 
#> ── webR Directory Links ──
#> 
#> Source: /tmp/RtmpeoCUsF/file1b27311d2408
#> 
#> Generated 2 links:
#> hist.R → /home/web_user/hist.R
#> <https://webr.r-wasm.org/latest/?mode='editor-terminal'#code=eJyLrlbKS8xNVbJSysgsLtELUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlUOUaRXn5RbkahgYGmppKtbEAubwYfQ%3D%3D&jz>
#> 
#> plot.R → /home/web_user/plot.R
#> <https://webr.r-wasm.org/latest/?mode='editor-terminal'#code=eJyLrlbKS8xNVbJSKsjJL9ELUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlUOUahlaGBppKtbEAJ74WTg%3D%3D&jz>
#> 
#> Interface: "Editor" and "Terminal"
#> Version: "latest"

# Match a subset of files
webr_repl_directory(examples, pattern = "^plot")
#> ✔ Found 1 file matching pattern "^plot"
#> ℹ Processing files in /tmp/RtmpeoCUsF/file1b27311d2408...
#> ✔ Successfully created 1 WebR link
#> 
#> ── webR Directory Links ──
#> 
#> Source: /tmp/RtmpeoCUsF/file1b27311d2408
#> 
#> Generated 1 link:
#> plot.R → /home/web_user/plot.R
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKsjJL9ELUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlUOUahlaGBppKtbEAJ74WTg%3D%3D&jz>
#> Version: "latest"

# The URLs, named by file
repl_urls(links)
#>                                                                                                                                                      hist.R 
#> "https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSysgsLtELUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlUOUaRXn5RbkahgYGmppAmcTSkvyi0jwlq5Ki0tTaWABQkR31&jza" 
#>                                                                                                                                                      plot.R 
#>         "https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKsjJL9ELUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlUOUahlaGBppAscTSkvyi0jwlq5Ki0tTaWACdwxvG&jza" 
```
