# Create webR REPL sharelinks from a directory of R files

Batch processes all R files in a directory into webR sharelinks.

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

  Logical. Whether to enable autorun for all generated links. Defaults
  to `FALSE`. With `single_link = TRUE`, this runs every R file in the
  bundle on arrival.

- single_link:

  Logical. If `FALSE` (default), each matched file becomes its own link
  and the result is a `webr_directory`. If `TRUE`, all matched files are
  packed into one link and the result is a single `webr_project`.

- pattern:

  Regular expression matched against file names in `directory_path`.
  Defaults to `"\\.R$"`, i.e. files ending in `.R`.

- base_path:

  Base directory path for files in webR. Defaults to
  `"/home/web_user/"`.

- panels:

  Character vector or string specifying which webR interface panels to
  show. The valid panels are "plot", "files", "terminal", and "editor".
  Can be c("plot", "files") or "plot-files". If NULL (default), shows
  all panels.

- version:

  webR version to use ("latest" or specific version \>= "v0.5.4")

- base_url:

  webR application URL. If NULL, uses global option or builds from
  version

## Value

By default, a `webr_directory` object, which is a list with these
entries.

- `urls`, the sharelinks, as a named character vector with one entry per
  matched file, named by file name.

- `base_path`, where the files are placed inside webR.

- `mode`, the panels the links ask for, or `NULL` for all of them.

- `version`, the webR version the links point at.

- `source_directory`, the directory the files were read from.

With `single_link = TRUE`, a `webr_project` object instead, which is a
list with these entries.

- `url`, the one sharelink that carries every matched file, as a single
  string.

- `files`, the file contents that went into the link, as a named list
  keyed by file name.

- `base_path`, where the files are placed inside webR.

- `mode`, the panels the link asks for, or `NULL` for all of them.

- `version`, the webR version the link points at.

- `autorun_files`, the files that run as soon as the link opens.

Use [`as.character()`](https://rdrr.io/r/base/character.html) on either
object to get the URLs on their own.

## Details

By default each file becomes its own webR sharelink. With
`single_link = TRUE` the whole directory is bundled into one link
instead, exactly as
[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md)
would. Useful for converting collections of scripts, examples, or course
materials.

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
#> ℹ Processing files in /tmp/RtmpsCyH2U/file1a63ef3533b...
#> ✔ Successfully created 2 WebR links
print(links)
#> 
#> ── webR Directory Links ──
#> 
#> Source: /tmp/RtmpsCyH2U/file1a63ef3533b
#> 
#> Generated 2 links:
#> hist.R → /home/web_user/hist.R
#> <https://webr.r-wasm.org/latest/#code=eJyb2LIkLzE3dVlGZnGJXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipINILZGUV5%2BUa6GoYGBpubyxNKS%2FKLSvMMAWH8dsw%3D%3D&mza>
#> 
#> plot.R → /home/web_user/plot.R
#> <https://webr.r-wasm.org/latest/#code=eJyb2LIkLzE3dVlBTn6JXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipJVILaGoZWhgebyxNKS%2FKLSvMMArjAbfg%3D%3D&mza>
#> Version: "latest"

# Bundle the whole directory into one link instead
webr_repl_directory(examples, single_link = TRUE, panels = c("editor", "plot"))
#> ✔ Bundling 2 files into one link
#> 
#> ── webR Project ──
#> 
#> <https://webr.r-wasm.org/latest/?mode='editor-plot'#code=eJyb1LwkLzE3dVlGZnGJXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipINILZGUV5%2BUa6GoYGBpiZUd0FOPi7dUCmQ7lUgtoahlaGBJgCNSy%2Bu&mz>
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
#> ℹ Processing files in /tmp/RtmpsCyH2U/file1a63ef3533b...
#> ✔ Successfully created 2 WebR links
#> 
#> ── webR Directory Links ──
#> 
#> Source: /tmp/RtmpsCyH2U/file1a63ef3533b
#> 
#> Generated 2 links:
#> hist.R → /home/web_user/hist.R
#> <https://webr.r-wasm.org/latest/?mode='editor-terminal'#code=eJyb2LwkLzE3dVlGZnGJXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipINILZGUV5%2BUa6GoYGBpiYAX1wZOg%3D%3D&mz>
#> 
#> plot.R → /home/web_user/plot.R
#> <https://webr.r-wasm.org/latest/?mode='editor-terminal'#code=eJyb2LwkLzE3dVlBTn6JXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipJVILaGoZWhgSYAyPAXBQ%3D%3D&mz>
#> 
#> Interface: "Editor" and "Terminal"
#> Version: "latest"

# Match a subset of files
webr_repl_directory(examples, pattern = "^plot")
#> ✔ Found 1 file matching pattern "^plot"
#> ℹ Processing files in /tmp/RtmpsCyH2U/file1a63ef3533b...
#> ✔ Successfully created 1 WebR link
#> 
#> ── webR Directory Links ──
#> 
#> Source: /tmp/RtmpsCyH2U/file1a63ef3533b
#> 
#> Generated 1 link:
#> plot.R → /home/web_user/plot.R
#> <https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dVlBTn6JXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipJVILaGoZWhgSYAyPAXBQ%3D%3D&mz>
#> Version: "latest"

# The URLs, named by file
repl_urls(links)
#>                                                                                                                                                  hist.R 
#> "https://webr.r-wasm.org/latest/#code=eJyb2LIkLzE3dVlGZnGJXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipINILZGUV5%2BUa6GoYGBpubyxNKS%2FKLSvMMAWH8dsw%3D%3D&mza" 
#>                                                                                                                                                  plot.R 
#>           "https://webr.r-wasm.org/latest/#code=eJyb2LIkLzE3dVlBTn6JXtCSgsSSjK36Gfm5qfrlqUnxpcWpRfpQqZLUipJVILaGoZWhgebyxNKS%2FKLSvMMArjAbfg%3D%3D&mza" 
```
