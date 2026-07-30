# Extract shareable URLs from livelink objects

Extracts the shareable URL(s) from any livelink object, covering both
webR REPL and Shinylive results. Provides a clear way to get just the
URLs for sharing or further work.

## Usage

``` r
repl_urls(x, ...)

# Default S3 method
repl_urls(x, ...)
```

## Arguments

- x:

  A livelink object. Supported classes are `webr_link`, `webr_project`,
  `webr_exercise`, `webr_directory`, `webr_decoded`,
  `webr_decoded_batch`, `webr_preview`, `shinylive_link`,
  `shinylive_project`, `shinylive_directory`, `shinylive_decoded`,
  `shinylive_decoded_batch`, and `shinylive_preview`.

- ...:

  Additional arguments passed to methods

## Value

A character vector of URLs.

- Most objects give a single URL.

- An exercise gives two, named `exercise` and `solution`.

- A directory or a batch gives one URL for each file.

## See also

The [`as.character()`](https://rdrr.io/r/base/character.html) methods
(for example
[`as.character.webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)),
which `repl_urls()` calls to do the work.

## Examples

``` r
# Single link
link <- webr_repl_link("plot(1:10)")
repl_urls(link)
#> [1] "https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dUVxclFmQYle0JKCxJKM7foZ%2Bbmp%2BuWpSfGlxalF%2BnDJktSKklUFOfklGoZWhgaaAC8DGLU%3D&mz"

# Exercise (returns named vector)
exercise <- webr_repl_exercise("# TODO", "plot(1:10)", "test")
repl_urls(exercise)
#>                                                                                                                                                        exercise 
#>                            "https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dX1JanFJfGpFalFyZnGqXtCSgsSSjH36Gfm5qfrlqUnxpcWpRfroakpSK0qWKSuE%2BLv4AwBJ1R0g&mz" 
#>                                                                                                                                                        solution 
#> "https://webr.r-wasm.org/latest/#code=eJyb2LIkLzE3dX1JanFJfHF%2BTmlJZn6eXtCSgsSSjH36Gfm5qfrlqUnxpcWpRfroakpSK0pWFeTkl2gYWhkaaC5PLC3JLyrNOwwA%2FhQjSg%3D%3D&mza" 

# Shinylive links work the same way
repl_urls(shinylive_r_link("library(shiny)"))
#> [1] "https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA"

# Decoded files (returns the original URL)
decoded <- decode_webr_link(as.character(link))
#> Decompressing webR data...
#> Parsing file data...
#> Decoding 1 file...
#> Warning: File already exists, skipping: script.R
#> ✔ Successfully decoded 0 files to /tmp/RtmpSE0idp/webr_files/webr_8430d3f5
repl_urls(decoded)
#> [1] "https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dUVxclFmQYle0JKCxJKM7foZ%2Bbmp%2BuWpSfGlxalF%2BnDJktSKklUFOfklGoZWhgaaAC8DGLU%3D&mz"
```
