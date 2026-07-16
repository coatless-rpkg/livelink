# Extract shareable URLs from livelink objects

Generic function to extract the shareable URL(s) from any livelink
object, covering both webR REPL and Shinylive results. Provides a clear
way to get just the URLs for sharing or further processing.

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

A character vector of URLs. Most objects yield a single URL; exercise
objects return a length-2 named vector (`exercise`, `solution`);
directory and batch objects return one URL per file.

## See also

The [`as.character()`](https://rdrr.io/r/base/character.html) methods
(for example
[`as.character.webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)),
which `repl_urls()` delegates to.

## Examples

``` r
# Single link
link <- webr_repl_link("plot(1:10)")
repl_urls(link)
#> [1] "https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNBUqo0FAJecF%2Fo%3D&jz"

# Exercise (returns named vector)
exercise <- webr_repl_exercise("# TODO", "plot(1:10)", "test")
repl_urls(exercise)
#>                                                                                                                                                                        exercise 
#>                              "https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKkktLolPrUgtSs4sTtULUtJRKkgsyQBK6Gfk56bql6cmxZcWpxbpY6orSa0oAapTVgjxd%2FFXqo0FANZtHFs%3D&jz" 
#>                                                                                                                                                                        solution 
#> "https://webr.r-wasm.org/latest/#code=eJxlykEKgCAQRuG7zKogsrYeo21EGAwYqCP6DwXR3XPf9r1vfSi5yGQJXLFXCYpT0rjQQNnBt2G8RDYXH7tWLubvwDeay0HQzXae%2BtacQoomsijK7%2FYBSMojgA%3D%3D&jza" 

# Shinylive links work the same way
repl_urls(shinylive_r_link("library(shiny)"))
#> [1] "https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA"

# Decoded files (returns the original URL)
decoded <- decode_webr_link(as.character(link))
#> Decompressing webR data...
#> Parsing file data...
#> Decoding 1 file...
#> Warning: File already exists, skipping: script.R
#> ✔ Successfully decoded 0 files to /tmp/RtmpvYl1I5/webr_files/webr_6376df5f
repl_urls(decoded)
#> [1] "https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNBUqo0FAJecF%2Fo%3D&jz"
```
