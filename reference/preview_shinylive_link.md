# Preview Shinylive link contents without writing files to disk

Decodes a Shinylive URL and returns information about the embedded files
without actually saving them to disk. Use print method options to
control display.

## Usage

``` r
preview_shinylive_link(url)
```

## Arguments

- url:

  Character string containing the Shinylive URL

## Value

shinylive_preview object with file information and metadata

## See also

[`decode_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_shinylive_link.md)
to extract files to disk;
[`shinylive_r_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_r_link.md)
and
[`shinylive_py_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_py_link.md)
to create links.

## Examples

``` r
url <- as.character(shinylive_r_link("library(shiny)"))

# Inspect a link without writing anything to disk
preview <- preview_shinylive_link(url)

# Default print (no content)
print(preview)
#> 
#> ── Shinylive R Preview ──
#> 
#> Source:
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA>
#> 
#> Files: 1
#> Total size: 14 bytes
#> Engine: "R"
#> Mode: "editor"
#> 
#> app.R (text, 14 bytes)
#> 
#> Use print(preview, show_content = TRUE) to see file contents

# Show file contents
print(preview, show_content = TRUE)
#> 
#> ── Shinylive R Preview ──
#> 
#> Source:
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA>
#> 
#> Files: 1
#> Total size: 14 bytes
#> Engine: "R"
#> Mode: "editor"
#> 
#> app.R (text, 14 bytes)
#>   library(shiny)

# Show file contents with custom length limit
print(preview, show_content = TRUE, max_content_length = 200)
#> 
#> ── Shinylive R Preview ──
#> 
#> Source:
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA>
#> 
#> Files: 1
#> Total size: 14 bytes
#> Engine: "R"
#> Mode: "editor"
#> 
#> app.R (text, 14 bytes)
#>   library(shiny)

# Access the preview data
preview$files_data
#> [[1]]
#> [[1]]$name
#> [1] "app.R"
#> 
#> [[1]]$content
#> [1] "library(shiny)"
#> 
#> [[1]]$type
#> [1] "text"
#> 
#> 
preview$total_files
#> [1] 1
```
