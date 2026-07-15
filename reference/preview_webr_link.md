# Preview webR REPL link contents without writing files to disk

Decodes a webR URL and returns information about the embedded files
without actually saving them to disk. Use print method options to
control display.

## Usage

``` r
preview_webr_link(url)
```

## Arguments

- url:

  Character string containing the webR URL

## Value

A `webr_preview` object (a list) with elements including `files_data`
(the embedded files), `total_files`, `total_size`, `mode`, `version`,
`flags`, and `autorun_files`. Its
[`print()`](https://rdrr.io/r/base/print.html) method accepts
`show_content` and `max_content_length` to display file bodies.

## See also

[`decode_webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_webr_link.md)
to extract the embedded files to disk once you are happy with the
preview.

## Examples

``` r
url <- as.character(webr_repl_link("plot(1:10)"))

# Inspect a link without writing anything to disk
preview <- preview_webr_link(url)

# Default print (no content)
print(preview)
#> 
#> ── webR Link Preview ──
#> 
#> URL:
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNBUqo0FAJecF%2Fo%3D&jz>
#> 
#> Files: 1
#> Total size: 10 bytes
#> Version: "latest"
#> Encoding: "jz"
#> 
#> script.R (10 bytes)
#> 
#> Use print(preview, show_content = TRUE) to see file contents

# Show file contents
print(preview, show_content = TRUE)
#> 
#> ── webR Link Preview ──
#> 
#> URL:
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNBUqo0FAJecF%2Fo%3D&jz>
#> 
#> Files: 1
#> Total size: 10 bytes
#> Version: "latest"
#> Encoding: "jz"
#> 
#> script.R (10 bytes)
#>   plot(1:10)

# Show file contents with custom length limit
print(preview, show_content = TRUE, max_content_length = 200)
#> 
#> ── webR Link Preview ──
#> 
#> URL:
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNBUqo0FAJecF%2Fo%3D&jz>
#> 
#> Files: 1
#> Total size: 10 bytes
#> Version: "latest"
#> Encoding: "jz"
#> 
#> script.R (10 bytes)
#>   plot(1:10)

# Access the preview data
preview$files_data
#> [[1]]
#> [[1]]$name
#> [1] "script.R"
#> 
#> [[1]]$path
#> [1] "/home/web_user/script.R"
#> 
#> [[1]]$text
#> [1] "plot(1:10)"
#> 
#> 
preview$total_files
#> [1] 1
```
