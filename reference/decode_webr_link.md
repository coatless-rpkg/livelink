# Decode webR REPL link(s) to extract files to local directory

Decodes webR REPL sharelinks to extract the embedded files and save them
to a local directory. This is the reverse operation of creating webR
links. Handles both single URLs and multiple URLs automatically.

## Usage

``` r
decode_webr_link(
  url,
  output_dir = file.path(tempdir(), "webr_files"),
  overwrite = FALSE,
  create_subdir = TRUE,
  name_dirs = TRUE
)
```

## Arguments

- url:

  Character string or vector containing webR REPL URL(s)

- output_dir:

  Character string specifying the output directory path. Defaults to a
  `webr_files` directory inside the session temporary directory; pass an
  explicit path to extract somewhere permanent.

- overwrite:

  Logical. Whether to overwrite existing files (default: FALSE)

- create_subdir:

  Logical. If `TRUE` (default), each decoded link is extracted into its
  own subdirectory under `output_dir` rather than directly into it. For
  a single URL the subdirectory is named `webr_<hash>`, where `<hash>`
  is a short fingerprint of the URL. For multiple URLs, see `name_dirs`.
  Set `FALSE` to extract straight into `output_dir`.

- name_dirs:

  Logical. For multiple URLs, controls how the per-link subdirectories
  are named: `TRUE` (default) numbers them `script_01`, `script_02`,
  ...; `FALSE` names each one `webr_<hash>` from the URL fingerprint.
  Ignored for a single URL, and ignored when `create_subdir = FALSE`
  (all files then extract into `output_dir`).

## Value

For a single URL, a `webr_decoded` object. For multiple URLs, a
`webr_decoded_batch` object.

## See also

[`preview_webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/preview_webr_link.md)
to inspect a link without writing files, and
[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md),
[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md),
and
[`webr_repl_exercise()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_exercise.md),
which create the links this function decodes.

## Examples

``` r
# Round-trip: build a link, then decode it back to files
url <- as.character(webr_repl_link("plot(1:10)"))

result <- decode_webr_link(url)
#> Decompressing webR data...
#> Parsing file data...
#> Created directory: /tmp/RtmpDD4y8T/webr_files/webr_6376df5f
#> Decoding 1 file...
#> script.R (10 bytes)
#> ✔ Successfully decoded 1 file to /tmp/RtmpDD4y8T/webr_files/webr_6376df5f
print(result)
#> 
#> ── webR Decoded Files ──
#> 
#> Source:
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNBUqo0FAJecF%2Fo%3D&jz>
#> Output: /tmp/RtmpDD4y8T/webr_files/webr_6376df5f
#> 
#> Files (1):
#> script.R (10 bytes)
#> 
#> Version: "latest"
#> Encoding: "jz"

# Extract to a directory of your choosing
out <- file.path(tempdir(), "my_code")
decode_webr_link(url, output_dir = out, create_subdir = FALSE, overwrite = TRUE)
#> Decompressing webR data...
#> Parsing file data...
#> Created directory: /tmp/RtmpDD4y8T/my_code
#> Decoding 1 file...
#> script.R (10 bytes)
#> ✔ Successfully decoded 1 file to /tmp/RtmpDD4y8T/my_code
#> 
#> ── webR Decoded Files ──
#> 
#> Source:
#> <https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKk4uyiwo0QtS0lEqSCzJAIroZ%2BTnpuqXpybFlxanFukjKShJrSgBKijIyS%2FRMLQyNNBUqo0FAJecF%2Fo%3D&jz>
#> Output: /tmp/RtmpDD4y8T/my_code
#> 
#> Files (1):
#> script.R (10 bytes)
#> 
#> Version: "latest"
#> Encoding: "jz"
list.files(out)
#> [1] "script.R"

# Several links at once
urls <- c(url, as.character(webr_repl_link("hist(rnorm(100))")))
decode_webr_link(urls, output_dir = file.path(tempdir(), "my_scripts"))
#> Processing 2 webR URLs...
#> 
#> 
#> ── Processing URL 1/2: script_01 
#> Decompressing webR data...
#> Parsing file data...
#> Created directory: /tmp/RtmpDD4y8T/my_scripts/script_01
#> Decoding 1 file...
#> script.R (10 bytes)
#> ✔ Successfully decoded 1 file to /tmp/RtmpDD4y8T/my_scripts/script_01
#> 
#> 
#> ── Processing URL 2/2: script_02 
#> Decompressing webR data...
#> Parsing file data...
#> Created directory: /tmp/RtmpDD4y8T/my_scripts/script_02
#> Decoding 1 file...
#> script.R (16 bytes)
#> ✔ Successfully decoded 1 file to /tmp/RtmpDD4y8T/my_scripts/script_02
#> 
#> ✔ Successfully processed 2/2 URLs
#> 
#> ── webR Decoded Batch ──
#> 
#> Base directory: /tmp/RtmpDD4y8T/my_scripts
#> Total URLs: 2
#> 
#> Successfully processed 2 URLs:
#> script_01: 1 file
#> /tmp/RtmpDD4y8T/my_scripts/script_01
#> 
#> script_02: 1 file
#> /tmp/RtmpDD4y8T/my_scripts/script_02
#> 
#> Summary: 2 total files saved across 2 URLs
```
