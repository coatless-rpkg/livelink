# Decode Shinylive link(s) to extract files to local directory

Decodes Shinylive sharelinks to extract the embedded files and save them
to a local directory. This is the reverse operation of creating
Shinylive links. Handles both single URLs and multiple URLs
automatically.

## Usage

``` r
decode_shinylive_link(
  url,
  output_dir = file.path(tempdir(), "shinylive_files"),
  overwrite = FALSE,
  create_subdir = TRUE,
  name_dirs = TRUE
)
```

## Arguments

- url:

  Character string or vector containing Shinylive URL(s)

- output_dir:

  Character string specifying the output directory path. Defaults to a
  `shinylive_files` directory inside the session temporary directory;
  pass an explicit path to extract somewhere permanent.

- overwrite:

  Logical. Whether to overwrite existing files (default: FALSE)

- create_subdir:

  Logical. If `TRUE` (default), each decoded link is extracted into its
  own subdirectory under `output_dir` rather than directly into it. For
  a single URL the subdirectory is named `shinylive_<hash>`, where
  `<hash>` is a short fingerprint of the URL. For multiple URLs, see
  `name_dirs`. Set `FALSE` to extract straight into `output_dir`; with
  multiple URLs this means identically named files (such as `app.R`)
  collide, so a later app overwrites an earlier one when
  `overwrite = TRUE`, or is skipped when `overwrite = FALSE`.

- name_dirs:

  Logical. For multiple URLs, controls how the per-link subdirectories
  are named: `TRUE` (default) numbers them `app_01`, `app_02`, ...;
  `FALSE` names each one `shinylive_<hash>` from the URL fingerprint.
  Ignored for a single URL, and ignored when `create_subdir = FALSE`.

## Value

For a single URL, a `shinylive_decoded` object. For multiple URLs, a
`shinylive_decoded_batch` object.

## See also

[`preview_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/preview_shinylive_link.md)
to inspect a link without writing files;
[`shinylive_r_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_r_link.md)
and
[`shinylive_py_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_py_link.md)
to create links.

## Examples

``` r
# Round-trip: build a link, then decode it back to files
url <- as.character(shinylive_r_link("library(shiny)"))

result <- decode_shinylive_link(url)
#> Decompressing Shinylive data...
#> Parsing file data...
#> Created directory: /tmp/RtmpeoCUsF/shinylive_files/shinylive_e2010f1a
#> Decoding 1 file...
#> app.R (text, 14 bytes)
#> ✔ Successfully decoded 1 file to
#>   /tmp/RtmpeoCUsF/shinylive_files/shinylive_e2010f1a
print(result)
#> 
#> ── Shinylive R Decoded Files ──
#> 
#> Source:
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA>
#> Output: /tmp/RtmpeoCUsF/shinylive_files/shinylive_e2010f1a
#> 
#> Files (1):
#> app.R (14 bytes)
#> 
#> Total size: 14 bytes
#> Mode: "editor"

# Extract to a directory of your choosing
out <- file.path(tempdir(), "my_app")
decode_shinylive_link(url, output_dir = out, create_subdir = FALSE, overwrite = TRUE)
#> Decompressing Shinylive data...
#> Parsing file data...
#> Created directory: /tmp/RtmpeoCUsF/my_app
#> Decoding 1 file...
#> app.R (text, 14 bytes)
#> ✔ Successfully decoded 1 file to /tmp/RtmpeoCUsF/my_app
#> 
#> ── Shinylive R Decoded Files ──
#> 
#> Source:
#> <https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlHjClWqBMnIAPUmAC+AXSA>
#> Output: /tmp/RtmpeoCUsF/my_app
#> 
#> Files (1):
#> app.R (14 bytes)
#> 
#> Total size: 14 bytes
#> Mode: "editor"
list.files(out)
#> [1] "app.R"

# Both engines at once
urls <- c(url, as.character(shinylive_py_link("from shiny import App")))
decode_shinylive_link(urls, output_dir = file.path(tempdir(), "my_apps"))
#> Processing 2 Shinylive URLs...
#> 
#> 
#> ── Processing URL 1/2: app_01 
#> Decompressing Shinylive data...
#> Parsing file data...
#> Created directory: /tmp/RtmpeoCUsF/my_apps/app_01
#> Decoding 1 file...
#> app.R (text, 14 bytes)
#> ✔ Successfully decoded 1 file to /tmp/RtmpeoCUsF/my_apps/app_01
#> 
#> 
#> ── Processing URL 2/2: app_02 
#> Decompressing Shinylive data...
#> Parsing file data...
#> Created directory: /tmp/RtmpeoCUsF/my_apps/app_02
#> Decoding 1 file...
#> app.py (text, 21 bytes)
#> ✔ Successfully decoded 1 file to /tmp/RtmpeoCUsF/my_apps/app_02
#> 
#> ✔ Successfully processed 2/2 URLs
#> 
#> ── Shinylive Decoded Batch ──
#> 
#> Base directory: /tmp/RtmpeoCUsF/my_apps
#> Total URLs: 2
#> 
#> Successfully processed 2 URLs:
#> /tmp/RtmpeoCUsF/my_apps/app_01 (1 file)
#> /tmp/RtmpeoCUsF/my_apps/app_02 (1 file)
#> 
#> Total files: 2
#> Total size: 35 bytes
```
