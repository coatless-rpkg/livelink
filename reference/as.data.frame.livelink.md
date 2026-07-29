# Turn a livelink container into a data frame

The container classes convert to a tidy data frame.

## Usage

``` r
# S3 method for class 'webr_directory'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'shinylive_directory'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'webr_decoded_batch'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'shinylive_decoded_batch'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `webr_directory`, `shinylive_directory`, `webr_decoded_batch`, or
  `shinylive_decoded_batch` object.

- row.names:

  A character vector of row names, or `NULL`.

- optional:

  Ignored. It is here so the arguments match those of
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Ignored.

## Value

A data frame.

- A directory gives one row for each generated link, with the columns
  `filename` and `url`.

- A decoded batch gives one row for each URL that decoded successfully,
  with the columns `name`, `url`, `total_files`, `total_size`, and
  `output_dir`. A URL that failed to decode carries no result and is
  left out. The counts are held in the object's `total_urls` and
  `successful_urls` fields.

## Details

A folder of links or a batch of decoded results can be tabulated,
filtered, joined, or written to CSV with the tools you already use. For
a tibble, wrap the result with `tibble::as_tibble(as.data.frame(x))`.

## See also

[`webr_repl_directory()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_directory.md)
for the webR directory objects tabulated here.

[`shinylive_directory()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_directory.md)
for the Shinylive directory objects tabulated here.

[`decode_webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_webr_link.md)
and
[`decode_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_shinylive_link.md)
for the decoded batch objects tabulated here.

## Examples

``` r
dir <- tempfile()
dir.create(dir)
writeLines("plot(1:10)",       file.path(dir, "one.R"))
writeLines("hist(rnorm(100))", file.path(dir, "two.R"))

links <- webr_repl_directory(dir)
#> ✔ Found 2 files matching pattern "\\.R$"
#> ℹ Processing files in /tmp/Rtmp04svWU/file1a4745adbe6a...
#> ✔ Successfully created 2 WebR links
as.data.frame(links)
#>   filename
#> 1    one.R
#> 2    two.R
#>                                                                                                                                          url
#> 1         https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dWl%2BXqpe0JKCxJKMLfoZ%2Bbmp%2BuWpSfGlxalF%2BhCZktSKklUFOfklGoZWhgaaAJPuFgk%3D&mz
#> 2 https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dWlJeb5e0JKCxJKMLfoZ%2Bbmp%2BuWpSfGlxalF%2BhCZktSKkg0ZmcUlGkV5%2BUW5GoYGBpqaAC4HGHw%3D&mz
```
