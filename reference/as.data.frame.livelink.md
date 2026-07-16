# Turn a livelink container into a data frame

The container classes coerce to a tidy data frame, so a folder of links
or a batch of decoded results can be tabulated, filtered, joined, or
written to CSV with the tools you already use. For a tibble, wrap the
result: `tibble::as_tibble(as.data.frame(x))`.

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

  Ignored; present for compatibility with the generic.

- ...:

  Ignored.

## Value

A data frame. For a directory, one row per generated link with columns
`filename` and `url`. For a decoded batch, one row per successfully
decoded URL with columns `name`, `url`, `total_files`, `total_size`, and
`output_dir` (URLs that failed to decode carry no result and are
omitted; the object's `total_urls` and `successful_urls` fields hold the
counts).

## Examples

``` r
dir <- tempfile()
dir.create(dir)
writeLines("plot(1:10)",       file.path(dir, "one.R"))
writeLines("hist(rnorm(100))", file.path(dir, "two.R"))

links <- webr_repl_directory(dir)
#> ✔ Found 2 files matching pattern "\\.R$"
#> ℹ Processing files in /tmp/RtmpIrzLvn/file1a52b5cb589...
#> ✔ Successfully created 2 WebR links
as.data.frame(links)
#>   filename
#> 1    one.R
#> 2    two.R
#>                                                                                                                                                      url
#> 1       https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSys9L1QtS0lEqSCzJAHL1M%2FJzU%2FXLU5PiS4tTi%2FRhsiWpFSVA2YKc%2FBINQytDA02l2lgA7HMVVA%3D%3D&jz
#> 2 https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSKinP1wtS0lEqSCzJAHL1M%2FJzU%2FXLU5PiS4tTi%2FRhsiWpFSVA2YzM4hKNorz8olwNQwMDTU2l2lgAhGcXwQ%3D%3D&jz
```
