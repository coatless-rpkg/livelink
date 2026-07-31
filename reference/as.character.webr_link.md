# Extract URLs as character vector

Pulls the shareable URL(s) out of a livelink object.

## Usage

``` r
# S3 method for class 'webr_link'
as.character(x, ...)

# S3 method for class 'shinylive_link'
as.character(x, ...)

# S3 method for class 'webr_project'
as.character(x, ...)

# S3 method for class 'webr_exercise'
as.character(x, ...)

# S3 method for class 'webr_directory'
as.character(x, ...)

# S3 method for class 'webr_decoded'
as.character(x, ...)

# S3 method for class 'webr_decoded_batch'
as.character(x, ...)

# S3 method for class 'webr_preview'
as.character(x, ...)

# S3 method for class 'shinylive_project'
as.character(x, ...)

# S3 method for class 'shinylive_directory'
as.character(x, ...)

# S3 method for class 'shinylive_decoded'
as.character(x, ...)

# S3 method for class 'shinylive_decoded_batch'
as.character(x, ...)

# S3 method for class 'shinylive_preview'
as.character(x, ...)
```

## Arguments

- x:

  Link object

- ...:

  Additional arguments

## Value

A character vector of URLs.

- Most objects give a single URL.

- An exercise gives two, named `exercise` and `solution`.

- A directory or a batch gives one URL for each file.

## See also

[`repl_urls()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/repl_urls.md)
for the same result from a function you can call by name.
