# Extract URLs as character vector

Extract URLs as character vector

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

A character vector of URLs. Most objects yield a single URL; exercise
objects return a length-2 named vector (`exercise`, `solution`);
directory and batch objects return one URL per file.

## See also

[`repl_urls()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/repl_urls.md)
for the same extraction as a named generic you can call explicitly.
