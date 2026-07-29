# Print method for webr_directory objects

Displays every sharelink generated from a source directory.

## Usage

``` r
# S3 method for class 'webr_directory'
print(x, ...)
```

## Arguments

- x:

  webr_directory object

- ...:

  Additional arguments (ignored)

## Value

The `webr_directory` object it was handed, returned invisibly, so it can
be passed straight on. Called for the summary it prints, which covers
the source folder, one sharelink per file, the interface, and the
version. See
[`webr_repl_directory()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_directory.md)
for the entries the object holds.
