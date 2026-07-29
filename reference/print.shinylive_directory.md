# Print method for shinylive_directory objects

Displays every app sharelink generated from a source directory.

## Usage

``` r
# S3 method for class 'shinylive_directory'
print(x, ...)
```

## Arguments

- x:

  shinylive_directory object

- ...:

  Additional arguments (ignored)

## Value

The `shinylive_directory` object it was handed, returned invisibly, so
it can be passed straight on. Called for the summary it prints, which
covers the source folder, one sharelink per app, the engine, and the
mode. See
[`shinylive_directory()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_directory.md)
for the entries the object holds.
