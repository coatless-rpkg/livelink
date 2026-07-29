# Print method for shinylive_link objects

Displays the app sharelink and the files it carries.

## Usage

``` r
# S3 method for class 'shinylive_link'
print(x, ...)
```

## Arguments

- x:

  shinylive_link object

- ...:

  Additional arguments (ignored)

## Value

The `shinylive_link` object it was handed, returned invisibly, so it can
be passed straight on. Called for the summary it prints, which covers
the URL, the files the app is made of, the engine, and the mode. See
[`shinylive_r_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_r_link.md)
for the entries the object holds.
