# Print method for webr_link objects

Displays the sharelink and its details in the console.

## Usage

``` r
# S3 method for class 'webr_link'
print(x, ...)
```

## Arguments

- x:

  webr_link object

- ...:

  Additional arguments (ignored)

## Value

The `webr_link` object it was handed, returned invisibly, so it can be
passed straight on. Called for the summary it prints, which covers the
URL, the file and where it lands, the interface, the version, and
autorun. See
[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md)
for the entries the object holds.
