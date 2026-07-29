# Print method for shinylive_decoded objects

Displays the files recovered from a Shinylive link and where they were
written.

## Usage

``` r
# S3 method for class 'shinylive_decoded'
print(x, ...)
```

## Arguments

- x:

  shinylive_decoded object

- ...:

  Additional arguments (ignored)

## Value

The `shinylive_decoded` object it was handed, returned invisibly, so it
can be passed straight on. Called for the summary it prints, which
covers the link, the output folder, each file with its size, the total
size, and the mode. See
[`decode_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_shinylive_link.md)
for the entries the object holds.
