# Print method for webr_decoded objects

Displays the files recovered from a webR link and where they were
written.

## Usage

``` r
# S3 method for class 'webr_decoded'
print(x, ...)
```

## Arguments

- x:

  webr_decoded object

- ...:

  Additional arguments (ignored)

## Value

The `webr_decoded` object it was handed, returned invisibly, so it can
be passed straight on. Called for the summary it prints, which covers
the link, the output folder, each file with its size, and any file that
was skipped along with the reason. See
[`decode_webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_webr_link.md)
for the entries the object holds.
