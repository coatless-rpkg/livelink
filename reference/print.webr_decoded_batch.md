# Print method for webr_decoded_batch objects

Displays a summary of the webR links decoded in a batch.

## Usage

``` r
# S3 method for class 'webr_decoded_batch'
print(x, ...)
```

## Arguments

- x:

  webr_decoded_batch object

- ...:

  Additional arguments (ignored)

## Value

The `webr_decoded_batch` object it was handed, returned invisibly, so it
can be passed straight on. Called for the summary it prints, which
covers the base folder, each link that decoded with its file count and
output folder, and a count of the links that failed. See
[`decode_webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_webr_link.md)
for the entries the object holds.
