# Print method for shinylive_decoded_batch objects

Displays a summary of the Shinylive links decoded in a batch.

## Usage

``` r
# S3 method for class 'shinylive_decoded_batch'
print(x, ...)
```

## Arguments

- x:

  shinylive_decoded_batch object

- ...:

  Additional arguments (ignored)

## Value

The `shinylive_decoded_batch` object it was handed, returned invisibly,
so it can be passed straight on. Called for the summary it prints, which
covers the base folder, the output folder and file count for each link
that decoded, and the totals across all of them. See
[`decode_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_shinylive_link.md)
for the entries the object holds.
