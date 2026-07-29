# Print method for webr_preview objects

Displays what a webR link contains without writing anything to disk.

## Usage

``` r
# S3 method for class 'webr_preview'
print(x, show_content = FALSE, max_content_length = 500, ...)
```

## Arguments

- x:

  webr_preview object

- show_content:

  Logical. Whether to print the contents of each file. Defaults to
  `FALSE`.

- max_content_length:

  Maximum number of characters of content to show per file. Defaults to
  500.

- ...:

  Additional arguments (ignored)

## Value

The `webr_preview` object it was handed, returned invisibly, so it can
be passed straight on. Called for the summary it prints, which covers
the link, the file count and total size, the interface, the version, the
encoding flags, and each file with its size. See
[`preview_webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/preview_webr_link.md)
for the entries the object holds.
