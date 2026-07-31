# Print method for shinylive_preview objects

Displays what a Shinylive link contains without writing anything to
disk.

## Usage

``` r
# S3 method for class 'shinylive_preview'
print(x, show_content = FALSE, max_content_length = 500, ...)
```

## Arguments

- x:

  shinylive_preview object

- show_content:

  Logical. Whether to print the contents of each file. Defaults to
  `FALSE`.

- max_content_length:

  Maximum number of characters of content to show per file. Defaults to
  500.

- ...:

  Additional arguments (ignored)

## Value

The `shinylive_preview` object it was handed, returned invisibly, so it
can be passed straight on. Called for the summary it prints, which
covers the link, the file count and total size, the engine, the mode,
and each file with its kind and size. See
[`preview_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/preview_shinylive_link.md)
for the entries the object holds.
