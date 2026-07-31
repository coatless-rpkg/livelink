# Render livelink objects as links in knitted documents

Emit a clickable Markdown link when a link object is the visible result
of a 'knitr' chunk.

## Usage

``` r
# S3 method for class 'webr_link'
knit_print(x, ...)

# S3 method for class 'webr_project'
knit_print(x, ...)

# S3 method for class 'webr_exercise'
knit_print(x, ...)

# S3 method for class 'webr_directory'
knit_print(x, ...)

# S3 method for class 'shinylive_link'
knit_print(x, ...)

# S3 method for class 'shinylive_project'
knit_print(x, ...)

# S3 method for class 'shinylive_directory'
knit_print(x, ...)
```

## Arguments

- x:

  A livelink object (a link, project, exercise, or directory).

- ...:

  Ignored.

## Value

A `knit_asis` object (via
[`knitr::asis_output()`](https://rdrr.io/pkg/knitr/man/asis_output.html)).

## Details

'knitr' calls `knit_print()` on the last value of a chunk. Without these
methods a link object would fall back to
[`print()`](https://rdrr.io/r/base/print.html), dumping cli console
output (a header, box glyphs, metadata) into the rendered page. These
methods instead emit a clickable Markdown link, which is almost always
what you want when a link object is the visible result of a chunk.

These methods apply only inside 'knitr'. Call
[`print()`](https://rdrr.io/r/base/print.html) explicitly for the full
cli description, or
[`as.character()`](https://rdrr.io/r/base/character.html) for the bare
URL.

## Shape of the rendered link

A single link becomes `[Open in webR](url)`, or the Shinylive
equivalent, and a project renders the same way. A directory or an
exercise carries several named URLs, so it becomes a bulleted list with
one titled link per entry.

## See also

[livelink-knitr](https://r-pkg.thecoatlessprofessor.com/livelink/reference/livelink-knitr.md)
for the chunk hook and engine.

[`format.livelink()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
and
[`as.character.webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
for other renderings.

[`vignette("links-in-documents", package = "livelink")`](https://r-pkg.thecoatlessprofessor.com/livelink/articles/links-in-documents.md).
