# Print method for webr_project objects

Displays the project sharelink and the files it carries.

## Usage

``` r
# S3 method for class 'webr_project'
print(x, ...)
```

## Arguments

- x:

  webr_project object

- ...:

  Additional arguments (ignored)

## Value

The `webr_project` object it was handed, returned invisibly, so it can
be passed straight on. Called for the summary it prints, which covers
the URL, every file and where it lands, which files run on open, the
interface, and the version. See
[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md)
for the entries the object holds.
