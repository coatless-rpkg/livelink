# Print method for webr_exercise objects

Displays the exercise sharelink and the matching solution sharelink.

## Usage

``` r
# S3 method for class 'webr_exercise'
print(x, ...)
```

## Arguments

- x:

  webr_exercise object

- ...:

  Additional arguments (ignored)

## Value

The `webr_exercise` object it was handed, returned invisibly, so it can
be passed straight on. Called for the summary it prints, which covers
both the exercise sharelink and the solution sharelink. See
[`webr_repl_exercise()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_exercise.md)
for the entries the object holds.
