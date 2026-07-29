# Format a livelink object as a character vector

Returns an object's printed representation as a character vector, one
element per line.

## Usage

``` r
# S3 method for class 'webr_link'
format(x, ...)

# S3 method for class 'webr_project'
format(x, ...)

# S3 method for class 'webr_exercise'
format(x, ...)

# S3 method for class 'webr_directory'
format(x, ...)

# S3 method for class 'webr_decoded'
format(x, ...)

# S3 method for class 'webr_decoded_batch'
format(x, ...)

# S3 method for class 'webr_preview'
format(x, ...)

# S3 method for class 'shinylive_link'
format(x, ...)

# S3 method for class 'shinylive_project'
format(x, ...)

# S3 method for class 'shinylive_directory'
format(x, ...)

# S3 method for class 'shinylive_decoded'
format(x, ...)

# S3 method for class 'shinylive_decoded_batch'
format(x, ...)

# S3 method for class 'shinylive_preview'
format(x, ...)
```

## Arguments

- x:

  A livelink object (a link, project, exercise, directory, decoded
  result, batch, or preview).

- ...:

  Passed to the object's [`print()`](https://rdrr.io/r/base/print.html)
  method, so preview-specific options such as `show_content` work here
  too.

## Value

A character vector of formatted lines.

## Details

Having the printed form as a character vector means it can be captured,
logged, pasted into a report, or otherwise reused.
[`print()`](https://rdrr.io/r/base/print.html) renders the very same
content to the console. [`format()`](https://rdrr.io/r/base/format.html)
hands it back to you instead.

## See also

[`as.character.webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
for the shareable URL alone, without the surrounding printed report.

[as.data.frame.livelink](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.data.frame.livelink.md)
for turning directory and batch objects into a data frame.

## Examples

``` r
link <- webr_repl_link("plot(1:10)")
format(link)
#> [1] ""                                                                                                                                                                     
#> [2] "── \033[1m\033[1mwebR Link\033[1m\033[22m ──"                                                                                                                         
#> [3] ""                                                                                                                                                                     
#> [4] "\033[3m\033[34m<https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dUVxclFmQYle0JKCxJKM7foZ%2Bbmp%2BuWpSfGlxalF%2BnDJktSKklUFOfklGoZWhgaaAC8DGLU%3D&mz>\033[39m\033[23m"
#> [5] ""                                                                                                                                                                     
#> [6] "File: \033[34mscript.R\033[39m → \033[34m/home/web_user/script.R\033[39m"                                                                                             
#> [7] "Version: \033[34m\"latest\"\033[39m"                                                                                                                                  
#> [8] "Autorun: \033[34mFALSE\033[39m"                                                                                                                                       
writeLines(format(link))
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJyb2LwkLzE3dUVxclFmQYle0JKCxJKM7foZ%2Bbmp%2BuWpSfGlxalF%2BnDJktSKklUFOfklGoZWhgaaAC8DGLU%3D&mz>
#> 
#> File: script.R → /home/web_user/script.R
#> Version: "latest"
#> Autorun: FALSE
```
