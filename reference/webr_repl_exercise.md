# Create paired exercise and solution webR REPL links

Generates a pair of webR links for educational purposes: one for student
exercises (without autorun) and one for solutions (with autorun
enabled).

## Usage

``` r
webr_repl_exercise(
  exercise_text,
  solution_text,
  exercise_name,
  base_path = "/home/web_user/",
  version = "latest",
  base_url = NULL
)
```

## Arguments

- exercise_text:

  Character string containing the exercise code with placeholders or
  TODOs

- solution_text:

  Character string containing the complete solution code

- exercise_name:

  Base name for the exercise (will create `"name_exercise.R"` and
  `"name_solution.R"`)

- base_path:

  Base directory path for files (default: `"/home/web_user/"`)

- version:

  webR version to use ("latest" or specific version \>= "v0.5.4")

- base_url:

  webR application URL. If NULL, uses global option or builds from
  version

## Value

webr_exercise object holding the paired `exercise` and `solution` links

## See also

[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md),
which this builds on;
[`vignette("teaching", package = "livelink")`](https://r-pkg.thecoatlessprofessor.com/livelink/articles/teaching.md)
for using links in a course.

## Examples

``` r
exercise_code <- "
# Exercise: Calculate mean of mtcars$mpg
# TODO: Complete the line below
mean_mpg <- 0
print(mean_mpg)
"

solution_code <- "
# Solution: Calculate mean of mtcars$mpg
mean_mpg <- mean(mtcars$mpg)
print(mean_mpg)
"

links <- webr_repl_exercise(exercise_code, solution_code, "basic_stats")
links$exercise
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJx1yzELwjAQhuG%2FckQHBbXOxU2dC%2BJmJVzDaQPJJSRXWhD%2Fuyno6Pp%2Bz3d7KUZPqlYdZmt0FpSsaaJkbKbdRW1UROnLXvXBUzVSp4dMqfrLhSYpvOUFnL%2B9hiM6MzgUAk%2FIEB7gxWDKSx%2Bfs7w2p6ao4KOjgqQncJYJOnJhbHk%2B6ULhsIV9yzFZltUvrltW7%2FsHAiFG7A%3D%3D&jz>
#> 
#> File: basic_stats_exercise.R → /home/web_user/basic_stats_exercise.R
#> Version: "latest"
#> Autorun: FALSE
links$solution
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJx1jcEKAiEURX9FrMUElXtp1x%2FUMkPeyKsR9Cn6pCD69yyKVrO7nHPgnh6SIKLUcoTqna0MXG1NobFPtD3ItczAU%2FdqShHVDUfbKhY1mzPeueeGFuL45VrsIbgWgFFEBBLpIiI7KHUZ89XQm9m%2BxG7z8cNfrgzl4omHX9NBP4HGqTSSmkvD5%2FkFmdJHTw%3D%3D&jza>
#> 
#> File: basic_stats_solution.R → /home/web_user/basic_stats_solution.R
#> Version: "latest"
#> Autorun: TRUE

# Custom path and version
webr_repl_exercise(exercise_code, solution_code, "stats",
                   base_path = "/exercises/", version = "v0.5.4")
#> 
#> ── webR Exercise ──
#> 
#> 
#> 
#> ── Exercise 
#> <https://webr.r-wasm.org/v0.5.4/#code=eJxtyzELwjAQhuG%2FcpwOCmqdi5s6F8TNSDnLaQPJJSQnFsT%2Fbgp2c32%2F57u8Ucgz1piVNLc8cOps5s0JVxhJ%2B7JUU8zVH6Q8aEFGZnD89Rr25LqnI2XwTALhDl47Snnu42OU5%2BbQFBV8dFyQ9gzOCsONXXgZGU9tobBbw9ZITFZ0McWlEfxcv24aQME%3D&jz>
#> File: stats_exercise.R → /exercises/stats_exercise.R
#> 
#> 
#> ── Solution 
#> <https://webr.r-wasm.org/v0.5.4/#code=eJxtjTEKAjEQRa8SRosV1O2DnTfQ0sgyhFEDySRkJrAg3t0oio3d570H%2F3QHxkRgQRRVJsmxaci8PcAaCuqtm5Fmqj4IyfgnUpq1R44X5vjh1uwx%2BhZRySRCNvliknqsskzl6vjFpr7MbvP2w0%2BuHJcaWIdv00E%2Fwaa5NgartdHj%2FAQW6UEk&jza>
#> File: stats_solution.R → /exercises/stats_solution.R (autorun)
#> 
#> Version: "v0.5.4"
```
