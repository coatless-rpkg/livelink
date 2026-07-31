# Create paired exercise and solution webR REPL links

Generates a pair of webR links for teaching. One link holds the exercise
for the student and the other holds the solution.

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

A `webr_exercise` object, which is a list with these entries.

- `exercise`, the student's link, a `webr_link` object that does not
  autorun.

- `solution`, the answer link, a `webr_link` object that autoruns on
  opening.

Each of the two is itself a list holding the entries described in
[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md),
so `links$solution$url` is the solution URL on its own.

## Details

The exercise link is built without autorun, so the student works through
it, while the solution link is built with autorun enabled.

## See also

[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md),
which this builds on.

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
#> <https://webr.r-wasm.org/latest/#code=eJx1yz0KAjEQQOE%2BpxhQQQtda7FT6wWxD5MwmkD%2BSGbZ7T2Bd9gzeJe9jRG03PbxvddzDOjprbBYLQsjF0kDZW0L7a5jQjbTqjHRU9OTkl2h3MxYpoEnIxZw%2BbUDnNDpziETeMIA8Q6eNeay9OlR4a09txVFnxxVw4bA2UCgyMVefBdZIRy3sBcp28Drf9uID3wZRNM%3D&mz>
#> 
#> File: basic_stats_exercise.R → /home/web_user/basic_stats_exercise.R
#> Version: "latest"
#> Autorun: FALSE
links$solution
#> 
#> ── webR Link ──
#> 
#> <https://webr.r-wasm.org/latest/#code=eJyb2LIkLzE3dVtSYnFmcnxxSWJJcXxxfk5pSWZ%2Bnl7QkoLEkoybqvoZ%2Bbmp%2BuWpSfGlxalF%2BjjUlqRWlNwM51JWCIaKWSk4J%2BYkl%2BYklqQq5KYm5inkpynkliQnFhWr5Bakc4GE4oEMBRtdsLQGQk6Tq6AoM69EA6ZEk2t5YmlJflFp3mEAuo1E5g%3D%3D&mza>
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
#> <https://webr.r-wasm.org/v0.5.4/#code=eJyb2LwkLzE3dUNxSWJJcXxqRWpRcmZxql7QkoLEkozd%2BjCBYn0MBSWpFSU3M7iUFVyhYlYKzok5yaU5iSWpCrmpiXkK%2BWkKuSXJiUXFKrkF6UCFIf4u%2FkBF%2BbkFOalANSUZqQo5mXmpCkmpOfnlXCAt8UCFCja6CgZcBUWZeSUaMDFNLgDmvz5f&mz>
#> File: stats_exercise.R → /exercises/stats_exercise.R
#> 
#> 
#> ── Solution 
#> <https://webr.r-wasm.org/v0.5.4/#code=eJyb2LIkLzE3dUNxSWJJcXxxfk5pSWZ%2Bnl7QkoLEkozd%2BqkVqUXJmcWpxfoYCkpSK0puhnMpKwRDxawUnBNzkktzEktSFXJTE%2FMU8tMUckuSE4uKVXIL0rlAQvFAhoKNLlhaAyGnyVVQlJlXogFTosm1PLG0JL%2BoNO8wAFc6PnI%3D&mza>
#> File: stats_solution.R → /exercises/stats_solution.R (autorun)
#> 
#> Version: "v0.5.4"
```
