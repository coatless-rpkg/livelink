% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/webr-repl-links.R
\name{webr_repl_exercise}
\alias{webr_repl_exercise}
\title{Create paired exercise and solution WebR REPL links}
\usage{
webr_repl_exercise(
  exercise_text,
  solution_text,
  exercise_name,
  base_path = "/home/web_user/",
  version = "latest",
  base_url = NULL
)
}
\arguments{
\item{exercise_text}{Character string containing the exercise code with placeholders or TODOs}

\item{solution_text}{Character string containing the complete solution code}

\item{exercise_name}{Base name for the exercise (will create \code{"name_exercise.R"} and \code{"name_solution.R"})}

\item{base_path}{Base directory path for files (default: \code{"/home/web_user/"})}

\item{version}{WebR version to use ("latest" or specific version >= "v0.5.4")}

\item{base_url}{WebR application URL. If NULL, uses global option or builds from version}
}
\value{
Named list with 'exercise' and 'solution' WebR sharelinks
}
\description{
Generates a pair of WebR links for educational purposes: one for student exercises
(without autorun) and one for solutions (with autorun enabled).
}
\examples{
exercise_code <- "
# Exercise: Calculate mean of mtcars$mpg
# TODO: Complete the line below
mean_mpg <- # YOUR CODE HERE
print(mean_mpg)
"

solution_code <- "
# Solution: Calculate mean of mtcars$mpg
mean_mpg <- mean(mtcars$mpg)
print(mean_mpg)
"

links <- webr_repl_exercise(exercise_code, solution_code, "basic_stats")
# Access with links$exercise and links$solution

# Custom path and version
links <- webr_repl_exercise(exercise_code, solution_code, "stats",
                          base_path = "/exercises/", version = "v0.5.4")

}
