# Multi-file projects and Shiny apps

Real work is rarely one file. A script sources a helper, an app is split
across `ui.R` and `server.R`, an analysis needs its data. All of it fits
in one link.

This article covers webR projects, Shiny apps in R and Python, and
turning a whole directory into links.

## webR projects

[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md)
packs several files into a single link. webR unpacks them into its
filesystem, so `source("utils.R")` finds `utils.R` exactly where it
expects it.

![A named list or a vector of file paths goes in, and one URL carrying
every file comes out, which webR unpacks into its
filesystem.](../reference/figures/projects-light.svg)![](../reference/figures/projects-dark.svg)

### Describing the files

Two input shapes, and you can use whichever is closer to hand.

A **named list**, where the names become the filenames. Write each file
as R, in braces. No escaped newlines, no fighting with quotes, and your
editor keeps highlighting it:

``` r

webr_repl_project(list(
  "main.R"    = { source("utils.R"); summarise(mtcars) },
  "utils.R"   = { summarise <- function(d) summary(d) },
  "README.md" = "# Analysis\n\nRun main.R to start."
))
```

[Open in
webR](https://webr.r-wasm.org/latest/#code=eJx1j8sKwjAQRX8ljJsWtN2Lm4JdusnWiMQ20kAzkcwELdJ%2Ft9CH3XQ3w7ncw71%2BAbUzcASnLWYS9vDS3Ax%2F3nhn8rd53COZkC%2BYzYcHTD6GyiQKItuWMqkgVUjROR0smcRxpQOl0O8XwRTcNPz5rJjbxOkgnhErth6TOhUj6IZz3S%2FL4nwpM1dvGtaJybETBeq2I0sKFcqIYhwq2AtiHTiD%2FvYDD4JhYw%3D%3D&jz)

Non-R files stay strings, and the two forms mix freely, since a project
is usually code *and* something else.

> **Write the list inside the call**
>
> The braces only work when the list is written *in* the call.
> [`list()`](https://rdrr.io/r/base/list.html) is an ordinary function,
> so if you assign it to a variable first, R evaluates its arguments.
> The block runs, right there, and livelink never sees the source:
>
> ``` r
>
> # WRONG: the block runs now, and actually tries to source("utils.R")
> project <- list("main.R" = { source("utils.R"); summarise(mtcars) })
> webr_repl_project(project)
>
> # RIGHT: written in the call, the block is captured and never run
> webr_repl_project(list("main.R" = { source("utils.R"); summarise(mtcars) }))
> ```
>
> livelink notices the first case and stops with an error rather than
> encoding whatever the block happened to return. A list of *strings* in
> a variable is fine, of course. That is the older form, and it still
> works.

Because the blocks are captured rather than evaluated, an assignment
inside one leaves nothing behind in your session.
`{ summarise <- function(d) summary(d) }` defines nothing here. It is
source to ship, not code to run.

Two caveats carry over from single-file expressions. Comments inside
[`{ }`](https://rdrr.io/r/base/Paren.html) survive interactively but not
in a knitted document, and a
[`library()`](https://rdrr.io/r/base/library.html) call inside a block
is visible to `R CMD check`, which will report that package as an
undeclared dependency of *yours*. Use a string for code that loads
packages.

When a project has to be reused across several calls, as it is through
the rest of this section, keep it as a list of **strings** in a
variable. A list of strings is a plain value, so it assigns without
surprises:

``` r

project <- list(
  "main.R"    = "source('utils.R')\nsummarise(mtcars)",
  "utils.R"   = "summarise <- function(d) summary(d)",
  "README.md" = "# Analysis\n\nRun main.R to start."
)

webr_repl_project(project)
```

[Open in
webR](https://webr.r-wasm.org/latest/#code=eJx1j8sKgzAQRX8lpAsVbNyXboS67CbbWkqqKQbMpGQmtFL89wo%2BN%2B5mOJdzubcfB2U1P3GrDAjJU%2F5W1Ax%2F1jirs49%2BPgJqny2Y9JcGjC74SsdRINOikFFSAgZrlTeoY0uV8pjwPl30U27Xv%2FK5YLax85G9AlRkHMR1wkbQDefWL4v8ci2ErXcbtomp48ByUG2HBksoQQZg40xGjiEpT4L39z9KBGC1&jz)

A **vector of file paths**, when the code is already on disk:

``` r

dir <- file.path(tempdir(), "project")
dir.create(dir, showWarnings = FALSE)
writeLines("source('utils.R')",  file.path(dir, "main.R"))
writeLines("f <- function() 42", file.path(dir, "utils.R"))

webr_repl_project(c(file.path(dir, "main.R"), file.path(dir, "utils.R")))
```

[Open in
webR](https://webr.r-wasm.org/latest/#code=eJyLrlbKS8xNVbJSyk3MzNMLUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlQOni%2FNKi5FQN9dKSzJxivSB1TaVaHbhZUEGchiHkoaalKdjoKqSV5iWXZObnaWgqmBgp1cYCAIlMMjQ%3D&jz)

Files are read and named by their basename, so the paths on your machine
never leave it.

### Running something on arrival

`autorun_files` names the files to execute when the link opens. The link
then carries webR’s `a` flag, which is what actually triggers the run:

``` r

link <- webr_repl_project(project, autorun_files = "main.R")

preview_webr_link(as.character(link))$autorun_files
#> [1] "main.R"
```

Pass `"all"` to run every R file in the project:

``` r

webr_repl_project(project, autorun_files = "all")
```

[Open in
webR](https://webr.r-wasm.org/latest/#code=eJx9kE0LgkAQhv%2FKsh1UML1LFyGPXbxmxKYbLrizsTNDSfjfE%2Fyoi91meN6ZB97zW4KyWmbSKgNJKWP5UNSOe9o6q9Onvl0ZtU9XTPpFI0bHvtZhwGQ6TMogqgDZWuUN6tBSrTxGY1oxOc8gM%2FKsh3i1zWebui9ffMtzcdiLO0NNxkHYRGIC%2FTj%2B0ZVFfjwViW02hb%2BJWbkTOaiuR4MVVFAyiKkEQU4gKU%2BJHC4f6X5rpQ%3D%3D&jza)

Naming a file that is not in the project is an error, rather than a link
that quietly does nothing:

``` r

webr_repl_project(project, autorun_files = "nonexistent.R")
#> Error in `ensure_files_in_list()`:
#> ! Files specified in `autorun_files` not found in `input`
#> ✖ Missing files: 'nonexistent.R'
#> ℹ Available files: 'main.R', 'utils.R', and 'README.md'
```

Only `.R` files can autorun. Listing a `.csv` is harmless, and simply is
not run.

### Where the files land

By default everything is placed in `/home/web_user/`, which is webR’s
working directory, so relative paths in your code just work. `base_path`
moves them:

``` r

webr_repl_project(project, base_path = "/home/web_user/analysis/")
```

[Open in
webR](https://webr.r-wasm.org/latest/#code=eJyFj8sKwjAQRX8lxIUt1GQvbgp26SZbKxLbSAPNRDITtEj%2F3UIfdtfdDPdwD%2Ff65aCd4UfutAWheMZfmprhl413Rr7N4x7RBKlBtx1alAtH5kMDhz6GyiT7SLZFofZpCRid08GiSRxVOmDK%2B2zxTNy26A%2FOprmWnQ7sGaEi6yGpUzYG3XCuRarIz5dCuHpbtUYn2Y7lU1pCCSoCG4cz8gxJBxK8v%2F0AJiJrbg%3D%3D&jz)

Move them and relative [`source()`](https://rdrr.io/r/base/source.html)
calls have to move with them, so leave this alone unless you have a
reason.

## Shiny apps

![shinylive_r_link() and shinylive_py_link() pick the R or Python path,
and mode picks whether a visitor lands on the code beside the app, or
the running app
alone.](../reference/figures/shinylive-light.svg)![](../reference/figures/shinylive-dark.svg)

Write the app as a braced expression, exactly as you would write it
anywhere. No quotes around the whole thing, no escaped newlines, and
your editor keeps highlighting the code:

``` r

shinylive_r_link({
  library(shiny)

  ui <- fluidPage(
    titlePanel("Old Faithful"),
    sliderInput("bins", "Bins:", 1, 50, 30),
    plotOutput("hist")
  )

  server <- function(input, output) {
    output$hist <- renderPlot({
      hist(faithful$waiting, breaks = input$bins, col = "steelblue")
    })
  }

  shinyApp(ui, server)
})
```

That is real, working code. It is marked `eval: false` here for one
narrow reason, which does not apply to your own documents.

> **Why that chunk is not run**
>
> Only to keep this package’s vignette honest about its dependencies.
> `R CMD check` reads the live code in a vignette and, finding
> [`library(shiny)`](https://shiny.posit.co/) in a braced expression,
> would decide livelink depends on shiny. It does not. Marking the chunk
> `eval: false` keeps that call out of the scanned code.
>
> Your own course notes are not a package under check, so there is no
> such constraint. Write the app as an expression and let it run.

When you want the link built and inspected here, a string sidesteps the
scan, so this vignette can execute it. A bare string becomes `app.R`:

``` r

app <- "library(shiny)\nshinyApp(fluidPage(), function(input, output) {})"

shinylive_r_link(app)
```

[Open in
Shinylive](https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlAB0IPPqwCC6dgDNqAV1oATAApQA5nHYDcAAhnyIBUrRLs+qeaT1Erl0gN0gAvgLxhSrVAmTkAHqRgzgC6QA)

For an app split across files, the same choice applies. Write each file
in braces:

``` r

shinylive_r_link(list(
  "app.R"    = { library(shiny); source("ui.R"); source("server.R"); shinyApp(ui, server) },
  "ui.R"     = { ui <- fluidPage(titlePanel("Split app")) },
  "server.R" = { server <- function(input, output) {} }
))
```

### Editor or app?

`mode` decides what a visitor lands on.

``` r

# code and app side by side -- for teaching and review
shinylive_r_link(app, mode = "editor")
```

[Open in
Shinylive](https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlAB0IPPqwCC6dgDNqAV1oATAApQA5nHYDcAAhnyIBUrRLs+qeaT1Erl0gN0gAvgLxhSrVAmTkAHqRgzgC6QA)

``` r

# the running application only -- for the people who just want to use it
shinylive_r_link(app, mode = "app")
```

[Open in
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlAB0IPPqwCC6dgDNqAV1oATAApQA5nHYDcAAhnyIBUrRLs+qeaT1Erl0gN0gAvgLxhSrVAmTkAHqRgzgC6QA)

In `app` mode you can also drop the Shinylive header, which is what you
want when the app is going into an `<iframe>` on someone else’s page:

``` r

shinylive_r_link(app, mode = "app", header = FALSE)
```

[Open in
Shinylive](https://shinylive.io/r/app/#h=0&code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlAB0IPPqwCC6dgDNqAV1oATAApQA5nHYDcAAhnyIBUrRLs+qeaT1Erl0gN0gAvgLxhSrVAmTkAHqRgzgC6QA)

`header` does nothing in `editor` mode, where there is no header to
hide.

> **`mode` is not `panels`**
>
> Shinylive has two display *modes*, `"editor"` and `"app"`. webR has
> four *panels* you can show or hide. They are different arguments
> because they are different ideas: one picks a layout, the other picks
> which parts of a layout appear.

### Python

Python apps work the same way, and a bare string becomes `app.py`:

``` r

py_app <- "
from shiny import App, render, ui

app_ui = ui.page_fluid(
    ui.h2('Hello from Python'),
    ui.output_text('greeting'),
)

def server(input, output, session):
    @render.text
    def greeting():
        return 'Running in the browser, no server required.'

app = App(app_ui, server)
"

shinylive_py_link(py_app, mode = "app")
```

[Open in
Shinylive](https://shinylive.io/py/app/#code=NobwRAdghgtgpmAXGKAHVA6VBPMAaMAYwHsIAXOcpMAHQgDMAnYmAAgGcALASwm1e4xUxRmVYBBdHlaNKAEziNpAV2506aVAH1VrALytVWKAHM4W+gBtVcgBR1Wjw9wycATLYDkACTiXLxKxMLKwACthknKSeAJR4Dk5GxMpkqClaFAAeZF4msnBkvCax8RAx6hAK9ByKAG6KtrxpZNLJqSnS7HDs7NykMYgJjgACspWKGFlkQ6xVrHlwBUW2AzNOMgXKjBCsngBKyhAQRQI7kXCsAEbMAO5dSqwQgff1jBsAjqqychieFZr6CToWyaHTcTp1RTlCD4MBkbCoBDIKZgAC+AF0gA)

The engine is in the URL, `shinylive.io/r/` versus `shinylive.io/py/`,
so a link carries its own language with it.

[`shinylive_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_project.md)
is the same thing with the engine named explicitly, which is useful when
the engine is a variable rather than a decision you already made:

``` r

shinylive_project(
  list("app.py" = py_app, "utils.py" = "def helper():\n    return 42"),
  engine = "python"
)
```

[Open in
Shinylive](https://shinylive.io/py/editor/#code=NobwRAdghgtgpmAXGKAHVA6VBPMAaMAYwHsIAXOcpMAHQgDMAnYmAAgGcALASwm1e4xUxRmVYBBdHlaNKAEziNpAV2506aVAH1VrALytVWKAHM4W+gBtVcgBR1Wjw9wycATLYDkACTiXLxKxMLKwACthknKSeAJR4Dk5GxMpkqClaFAAeZF4msnBkvCax8RAx6hAK9ByKAG6KtrxpZNLJqSnS7HDs7NykMYgJjgACspWKGFlkQ6xVrHlwBUW2AzNOMgXKjBCsngBKyhAQRQI7kXCsAEbMAO5dSqwQgff1jBsAjqqychieFZr6CToWyaHTcTp1RTlCD4MBkbCoBDIKZgAC+eHA0Hg1BS3Es7CwuAIJHIlDI1DmnD8iMYK0GOycsjIWx2ABY3LD4YjqCjUQBdIA)

## A directory at a time

Each Shiny app in its own folder is the usual layout, and
[`shinylive_directory()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_directory.md)
turns the lot into links in one pass:

``` r

apps <- file.path(tempdir(), "apps")
dir.create(file.path(apps, "histogram"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(apps, "scatter"),   recursive = TRUE, showWarnings = FALSE)
writeLines(app, file.path(apps, "histogram", "app.R"))
writeLines(app, file.path(apps, "scatter",   "app.R"))

links <- shinylive_directory(apps, engine = "r", mode = "app")
#> ✔ Found 2 Shiny apps in '/tmp/RtmpFMAXup/apps'
#> ℹ Processing r Shiny apps...
#> ✔ Successfully created 2 Shinylive links

repl_urls(links)
#>                                                                                                                                                                                 histogram 
#> "https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlAB0IPPqwCC6dgDNqAV1oATAApQA5nHYDcAAhnyIBUrRLs+qeaT1Erl0gN0gAvgLxhSrVAmTkAHqRgzgC6QA" 
#>                                                                                                                                                                                   scatter 
#> "https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKAZwAtaJWAlAB0IPPqwCC6dgDNqAV1oATAApQA5nHYDcAAhnyIBUrRLs+qeaT1Erl0gN0gAvgLxhSrVAmTkAHqRgzgC6QA"
```

Every subdirectory holding an `app.R` (or `app.py`, for Python) becomes
one link, named after the folder. Helpers, data and CSS in that folder
are bundled in with it. Binary files are skipped with a warning, because
a link is the wrong place for them.

A directory with no apps in it warns and hands back an empty result,
rather than failing:

``` r

empty <- file.path(tempdir(), "no-apps")
dir.create(empty, showWarnings = FALSE)

shinylive_directory(empty, engine = "r")
#> Warning: No Shiny apps found
#> ! No directories containing 'app.R' found in '/tmp/RtmpFMAXup/no-apps'
#> ℹ Each app should be in its own subdirectory with 'app.R' as the main file
```

*(no links)*

[`webr_repl_directory()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_directory.md)
is the webR counterpart, and by default it works the other way round: it
gives each R script in a directory *its own* link, rather than bundling
a folder into one. That suits a folder of course scripts, and [Teaching
with
livelink](https://r-pkg.thecoatlessprofessor.com/livelink/articles/teaching.md)
puts it to work.

When you do want the whole directory in a single link, pass
`single_link = TRUE`. The files are bundled exactly as
[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md)
would bundle them, and you get one `webr_project` back instead of a
directory of links:

``` r

scripts <- file.path(tempdir(), "scripts")
dir.create(scripts, showWarnings = FALSE)
writeLines("source('utils.R')", file.path(scripts, "main.R"))
writeLines("f <- function() 42", file.path(scripts, "utils.R"))

webr_repl_directory(scripts, single_link = TRUE, panels = c("editor", "plot"))
#> ✔ Bundling 2 files into one link
```

[Open in
webR](https://webr.r-wasm.org/latest/?mode='editor-plot'#code=eJyLrlbKS8xNVbJSyk3MzNMLUtJRKkgsyQDy9TPyc1P1y1OT4kuLU4v04dIlqRUlQOni%2FNKi5FQN9dKSzJxivSB1TaVaHbhZUEGchiHkoaalKdjoKqSV5iWXZObnaWgqmBgp1cYCAIlMMjQ%3D&jz)

With `autorun = TRUE` alongside it, every R file in the bundle runs on
arrival.

## How big can a link get?

The files travel *inside* the URL, so the ceiling is whatever a browser
accepts in a URL. Current browsers handle tens of thousands of
characters, which is a lot of R.

The payload is gzipped before it is encoded, and source code compresses
well. Real R source, rather than a toy example:

``` r

src <- paste(
  unlist(lapply(c("lm", "glm", "aov"), function(f) {
    paste(deparse(get(f, envir = asNamespace("stats"))), collapse = "\n")
  })),
  collapse = "\n\n"
)

url <- as.character(webr_repl_link(src, filename = "stats.R"))

c(source_chars = nchar(src), url_chars = nchar(url))
#> source_chars    url_chars 
#>        10193         4358
```

The link comes out *smaller than the code it carries*, because gzip more
than pays for what base64 costs.

Binary data is the case that does not work: it does not compress, and
base64 inflates it by a third. Do not put a PNG in a link. Have the code
fetch it, or point at it.
