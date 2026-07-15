# Turn document chunks into shareable links

livelink plugs into 'knitr' two ways, and which you want depends on one
question: **should the code also run in your document?**

**A chunk hook**, on an ordinary `r` chunk. The chunk runs as usual –
its output, plots and all, appear in the rendered page – and a link is
added underneath. Use this for code you want your reader to *see the
result of* and *also* be able to open and play with.

    ```{r}
    #| livelink: true
    #| autorun: true
    # Load the data
    data(mtcars)
    plot(mtcars$mpg, mtcars$wt)
    ```

**An engine**, as ```` ```{livelink} ````. The chunk is displayed but
**not** run: only the link is produced. Use this for code your session
cannot or should not execute – a Shiny app, something needing a package
you have not installed, or anything slow.

    ```{livelink}
    #| engine.target: shinylive-r
    library(shiny)
    shinyApp(fluidPage(), function(input, output) {})
    ```

There is deliberately no `{shinylive-r}` or `{shinylive-py}` engine.
'knitr' cannot dispatch a chunk whose engine name contains a hyphen (its
chunk syntax forbids it), and in Quarto such a cell is handed to the
Shinylive extension rather than to 'knitr'. Name Shinylive through
`engine.target` instead.

## Usage

``` r
use_livelink_hook()

use_livelink_engine()
```

## Value

Called for their side effect. Invisibly `TRUE` if registration happened,
`FALSE` if 'knitr' is not installed.

## Details

Reach for either of these rather than expression input
(`webr_repl_link({ ... })`) inside a knitted document. 'knitr' evaluates
chunks through
[`evaluate::evaluate()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/evaluate.r-lib.org/reference/evaluate.md),
which discards source references, and comments live in those – so
comments inside a [`{ }`](https://rdrr.io/r/base/Paren.html) expression
are silently dropped from the link when the document renders, and no
`keep.source` setting brings them back. Both the hook and the engine are
handed the chunk's verbatim source, so nothing is lost.

Both are registered automatically when livelink is loaded, provided
'knitr' is installed. Call these yourself only if you have reset
[`knitr::knit_hooks`](https://rdrr.io/pkg/knitr/man/knit_hooks.html) or
[`knitr::knit_engines`](https://rdrr.io/pkg/knitr/man/knit_engines.html).

## Chunk options

- `livelink`:

  Hook only. `true` for a webR link, or name the target directly:
  `"webr"`, `"shinylive-r"`, `"shinylive-py"`.

- `engine.target`:

  Engine only. `"webr"` (default), `"shinylive-r"`, or `"shinylive-py"`.

- `autorun`:

  Logical. Run the code as soon as the link opens. webR only.

- `panels`:

  Character vector of webR panels, e.g. `c("editor", "plot")`.

- `mode`:

  Shinylive only. Display mode, `"editor"` (default) or `"app"`.

- `filename`:

  webR only. Name for the script file in the webR virtual filesystem
  (default `"script.R"`); must end in `.R` for `autorun` to work.

- `link.text`:

  Text for the hyperlink. Defaults to `"Open in webR"` or
  `"Open in Shinylive"`.

- `link.only`:

  Engine only. If `TRUE`, emit the link without the source.

## Setting options once

These are ordinary 'knitr' chunk options, so `opts_chunk` sets them for
a whole document, and a single chunk opts out with `livelink: false`:

    knitr::opts_chunk$set(livelink = TRUE, autorun = TRUE)

## `echo` does not gate the link

It is natural to assume the code must be visible for a link to be made.
It need not be. `echo` controls whether the **source is shown in your
page**; the link is built from the chunk's source, which 'knitr' hands
over either way. So `echo: false` gives a working link whose code the
reader simply cannot see.

`eval: false` is the other half: the chunk is displayed but not run,
which makes an `r` chunk behave rather like the engine.

## See also

[`vignette("links-in-documents", package = "livelink")`](https://r-pkg.thecoatlessprofessor.com/livelink/articles/links-in-documents.md)
for the whole picture, and
[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md)
for why a braced expression loses comments in a knitted document.

## Examples

``` r
# Both are registered on load; call directly only after resetting knitr's hooks.
use_livelink_hook()
use_livelink_engine()
```
