# Package index

## WebR REPL Links

Create shareable links for R code in WebR environments

- [`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md)
  : Create a webR REPL sharelink from R code
- [`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md)
  : Create webR REPL sharelink for multiple files
- [`webr_repl_exercise()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_exercise.md)
  : Create paired exercise and solution webR REPL links
- [`webr_repl_directory()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_directory.md)
  : Create webR REPL sharelinks from a directory of R files

## Shinylive Links

Create shareable links for Shiny applications

- [`shinylive_r_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_r_link.md)
  : Create a Shinylive sharelink for R Shiny apps
- [`shinylive_py_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_py_link.md)
  : Create a Shinylive sharelink for Python Shiny apps
- [`shinylive_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_project.md)
  : Create a Shinylive sharelink for multi-file projects
- [`shinylive_directory()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/shinylive_directory.md)
  : Create Shinylive sharelinks from a directory of Shiny apps

## Link Decoding & Preview

Extract files and preview contents from existing links

- [`decode_webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_webr_link.md)
  : Decode webR REPL link(s) to extract files to local directory
- [`decode_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/decode_shinylive_link.md)
  : Decode Shinylive link(s) to extract files to local directory
- [`preview_webr_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/preview_webr_link.md)
  : Preview webR REPL link contents without writing files to disk
- [`preview_shinylive_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/preview_shinylive_link.md)
  : Preview Shinylive link contents without writing files to disk

## URL Extraction

Extract URLs from link objects

- [`repl_urls()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/repl_urls.md)
  : Extract shareable URLs from livelink objects
- [`as.character(`*`<webr_link>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<shinylive_link>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<webr_project>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<webr_exercise>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<webr_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<webr_decoded>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<webr_decoded_batch>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<webr_preview>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<shinylive_project>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<shinylive_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<shinylive_decoded>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<shinylive_decoded_batch>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  [`as.character(`*`<shinylive_preview>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.character.webr_link.md)
  : Extract URLs as character vector

## Documents

Turn a knitr or Quarto chunk into a shareable link

- [`use_livelink_hook()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/livelink-knitr.md)
  [`use_livelink_engine()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/livelink-knitr.md)
  : Turn document chunks into shareable links
- [`knit_print(`*`<webr_link>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/knit_print.livelink.md)
  [`knit_print(`*`<webr_project>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/knit_print.livelink.md)
  [`knit_print(`*`<webr_exercise>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/knit_print.livelink.md)
  [`knit_print(`*`<webr_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/knit_print.livelink.md)
  [`knit_print(`*`<shinylive_link>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/knit_print.livelink.md)
  [`knit_print(`*`<shinylive_project>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/knit_print.livelink.md)
  [`knit_print(`*`<shinylive_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/knit_print.livelink.md)
  : Render livelink objects as links in knitted documents

## Configuration

Package configuration and options

- [`set_webr_base_url()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/set_webr_base_url.md)
  : Set global base URL for webR links

## Object Classes

S3 classes and their methods

- [`print(`*`<shinylive_decoded>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.shinylive_decoded.md)
  : Print method for shinylive_decoded objects
- [`print(`*`<shinylive_decoded_batch>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.shinylive_decoded_batch.md)
  : Print method for shinylive_decoded_batch objects
- [`print(`*`<shinylive_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.shinylive_directory.md)
  : Print method for shinylive_directory objects
- [`print(`*`<shinylive_link>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.shinylive_link.md)
  : Print method for shinylive_link objects
- [`print(`*`<shinylive_preview>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.shinylive_preview.md)
  : Print method for shinylive_preview objects
- [`print(`*`<shinylive_project>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.shinylive_project.md)
  : Print method for shinylive_project objects
- [`print(`*`<webr_decoded>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.webr_decoded.md)
  : Print method for webr_decoded objects
- [`print(`*`<webr_decoded_batch>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.webr_decoded_batch.md)
  : Print method for webr_decoded_batch objects
- [`print(`*`<webr_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.webr_directory.md)
  : Print method for webr_directory objects
- [`print(`*`<webr_exercise>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.webr_exercise.md)
  : Print method for webr_exercise objects
- [`print(`*`<webr_link>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.webr_link.md)
  : Print method for webr_link objects
- [`print(`*`<webr_preview>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.webr_preview.md)
  : Print method for webr_preview objects
- [`print(`*`<webr_project>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/print.webr_project.md)
  : Print method for webr_project objects
- [`format(`*`<webr_link>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<webr_project>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<webr_exercise>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<webr_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<webr_decoded>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<webr_decoded_batch>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<webr_preview>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<shinylive_link>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<shinylive_project>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<shinylive_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<shinylive_decoded>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<shinylive_decoded_batch>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  [`format(`*`<shinylive_preview>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/format.livelink.md)
  : Format a livelink object as a character vector
- [`as.data.frame(`*`<webr_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.data.frame.livelink.md)
  [`as.data.frame(`*`<shinylive_directory>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.data.frame.livelink.md)
  [`as.data.frame(`*`<webr_decoded_batch>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.data.frame.livelink.md)
  [`as.data.frame(`*`<shinylive_decoded_batch>`*`)`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/as.data.frame.livelink.md)
  : Turn a livelink container into a data frame
