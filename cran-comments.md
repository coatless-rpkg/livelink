## R CMD check results

0 errors | 0 warnings | 1 note

The note is the standard "New submission" note for a first submission.

## First submission

This is the first submission of the livelink package to CRAN.

## Method References

There are no published references describing the methods in this package.
The package implements original functionality for creating shareable links
to WebAssembly-based R and Shiny environments.

## Notes for the reviewer

* `SystemRequirements` lists the Quarto command line tools. They are needed only
  to build the vignette, which uses `VignetteBuilder: quarto`.

* The package writes files only from `decode_webr_link()` and
  `decode_shinylive_link()`, whose purpose is to extract the files embedded in a
  sharelink. Both default `output_dir` to a subdirectory of `tempdir()`; writing
  anywhere else requires the user to pass an explicit path. Examples, tests, and
  the vignette write only inside the session temporary directory.

* The package does not access the network. All link creation and decoding is
  local computation on the URL string, so examples and tests run offline.
