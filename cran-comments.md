## Submission

Patch release, submitted soon after 0.1.0 because it fixes two problems worth
correcting promptly:

* `decode_webr_link()` and `decode_shinylive_link()` could write outside the
  directory the caller asked for, since the file names come from the link.
* Expression input failed in Positron, which parses an unsaved buffer against a
  source file name that is not a path. It worked in every other front end.

## R CMD check results

0 errors | 0 warnings | 0 notes

Checked on macOS and on win-builder R-devel.

The package makes no network connections, so examples, tests and vignettes all
run offline.
