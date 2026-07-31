# Set global base URL for webR links

Overrides the base URL used when building webR REPL links, for instance
to point at a webR site you host yourself or at a fixed webR version.

## Usage

``` r
set_webr_base_url(base_url = NULL)
```

## Arguments

- base_url:

  Custom base URL to use for all webR links

## Value

Invisibly returns the value supplied to `base_url`.

- The custom URL when you set one.

- `NULL` when you reset to the default.

## Details

The value is stored in the `livelink.base_url` option and applies to
webR links only. Shinylive links are unaffected. Once set, a custom base
URL takes precedence over the `version` argument given to the functions
that build links.

## See also

[`webr_repl_link()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_link.md)
for single-script links that honor this option.

[`webr_repl_project()`](https://r-pkg.thecoatlessprofessor.com/livelink/reference/webr_repl_project.md)
for multi-file projects that honor this option.

## Examples

``` r
# Remember the current setting so it can be restored afterwards
old <- getOption("livelink.base_url")

# Set custom base URL
set_webr_base_url("https://my-custom-webr.com/")
#> webR base URL set to: <https://my-custom-webr.com/>

# Reset to default (removes custom setting)
set_webr_base_url(NULL)
#> webR base URL reset to default

# Restore the previous setting
set_webr_base_url(old)
#> webR base URL reset to default
```
