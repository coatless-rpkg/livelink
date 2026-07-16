# Set global base URL for webR links

Overrides the base URL used when building webR REPL links, for instance
to point at a self-hosted or pinned webR deployment. The value is stored
in the `livelink.base_url` option and applies to webR links only;
Shinylive links are unaffected. Once set, a custom base URL takes
precedence over the `version` argument passed to the link builders.

## Usage

``` r
set_webr_base_url(base_url = NULL)
```

## Arguments

- base_url:

  Custom base URL to use for all webR links

## Value

Invisibly returns the `base_url` value (or `NULL` if resetting to
default).

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
