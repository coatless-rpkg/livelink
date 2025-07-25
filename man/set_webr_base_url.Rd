% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{set_webr_base_url}
\alias{set_webr_base_url}
\title{Set global base URL for WebR links}
\usage{
set_webr_base_url(base_url = NULL)
}
\arguments{
\item{base_url}{Custom base URL to use for all WebR links}
}
\description{
Set global base URL for WebR links
}
\examples{
# Set custom base URL
set_webr_base_url("https://my-custom-webr.com/")

# Reset to default (removes custom setting)
set_webr_base_url(NULL)
}
