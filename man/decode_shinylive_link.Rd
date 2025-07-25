% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decode-shinylive-link.R
\name{decode_shinylive_link}
\alias{decode_shinylive_link}
\title{Decode Shinylive link(s) to extract files to local directory}
\usage{
decode_shinylive_link(
  url,
  output_dir = "./shinylive_files",
  overwrite = FALSE,
  create_subdir = TRUE,
  name_dirs = TRUE
)
}
\arguments{
\item{url}{Character string or vector containing Shinylive URL(s)}

\item{output_dir}{Character string specifying the output directory path (default: "./shinylive_files")}

\item{overwrite}{Logical. Whether to overwrite existing files (default: FALSE)}

\item{create_subdir}{Logical. Whether to create subdirectories. For single URLs, creates
a subdirectory named after the URL hash. For multiple URLs, creates numbered subdirectories
(default: TRUE)}

\item{name_dirs}{Logical. For multiple URLs only: whether to name directories by URL index
rather than hash (default: TRUE). Ignored for single URLs.}
}
\value{
For single URL: shinylive_decoded object. For multiple URLs: shinylive_decoded_batch object.
}
\description{
Decodes Shinylive sharelinks to extract the embedded files and save them
to a local directory. This is the reverse operation of creating Shinylive links.
Handles both single URLs and multiple URLs automatically.
}
\examples{
\dontrun{
# Single URL
url <- "https://shinylive.io/r/editor/#code=NobwRAdgh..."
result <- decode_shinylive_link(url)
print(result)

# Multiple URLs
urls <- c(
  "https://shinylive.io/r/editor/#code=...",
  "https://shinylive.io/py/app/#code=..."
)
results <- decode_shinylive_link(urls)
print(results)

# Custom settings for single URL
result <- decode_shinylive_link(url,
                               output_dir = "./my_app",
                               create_subdir = FALSE,
                               overwrite = TRUE)

# Custom settings for multiple URLs
results <- decode_shinylive_link(urls,
                                output_dir = "./my_apps",
                                name_dirs = FALSE)  # Use hash-based names
}

}
