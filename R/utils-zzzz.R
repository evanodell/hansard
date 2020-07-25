## Base URL for API calls
url_util <- "http://lda.data.parliament.uk/"

veb <- function(verbose) {
  if (verbose) {
    message("Connecting to API")
  }
}

jpage_func <- function(query) {
  x <- jsonlite::fromJSON(paste0(query, "&_pageSize=1"), flatten = TRUE)

  floor(x$result$totalResults / 100)
}
