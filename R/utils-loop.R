

## Retrieves data from the API, using pagination
loop_query <- function(query, jpage, verbose){

  pages <- list()

  for (i in 0:jpage) {
    mydata <- jsonlite::fromJSON(paste0(query, i), flatten = TRUE)
    if (verbose == TRUE) {
      message("Retrieving page ", i + 1, " of ", jpage + 1)
    }
    pages[[i + 1]] <- mydata$result$items
  }

  df <- tibble::as_tibble(dplyr::bind_rows(pages))

  df

}
