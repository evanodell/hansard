

## Retrieves data from the API, using pagination
loop_query <- function(query, jpage, verbose){

  seq_list <- seq(from=0, to=jpage, by=1)

  pages <- list()

  for (i in seq_along(seq_list)) {
    mydata <- jsonlite::fromJSON(paste0(query, i), flatten = TRUE)
    if (verbose == TRUE) {
      message("Retrieving page ", i + 1, " of ", jpage + 1)
    }
    pages[[i + 1]] <- mydata$result$items
  }

  df <- tibble::as_tibble(dplyr::bind_rows(pages))

  df

}

