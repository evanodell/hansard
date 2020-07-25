

# Bespoke looping function to manage the weird stuff going on
# with EDM primary sponsors
edm_loop_query <- function(query, verbose) {
  veb(verbose)

  jpage <- jpage_func(query)

  seq_list <- seq(from = 0, to = jpage, by = 1)

  pages <- list()

  for (i in seq_along(seq_list)) {
    mydata <- jsonlite::fromJSON(paste0(query, seq_list[[i]]), flatten = TRUE)
    if (verbose) {
      message("Retrieving page ", seq_list[[i]] + 1, " of ", jpage + 1)
    }
    mydata$result$items$primarySponsorPrinted <-
      as.list(mydata$result$items$primarySponsorPrinted)

    pages[[seq_list[[i]] + 1]] <- mydata$result$items
  }

  df <- tibble::as_tibble(dplyr::bind_rows(pages))

  df
}
