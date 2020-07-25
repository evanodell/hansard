

## Retrieves data from the API, using pagination
loop_query <- function(query, verbose) {
  veb(verbose)

  jpage <- jpage_func(query)

  seq_list <- seq(from = 0, to = jpage, by = 1)

  pages <- list()

  for (i in seq_along(seq_list)) {
    mydata <- jsonlite::fromJSON(paste0(
      query, "&_pageSize=100&_page=",
      seq_list[[i]]
    ), flatten = TRUE)
    if (verbose) {
      message("Retrieving page ", seq_list[[i]] + 1, " of ", jpage + 1)
    }
    pages[[seq_list[[i]] + 1]] <- mydata$result$items
  }

  df <- tibble::as_tibble(dplyr::bind_rows(pages))

  df
}



# Special all_answered_questions loop query
loop_query_aaq <- function(query, verbose) {
  veb(verbose)

  jpage <- jpage_func(query)

  seq_list <- seq(from = 0, to = jpage, by = 1)

  pages <- list()

  for (i in seq_along(seq_list)) {
    mydata <- jsonlite::fromJSON(paste0(
      query, "&_pageSize=100&_page=",
      seq_list[[i]]
    ), flatten = TRUE)
    if (verbose) {
      message("Retrieving page ", seq_list[[i]] + 1, " of ", jpage + 1)
    }
    mydata$result$items$answer.previousAnswerVersion.answeringMember <- NULL

    pages[[seq_list[[i]] + 1]] <- mydata$result$items
  }

  df <- tibble::as_tibble(dplyr::bind_rows(pages))

  df
}
