

## Retrieves data from the API, using pagination
loop_query <- function(query, verbose) {
  veb(verbose)

  jpage <- jpage_func(query)

  seq_list <- seq(from = 0, to = jpage, by = 1)

  pages <- list()

  for (i in seq_along(seq_list)) {

    api_get <- httr::GET(paste0(query, "&_pageSize=100&_page=", seq_list[[i]]))

    if (httr::http_type(api_get) != "application/json") {

      stop("API did not return data in required JSON format", call. = FALSE)

    } else if (httr::status_code(api_get) != "200") {

      stop(paste("Request returned error code:", httr::status_code(api_get)),
        call. = FALSE
      )

    } else {

      mydata <- jsonlite::fromJSON(
        httr::content(api_get, as = "text", encoding = "utf8"),
        flatten = TRUE
      )
      if (verbose) {
        message("Retrieving page ", seq_list[[i]] + 1, " of ", jpage + 1)
      }
      pages[[seq_list[[i]] + 1]] <- mydata$result$items

    }
  }
  tibble::as_tibble(dplyr::bind_rows(pages))
}



# Special all_answered_questions loop query
loop_query_aaq <- function(query, verbose) {
  veb(verbose)

  jpage <- jpage_func(query)

  seq_list <- seq(from = 0, to = jpage, by = 1)

  pages <- list()

  for (i in seq_along(seq_list)) {
    api_get <- httr::GET(paste0(query, "&_pageSize=100&_page=", seq_list[[i]]))
    if (httr::http_type(api_get) != "application/json") {

      stop("API did not return data in required JSON format", call. = FALSE)

    } else if (httr::status_code(api_get) != "200") {

      stop(paste("Request returned error code:", httr::status_code(api_get)),
        call. = FALSE
      )

    } else {

      mydata <- jsonlite::fromJSON(
        httr::content(api_get, as = "text", encoding = "utf8"),
        flatten = TRUE
      )
      if (verbose) {
        message("Retrieving page ", seq_list[[i]] + 1, " of ", jpage + 1)
      }
      mydata$result$items$answer.previousAnswerVersion.answeringMember <- NULL

      pages[[seq_list[[i]] + 1]] <- mydata$result$items

    }
  }
  tibble::as_tibble(dplyr::bind_rows(pages))
}
