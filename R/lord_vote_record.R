
#' lord_vote_record
#'
#' Accepts an ID number for a member of the House of Commons, and returns a data frame of their votes.
#' @param peer_id The ID number of a member of the House of Lords.
#' @param lobby Accepts one of 'all', 'content', 'notcontent'. 'content' returns votes where the peer voted 'content', 'notcontent' returns votes where the peer voted 'notcontent', 'all' returns all available votes by the peer. Defaults to 'all'.
#' @param start_date The earliest date to include in the data frame. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the data frame. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the data frame to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- lord_vote_record(530, lobby='all')
#'
#' x <- lord_vote_record(530, lobby='content')
#'
#' x <- lord_vote_record(530, lobby='notcontent')
#' }


lord_vote_record <- function(peer_id = NULL, lobby = "all", start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    if (is.null(peer_id) == TRUE) {
        stop("peer_id must not be empty", call. = FALSE)
    }

    dates <- paste0("&_properties=date&max-date=", end_date, "&min-date=", start_date)

    if (lobby == "content") {

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/content.json?mnisId="

        message("Connecting to API")

        content <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, extra_args), flatten = TRUE)

        if (content$result$itemsPerPage < content$result$totalResults) {
            jpage <- round(content$result$totalResults/content$result$itemsPerPage, digits = 0)
        } else {
            jpage <- 0
        }

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, "&_page=", i, extra_args),
                flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- dplyr::bind_rows(pages)

        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)

    } else if (lobby == "notcontent") {

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId="

        message("Connecting to API")

        notcontent <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, extra_args), flatten = TRUE)

        if (notcontent$result$itemsPerPage < notcontent$result$totalResults) {
            jpage <- round(notcontent$result$totalResults/notcontent$result$itemsPerPage, digits = 0)
        } else {
            jpage <- 0
        }

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, "&_page=", i, extra_args),
                flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- dplyr::bind_rows(pages)

        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)

    } else {

        message("Retrieving content votes:")

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/content.json?mnisId="

        message("Connecting to API")

        content <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, extra_args), flatten = TRUE)

        if (content$result$itemsPerPage < content$result$totalResults) {
            jpage <- round(content$result$totalResults/content$result$itemsPerPage, digits = 0)
        } else {
            jpage <- 0
        }

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, "&_page=", i, extra_args),
                flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df_content <- dplyr::bind_rows(pages)

        df_content$vote <- "content"

        message("Retrieving not content votes:")

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId="

        message("Connecting to API")

        notcontent <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, extra_args), flatten = TRUE)

        if (notcontent$result$itemsPerPage < notcontent$result$totalResults) {
            jpage <- round(notcontent$result$totalResults/notcontent$result$itemsPerPage, digits = 0)
        } else {
            jpage <- 0
        }

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, "&_page=", i, extra_args),
                flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df_notcontent <- dplyr::bind_rows(pages)

        df_notcontent$vote <- "not-content"

        df <- rbind(df_content, df_notcontent)
        df$vote <- as.factor(df$vote)
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.Date(df$date._value)

    }

    if (nrow(df) == 0) {
      message("The request did not return any data. Please check your search parameters.")
    } else {

      if (tidy == TRUE) {

        df <- hansard_tidy(df)

      } else {

        df

      }

    }

}



lords_vote_record <- function(lord.id, lordsRecord = c("all", "content", "notContent")) {
    .Deprecated("lord_vote_record")
    lord_vote_record(peer_id = lord.id, lobby = tolower(lordsRecord))
}
