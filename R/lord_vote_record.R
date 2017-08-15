

#' Voting record of members of the House of Lords
#'
#' Accepts an ID number for a member of the House of Commons, and returns a tibble of their votes.
#' @param peer_id The ID number of a member of the House of Lords. A value must be included for this parameter. Use the \code{\link{lords_members}} to find IDs for members of the House of Lords. Defaults to \code{NULL}.
#' @param lobby Accepts one of 'all', 'content', 'notcontent'. 'content' returns votes where the peer voted 'content', 'notcontent' returns votes where the peer voted 'not content' and 'all' returns all available votes by the peer. Defaults to 'all'.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return  A tibble with details on the voting record of a member of the House of Lords
#'
### @keywords divisions
#' @export
#' @examples \dontrun{
#' x <- lord_vote_record(530, lobby='all')
#'
#' x <- lord_vote_record(530, lobby='content')
#'
#' x <- lord_vote_record(530, lobby='notcontent')
#' }


lord_vote_record <- function(peer_id = NULL, lobby = "all", start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    if (is.null(peer_id) == TRUE) {
        stop("peer_id must not be empty", call. = FALSE)
    }

    dates <- paste0("&_properties=date&max-date=", as.Date(end_date), "&min-date=", as.Date(start_date))

    if (lobby == "content") {

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/content.json?mnisId="

        if(verbose==TRUE){message("Connecting to API")}

        content <- jsonlite::fromJSON(paste0(baseurl, peer_id, dates, extra_args), flatten = TRUE)

        jpage <- floor(content$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, "&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    } else if (lobby == "notcontent") {

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId="

        if(verbose==TRUE){message("Connecting to API")}

        notcontent <- jsonlite::fromJSON(paste0(baseurl, peer_id, dates, extra_args), flatten = TRUE)

        jpage <- floor(notcontent$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, "&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    } else {

        message("Retrieving content votes:")

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/content.json?mnisId="

        if(verbose==TRUE){message("Connecting to API")}

        content <- jsonlite::fromJSON(paste0(baseurl, peer_id,  dates, extra_args), flatten = TRUE)

        jpage <- floor(content$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, "&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df_content <- tibble::as_tibble(dplyr::bind_rows(pages))

        if (nrow(df_content) > 0) {

        df_content$vote <- "content"

        }

        message("Retrieving not content votes:")

        baseurl <- "http://lda.data.parliament.uk/lordsdivisions/notcontent.json?mnisId="

        if(verbose==TRUE){message("Connecting to API")}

        notcontent <- jsonlite::fromJSON(paste0(baseurl, peer_id, dates, extra_args), flatten = TRUE)

        jpage <- floor(notcontent$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, peer_id, "&_pageSize=500", dates, "&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df_notcontent <- tibble::as_tibble(dplyr::bind_rows(pages))

        if (nrow(df_notcontent) > 0) {

        df_notcontent$vote <- "not-content"

        }

        df <- rbind(df_content, df_notcontent)
        df$vote <- as.factor(df$vote)
        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.POSIXct(df$date._value)

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

          df <- lord_vote_record_tidy(df, tidy_style)

        }

          df

    }

}


#' @rdname lord_vote_record
#' @export
hansard_lord_vote_record <- function(peer_id = NULL, lobby = "all", start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- lord_vote_record(peer_id = peer_id, lobby = lobby, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
