

#' Individual MP voting records
#'
#' Accepts an ID number for a member of the House of Commons, and returns a tibble of their votes.
#' @param mp_id The ID number of a member of the House of Commons.
#' @param lobby Accepts one of \code{'all'}, \code{'aye'} or \code{'no'}. \code{'aye'} returns votes where the MP voted \code{'aye'}, \code{'no'} returns votes where the MP voted \code{'no'}, \code{'all'} returns all available votes by the MP. Defaults to \code{'all'}.
#' @param session The parliamentary session to return votes from, in \code{'YYYY/YY'} format. Defaults to \code{NULL}.
#' @param start_date The earliest date to include in the tibble. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to \code{'1900-01-01'}. Accepts character values in \code{'YYYY-MM-DD'} format and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return A tibble with details on the voting record of the given MP.
#' @export
#' @examples \dontrun{
#' x <- mp_vote_record(172, lobby='all')
#'
#' x <- mp_vote_record(172, lobby='aye')
#'
#' x <- mp_vote_record(172, lobby='no')
#'
#' x <- mp_vote_record(172, session = '2016/17')
#' }


mp_vote_record <- function(mp_id = NULL, lobby = "all", session = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    if (is.null(extra_args) == FALSE) {
        extra_args <- utils::URLencode(extra_args)
    }

    if (is.null(mp_id) == TRUE) {
        stop("mp_id must not be empty", call. = FALSE)
    }

    if (is.null(session) == FALSE) {

        session_query <- paste0("&session=", session)

    } else {

        session_query <- NULL

    }

    dates <- paste0("&_properties=date&max-date=", as.Date(end_date), "&min-date=", as.Date(start_date))

    if (lobby == "aye") {

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="

        if(verbose==TRUE){message("Connecting to API")}

        url_aye <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, session_query, extra_args), flatten = TRUE)

        jpage <- floor(url_aye$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, session_query, extra_args, "&_page=",
                i), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.POSIXct(df$date._value)

    } else if (lobby == "no") {

        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="

        if(verbose==TRUE){message("Connecting to API")}

        url_no <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, session_query, extra_args), flatten = TRUE)

        jpage <- floor(url_no$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, session_query, extra_args, "&_page=",
                i), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

        df$date._datatype <- as.factor(df$date._datatype)
        df$date._value <- as.POSIXct(df$date._value)

    } else {

        message("Retrieving aye votes:")
        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/aye.json?mnisId="

        if(verbose==TRUE){message("Connecting to API")}

        url_aye <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, session_query, extra_args), flatten = TRUE)

        jpage <- floor(url_aye$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, session_query, extra_args, "&_page=",
                i), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df_aye <- tibble::as_tibble(dplyr::bind_rows(pages))

        df_aye$vote <- "aye"

        message("Retrieving no votes:")
        baseurl <- "http://lda.data.parliament.uk/commonsdivisions/no.json?mnisId="

        if(verbose==TRUE){message("Connecting to API")}

        url_no <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, session_query, extra_args), flatten = TRUE)

        jpage <- floor(url_no$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_pageSize=500", dates, session_query, extra_args, "&_page=",
                i), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df_no <- tibble::as_tibble(dplyr::bind_rows(pages))

        df_no$divisionNumber <- NULL

        df_no$vote <- "no"

        df <- rbind(df_aye, df_no)
        df$vote <- as.factor(df$vote)

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df$date._datatype <- "POSIXct"

            df$date._value <- as.POSIXct(df$date._value)

            df <- hansard_tidy(df, tidy_style)

        }

            df

    }

}


#' @rdname mp_vote_record
#' @export
hansard_mp_vote_record <- function(mp_id = NULL, lobby = "all", session = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- mp_vote_record(mp_id = mp_id, lobby = lobby, session = session, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
