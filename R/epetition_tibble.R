
#' Bulk Epetition data
#'
#' Get data on all epetitions submitted to parliament, with the label, sponsor, number of signatures, date created and epetition ID. For greater detail on indidivual epetitions, see \code{\link{epetition}}.
#'
#' @param min_signatures The minimum number of signatures required for inclusion in the tibble. Defaults to 1.
#' @param max_signatures The maximum number of signatures required for inclusion in the tibble. If \code{NULL}, there is no maximum number of signatures. Defaults to \code{NULL}.
#' @param status The status of the petition, either \code{'open'} or \code{'closed'}. If \code{NULL}, returns all petitions both open and closed. Defaults to \code{NULL}.
#' @param start_date Only includes epetitions created on or after this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes epetitions created on or before this date. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on electronic petitions submitted to parliament.
#' @seealso \code{\link{epetition}}
#'
#' @export
#' @examples \dontrun{
#' x <- epetition_tibble()
#'
#' y <- epetition_tibble(max_signatures=500)
#'
#' z <- epetition_tibble(start_date="2016-12-01", end_date="2017-04-25")
#'}


epetition_tibble <- function(min_signatures=1, max_signatures=NULL, status=NULL, start_date="1900-01-01", end_date=Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  dates <- paste0("&max-created=", as.Date(end_date), "&min-created=", as.Date(start_date))

  if(is.null(status)==TRUE){

    status_query <- NULL

  } else {

    status_query <- paste0("&status=", status)

  }

  if(is.null(max_signatures)==TRUE){

    sig_query <- paste0("&min-numberOfSignatures=", min_signatures)

  } else {

    sig_query <- paste0("&min-numberOfSignatures=", min_signatures, "&max-numberOfSignatures=", max_signatures)

  }

  baseurl <- "http://lda.data.parliament.uk/epetitions.json?"

  if(verbose==TRUE){message("Connecting to API")}

  petition <- jsonlite::fromJSON(paste0(baseurl, status_query, sig_query, dates, extra_args), flatten = TRUE)

  jpage <- floor(petition$result$totalResults/500)

  pages <- list()

  for (i in 0:jpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl, status_query, sig_query, dates, extra_args, "&_pageSize=500&_page=", i), flatten = TRUE)
    if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
    pages[[i + 1]] <- mydata$result$items
  }

  df <- tibble::as_tibble(dplyr::bind_rows(pages))

  if (nrow(df) == 0 && verbose==TRUE) {

    message("The request did not return any data. Please check your search parameters.")

  } else {

    if (tidy == TRUE) {

      df <- epetition_tibble_tidy(df, tidy_style) ## in utils-epetition.R

    }

    df

  }
}


#' @rdname epetition_tibble
#' @export

hansard_epetition_tibble <- epetition_tibble
