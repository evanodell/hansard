



#' Bill Publications
#'
#' Returns details all publications associated with a specific bill or
#' all bills.
#'
#' @param ID The ID of a specific bill to return publications for.
#' If \code{NULL}, returns all bill publications subject to other parameters.
#' @param start_date Only includes bill publications on or after this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes bill publicationson or before this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#'
#' @return A tibble with details on bill publications.
#' @export
#'
#' @examples \dontrun{
#'
#' x <- bill_publications(ID=752025)
#'
#' }
bill_publications <- function(ID=NULL, start_date = "1900-01-01",
                              end_date = Sys.Date(), extra_args = NULL,
                              tidy = TRUE, tidy_style = "snake_case",
                              verbose = FALSE){

  baseurl <- "http://lda.data.parliament.uk/billpublications.json?"

  dates <- paste0("&max-date=",
                    as.Date(end_date),
                    "&min-date=",
                    as.Date(start_date))

  bill_id <- ifelse(is.null(ID)==TRUE,
                    "",
                    paste0("&bill=http://data.parliament.uk/resources/", ID))

  if (verbose == TRUE) {
        message("Connecting to API")
    }


  bills <- jsonlite::fromJSON(paste0(baseurl, bill_id, dates,
                                     extra_args, "&_pageSize=1"),
                                flatten = TRUE)

  jpage <- floor(bills$result$totalResults/500)

  query <- paste0(baseurl, bill_id, dates, extra_args,
                  "&_pageSize=500&_page=")

  df <- loop_query(query, jpage, verbose) # in utils-loop.R

    if (nrow(df) == 0) {

        message("The request did not return any data. Please check your parameters.")

    } else {

        if (tidy == TRUE) {

            df <- bills_tidy(df, tidy_style)  ### in utils-bills.R

        }

        df

    }


}
