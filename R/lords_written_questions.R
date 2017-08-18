

#' House of Lords written questions
#'
#' Imports data on House of Lords written questions asked by a given peer(s), and/or directed to a given department.
#' @param peer_id Accepts a member ID or array of IDs, and returns a tibble with all written questions asked by that member. If \code{NULL}, peer_id is not included in the query to the API. Defaults to \code{NULL}.
#' @param answering_department Accepts a string with a department name or partial name or an array of departmental names, and returns all written questions by that department. The query acts as a search, so entering \code{'health'} will return all questions answered by the Department of Health. If \code{NULL}, answering_department is not included as a query parameter. Defaults to \code{NULL}.
#' @param start_date The earliest date to include in the tibble. Defaults to \code{'1900-01-01'}. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to \code{'1900-01-01'}. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return  A tibble with details on written questions in the House of Lords.
#'
### @keywords House of Lords Written Questions
#' @seealso \code{\link{all_answered_questions}}
#' @seealso \code{\link{commons_answered_questions}}
#' @seealso \code{\link{commons_oral_questions}}
#' @seealso \code{\link{commons_oral_question_times}}
#' @seealso \code{\link{commons_written_questions}}
#' @seealso \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#' # Returns all written questions ever
#' x <- lords_written_questions()
#'
#' x <- lords_written_questions(peer_id = c(3526,4176), answering_department = c('cabinet', 'Transport'))
#'
#' }

lords_written_questions <- function(peer_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  if (length(answering_department)> 1 || length(peer_id) > 1 )  {

    df <- lwq_multi(answering_department, peer_id, start_date, end_date, extra_args, verbose)### in utils-lords.R

  } else {

    dates <- paste0("&_properties=dateTabled&max-dateTabled=", as.Date(end_date), "&min-dateTabled=", as.Date(start_date))

    if (is.null(peer_id) == FALSE && is.na(peer_id)==FALSE) {

        peer_id <- utils::URLencode(paste0("&tablingMember=http://data.parliament.uk/members/", peer_id))

    } else{

      peer_id <- NULL

    }

    if (is.null(answering_department) == FALSE  && is.na(answering_department) == FALSE) {

        query <- "/answeringdepartment"

        answering_department <- utils::URLencode(paste0("q=", answering_department))

    } else {

        query <- NULL

    }

    baseurl <- "http://lda.data.parliament.uk/lordswrittenquestions"

    if(verbose==TRUE){message("Connecting to API")}

    writ <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, peer_id, dates, extra_args), flatten = TRUE)

    jpage <- floor(writ$result$totalResults/500)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, peer_id, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

  }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- lwq_tidy(df, tidy_style) ## in utils-lords.R

        }

      df

    }

}

#' @rdname lords_written_questions
#' @export
hansard_lords_written_questions <- function(peer_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- lords_written_questions(peer_id = peer_id, answering_department = answering_department, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
