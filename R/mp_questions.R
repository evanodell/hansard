
#' Oral and written questions asked by MPs
#'
#' Accepts an ID number for a member of the House of Commons, and returns a tibble of of all their oral and written questions.
#'
#' @param mp_id The ID number of a member of the House of Commons. Defaults to \code{NULL}.
#' @param question_type Accepts the arguments \code{'all'}, \code{'oral'} and \code{'written'}. Defaults to \code{'all'}.
#' @param start_date The earliest date to include in the tibble. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to \code{'1900-01-01'}.
#' @param end_date The latest date to include in the tibble. Defaults to \code{'1900-01-01'}. Accepts character values in \code{'YYYY-MM-DD'} format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything else than can be coerced to a date with \code{as.Date()}. Defaults to the current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to \code{NULL}.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'} and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress of the API request. Defaults to \code{FALSE}.
#' @return A tibble with details on all questions asked by a member of the House of Commons.
#'
#' @seealso \code{\link{all_answered_questions}}
#' @seealso \code{\link{commons_answered_questions}}
#' @seealso \code{\link{commons_oral_questions}}
#' @seealso \code{\link{commons_oral_question_times}}
#' @seealso \code{\link{commons_written_questions}}
#' @seealso \code{\link{lords_written_questions}}
#'
#' @export
#' @examples \dontrun{
#' x <- mp_questions(172, 'all')
#' }

mp_questions <- function(mp_id = NULL, question_type = "all", start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose = FALSE) {

  if (is.null(mp_id) == TRUE) {
    stop("mp_id must not be empty", call. = FALSE)
  }

    question_type <- tolower(question_type)

    if (length(mp_id) > 1) {#for vectors of more than 1 ID

      df <- mp_question_multi(mp_id=mp_id, question_type=question_type, start_date = start_date, end_date = end_date, extra_args = extra_args, verbose = verbose)

    } else {


    if (question_type == "all") {
      if (verbose == TRUE) {
        message("Retrieving oral questions")
      }

      df_oral <- hansard::mp_questions(mp_id = mp_id, question_type = "oral", start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = FALSE, tidy_style = tidy_style)

      if (verbose == TRUE) {
        message("Retrieving written questions")
      }

      df_writ <- hansard::mp_questions(mp_id = mp_id, question_type = "written", start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = FALSE, tidy_style = tidy_style)

      if (verbose == TRUE) {
        message("Combining oral and written questions")
      }

      if (is.null(df_oral)) {

        df <- df_writ

      } else if (is.null(df_writ)) {

        df <- df_oral

      } else {

        common <- intersect(colnames(df_writ), colnames(df_oral))

        df <- rbind(subset(df_writ, select = common), subset(df_oral, select = common))

      }

      df

    } else if (question_type == "oral") {

      dates <- paste0("&_properties=AnswerDate&max-AnswerDate=", as.Date(end_date), "&min-AnswerDate=", as.Date(start_date))

      baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json?mnisId="

      oral <- jsonlite::fromJSON(paste0(baseurl_oral, mp_id, dates, extra_args))

      jpage <- floor(oral$result$totalResults/500)

      pages <- list()

      for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_oral, mp_id, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        if (verbose == TRUE) {
          message("Retrieving page ", i + 1, " of ", jpage + 1)
        }
        pages[[i + 1]] <- mydata$result$items
      }

      df <- tibble::as_tibble(dplyr::bind_rows(pages))

    } else if (question_type == "written") {

      baseurl <- "http://lda.data.parliament.uk/commonswrittenquestions.json?mnisId="

      dates <- paste0("&_properties=dateTabled&max-dateTabled=", as.Date(end_date), "&min-dateTabled=", as.Date(start_date))

      writ <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, extra_args))

      jpage <- floor(writ$result$totalResults/500)

      pages <- list()

      for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        if (verbose == TRUE) {
          message("Retrieving page ", i + 1, " of ", jpage + 1)
        }
        pages[[i + 1]] <- mydata$result$items
      }

      df <- tibble::as_tibble(dplyr::bind_rows(pages))
    }

  }

  if (nrow(df)==0 && verbose == TRUE) {
    message("The request did not return any data. Please check your search parameters.")
  } else {

    if (tidy == TRUE) {

      df <- mp_question_tidy(df, tidy_style)

    }

    df

  }
}


#' @rdname mp_questions
#' @export
hansard_mp_questions <- function(mp_id = NULL, question_type = "all", start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- mp_questions(mp_id = mp_id, question_type = question_type, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
