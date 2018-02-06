
#' Questions asked by a given MP or MPs
#'
#' Accepts an ID number for a member of the House of Commons, and returns a
#' tibble of of all their oral and written questions.
#'
#' @param mp_id The ID number of a member of the House of Commons, or a vector
#' of IDs. Defaults to \code{NULL}.
#' @param question_type Accepts the arguments \code{'all'}, \code{'oral'} and
#' \code{'written'}. This parameter is not case sensitive.
#' Defaults to \code{'all'}.
#' @param start_date Only includes questions answered on or after this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#' @param end_date Only includes questions answered on or before this date.
#' Accepts character values in \code{'YYYY-MM-DD'} format, and objects of
#' class \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or
#' anything else that can be coerced to a date with \code{as.Date()}.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on all questions asked by a
#' member of the House of Commons.
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
#' x <- mp_questions(c(172,3967) 'all')
#'
#' y <- mp_questions(172, 'all')
#'
#' z <- mp_questions(c(172,3967) 'written')
#' }

mp_questions <- function(mp_id = NULL, question_type = "all",
                         start_date = "1900-01-01", end_date = Sys.Date(),
                         extra_args = NULL, tidy = TRUE,
                         tidy_style = "snake_case", verbose = TRUE) {

    if (is.null(mp_id) == TRUE) {

        stop("mp_id must not be empty", call. = FALSE)

    }

    question_type <- tolower(question_type)

    if (length(mp_id) > 1) {
        # for lists of more than 1 ID

        df <- mp_question_multi(mp_id = mp_id, question_type = question_type,
                                start_date = start_date, end_date = end_date,
                                extra_args = extra_args, verbose = verbose)

    } else {

        if (question_type == "all") {

            if (verbose == TRUE) {
                message("Retrieving oral questions")
            }

            df_oral <- hansard::mp_questions(mp_id = mp_id,
                                             question_type = "oral",
                                             start_date = start_date,
                                             end_date = end_date,
                                             extra_args = extra_args,
                                             tidy = FALSE,
                                             tidy_style = tidy_style)

            if (verbose == TRUE) {
                message("Retrieving written questions")
            }

            df_writ <- hansard::mp_questions(mp_id = mp_id,
                                             question_type = "written",
                                             start_date = start_date,
                                             end_date = end_date,
                                             extra_args = extra_args,
                                             tidy = FALSE,
                                             tidy_style = tidy_style)

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

            dates <- paste0("&_properties=AnswerDate&max-AnswerDate=",
                            as.Date(end_date),
                            "&min-AnswerDate=",
                            as.Date(start_date))

            baseurl <- "http://lda.data.parliament.uk/commonsoralquestions.json?mnisId="

            oral <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates,
                                              extra_args, "&_pageSize=1"))

            jpage <- floor(oral$result$totalResults/500)

            query <- paste0(baseurl, mp_id, dates, extra_args,
                            "&_pageSize=500&_page=")

            df <- loop_query(query, jpage, verbose) # in utils-loop.R

        } else if (question_type == "written") {

            baseurl <- "http://lda.data.parliament.uk/commonswrittenquestions.json?mnisId="

            dates <- paste0("&_properties=dateTabled&max-dateTabled=",
                            as.Date(end_date),
                            "&min-dateTabled=",
                            as.Date(start_date))

            wr <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, extra_args))

            jpage <- floor(wr$result$totalResults/500)

            pages <- list()

            query <- paste0(baseurl, mp_id, dates, extra_args,
                            "&_pageSize=500&_page=")

            df <- loop_query(query, jpage, verbose) # in utils-loop.R
        }

    }

    if (nrow(df) == 0) {

        message("The request did not return any data. Please check your parameters.")

    } else {

        if (tidy == TRUE) {

            df <- mp_question_tidy(df, tidy_style)

        }

        df

    }
}


#' @rdname mp_questions
#' @export
hansard_mp_questions <- mp_questions
