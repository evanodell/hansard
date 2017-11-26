
#' All answered parliamentary questions
#'
#' Imports data on all answered parliamentary questions in the House of Commons
#' and/or in the House of Lords. The \code{mp_id}, \code{tabling_mp_id} and
#' \code{answering_body} parameters accept a single ID or department names,
#' or a list of IDs or department names, respectively.
#'
#' This is the most flexible of the various functions that look up questions,
#' as it queries all types of questions in both houses with a wide selection
#' of parameters: The member who asks the question, the member who answers
#' it and the relevant department can all be used to query the API.
#' \code{mp_id}, \code{tabling_mp_id} and \code{answering_body} all accept
#' lists of multiple relevant search parameters. This can be in the form of a
#' list, a data.frame column, a character vector, etc.
#'
#' @param mp_id Accepts a member ID or list of member IDs, and returns a tibble
#' with all available questions answered by that member. Includes both oral
#' and written questions, and includes members of the House of Commons and the
#' House of Lords. If \code{NULL}, returns a tibble with all available answered
#' questions, subject to other parameters. Defaults to \code{NULL}.
#' @param tabling_mp_id Accepts a member ID or list of member IDs, and returns
#' a tibble with all available questions asked by that member, subject to all
#' other parameters. Includes both oral and written questions, and includes
#' members of the House of Commons and the House of Lords. If \code{NULL},
#' returns a tibble with all available answered questions, subject to other
#' parameters. Defaults to \code{NULL}.
#' @param house The house to return questions from. Accepts either the short
#' name of the legislature (e.g. \code{'commons'} or \code{'lords'}) or the
#' ID of the legislature (1 for the House of Commons, 2 for the House of Lords).
#' The short names are not case sensitive. If \code{NULL}, returns answers from
#' both houses, subject to other parameters. Defaults to \code{NULL}.
#' @param answering_body The government department that answers the question.
#' Accepts either the short name name of a department (e.g. \code{'Education'}
#' for the Department for Education, \code{'Digital, Culture, Media and Sport'}
#' for the Department for Digital, Culture, Media and Sport), or the ID of a
#' particular department (e.g. 60 for the Department for Education). If
#' \code{NULL}, returns answers from all departments, subject to other
#' parameters. Defaults to \code{NULL}.
#' @param start_date The earliest date to include in the tibble. Accepts
#' character values in \code{'YYYY-MM-DD'} format, and objects of class
#' \code{Date}, \code{POSIXt}, \code{POSIXct}, \code{POSIXlt} or anything
#' else that can be coerced to a date with \code{as.Date()}.
#' Defaults to \code{'1900-01-01'}.
#' @param end_date The latest date to include in the tibble. Defaults to
#' \code{'1900-01-01'}. Accepts character values in \code{'YYYY-MM-DD'}
#' format, and objects of class \code{Date}, \code{POSIXt}, \code{POSIXct},
#' \code{POSIXlt} or anything else that can be coerced to a date with
#' \code{as.Date()}. Defaults to the current system date.
#' @param extra_args Additional parameters to pass to API.
#' See the \href{http://explore.data.parliament.uk/}{API documentation}
#' or the package vignette for more details. Defaults to \code{NULL}.
#' @param tidy Logical parameter. If \code{TRUE}, fixes the variable names
#' in the tibble to remove special characters and superfluous text, and
#' converts the variable names to a consistent style. Defaults to \code{TRUE}.
#' @param tidy_style The style to convert variable names to, if
#' \code{tidy = TRUE}. Accepts one of \code{'snake_case'}, \code{'camelCase'}
#' and \code{'period.case'}. Defaults to \code{'snake_case'}.
#' @param verbose If \code{TRUE}, returns data to console on the progress
#' of the API request. Defaults to \code{FALSE}.
#'
#' @return A tibble with details on all answered questions in the House
#' of Commons and the House of Lords.
#' @seealso \code{\link{commons_answered_questions}}
#' @seealso \code{\link{commons_oral_questions}}
#' @seealso \code{\link{commons_oral_question_times}}
#' @seealso \code{\link{commons_written_questions}}
#' @seealso \code{\link{lords_written_questions}}
#' @seealso \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#' x <- all_answered_questions(4019, start_date ='2017-01-01')
#'
#' y <- all_answered_questions(4019, start_date ='2017-01-01',
#'                                         tidy_style='camelCase')
#'
#' z <- hansard_all_answered_questions(tabling_mp_id=179, start_date ='2017-01-01')
#'
#' a <- hansard_all_answered_questions(house='lords', answering_body=60)
#' # Returns all questions asked in the House of Lords
#' # answered by the Department for Education.
#'
#' b <- hansard_all_answered_questions(house=2, answering_body='Education')
#' # Returns all questions asked in the House of Lords
#' # answered by the Department for Education.
#'
#' w <- hansard_all_answered_questions(mp_id = c(4019, 3980), tabling_mp_id = c(338, 172),
#'                                     answering_body = c('health', 'justice'),
#'                                      start_date = '2016-12-18', end_date = '2017-03-12')
#' # Accepts multiple inputs for mp_id, tabling_mp_id and answering_body
#' }

all_answered_questions <- function(mp_id = NULL, tabling_mp_id = NULL, house = NULL,
                                   answering_body = NULL, start_date = "1900-01-01",
                                   end_date = Sys.Date(), extra_args = NULL,
                                   tidy = TRUE, tidy_style = "snake_case",
                                   verbose = FALSE) {

    if (length(mp_id) > 1 ||
        length(tabling_mp_id) > 1 ||
        length(answering_body) > 1) {

        df <- aaq_multi(mp_id, tabling_mp_id, house, answering_body,
                        start_date, end_date, extra_args, verbose)

    } else {

        dates <- paste0("&_properties=date&max-date=",
                        as.Date(end_date), "&min-date=",
                        as.POSIXct(start_date))

        if (is.null(house) == TRUE) {
            # House query

            house_query <- NULL

        } else {

            if (is.numeric(house) == FALSE) {

                house <- tolower(house)

            }

            if (house == "commons" | house == 1 | house == "1") {

                house_query <- utils::URLencode("&legislature.prefLabel=House of Commons")

            } else if (house == "lords" | house == 2 | house == "2") {

                house_query <- utils::URLencode("&legislature.prefLabel=House of Lords")

            } else {

                house_query <- NULL

            }

        }

        if (is.null(mp_id) == TRUE || is.na(mp_id) == TRUE) { # member type queries

            answering_member_query <- NULL

        } else {

            answering_member_query <- paste0("&answer.answeringMember=http://data.parliament.uk/members/", mp_id)

        }

        if (is.null(tabling_mp_id) == TRUE || is.na(tabling_mp_id) == TRUE) {

            tabling_member_query <- NULL

        } else {

            tabling_member_query <- utils::URLencode(paste0("&tablingMember=http://data.parliament.uk/members/", tabling_mp_id))

        }

        answering_body_check <- suppressWarnings(as.numeric(as.character(answering_body)))  ## In case departmental IDs are passed as strings.

        if (is.null(answering_body) == TRUE || is.na(answering_body) == TRUE) {
            # Department query

            dept_query <- NULL

        } else if (is.na(answering_body_check) == FALSE) {

            dept_query <- paste0("&answeringDeptId=", answering_body)

        } else {

            dept_query <- utils::URLencode((paste0("&answeringDeptShortName=",
                                                   stringi::stri_trans_totitle(answering_body))))

        }

        baseurl <- "http://lda.data.parliament.uk/answeredquestions.json?"

        if (verbose == TRUE) {
            message("Connecting to API")
        }

        all <- jsonlite::fromJSON(paste0(baseurl, answering_member_query,
                                         tabling_member_query, house_query,
                                         dept_query, dates, extra_args, "&_pageSize=1"),
                                  flatten = TRUE)

        jpage <- floor(all$result$totalResults/500)

        query <- paste0(baseurl, answering_member_query,
                        tabling_member_query, house_query,
                        dept_query, dates, extra_args,
                        "&_pageSize=500&_page=")

        df <- loop_query(query, jpage, verbose) # in utils-loop.R

    }

    if (nrow(df) == 0 & verbose == TRUE) {

        message("The request did not return any data. Please check your search parameters.")

    } else {

        if (tidy == TRUE) {

            df <- aaq_tidy(df, tidy_style)  ### in utils-aaq.R

        }

        df

    }

}


#' @rdname all_answered_questions
#' @export
hansard_all_answered_questions <- all_answered_questions
