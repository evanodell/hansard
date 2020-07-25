
#' All answered parliamentary questions
#'
#' @description Imports data on all answered parliamentary questions in the
#' House of Commons and/or in the House of Lords. The `mp_id`,
#' `tabling_mp_id` and `answering_body` parameters accept a single
#' ID or department names, or a list of IDs or department names, respectively.
#'
#' @description This is the most flexible of the various functions that look
#' up questions, as it queries all types of questions in both houses with a
#' wide selection of parameters: The member who asks the question, the member
#' who answers it and the relevant department can all be used to query the API.
#' `mp_id`, `tabling_mp_id` and `answering_body` all accept
#' lists of multiple relevant search parameters. This can be in the form of a
#' list, a data.frame column, a character vector, etc.
#'
#' @param mp_id Accepts a member ID or vector of member IDs, and returns a
#' tibble with all available questions answered by that member. Includes both
#' oral and written questions, and includes members of the House of Commons
#' and the House of Lords. If `NULL`, returns a tibble with all available
#' answered questions, subject to other parameters. Defaults to `NULL`.
#'
#' @param tabling_mp_id Accepts a member ID or vector of member IDs, and
#' returns a tibble with all available questions asked by that member,
#' subject to all other parameters. Includes both oral and written questions,
#' and includes members of the House of Commons and the House of Lords. If
#' `NULL`, returns a tibble with all available answered questions,
#' subject to other parameters. Defaults to `NULL`.
#'
#' @param house The house to return questions from. Accepts either the short
#' name of the legislature (e.g. `'commons'` or `'lords'`) or the
#' ID of the legislature (`1` for the House of Commons, `2` for the
#' House of Lords). The short names are not case sensitive.
#' If `NULL`, returns answers from both houses, subject to other
#' parameters. Defaults to `NULL`.
#'
#' @param answering_body The name of the government department that answers the
#' question, or a vector of government deparment names. Accepts either the
#' short name name of a department (e.g. `'Education'` for the Department
#' for Education, `'Digital, Culture, Media and Sport'` for the Department
#' for Digital, Culture, Media and Sport), or the ID of a particular department
#' (e.g. 60 for the Department for Education). If `NULL`, returns answers
#' from all departments, subject to other parameters. Defaults to `NULL`.
#'
#' @param start_date The earliest date to include in the tibble. Accepts
#' character values in `'YYYY-MM-DD'` format, and objects of class
#' `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else
#' that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#'
#' @param end_date The latest date to include in the tibble. Defaults to
#' `'1900-01-01'`. Accepts character values in `'YYYY-MM-DD'`
#' format, and objects of class `Date`, `POSIXt`, `POSIXct`,
#' `POSIXlt` or anything else that can be coerced to a date with
#' `as.Date()`. Defaults to the current system date.
#'
#' @param extra_args Additional parameters and queries to pass to API. These
#' queries must be strings and start with "&". See the
#' \href{http://explore.data.parliament.uk/}{API documentation}
#' or the package vignette for more details. Defaults to `NULL`.
#'
#' @param tidy Logical parameter. If `TRUE`, fixes the variable names
#' in the tibble to remove special characters and superfluous text, and
#' converts the variable names to a consistent style. Defaults to `TRUE`.
#'
#' @param tidy_style The style to convert variable names to, if
#' `tidy = TRUE`. Accepts any style accepted by \link[snakecase]{to_any_case}.
#' Defaults to `'snake'`.
#'
#' @param verbose If `TRUE`, displayes messages on the console on the
#' progress of the API request. Defaults to `TRUE`.
#'
#' @return A tibble with details on all answered questions in the House
#' of Commons and the House of Lords.
#' @seealso [commons_answered_questions()]
#' @seealso [commons_oral_questions()]
#' @seealso [commons_oral_question_times()]
#' @seealso [commons_written_questions()]
#' @seealso [lords_written_questions()]
#' @seealso [mp_questions()]
#' @export
#' @examples
#' \dontrun{
#' # All questions answered by Nicola Blackwood from 1 January 2017 onwards
#' x <- all_answered_questions(4019, start_date = "2017-01-01")
#'
#' # All questions answered by Nicola Blackwood from 1 January 2017 onwards
#' # returns variables in camelCase style
#' y <- all_answered_questions(4019,
#'   start_date = "2017-01-01",
#'   tidy_style = "small_camel"
#' )
#'
#' # All questions asked by Andrew Dismore from 1 January 2017 onwards
#' z <- hansard_all_answered_questions(
#'   tabling_mp_id = 179,
#'   start_date = "2017-01-01"
#' )
#'
#' # Return all questions asked in the House of Lords
#' # answered by the Department for Education.
#' a <- hansard_all_answered_questions(house = "lords", answering_body = 60)
#'
#' # Returns all questions asked in the House of Lords
#' # answered by the Department for Education.
#' b <- hansard_all_answered_questions(house = 2, answering_body = "Education")
#'
#' # Accepts multiple inputs for mp_id, tabling_mp_id and answering_body
#' w <- hansard_all_answered_questions(
#'   mp_id = c(4019, 3980),
#'   tabling_mp_id = c(338, 172),
#'   answering_body = c("health", "justice"),
#'   start_date = "2016-12-18",
#'   end_date = "2017-03-12"
#' )
#' }
#'
all_answered_questions <- function(mp_id = NULL, tabling_mp_id = NULL,
                                   house = NULL, answering_body = NULL,
                                   start_date = "1900-01-01",
                                   end_date = Sys.Date(), extra_args = NULL,
                                   tidy = TRUE, tidy_style = "snake",
                                   verbose = TRUE) {
  if (length(mp_id) > 1 ||
    length(tabling_mp_id) > 1 ||
    length(answering_body) > 1) {
    df <- aaq_multi(
      mp_id, tabling_mp_id, house, answering_body,
      start_date, end_date, extra_args, verbose
    )
  } else {
    dates <- paste0(
      "&_properties=date&max-date=",
      as.Date(end_date),
      "&min-date=",
      as.POSIXct(start_date)
    )

    if (is.null(house)) {
      house_query <- ""
    } else if (tolower(house) == "commons" | house == "1") {
      house_query <- "&legislature.prefLabel=House%20of%20Commons"
    } else if (tolower(house) == "lords" | house == "2") {
      house_query <- "&legislature.prefLabel=House%20of%20Lords"
    } else {
      house_query <- ""
    }

    if (is.null(mp_id) || is.na(mp_id)) {
      answering_member_query <- ""
    } else {
      answering_member_query <- paste0(
        "&answer.answeringMember=http://data.parliament.uk/members/",
        mp_id
      )
    }

    if (is.null(tabling_mp_id) || is.na(tabling_mp_id)) {
      tabling_member_query <- ""
    } else {
      tabling_member_query <- paste0(
        "&tablingMember=http://data.parliament.uk/members/", tabling_mp_id
      )
    }

    answering_body_check <- suppressWarnings(
      as.numeric(as.character(answering_body))
    )
    ## In case departmental IDs are passed as strings.

    if (is.null(answering_body) || is.na(answering_body)) {
      dept_query <- ""
    } else if (!is.na(answering_body_check)) {
      dept_query <- paste0("&answeringDeptId=", answering_body)
    } else {
      dept_query <- utils::URLencode(
        paste0(
          "&answeringDeptShortName=",
          gsub("\\b([[:lower:]])([[:lower:]]+)", "\\U\\1\\L\\2",
            tolower(answering_body),
            perl = TRUE
          )
        )
      )
    }

    dept_query <- gsub("And", "and", dept_query)
    dept_query <- gsub("Of", "of", dept_query)
    dept_query <- gsub("For", "for", dept_query)

    query <- paste0(
      url_util, "answeredquestions.json?", answering_member_query,
      tabling_member_query, house_query, dept_query, dates, extra_args
    )

    df <- loop_query_aaq(query, verbose) # in utils-loop.R
  }

  if (nrow(df) == 0 & verbose) {
    message("The request did not return any data.
              Please check your parameters.")
  } else {
    if (tidy) {
      df <- aaq_tidy(df, tidy_style) ### in utils-aaq.R
    }

    df
  }
}


#' @rdname all_answered_questions
#' @export
hansard_all_answered_questions <- all_answered_questions
