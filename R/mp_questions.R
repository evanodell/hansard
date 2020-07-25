
#' Questions asked by a given MP or MPs
#'
#' Accepts an ID number for a member of the House of Commons, and returns a
#' tibble of of all their oral and written questions.
#'
#' @param mp_id The ID number of a member of the House of Commons, or a vector
#' of IDs. Defaults to `NULL`.
#'
#' @param question_type Accepts the arguments `'all'`, `'oral'` and
#' `'written'`. This parameter is not case sensitive.
#' Defaults to `'all'`.
#'
#' @param start_date Only includes questions answered on or after this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to `'1900-01-01'`.
#'
#' @param end_date Only includes questions answered on or before this date.
#' Accepts character values in `'YYYY-MM-DD'` format, and objects of
#' class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or
#' anything else that can be coerced to a date with `as.Date()`.
#' Defaults to the current system date.
#' @inheritParams all_answered_questions
#' @return A tibble with details on all questions asked by a
#' member of the House of Commons.
#'
#' @seealso [all_answered_questions()]
#' @seealso [commons_answered_questions()]
#' @seealso [commons_oral_questions()]
#' @seealso [commons_oral_question_times()]
#' @seealso [commons_written_questions()]
#' @seealso [lords_written_questions()]
#'
#' @export
#' @examples
#' \dontrun{
#' x <- mp_questions(c(172, 3967), "all")
#'
#' y <- mp_questions(mp_id = 172, question_type = "all")
#'
#' z <- mp_questions(c(172, 3967), "written")
#' }
#'
mp_questions <- function(mp_id = NULL, question_type = "all",
                         start_date = "1900-01-01", end_date = Sys.Date(),
                         extra_args = NULL, tidy = TRUE,
                         tidy_style = "snake", verbose = TRUE) {
  if (is.null(mp_id)) {
    stop("mp_id must not be empty", call. = FALSE)
  }

  question_type <- tolower(question_type)

  if (length(mp_id) > 1) {
    # for lists of more than 1 ID

    df <- mp_question_multi(
      mp_id = mp_id, question_type = question_type,
      start_date = start_date, end_date = end_date,
      extra_args = extra_args, verbose = verbose
    )
  } else {
    if (question_type == "all") {
      veb(verbose)

      df_oral <- hansard::mp_questions(
        mp_id = mp_id,
        question_type = "oral",
        start_date = start_date,
        end_date = end_date,
        extra_args = extra_args,
        tidy = FALSE,
        tidy_style = tidy_style
      )

      if (verbose) {
        message("Retrieving written questions")
      }

      df_writ <- hansard::mp_questions(
        mp_id = mp_id,
        question_type = "written",
        start_date = start_date,
        end_date = end_date,
        extra_args = extra_args,
        tidy = FALSE,
        tidy_style = tidy_style
      )

      if (verbose) {
        message("Combining oral and written questions")
      }

      if (is.null(df_oral)) {
        df <- df_writ
      } else if (is.null(df_writ)) {
        df <- df_oral
      } else {
        common <- intersect(colnames(df_writ), colnames(df_oral))

        df <- rbind(
          subset(df_writ, select = common),
          subset(df_oral, select = common)
        )
      }

      df
    } else if (question_type == "oral") {
      dates <- paste0(
        "&_properties=AnswerDate&max-AnswerDate=",
        as.Date(end_date),
        "&min-AnswerDate=",
        as.Date(start_date)
      )

      query <- paste0(
        url_util, "commonsoralquestions.json?mnisId=",
        mp_id, dates, extra_args
      )

      oral <- jsonlite::fromJSON(paste0(
        query, "&_pageSize=1"
      ))



      df <- loop_query(query, verbose) # in utils-loop.R
    } else if (question_type == "written") {
      dates <- paste0(
        "&_properties=dateTabled&max-dateTabled=",
        as.Date(end_date),
        "&min-dateTabled=",
        as.Date(start_date)
      )

      query <- paste0(
        url_util, "commonswrittenquestions.json?mnisId=",
        mp_id, dates, extra_args
      )

      wr <- jsonlite::fromJSON(query)



      pages <- list()

      df <- loop_query(query, verbose) # in utils-loop.R
    }
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy) {
      df <- mp_question_tidy(df, tidy_style)
    }

    df
  }
}


#' @rdname mp_questions
#' @export
hansard_mp_questions <- mp_questions
