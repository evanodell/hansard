
#' Commons oral question times
#'
#' Imports data on House of Commons oral question times. Query with parameters
#' for the parliamentary session or the question ID. If `tidy=TRUE`,
#' datetime variables are converted to `POSIXct` class.
#'
#'
#' @param session Accepts a session in format `yyyy/yy`
#' (e.g. `"2016/17"`) and returns a tibble of all oral question times from
#' that session. Defaults to `NULL`.
#'
#' @param question_id Accepts a question time ID, and returns a tibble of
#' that question time.
#' @inheritParams all_answered_questions
#' @return A tibble with information on oral question times in the House of
#' Commons.
#' @seealso [all_answered_questions()]
#' @seealso [commons_answered_questions()]
#' @seealso [commons_oral_questions()]
#' @seealso [commons_written_questions()]
#' @seealso [lords_written_questions()]
#' @seealso [mp_questions()]
#' @export
#' @examples
#' \dontrun{
#' x <- commons_oral_question_times(session = "2016/17", question_id = "685697")
#' }
#'
commons_oral_question_times <- function(session = NULL, question_id = NULL,
                                        extra_args = NULL, tidy = TRUE,
                                        tidy_style = "snake",
                                        verbose = TRUE) {
  session_query <- ifelse(is.null(session) == FALSE,
    utils::URLencode(paste0("session=", session)), ""
  )

  question_query <- ifelse(
    is.null(question_id) == FALSE,
    paste0("/", question_id),
    ""
  )

  baseurl <- paste0(url_util, "commonsoralquestiontimes")

  if (verbose == TRUE) {
    message("Connecting to API")
  }

  if (is.null(question_id) == TRUE) {
    times <- jsonlite::fromJSON(paste0(
      baseurl, ".json?",
      session_query,
      extra_args, "&_pageSize=1"
    ),
    flatten = TRUE
    )

    jpage <- floor(times$result$totalResults / 100)

    query <- paste0(baseurl, ".json?", session_query, extra_args)

    df <- loop_query(query, jpage, verbose) # in utils-loop.R
  } else {
    mydata <- jsonlite::fromJSON(paste0(
      baseurl, question_query,
      ".json?", session_query,
      extra_args
    ),
    flatten = TRUE
    )

    df <- tibble::tibble(
      about = mydata$result$primaryTopic$`_about`,
      AnswerBody = list(mydata$result$primaryTopic$AnswerBody),
      session = mydata$result$primaryTopic$session,
      title = mydata$result$primaryTopic$title,
      AnswerDateTime._value =
        mydata$result$primaryTopic$AnswerDateTime$`_value`,
      AnswerDateTime._datatype =
        mydata$result$primaryTopic$AnswerDateTime$`_datatype`,
      Location._about = mydata$result$primaryTopic$Location$`_about`,
      Location.prefLabel._value =
        mydata$result$primaryTopic$Location$prefLabel$`_value`,
      QuestionType._value =
        mydata$result$primaryTopic$QuestionType$`_value`,
      date._value = mydata$result$primaryTopic$date$`_value`,
      date._datatype = mydata$result$primaryTopic$date$`_datatype`,
      modified._value = mydata$result$primaryTopic$modified$`_value`,
      modified._datatype = mydata$result$primaryTopic$modified$`_datatype`,
      sessionNumber._value =
        mydata$result$primaryTopic$sessionNumber$`_value`
    )
  }

  if (nrow(df) == 0) {
    message("The request did not return any data.
                Please check your parameters.")
  } else {
    if (tidy == TRUE) {
      df <- coqt_tidy(df, tidy_style) ## in utils-commons.R
    }

    df
  }
}

#' @rdname commons_oral_question_times
#' @export

hansard_commons_oral_question_times <- commons_oral_question_times
