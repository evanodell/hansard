
#' All answered parliamentary questions.
#'
#' Imports data on all answered parliamentary questions in the House of Commons and/or in the House of Lords.
#' @param mp_id Accepts a member ID, and returns a tibble with all available questions answered by that member. Includes both oral and written questions, and includes members of the House of Commons and the House of Lords. If NULL, returns a tibble with all available answered questions. Defaults to NULL.
#' @param tabling_mp_id Accepts a member ID, and returns a tibble with all available questions asked by that member, subject to all other parameters. Includes both oral and written questions, and includes members of the House of Commons and the House of Lords. If NULL, returns a tibble with all available answered questions. Defaults to NULL.
#' @param house The house to return questions from. Accepts either the short name of the legislature (e.g. 'commons' or 'lords') or the ID of the legislature (1 for the House of Commons, 2 for the House of Lords). The short names are not case sensitive. If NULL, returns answers from both houses, subject to other parameters. Defaults to NULL.
#' @param answering_body The government department that answers the question. Accepts either the short name name of a department (e.g. 'Education' for the Department for Education), or the ID of a particular department (e.g. 60 for education.) Note that if using departmental short names the API is case sensitive, so 'Education' will work but 'education' will not return any data. If NULL, returns answers from all departments, subject to other parameters. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on all answered questions in the House of Commons and the House of Lords.
#' @keywords Answered Questions
#' @seealso \code{\link{commons_answered_questions}}
#' @seealso \code{\link{commons_oral_questions}}
#' @seealso \code{\link{commons_oral_question_times}}
#' @seealso \code{\link{commons_written_questions}}
#' @seealso \code{\link{lords_written_questions}}
#' @seealso \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#'
#' x <- all_answered_questions(4019, start_date ='2017-01-01')
#'
#' y <- all_answered_questions(4019, start_date ='2017-01-01', tidy_style='camelCase')
#'
#' z <- hansard_all_answered_questions(tabling_mp_id=179, start_date ='2017-04-01')
#'
#' a <- hansard_all_answered_questions(house="lords", answering_body=60)
#' # Returns all questions asked in the House of Lords answered by the Department for Education.
#'
#' b <- hansard_all_answered_questions(house=2, answering_body="Education")
#' # Returns all questions asked in the House of Lords answered by the Department for Education.
#'
#' }

all_answered_questions <- function(mp_id = NULL, tabling_mp_id = NULL, house = NULL, answering_body=NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

    dates <- paste0("&_properties=date&max-date=", as.Date(end_date), "&min-date=", as.POSIXct(start_date))

    if (length(mp_id) > 1) {

      df <- aaq_multi(mp_id, tabling_mp_id, house, answering_body, start_date, end_date, extra_args)

    } else {

    # House query
    if (is.null(house) == TRUE) {

      house_query <- NULL

    } else {

    if(is.numeric(house)==FALSE) {

      house <- tolower(house)

    }

    if (house == "commons" | house==1) {

      house_query <- utils::URLencode("&legislature.prefLabel=House of Commons")

    } else if (house == "lords" | house==2) {

      house_query <- utils::URLencode("&legislature.prefLabel=House of Lords")

    } else {

      house_query <- NULL

    }

    }

    # Department query
    if (is.null(answering_body) == TRUE) {

      dept_query <- NULL

    } else if (is.numeric(answering_body)) {

      dept_query <- paste0("&answeringDeptId=", answering_body)

    } else {

      dept_query <- paste0("&answeringDeptShortName=", answering_body)

    }

    if (is.null(mp_id) == TRUE & is.null(tabling_mp_id) == TRUE) {

        baseurl <- "http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500"

        message("Connecting to API")

        all <- jsonlite::fromJSON(paste0(baseurl, house_query, dept_query, dates, extra_args), flatten = TRUE)

        jpage <- floor(all$result$totalResults/all$result$itemsPerPage)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, house_query, dept_query, dates, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (is.null(mp_id) == TRUE & is.null(tabling_mp_id) == FALSE) {

        mem <- suppressMessages(members(tabling_mp_id))

        tabler <- paste0("&tablingMemberPrinted=", utils::URLencode(as.character(mem$full_name[[1]])))

        baseurl <- "http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500&mnisId="

        message("Connecting to API")

        all <- jsonlite::fromJSON(paste0(baseurl, mp_id, tabler, house_query, dept_query, dates, extra_args))

        jpage <- floor(all$result$totalResults/all$result$itemsPerPage)

        jpage2 <- round(all$result$totalResults/all$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, house_query, dept_query, tabler, "&_page=", i, dates, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else {

      if (is.null(tabling_mp_id) == FALSE) {

        mem <- suppressMessages(members(tabling_mp_id))

        tabler <- paste0("&tablingMemberPrinted=", utils::URLencode(as.character(mem$full_name[[1]])))

      } else {

        tabler <- NULL

      }

      baseurl <- "http://lda.data.parliament.uk/questionsanswers.json?_pageSize=500&mnisId="

      message("Connecting to API")

      all <- jsonlite::fromJSON(paste0(baseurl, mp_id, tabler, house_query, dept_query, dates, extra_args))

      jpage <- floor(all$result$totalResults/all$result$itemsPerPage)

      jpage2 <- round(all$result$totalResults/all$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, tabler, "&_page=", i, house_query, dept_query, dates, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
      }

    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            names(df) <- gsub("answer.answeringMember.fullName._value", "answeringMember.fullName._value", names(df))

            names(df) <- gsub("answer.answeringMember._about", "answeringMember._about", names(df))

            names(df) <- gsub("answer.answerText._value", "answerText._value", names(df))

            names(df) <- gsub("answer.dateOfAnswer._datatype", "dateOfAnswer._datatype", names(df))

            names(df) <- gsub("answer.dateOfAnswer._value", "dateOfAnswer._value", names(df))

            df$dateOfAnswer._value <- as.POSIXct(df$dateOfAnswer._value)

            df$answeringMember._about <- gsub("http://data.parliament.uk/members/", "", df$answeringMember._about)

            df$tablingMember._about <- gsub("http://data.parliament.uk/members/", "", df$tablingMember._about)

            df$AnsweringBody <- unlist(df$AnsweringBody)

            df$legislature <- do.call("rbind", df$legislature)

            df$legislature.prefLabel._value <- df$legislature$prefLabel._value

            df$legislature_about <- df$legislature$`_about`

            df$legislature_about <- gsub("http://data.parliament.uk/terms/", "", df$legislature_about)

            df$legislature <- NULL

            df <- hansard_tidy(df, tidy_style)

        }

            df

    }
}





#' @rdname all_answered_questions
#' @export

hansard_all_answered_questions <- function(mp_id = NULL, tabling_mp_id = NULL, house=NULL, answering_body=NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {

  df <- all_answered_questions(mp_id = mp_id, tabling_mp_id = tabling_mp_id, house=house, answering_body=answering_body, start_date = start_date, end_date = end_date, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style)

  df

}

