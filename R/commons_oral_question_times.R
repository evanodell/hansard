
#' Imports data on House of Commons oral question times
#' @param session Accepts a session in format yyyy/yy (e.g. 2016/17) and returns a tibble of all oral question times from that session
#' @param question_id Accepts a question time ID, and returns a tibble of that question time.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE, tidy_style="snake_case". Accepts one of "snake_case", "camelCase" and "period.case". Defaults to "snake_case".
#' @return A tibble with information on oral question times in the House of Commons.
#' @keywords Oral Questions Time
#' @seealso \code{\link{all_answered_questions}} \code{\link{commons_answered_questions}} \code{\link{commons_oral_questions}} \code{\link{commons_written_questions}}  \code{\link{lords_written_questions}} \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#'
#'  x <- commons_oral_question_times(session='2016/17', question_id='685697')
#'
#' }

commons_oral_question_times <- function(session = NULL, question_id = NULL, extra_args = NULL, tidy = TRUE, tidy_style="snake_case") {

    if (is.null(session) == FALSE) {

        session <- paste0("?session=", session)

        session <- utils::URLencode(session)

    }

    if (is.null(question_id) == FALSE) {

        question_id <- paste0("/", question_id)

        page_size <- NULL

    } else {

        page_size <- "&_pageSize=500"

    }

    baseurl <- "http://lda.data.parliament.uk/commonsoralquestiontimes"

    message("Connecting to API")

    if (is.null(question_id) == FALSE) {

        mydata <- jsonlite::fromJSON(paste0(baseurl, ".json", session, extra_args), flatten = TRUE)

        df <- mydata$result$items

        df <- tibble::as_tibble(df)

    } else {

        oral <- jsonlite::fromJSON(paste0(baseurl, question_id, ".json", session, page_size, extra_args))

        jpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, question_id, ".json", session, page_size, "&_page=", i, extra_args),
                flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df, tidy_style)

            df

        } else {

            df

        }

    }
}
