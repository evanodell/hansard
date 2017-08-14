
#' Imports data on House of Commons oral question times
#' @param session Accepts a session in format yyyy/yy (e.g. 2016/17) and returns a tibble of all oral question times from that session
#' @param question_id Accepts a question time ID, and returns a tibble of that question time.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @param verbose If TRUE, returns data to console on the progress of the API request. Defaults to FALSE.
#' @return  A tibble with information on oral question times in the House of Commons.
#' @keywords Oral Questions Time
#' @seealso \code{\link{all_answered_questions}}
#' @seealso \code{\link{commons_answered_questions}}
#' @seealso \code{\link{commons_oral_questions}}
#' @seealso \code{\link{commons_written_questions}}
#' @seealso \code{\link{lords_written_questions}}
#' @seealso \code{\link{mp_questions}}
#' @export
#' @examples \dontrun{
#'
#'  x <- commons_oral_question_times(session='2016/17', question_id='685697')
#'
#' }

commons_oral_question_times <- function(session = NULL, question_id = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

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

    if(verbose==TRUE){message("Connecting to API")}

    if (is.null(question_id) == FALSE) {

        mydata <- jsonlite::fromJSON(paste0(baseurl, ".json", session, extra_args), flatten = TRUE)

        df <- mydata$result$items

        df <- tibble::as_tibble(df)

    } else {

        oral <- jsonlite::fromJSON(paste0(baseurl, question_id, ".json", session, page_size, extra_args))

        jpage <- floor(oral$result$totalResults/oral$result$itemsPerPage)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, question_id, ".json", session, page_size, "&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df$date._value <- gsub("T", " ", df$date._value)

            df$date._value <- lubridate::parse_date_time(df$date._value, "Y-m-d H:M:S")

            df$modified._value <- gsub("T", " ", df$modified._value)

            df$modified._value <- lubridate::parse_date_time(df$modified._value, "Y-m-d H:M:S")

            df$modified._datatype <- "POSIXct"

            df$date._datatype <- "POSIXct"

            df <- hansard_tidy(df, tidy_style)

            df

        } else {

            df

        }

    }
}




#' @rdname commons_oral_question_times
#' @export

hansard_commons_oral_question_times <- function(session = NULL, question_id = NULL, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- commons_oral_question_times(session = session, question_id = question_id, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}
