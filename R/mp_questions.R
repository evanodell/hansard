
#' mp_questions
#'
#' Accepts an ID number for a member of the House of Commons, and returns a tibble of of all their oral and written questions.
#' @param mp_id The ID number of a member of the House of Commons.
#' @param question_type Accepts the arguments 'all', 'oral' and 'written'. Defaults to 'all'.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the tibble. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @keywords questions
#' @export
#' @examples \dontrun{
#' x <- mp_questions(172, 'all')
#' }
#'


mp_questions <- function(mp_id, question_type = "all", start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {

    question_type <- tolower(question_type)

    if (question_type == "all") {
        message("Retrieving oral questions:")
        df_oral <- mp_questions(mp_id = mp_id, question_type = "oral", start_date = start_date, end_date = end_date, extra_args = extra_args)

        message("Retrieving written questions:")
        df_writ <- mp_questions(mp_id = mp_id, question_type = "written", start_date = start_date, end_date = end_date, extra_args = extra_args)

        message("Combining oral and written questions")
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

        dates <- paste0("&_properties=AnswerDate&max-AnswerDate=", end_date, "&min-AnswerDate=", start_date)

        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json?mnisId="

        oral <- jsonlite::fromJSON(paste0(baseurl_oral, mp_id, dates, "&_pageSize=500", extra_args))

        if (oral$result$totalResults > oral$result$itemsPerPage) {
            oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)
        } else {
            oralJpage <- 0
        }

        pages <- list()

        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, mp_id, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        df <- dplyr::bind_rows(pages)

        df <- tibble::as_tibble(df)

    } else if (question_type == "written") {

        baseurl <- "http://lda.data.parliament.uk/commonswrittenquestions.json?mnisId="

        dates <- paste0("&_properties=dateTabled&max-dateTabled=", end_date, "&min-dateTabled=", start_date)

        writ <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, "&_pageSize=500", extra_args))

        if (writ$result$totalResults > writ$result$itemsPerPage) {
            jpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
        } else {
            jpage <- 0
        }

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        df <- dplyr::bind_rows(pages)

        df <- tibble::as_tibble(df)
    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df)
      
            df

        } else {

            df

        }

    }
}
