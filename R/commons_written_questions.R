
#' Imports data in a tibble on House of Commons written questions.
#' @param mp_id Requests a member ID and returns a tibble with all written questions asked by that member. If NULL, mp_id is not included as a query parameter. Defaults to NULL.
#' @param answering_department Accepts a string with a department name or partial name, and returns all written questions by that department. The query acts as a search, so entering <health> will return all questions answered by the Department of Health. If NULL, answering_department is not included as a query parameter. Defaults to NULL.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on written questions in the House of Commons.
#' @keywords House of Commons Written Questions
#' @export
#' @examples \dontrun{
#'
#' x <- commons_written_questions(mp_id=410, 'cabinet office')
#' #Returns a tibble with written questions from Jon Trickett, answered by the Cabinet Office.
#'
#' }

commons_written_questions <- function(mp_id = NULL, answering_department = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE, tidy_style = "snake_case") {
    
    dates <- paste0("&_properties=dateTabled&max-dateTabled=", as.Date(end_date), "&min-dateTabled=", as.Date(start_date))
    
    if (is.null(mp_id) == FALSE) {
        mp_id <- paste0("&tablingMember=http://data.parliament.uk/members/", mp_id)
        
        mp_id <- utils::URLencode(mp_id)
    }
    
    if (is.null(answering_department) == FALSE) {
        
        query <- "/answeringdepartment"
        
        answering_department <- paste0("q=", answering_department)
        
        answering_department <- utils::URLencode(answering_department)
        
    } else {
        
        query <- NULL
        
    }
    
    baseurl <- "http://lda.data.parliament.uk/commonswrittenquestions"
    
    message("Connecting to API")
    
    writ <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, mp_id, dates, "&_pageSize=500", extra_args), flatten = TRUE)
    
    jpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
    
    pages <- list()
    
    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?", answering_department, mp_id, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }
    
    df <- tibble::as_tibble(dplyr::bind_rows(pages))
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        
        if (tidy == TRUE) {
            
            df$AnswerDate._value <- as.Date(df$AnswerDate._value)
            
            df$dateTabled._value <- as.Date(df$dateTabled._value)
            
            df$dateTabled._datatype <- "Date"
            
            df$AnswerDate._datatype <- "Date"
            
            df <- hansard_tidy(df, tidy_style)
            
            df
            
        } else {
            
            df
            
        }
        
    }
}
