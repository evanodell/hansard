
#' Imports data on all answered parliamentary questions.
#' @param mp_id Accepts a member ID, and returns a tibble with all available questions asked by that member. If NULL, returns a tibble with all available answered questions.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'.
#' @param end_date The latest date to include in the tibble. Defaults to current system date.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove extra characters, superfluous text and convert variable names to snake_case. Defaults to TRUE.
#' @return A tibble with details on all answered questions in the House of Commons and the House of Lords.
#' @keywords Answered Questions
#' @export
#' @examples \dontrun{
#'
#' x <- all_answered_questions(4019)
#'
#' }

all_answered_questions <- function(mp_id = NULL, start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, tidy = TRUE) {
    
    dates <- paste0("&_properties=date&max-date=", end_date, "&min-date=", start_date)
    
    if (is.null(mp_id) == TRUE) {
        
        baseurl <- "http://lda.data.parliament.uk/answeredquestions.json?_pageSize=500"
        
        message("Connecting to API")
        
        all <- jsonlite::fromJSON(paste0(baseurl, dates, extra_args), flatten = TRUE)
        
        jpage <- round(all$result$totalResults/all$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, dates, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else {
        
        mp_id <- as.character(mp_id)
        
        baseurl <- "http://lda.data.parliament.uk/questionsanswers.json?_pageSize=500&mnisId="
        
        message("Connecting to API")
        
        all <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, extra_args))
        
        jpage <- round(all$result$totalResults/all$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, "&_page=", i, dates, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    }
    
    df <- dplyr::bind_rows(pages)
    
    df <- tibble::as_tibble(df)
    
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
