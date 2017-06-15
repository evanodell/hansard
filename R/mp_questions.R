
#' Accepts an ID number for a member of the House of Commons, and returns a tibble of of all their oral and written questions.
#' @param mp_id The ID number of a member of the House of Commons. Defaults to NULL.
#' @param question_type Accepts the arguments 'all', 'oral' and 'written'. Defaults to 'all'.
#' @param start_date The earliest date to include in the tibble. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param end_date The latest date to include in the tibble. Defaults to current system date. Defaults to '1900-01-01'. Accepts character values in 'YYYY-MM-DD' format, and objects of class Date, POSIXt, POSIXct, POSIXlt or anything else than can be coerced to a date with \code{as.Date()}.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE. Accepts one of 'snake_case', 'camelCase' and 'period.case'. Defaults to 'snake_case'.
#' @return A tibble with details on all questions asked by a member of the House of Commons.
#' @seealso \code{\link{all_answered_questions}} \code{\link{commons_answered_questions}} \code{\link{commons_oral_questions}} \code{\link{commons_oral_question_times}} \code{\link{commons_written_questions}} \code{\link{lords_written_questions}}
#' @keywords questions
#' @export
#' @examples \dontrun{
#' x <- mp_questions(172, 'all')
#' }
#'

mp_questions <- function(mp_id = NULL, question_type = "all", start_date = "1900-01-01", end_date = Sys.Date(), extra_args = NULL, 
    tidy = TRUE, tidy_style = "snake_case") {
    
    if (is.null(mp_id) == TRUE) {
        stop("mp_id must not be empty", call. = FALSE)
    }
    
    question_type <- tolower(question_type)
    
    if (question_type == "all") {
        message("Retrieving oral questions:")
        df_oral <- mp_questions(mp_id = mp_id, question_type = "oral", start_date = as.Date(start_date), end_date = as.Date(end_date), 
            extra_args = extra_args, tidy = FALSE, tidy_style = tidy_style)
        
        message("Retrieving written questions:")
        df_writ <- mp_questions(mp_id = mp_id, question_type = "written", start_date = as.Date(start_date), end_date = as.Date(end_date), 
            extra_args = extra_args, tidy = FALSE, tidy_style = tidy_style)
        
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
        
        dates <- paste0("&_properties=AnswerDate&max-AnswerDate=", as.Date(end_date), "&min-AnswerDate=", as.Date(start_date))
        
        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json?mnisId="
        
        oral <- jsonlite::fromJSON(paste0(baseurl_oral, mp_id, dates, "&_pageSize=500", extra_args))
        
        if (oral$result$totalResults > oral$result$itemsPerPage) {
            oraljpage <- floor(oral$result$totalResults/oral$result$itemsPerPage)
        } else {
            oralJpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, mp_id, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- tibble::as_tibble(dplyr::bind_rows(pages))
        
    } else if (question_type == "written") {
        
        baseurl <- "http://lda.data.parliament.uk/commonswrittenquestions.json?mnisId="
        
        dates <- paste0("&_properties=dateTabled&max-dateTabled=", as.Date(end_date), "&min-dateTabled=", as.Date(start_date))
        
        writ <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, "&_pageSize=500", extra_args))
        
        if (writ$result$totalResults > writ$result$itemsPerPage) {
            jpage <- floor(writ$result$totalResults/writ$result$itemsPerPage)
        } else {
            jpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, mp_id, dates, "&_pageSize=500&_page=", i, extra_args), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", jpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        df <- tibble::as_tibble(dplyr::bind_rows(pages))
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        
        if (tidy == TRUE) {
            
            df$dateTabled._value <- as.POSIXct(df$dateTabled._value)
            
            df$AnswerDate._value <- as.POSIXct(df$AnswerDate._value)
            
            df$AnswerDate._datatype <- "POSIXct"
            
            df$dateTabled._datatype <- "POSIXct"
            
            df$tablingMemberPrinted <- unlist(df$tablingMemberPrinted)
            
            df$AnsweringBody <- unlist(df$AnsweringBody)
            
            df$tablingMember._about <- gsub("http://data.parliament.uk/members/", "", df$tablingMember._about)
            
            df <- hansard::hansard_tidy(df, tidy_style)
            
            df
            
        } else {
            
            df
            
        }
        
    }
}
