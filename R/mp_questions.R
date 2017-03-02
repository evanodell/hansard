

#' mp_questions
#'
#' Accepts an ID number for a member of the House of Commons, and returns a data frame of of all their oral and written questions.
#' @param mp.id The ID number of a member of the House of Commons.
#' @param question_type Accepts the arguments 'all', 'oral' and 'written'. Defaults to 'all'.
#' @param all Returns a data frame with all recorded questions for a given member of the house of commons.
#' @param oral Returns a data frame with all oral questions asked by a given MP.
#' @param written Returns a data frame with all written questions asked by a given MP.
#' @keywords questions
#' @export
#' @examples \dontrun{
#' x <- mp_questions(172, 'all')
#' }
#'


mp_questions <- function(mp.id, question_type = c("all", "oral", "written")) {
    
    match.arg(question_type)
    
    if (question_type == "all") {
        message("Retrieving oral questions:")
        df_oral <- mp_questions(mp.id, "oral")
        
        message("Retrieving written questions:")
        df_writ <- mp_questions(mp.id, "written")
        
        message("Combining oral and written questions")
        common <- intersect(colnames(df_writ), colnames(df_oral))
        
        df <- rbind(subset(df_writ, select = common), subset(df_oral, select = common))
        
        df
        
    } else if (question_type == "oral") {
        baseurl_oral <- "http://lda.data.parliament.uk/commonsoralquestions.json?mnisId="
        
        oral <- jsonlite::fromJSON(paste0(baseurl_oral, mp.id, "&_pageSize=500"))
        
        if (oral$result$totalResults > oral$result$itemsPerPage) {
            oralJpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)
        } else {
            oralJpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:oralJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_oral, mp.id, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", oralJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
        
    } else if (question_type == "written") {
        
        baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions.json?mnisId="
        
        writ <- jsonlite::fromJSON(paste0(baseurl_writ, mp.id, "&_pageSize=500"))
        
        if (writ$result$totalResults > writ$result$itemsPerPage) {
            writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
        } else {
            writJpage <- 0
        }
        
        pages <- list()
        
        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, mp.id, "&_pageSize=500&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    }
    
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
