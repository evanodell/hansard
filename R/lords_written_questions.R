
#' House of Lords Written Questions
#'
#' Imports data on House of Lords written questions
#' @param lordsWritType The type of data you want, allows the arguments 'all', 'department' and 'dates'
#' @param all Returns a data frame all written questions from the house of lords
#' @param department Requests a department, and then returns a data frame with all written questions answered by that department
#' @param dates Requests two dates, and returns a data frame with all available written questions from between the two given dates
#' @keywords House of Lords Written Questions
#' @export
#' @examples \dontrun{
#' x <- lords_written_questions('all')
#'
#' # x <- lords_written_questions('department')
#'
#' # x <- lords_written_questions('dates')
#' }

lords_written_questions <- function(lordsWritType = c("all", "department", "dates")) {
    
    match.arg(lordsWritType)
    
    if (lordsWritType == "all") {
        
        baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions.json"
        
        writ <- jsonlite::fromJSON(baseurl_writ)
        
        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "?_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (lordsWritType == "department") {
        
        answering.department <- readline("Enter the name of the answering department: ")
        
        answering.department <- URLencode(answering.department)
        
        baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions/answeringdepartment.json?q="
        
        writ <- jsonlite::fromJSON(paste0(baseurl_writ, answering.department, "&pageSize=500"))
        
        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (lordsWritType == "dates") {
        
        start.date <- readline("Enter start date(yyyy-mm-dd): ")
        
        end.date <- readline("Enter end date (yyyy-mm-dd): ")
        
        start.date <- URLencode(start.date)
        
        end.date <- URLencode(end.date)
        
        baseurl_writ <- "http://lda.data.parliament.uk/lordswrittenquestions/tabled.json?startDate="
        
        writ <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500"))
        
        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500", "&_page=", i), 
                flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}
