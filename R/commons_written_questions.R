
#' House of Commons Bills Written Questions
#'
#' Imports data on House of Commons written questions
#' @param comsWritType The type of data you want, allows the arguments 'all', 'department' and 'dates'
#' @param all Imports all available written questions
#' @param department Imports all available written questions answered by a given department
#' @param dates Imports all available written questions from between two given dates
#' @keywords House of Commons Written Questions
#' @export
#' @examples \donttest{
#' #### NOT RUN:
#' # x <- commons_written_questions('all')
#' #Returns all written questions
#'
#' # x <- commons_written_questions('department')
#' #Returns all written questions by department. The query acts as a search, so entering <health>
#' # will return all questions answered by the Department of Health
#'
#'
#' # x <- commons_written_questions('dates')
#' #Returns all written questions tabled between two chosen dates
#' }

commons_written_questions <- function(comsWritType = c("all", "department", "dates")) {
    
    match.arg(comsWritType)
    
    if (comsWritType == "all") {
        
        baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions.json"
        
        writ <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")
        
        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "?_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (comsWritType == "department") {
        
        answering.department <- readline("Enter the name of the answering department: ")
        
        baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions/answeringdepartment.json?q="
        
        writ <- jsonlite::fromJSON(paste0(baseurl_writ, answering.department, "&pageSize=500"))
        
        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    } else if (comsWritType == "dates") {
        
        start.date <- readline("Enter start date (yyyy-mm-dd): ")
        
        end.date <- readline("Enter end date (yyyy-mm-dd): ")
        
        baseurl_writ <- "http://lda.data.parliament.uk/commonswrittenquestions/tabled.json?startDate="
        
        writ <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500"))
        
        writJpage <- round(writ$result$totalResults/writ$result$itemsPerPage, digits = 0)
        
        pages <- list()
        
        for (i in 0:writJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_writ, start.date, "&endDate=", end.date, "&_pageSize=500", "&_page=", 
                i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", writJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
        
    }
    
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned
    
}
