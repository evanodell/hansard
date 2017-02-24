
#' commons_oral_questions
#'
#' Imports data on House of Commons oral questions
#' @param mp_id The ID of a given MP. Defaults to NULL.
#' @param answering_department The department that answers a question
#' @param start_date The earliest date to include in the data frame. Defaults to "1900-01-01".
#' @param end_date The latest date to include in the data frame. Defaults to current system date.
#' @keywords questions
#' @export
#' @examples \dontrun{
#'
#' x <- commons_oral_questions(mp_id=4019, answering_department = 'education',
#' start_date="1900-01-01", end_date=Sys.Date())
#'
#' }

commons_oral_questions <- function(mp_id = NULL, answering_department=NULL, start_date="1900-01-01", end_date=Sys.Date()) {


  if (is.null(mp_id)==FALSE){
    mp_id <- paste0("&tablingMember=http://data.parliament.uk/members/", mp_id)
  }

  if (is.null(answering_department)==FALSE){

    query <- "/answeringdepartment"

    answering_department <- paste0("q=",answering_department)

  } else {

    query <- NULL

    }

  dates <-paste0("&_properties=AnswerDate&max-AnswerDate=",end_date, "&min-AnswerDate=",start_date)

  baseurl <- "http://lda.data.parliament.uk/commonsoralquestions"

  message("Connecting to API")

  oral <- jsonlite::fromJSON(paste0(baseurl, query, ".json?",answering_department, mp_id, dates, "&_pageSize=500"), flatten=TRUE)

  jpage <- round(oral$result$totalResults/oral$result$itemsPerPage, digits = 0)

  pages <- list()

  for (i in 0:jpage) {
    mydata <- jsonlite::fromJSON(paste0(baseurl, query, ".json?",answering_department, mp_id, dates, "&_pageSize=500&_page=", i), flatten = TRUE)
    message("Retrieving page ", i + 1, " of ", jpage + 1)
    pages[[i + 1]] <- mydata$result$items
  }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }

}




