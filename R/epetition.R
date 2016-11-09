

#' ePetitions
#'
#' This imports data on petitions
#' @param type The type of data you want, allows the arguments "all", "department" and "dates"
#' @param all Imports all available papers laid
#' @keywords Papers Laid
#' @export
#' @examples
#' x <- papers_laid("all")
#'


epetition <- function(type =c("all", "date", "no","aye")) {

  match.arg(type)

  if(type=="all") {

    baseurl_petition <- "http://lda.data.parliament.uk/epetitions.json?_pageSize=500"

    petition <- jsonlite::fromJSON(baseurl_petition)

    petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:petitionJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", petitionJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  }else if(type=="byConstituency") {

    baseurl_petition <- "http://lda.data.parliament.uk/epetitions/signaturesbyconstituency.json?_pageSize=500"

    petition <- jsonlite::fromJSON(baseurl_petition)

    petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:petitionJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", petitionJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  } else if(type=="petitionID") {

    pet_ID <- readline("Enter the petition ID: ")

    baseurl_petition <- "http://lda.data.parliament.uk/epetitions/"

    petition <- jsonlite::fromJSON(paste0(baseurl_petition,pet_ID,".json?_pageSize=500"))

    petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:petitionJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", petitionJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  } else if(type=="response") {

    pet_ID <- readline("Enter the petition ID: ")

    baseurl_petition <- "http://lda.data.parliament.uk/epetitions/"

    petition <- jsonlite::fromJSON(paste0(baseurl_petition,pet_ID,"/governmentresponse.json?_pageSize=500"))

    petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:petitionJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", petitionJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  } else if(type=="idConstituency") {

    pet_ID <- readline("Enter the petition ID: ")

    baseurl_petition <- "http://lda.data.parliament.uk/epetitions/"

    petition <- jsonlite::fromJSON(paste0(baseurl_petition,pet_ID,"/signaturesbyconstituency?_pageSize=500"))

    petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:petitionJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", petitionJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

  }

  df<- jsonlite::rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned
}
