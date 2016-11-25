
#' epetition
#'
#' Imports data on Epetitions
#' @param petType The type of data you want, allows the arguments 'all', 'byConstituency', 'petitionID', 'response', and 'idConstituency'
#' @param all Imports all available epetitions
#' @param byConstituency Imports all available epetition signatures by constituency
#' @param petitionID Requests a petition ID, and then returns information on that petition
#' @param response Requests a petition ID, and then returns the governments response to that petition
#' @param idConstituency Requests a petition ID, and then returns the signatures per constituency for that petition
#' @keywords ePetitions
#' @export
#' @examples \dontrun{
#' x <- epetition('all')
#'
#' x <- epetition('byConstituency')
#'
#' x <- epetition('petitionID')
#'
#' # x <- epetition('response')
#'
#' # x <- epetition('idConstituency')
#'
#'}


epetition <- function(petType = c("all", "byConstituency", "petitionID", "response", "idConstituency")) {

    match.arg(petType)

    if (petType == "all") {

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions.json?_pageSize=500"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(baseurl_petition)

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (petType == "byConstituency") {

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions/signaturesbyconstituency.json?_pageSize=500"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(baseurl_petition)

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (petType == "petitionID") {

        pet_ID <- readline("Enter the petition ID: ")

        pet_ID <- URLencode(pet_ID)

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions/"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(paste0(baseurl_petition, pet_ID, ".json?_pageSize=500"))

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (petType == "response") {

        pet_ID <- readline("Enter the petition ID: ")

        pet_ID <- URLencode(pet_ID)

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions/"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(paste0(baseurl_petition, pet_ID, "/governmentresponse.json?_pageSize=500"))

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (petType == "idConstituency") {

        pet_ID <- readline("Enter the petition ID: ")

        pet_ID <- URLencode(pet_ID)

        baseurl_petition <- "http://lda.data.parliament.uk/epetitions/"

        message("Connecting to API")

        petition <- jsonlite::fromJSON(paste0(baseurl_petition, pet_ID, "/signaturesbyconstituency?_pageSize=500"))

        petitionJpage <- round(petition$result$totalResults/petition$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:petitionJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_petition, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", petitionJpage + 1)
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
