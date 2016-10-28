### MEMBERSHIP

### 16a ALL MEMBERS

all_members <- function(all = TRUE) {

    baseurl_allMems <- "http://lda.data.parliament.uk/members.json"

    allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/members.json")

    allMemsJpage <- round(allMems$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:allMemsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}


### 16b COMMONS MEMBERS

commons_members <- function(all = TRUE) {

    baseurl_comMems <- "http://lda.data.parliament.uk/commonsmembers.json"

    comMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsmembers.json")

    comMemsJpage <- round(comMems$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:comMemsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comMems, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}


### 16c LORDS MEMBERS

lords_members <- function(all = TRUE) {

    baseurl_lordMems <- "http://lda.data.parliament.uk/lordsmembers.json"

    lordMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsmembers.json")

    lordMemsJpage <- round(lordMems$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:lordMemsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_lordsMems, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}



### 16d COMMONS REGISTERED INTERESTS

commons_interest <- function(all = TRUE) {

    baseurl_comInterest <- "http://lda.data.parliament.uk/commonsregisteredinterests.json"

    comInterest <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsregisteredinterests.json")

    comInterestJpage <- round(comInterest$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:comInterestJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_comInterest, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}



### 16e LORDS REGISTERED INTERESTS

lords_interest <- function(all = TRUE) {

    baseurl_lordInterest <- "http://lda.data.parliament.uk/lordsregisteredinterests.json"

    lordInterest <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsregisteredinterests.json")

    lordInterestJpage <- round(lordInterest$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:lordInterestJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_lordInterest, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}
