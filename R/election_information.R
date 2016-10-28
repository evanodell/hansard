### ELECTION INFORMATION


### 8 CONSTITUENCIES

constituencies <- function(all = TRUE) {

    baseurl_conts <- "http://lda.data.parliament.uk/constituencies.json"

    conts <- jsonlite::fromJSON("http://lda.data.parliament.uk/constituencies.json")

    contsJpage <- round(conts$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:contsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_conts, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}


### 10 ELECTION RESULTS

election_results <- function(all = TRUE) {

    baseurl_electR <- "http://lda.data.parliament.uk/electionresults.json"

    electR <- jsonlite::fromJSON("http://lda.data.parliament.uk/electionresults.json")

    electRJpage <- round(electR$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:electRJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_electR, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}



### 11 ELECTIONS

elections <- function(all = TRUE) {

    baseurl_elect <- "http://lda.data.parliament.uk/elections.json"

    elect <- jsonlite::fromJSON("http://lda.data.parliament.uk/elections.json")

    electJpage <- round(elect$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:electJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_elect, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}


