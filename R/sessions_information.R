### SESSION INFORMATION


### 21 SESSIONS

sessions <- function(all = TRUE) {

    baseurl_sessions <- "http://lda.data.parliament.uk/sessions.json"

    sessions <- jsonlite::fromJSON("http://lda.data.parliament.uk/sessions.json")

    sessionsJpage <- round(sessions$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:sessionsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_sessions, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}


### 22 TERMS

commons_terms <- function(all = TRUE) {

    baseurl_terms <- "http://lda.data.parliament.uk/terms.json"

    cTerms <- jsonlite::fromJSON("http://lda.data.parliament.uk/terms.json")

    cTermsJpage <- round(cTerms$result$totalResults/10 + 1, digits = 0)

    pages <- list()

    for (i in 0:cTermsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_terms, "?_page=", i), flatten = TRUE)
        message("Retrieving page ", i)
        pages[[i + 1]] <- mydata$result$items
    }
}
