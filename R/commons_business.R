### Commons Business



### 3 DIVISIONS

commons_divisions <- function(all=TRUE) {

  baseurl_divis <- "http://lda.data.parliament.uk/commonsdivisions.json"

  divis <- fromJSON("http://lda.data.parliament.uk/commonsdivisions.json")

  divisJpage <- round(divis$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:divisJpage){
    mydata <- fromJSON(paste0(baseurl_divis, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


### 5 BILLS

commons_bills <- function(all=TRUE) {

  baseurl_bills <- "http://lda.data.parliament.uk/commonswrittenquestions.json"

  bills <- fromJSON("http://lda.data.parliament.uk/commonswrittenquestions.json")

  billsJpage <- round(bills$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:billsJpage){
    mydata <- fromJSON(paste0(baseurl_bills, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


### 9 EARLY DAY MOTIONS

early_day_motions <- function(all=TRUE) {

  baseurl_edms <- "http://lda.data.parliament.uk/edms.json"

  edms <- fromJSON("http://lda.data.parliament.uk/edms.json")

  edmsJpage <- round(edms$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:edmsJpage){
    mydata <- fromJSON(paste0(baseurl_edms, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}
