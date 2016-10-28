### LORDS BUSINESS


### 12 LORDS ATTENDANCE

lords_attendance <- function(all=TRUE) {

  baseurl_lordsAttend <- " http://lda.data.parliament.uk/lordsattendances.json"

  lordsAttend <- fromJSON(" http://lda.data.parliament.uk/lordsattendances.json")

  lordsAttendJpage <- round(lordsAttend$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:lordsAttendJpage){
    mydata <- fromJSON(paste0(baseurl_lordsAttend, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 13 LORDS AMMENDMENTS

lords_ammendments <- function(all=TRUE) {

  baseurl_lordsAmmend <- "http://lda.data.parliament.uk/lordsbillamendments.json"

  lordsAmmend <- fromJSON("http://lda.data.parliament.uk/lordsbillamendments.json")

  lordsAmmendJpage <- round(lordsAmmend$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:lordsAmmendJpage){
    mydata <- fromJSON(paste0(baseurl_lordsAmmend, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}



### 14 LORDS DIVISIONS

lords_divisions <- function(all=TRUE) {

  baseurl_lordsDivs <- "http://lda.data.parliament.uk/lordsdivisions.json"

  lordsDivs <- fromJSON("http://lda.data.parliament.uk/lordsdivisions.json")

  lordsDivsJpage <- round(lordsDivs$result$totalResults/10+1,digits = 0)

  pages <- list()

  for(i in 0:lordsDivsJpage){
    mydata <- fromJSON(paste0(baseurl_lordsDivs, "?_page=", i), flatten=TRUE)
    message("Retrieving page ", i)
    pages[[i+1]] <- mydata$result$items
  }
}


