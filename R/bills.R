### 5 BILLS

#' Bills
#'
#' This imports data on House of Commons and House of Lords bills
#' @param type The type of data you want, allows the arguments "all", "ammended", "sponsor", "stage", "publications", "stageTypes"
#' @param all Imports all available bills
#' @param ammended Imports all ammended bills
#' @param sponsor Imports bills by sponsor
#' @param stage Imports bills by stage
#' @param publications Imports data on publications
#' @param stageTypes Imports all stage types
#' @keywords bills
#' @export
#' @examples
#' bills()


##Needs examples, still incomplete on all functions

bills <- function(type=c("all", "ammended", "sponsor",
                                 "stage", "publications", "stageTypes")) {

  match.arg(type)

  if(type=="all"){ #Not working

    baseurl_bills <- "http://lda.data.parliament.uk/bills.json?_pageSize=500"

    bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/bills.json?_pageSize=500")

    billsJpage <- round(bills$result$totalResults/bills$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:billsJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", billsJpage+1)
      pages[[i+1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length)>0])

  } else if(type=="ammended") { #Working but return is weird

    baseurl_bills <- "http://lda.data.parliament.uk/billswithamendments.json?_pageSize=500"

    bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/billswithamendments.json?_pageSize=500")

    billsJpage <- round(bills$result$totalResults/bills$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:billsJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", billsJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length)>0])

  } else if(type=="sponsor") { ##Not working

    baseurl_bills <- "http://lda.data.parliament.uk/bills.json?_pageSize=500"

    bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/bills.json?_pageSize=500")

    billsJpage <- round(bills$result$totalResults/bills$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:billsJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", billsJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length)>0])

  } else if(type=="stage") { #Not working

    baseurl_bills <- "http://lda.data.parliament.uk/bills.json?_pageSize=500"

    bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/bills.json?_pageSize=500")

    billsJpage <- round(bills$result$totalResults/bills$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:billsJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", billsJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length)>0])

  } else if(type=="publications") { ## Not yet Working

    baseurl_bills <- "http://lda.data.parliament.uk/billpublications.json?_pageSize=500"

    bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/billpublications.json?_pageSize=500")

    billsJpage <- round(bills$result$itemsPerPage/bills$result$totalResults, digits = 0)

    pages <- list()

    for (i in 0:billsJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", billsJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length)>0])

  } else if(type=="stageTypes") { #Working

    baseurl_bills <- "http://lda.data.parliament.uk/billstagetypes.json?_pageSize=500"

    bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/billstagetypes.json?_pageSize=500")

    df <- bills$result$items
    }
}
