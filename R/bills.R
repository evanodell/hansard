### 5 BILLS

#' Bills
#'
#' This imports data on House of Commons and House of Lords bills
#' @param all Imports all available bills Defaults to TRUE.
#' @keywords bills
#' @export
#' @examples
#' bills()


bills <- function(type=c("all", "ammended", "sponsor",
                                 "stage", "publications", "stage types")) {

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

    df <- rbind.pages(pages[sapply(pages, length)>0])

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

    df <- rbind.pages(pages[sapply(pages, length)>0])

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

    df <- rbind.pages(pages[sapply(pages, length)>0])

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

    df <- rbind.pages(pages[sapply(pages, length)>0])

  } else if(type=="publications") {

    baseurl_bills <- "http://lda.data.parliament.uk/billpublications.json?_pageSize=500"

    bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/billpublications.json?_pageSize=500")

    billsJpage <- round(bills$result$itemsPerPage/bills$result$totalResults, digits = 0)

    pages <- list()

    for (i in 0:billsJpage) {
      mydata <- jsonlite::fromJSON(paste0(baseurl_bills, "&_page=", i), flatten = TRUE)
      message("Retrieving page ", i+1, " of ", billsJpage+1)
      pages[[i + 1]] <- mydata$result$items
    }

    df <- rbind.pages(pages[sapply(pages, length)>0])

  } else if(type=="stage types") { #Working

    baseurl_bills <- "http://lda.data.parliament.uk/billstagetypes.json?_pageSize=500"

    bills <- jsonlite::fromJSON("http://lda.data.parliament.uk/billstagetypes.json?_pageSize=500")

    df <- bills$result$items
    }
}
