

### 16a ALL MEMBERS

#' All Members of Parliament
#'
#' This imports data on All Members of Parliament including the Lords and the Commons
#' @param all Imports all available members Defaults to TRUE.
#' @keywords All Members of Parliament
#' @export
#' @examples
#' x <- members("all")
#' Returns a data frame with information on all members of Parliament, including both the House of Lords and the House of Commons. The data frame includes both current and previous members, and the API currently does not have information on when a member first sat in the house, or to distinguish current from former members.
#'
#' y <- members("commons")
#' Returns a data frame with information on all members of the House of Commons. The data frame includes both current and previous members of the House of Commons, and the API currently does not have information on when a member first sat in the house, or to distinguish current from former members.
#'
#' z <- members("lords")
#' Returns a data frame with information on all members of the House of Lords. The data frame includes both current and previous members of the House of Lords, and the API currently does not have information on when a member first sat in the house, or to distinguish current from former members.
#'


members <- function(house=c("all","commons","commons interests",
                                "lords", "lords interests")) {

    if (house=="all") { # Working

      baseurl_allMems <- "http://lda.data.parliament.uk/members.json?_pageSize=500"

      allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/members.json?_pageSize=500")

      allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:allMemsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", allMemsJpage+1)
        pages[[i + 1]] <- mydata$result$items
        }

    } else if (house=="commons") { #Working

      baseurl_allMems <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"

      allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500")

      allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:allMemsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", allMemsJpage+1)
        pages[[i + 1]] <- mydata$result$items
      }

    } else if (house=="lords") { #Working

      baseurl_allMems <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"

      allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500")

      allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:allMemsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", allMemsJpage+1)
        pages[[i + 1]] <- mydata$result$items
        }

    } else if (house=="commons interests") {#Kinda working

      baseurl_allMems <- "http://lda.data.parliament.uk/commonsregisteredinterests.json?_pageSize=500"

      allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsregisteredinterests.json?_pageSize=500")

      allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:allMemsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", allMemsJpage+1)
        pages[[i + 1]] <- mydata$result$items
        }

    } else if (house=="lords interests") { #Kinda working

      baseurl_allMems <- "http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500"

      allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500")

      allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

      pages <- list()

      for (i in 0:allMemsJpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
        message("Retrieving page ", i+1, " of ", allMemsJpage+1)
        pages[[i + 1]] <- mydata$result$items
      }
    }
  df<- rbind.pages(pages[sapply(pages, length)>0]) #The data frame that is returned
}



