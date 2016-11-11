
#' All Members of Parliament
#'
#' Imports data on All Members of Parliament including the Lords and the Commons
#' @param house The type of data you want, allows the arguments
#' @param all Imports all available members. The default value return
#' @param commons Imports all available members
#' @param lords Imports all available members
#' @param lordsInterests Imports all available members
#' @keywords All Members of Parliament
#' @export
#' @examples \donttest{
#' ### NOT RUN:
#' # x <- members('all')
#' # Returns a data frame with information on all members of Parliament, including both
#' # the House of Lords and the House of Commons. The data frame includes both current and
#' # previous members, and the API currently does not have information on when a member first
#' # sat in the house, or to distinguish current from former members.
#'
#' # y <- members('commons')
#' # Returns a data frame with information on all members of the House of Commons. The data
#' # frame includes both current and previous members of the House of Commons, and the API
#' # currently does not have information on when a member first sat in the house, or to
#' # distinguish current from former members.
#'
#' # z <- members('lords')
#' # Returns a data frame with information on all members of the House of Lords. The data
#' # frame includes both current and previous members of the House of Lords, and the API
#' # currently does not have information on when a member first sat in the house, or to
#' # distinguish current from former members.
#'
#'
#' # b <- members('lordsInterests')
#' # Requests a member ID, and returns a dataframe of the registered interests of that
#' # member of the House of Lords. }

members <- function(house = c("all", "commons", "lords", "lordsInterests")) {

    match.arg(house)

    if (house == "all") {

        baseurl_allMems <- "http://lda.data.parliament.uk/members.json?_pageSize=500"

        allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/members.json?_pageSize=500")

        allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (house == "commons") {

        baseurl_allMems <- "http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500"

        allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/commonsmembers.json?_pageSize=500")

        allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

    } else if (house == "lords") {

        baseurl_allMems <- "http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500"

        allMems <- jsonlite::fromJSON("http://lda.data.parliament.uk/lordsmembers.json?_pageSize=500")

        allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

        pages <- list()

        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }

        # } else if (house=='commonsInterests') {#Not working

        # baseurl_allMems <- 'http://lda.data.parliament.uk/commonsregisteredinterests.json?_pageSize=500'

        # allMems <- jsonlite::fromJSON('http://lda.data.parliament.uk/commonsregisteredinterests.json?_pageSize=500')

        # allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)

        # pages <- list()

        # for (i in 0:allMemsJpage) { mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, '&_page=', i), flatten = TRUE)
        # message('Retrieving page ', i+1, ' of ', allMemsJpage+1) pages[[i + 1]] <- mydata$result$items }

    } else if (house == "lordsInterests") {

        MP.ID <- readline("Enter the members ID number: ")

        MP.ID <- as.numeric(MP.ID)

        baseurl_allMems <- "http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500&member="

        allMems <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/lordsregisteredinterests.json?_pageSize=500&member=",
            MP.ID))

        if (allMems$result$totalResults > allMems$result$itemsPerPage) {

            allMemsJpage <- round(allMems$result$totalResults/allMems$result$itemsPerPage, digits = 0)
        } else {
            allMemsJpage <- 0
        }

        pages <- list()

        for (i in 0:allMemsJpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl_allMems, MP.ID, "&_page=", i), flatten = TRUE)
            message("Retrieving page ", i + 1, " of ", allMemsJpage + 1)
            pages[[i + 1]] <- mydata$result$items
        }
    }
    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

}


#' MP Search
#'
#' Search for an MP or Lord by name and constituency
#'
#'#' # Function searches for the string and returns a data frame with all matches from
#' # both houses of parliament. Returns all partial matches in the members' names,
#' # constituencies, twitter handle and webpage.
#' # The default search is NULL, which returns a data frame of all members of both
#' # houses, the same result as members('all').
#'
#' @param Search Search for members by name and constituency. Defaults to NULL.
#' @keywords All Members of Parliament
#' @export
#' @examples \donttest{
#' ###NOT RUN
#' # x <- members_search(Search=NULL)
#' # Returns members('all')
#' # x <- members_search(Search='chris')
#' # head(x)
#' # Returns all members with 'chris' in their name or the name of their constituency}

members_search <- function(Search = NULL) {

    if (is.null(Search)) {
        members("all")
    } else {

        baseurl_MPID <- "http://lda.data.parliament.uk/members.json?_pageSize=500&_search=*"

        mpidResults <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/members.json?_pageSize=500&_search=*", Search,
            "*"))

        if (mpidResults$result$totalResults > mpidResults$result$itemsPerPage) {
            mpidJpage <- round(mpidResults$result$totalResults/mpidResults$result$itemsPerPage, digits = 0)

            pages <- list()

            for (i in 0:mpidJpage) {
                mydata <- jsonlite::fromJSON(paste0(baseurl_MPID, Search, "*", "&_page=", i), flatten = TRUE)
                message("Retrieving page ", i + 1, " of ", mpidJpage + 1)
                pages[[i + 1]] <- mydata$result$items
            }

            df2 <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

        } else {
            df2 <- mpidResults$result$items


            names(df2)[names(df2) == "_about"] <- "about"

            df2$about <- lapply(df2$about, function(x) {
                gsub("http://data.parliament.uk/members/", "", x)
            })

            df2$constituency$`_about` <- lapply(df2$constituency$`_about`, function(x) {
                gsub("http://data.parliament.uk/resources/", "", x)
            })

            df2$about <- as.character(df2$about)

        }

        df <- data.frame(matrix(ncol = 0, nrow = mpidResults$result$totalResults))

        df$MP.ID = df2$about
        df$additionalName <- df2$additionalName$`_value`
        df$constituency <- df2$constituency$label$`_value`
        df$constituencyCode <- df2$constituency$`_about`
        df$familyName <- df2$familyName$`_value`
        df$fullName <- df2$fullName$`_value`
        df$gender <- df2$gender$`_value`
        df$givenName <- df2$givenName$`_value`
        df$homePage <- df2$homePage
        df$label <- df2$label$`_value`
        df$party <- df2$party$`_value`
        df$twitter <- df2$twitter$`_value`

        return(df)
    }
}

