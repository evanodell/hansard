
#' Members Search
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
        
        Search <- URLencode(Search)
        
        baseurl_MPID <- "http://lda.data.parliament.uk/members.json?_pageSize=500&_search=*"
        
        mpidResults <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/members.json?_pageSize=500&_search=*", Search, "*"))
        
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
        if (nrow(df) == 0) {
            message("The request did not return any data. Please check your search parameters.")
        } else {
            
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
}

