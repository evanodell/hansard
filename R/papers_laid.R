
#' papers_laid
#'
#' Imports data on Papers Laid
#' @param withdrawn If TRUE, only returns withdrawn papers. Defaults to FALSE.
#' @param house The house the paper was laid in. Accepts 'commons' and 'lords'. If NULL, returns both House of Commons and House of Lords. Defaults to NULL.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @keywords Papers Laid
#' @export
#' @examples \dontrun{
#' x <- papers_laid('all') }
#'

papers_laid <- function(withdrawn=FALSE, house=NULL, extra_args=NULL) {

  if(house=="commons"){
    house <- "?legislature.prefLabel=House of Commons"
  } else if(house=="lords"){
    house <- "?legislature.prefLabel=House of Lords"
  }

  if(withdrawn==TRUE) {
    query <- "&withdrawn=true"
  }

    baseurl <- "http://lda.data.parliament.uk/paperslaid.json?_pageSize=500"

    message("Connecting to API")

    papers <- jsonlite::fromJSON(paste0(baseurl, query, house, extra_args), flatten = TRUE)

    jpage <- round(papers$result$totalResults/papers$result$itemsPerPage, digits = 0)

    pages <- list()

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "&_page=", i, extra_args), flatten = TRUE)
        message("Retrieving page ", i + 1, " of ", jpage + 1)
        pages[[i + 1]] <- mydata$result$items
    }

    df <- jsonlite::rbind.pages(pages[sapply(pages, length) > 0])  #The data frame that is returned

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {
        df
    }
}


