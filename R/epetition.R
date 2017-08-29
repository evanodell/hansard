

#' Individual epetitions
#'
#' Imports data on a given epetition. For bulk epetion data, see \code{\link{epetition_tibble}}.
#' @param ID The ID of a given petition. If \code{NULL}, returns all epetitions. Defaults to \code{NULL}. See \code{\link{epetition_tibble}} for a greater degree of flexibility when querying all epetitions.
#' @param by_constituency Accepts either \code{TRUE} or \code{FALSE}. If \code{TRUE}, provides a tibble with a breakdown of signatures for each petition, by constituency. Defaults to \code{FALSE}.
#' @inheritParams all_answered_questions
#' @return A tibble with details on electronic petitions submitted to parliament.
#' @seealso \code{\link{epetition_tibble}}
#'
#' @export
#' @examples \dontrun{
#' x <- epetition(ID = 706964, by_constituency=TRUE)
#'}

epetition <- function(ID = NULL, by_constituency = FALSE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    if (is.null(ID) == FALSE) {
        ID <- paste0("/", ID)
    }

    if (by_constituency == TRUE) {
        by_constituency <- "/signaturesbyconstituency"
    } else {
        by_constituency <- NULL
    }

    baseurl <- "http://lda.data.parliament.uk/epetitions"

    if(verbose==TRUE){message("Connecting to API")}

    if (is.null(ID) == FALSE & is.null(by_constituency) == TRUE) {

        petition <- jsonlite::fromJSON(paste0(baseurl, ID, by_constituency, ".json?", extra_args), flatten = TRUE)

        df <- tibble::as_tibble(petition$result$primaryTopic)

    } else {

        petition <- jsonlite::fromJSON(paste0(baseurl, ID, by_constituency, ".json?", extra_args), flatten = TRUE)

        jpage <- floor(petition$result$totalResults/500)

        pages <- list()

        for (i in 0:jpage) {
            mydata <- jsonlite::fromJSON(paste0(baseurl, ID, by_constituency, ".json?&_pageSize=500", "&_page=", i, extra_args), flatten = TRUE)
            if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
            pages[[i + 1]] <- mydata$result$items
        }

        df <- tibble::as_tibble(dplyr::bind_rows(pages))

        df$member <- NULL  # Removes superfluous member column

    }

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df, tidy_style)

        }

            df

    }
}



#' @rdname epetition
#' @export
hansard_epetition <- function(ID = NULL, by_constituency = FALSE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    df <- epetition(ID = ID, by_constituency = by_constituency, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

    df

}
