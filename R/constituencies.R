
#' House of Commons constituencies
#'
#' Imports data on House of Commons constituencies, returning a tibble of all current and former Westminster constituencies.
#' @param current If \code{TRUE}, returns only current constituencies. If FALSE, returns all current and former constituencies. Defaults to \code{TRUE}.
#' @inheritParams all_answered_questions
#' @return A tibble with details of Westminster constituencies.
#' @export
#' @examples \dontrun{
#' x <- constituencies()
#'
#' y <- constituencies(current = FALSE)
#' }



constituencies <- function(current = TRUE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

    baseurl <- "http://lda.data.parliament.uk/constituencies.json?"

    if(verbose==TRUE){message("Connecting to API")}

    conts <- jsonlite::fromJSON(paste0(baseurl, extra_args), flatten = TRUE)

    jpage <- floor(conts$result$totalResults/500)

    pages <- list()

    if(current==TRUE){

      current_query <- "&exists-endedDate=false"

    } else {

      current_query <- NULL

    }

    for (i in 0:jpage) {
        mydata <- jsonlite::fromJSON(paste0(baseurl, "_pageSize=500&_page=", i, extra_args, current_query), flatten = TRUE)
        if(verbose==TRUE){message("Retrieving page ", i + 1, " of ", jpage + 1)}
        pages[[i + 1]] <- mydata$result$items
    }

    df <- tibble::as_tibble(dplyr::bind_rows(pages))

    if (nrow(df) == 0 && verbose==TRUE) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- cons_tidy(df, current, tidy_style)

        }

            df

    }
}


#' @rdname constituencies
#' @export
hansard_constituencies <- function(current = TRUE, extra_args = NULL, tidy = TRUE, tidy_style = "snake_case", verbose=FALSE) {

  df <- constituencies(current=current, extra_args = extra_args, tidy = tidy, tidy_style = tidy_style, verbose=verbose)

  df

}

