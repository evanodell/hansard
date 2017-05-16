

#' Imports data on elections
#' @param ID Accepts an ID for a general or by-election from the 2010 general election onwards, and returns the date and type of the elction. If NULL, returns the date and type of all available elections. Defaults to NULL.
#' @param type Accepts "General Election" or "By-election" as arguments if ID is NULL. Returns all General Elections or all By-elections.
#' @param extra_args Additional parameters to pass to API. Defaults to NULL.
#' @param tidy Fix the variable names in the tibble to remove special characters and superfluous text, and converts the variable names to a consistent style. Defaults to TRUE.
#' @param tidy_style The style to convert variable names to, if tidy = TRUE, tidy_style="snake_case". Accepts one of "snake_case", "camelCase" and "period.case". Defaults to "snake_case".
#' @return A tibble with details on all elections from the 2010 general election onwards, subject to function parameters.
#' @keywords Elections
#' @export
#' @examples \dontrun{
#'
#' x <- elections(517994)
#' }


elections <- function(ID = NULL, type=c(NULL,"General Election", "By-election"), extra_args = NULL, tidy = TRUE, tidy_style="snake_case") {

    if (is.null(ID) == FALSE) {

        ID <- paste0("/", ID, ".json?")

        baseurl <- "http://lda.data.parliament.uk/elections"

        message("Connecting to API")

        elect <- jsonlite::fromJSON(paste0(baseurl, ID, extra_args), flatten = TRUE)

        df <- elect$result$primaryTopic

        df <- tibble::as_tibble(as.data.frame(df))

    } else {

      if(is.null(type)==FALSE){

        type_query <- paste0("&electionType=", type)
        type_query <- utils::URLencode(type_query)
      } else {
        type_query <- NULL
      }

        ID <- ".json?&_pageSize=500"

        baseurl <- "http://lda.data.parliament.uk/elections"

        message("Connecting to API")

        elect <- jsonlite::fromJSON(paste0(baseurl, ID,type_query, extra_args), flatten = TRUE)

        df <- tibble::as_tibble(elect$result$items)

    }

    if (nrow(df) == 0) {
        message("The request did not return any data. Please check your search parameters.")
    } else {

        if (tidy == TRUE) {

            df <- hansard_tidy(df, tidy_style)

            names(df)[names(df)=="x_about"] <- "about"

            names(df)[names(df)=="x_value"] <- "value"

            df

        } else {

            df

        }

    }

}
