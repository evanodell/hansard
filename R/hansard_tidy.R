
#A function to make the results of calls to the API easier to work with.

#A function to make the results of calls to the API easier to work with, mostly though fixing variable names, which by default contain non alpha-numeric characters and appear to use an inconsistent/idiosyncratic naming convention, at least by the standards of the various naming conventions used in R. Dates and datetimes are converted to POSIXct class. Some extra URL data included in the API is also stripped out.

#@param df The tibble to tidy.
#@param tidy_style The style to tidy variable names to.
#@export
#@return A tidied tibble

hansard_tidy <- function(df, tidy_style=c("snake_case", "camelCase", "period.case")) {

    names(df) <- gsub("\\.", "_", names(df))

    names(df) <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", names(df))

    names(df) <- gsub("__", "_", names(df))

    names(df) <- gsub("^_", "", names(df))

    names(df) <- tolower(names(df))

    names(df)[names(df) == "x_about"] <- "about"

    names(df)[names(df) == "x_value"] <- "value"

    if (tidy_style == "camelCase") {

        names(df) <- gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", names(df), perl = TRUE)

        substr(names(df), 1, 1) <- tolower(substr(names(df), 1, 1))

    } else if (tidy_style == "period.case") {

        names(df) <- gsub("_", ".", names(df))

    }

    df

}
