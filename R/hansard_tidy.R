


#' hansard_tidy
#'
#' @param df The tibble to tidy
#' @param tidy_style The style to tidy variable names to.
#' @return A tidied tibble


hansard_tidy <- function(df, tidy_style) {

    names(df) <- gsub("\\.", "_", names(df))

    names(df) <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", names(df))

    names(df) <- gsub("__", "_", names(df))

    names(df) <- gsub("^_", "", names(df))

    names(df) <- tolower(names(df))

    names(df)[names(df)=="x_about"] <- "about"

    names(df)[names(df)=="x_value"] <- "value"

    if(tidy_style=="camelCase") {

      names(df) <- gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", names(df), perl = TRUE)

      substr( names(df), 1, 1) <- tolower(substr(names(df), 1, 1))

    } else if (tidy_style=="period.case") {

      names(df) <- gsub("_", ".", names(df))

    }

    df

}
