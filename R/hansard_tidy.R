


#' hansard_tidy
#'
#' @param df The tibble to tidy
#' @return A tidied tibble


hansard_tidy <- function(df) {

    names(df) <- gsub("\\.", "_", names(df))

    names(df) <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", names(df))

    names(df) <- gsub("__", "_", names(df))

    names(df) <- gsub("^_", "", names(df))

    names(df) <- tolower(names(df))

    df

}
