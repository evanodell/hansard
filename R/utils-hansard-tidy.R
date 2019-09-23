
# A function to make the results of calls to the API easier to work with.

hansard_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    names(df) <- gsub("\\.", "_", names(df), perl = TRUE)

    names(df) <- gsub(
      "([[:lower:]])([[:upper:]])", "\\1_\\2", names(df),
      perl = TRUE
    )

    names(df) <- gsub("__", "_", names(df), perl = TRUE)

    names(df) <- gsub("^_", "", names(df), perl = TRUE)

    names(df) <- tolower(names(df))

    names(df)[names(df) == "x_about"] <- "about"

    names(df)[names(df) == "x_value"] <- "value"

    if ("about" %in% names(df)) {
      df$about <- gsub("http://data.parliament.uk/resources/", "", df$about)
    }
    names(df) <- snakecase::to_any_case(names(df), case = tidy_style)
  }

  df
}
