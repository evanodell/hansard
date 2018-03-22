

# elections tidying -------------------------------------------------------

elections_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$date._value <- as.POSIXct(df$date._value)

    df$date._datatype <- "POSIXct"
  }

  df <- hansard_tidy(df, tidy_style)

  df
}


## election_candidates tidy function -------------------------------------
elect_can_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$election._about <- stringi::stri_replace_all_fixed(
      df$election._about, "http://data.parliament.uk/resources/", "",
      vectorize_all = FALSE
    )

    df$constituency._about <- stringi::stri_replace_all_fixed(
      df$constituency._about, "http://data.parliament.uk/resources/", "",
      vectorize_all = FALSE
    )
  }

  df <- hansard_tidy(df, tidy_style)

  df
}
