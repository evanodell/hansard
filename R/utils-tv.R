


# 1st tv tidying function -------------------------------------------------

tv_tidy <- function(df, tidy_style) {
  if (nrow(df) > 0) {
    df$startDate._value <- gsub(
      "T", " ", df$startDate._value
    )

    df$startDate._value <- lubridate::parse_date_time(
      df$startDate._value, "Y-m-d H:M:Sz!*"
    )

    df$startDate._datatype <- "POSIXct"

    df$endDate._value <- gsub(
      "T", " ",
      df$endDate._value
    )

    df$endDate._value <- lubridate::parse_date_time(
      df$endDate._value, "Y-m-d H:M:Sz!*"
    )

    df$endDate._datatype <- "POSIXct"

    df$legislature <- dplyr::bind_rows(df$legislature)

    df$legislature.prefLabel._value <- df$legislature$prefLabel._value

    df$legislature_about <- df$legislature$`_about`

    df$legislature_about <- gsub(
      "http://data.parliament.uk/terms/", "",
      df$legislature_about
    )

    df$legislature <- NULL
  }

  df <- hansard_tidy(df, tidy_style)

  df
}



# 2nd tv tidying function -------------------------------------------------

tv_tidy2 <- function(df, mp_id, tidy_style) {
  if (nrow(df) > 0) {
    if (is.null(mp_id) == FALSE) {
      df <- tidyr::unnest_wider(df, "member", names_sep = "_")

      names(df) <- snakecase::to_any_case(names(df))

      df$member_label_value <- gsub(
        "Biography information for ", "",
        df$member_label_value
      )

      df$member_about <- gsub(
        "http://data.parliament.uk/terms/", "",
        df$member_about
      )

      df <- tibble::as_tibble(df)
    }
  }

  df <- hansard_tidy(df, tidy_style)

  df
}
