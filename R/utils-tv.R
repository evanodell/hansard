


# 1st tv tidying function -------------------------------------------------

tv_tidy <- function(df, tidy_style) {

    if (nrow(df) > 0) {

        df$startDate._value <- stringi::stri_replace_all_fixed(
          df$startDate._value, "T", " ", vectorize_all = FALSE
          )

        df$startDate._value <- lubridate::parse_date_time(
          df$startDate._value, "Y-m-d H:M:Sz!*"
          )

        df$startDate._datatype <- "POSIXct"

        df$endDate._value <- stringi::stri_replace_all_fixed(
          df$endDate._value, "T", " ", vectorize_all = FALSE
          )

        df$endDate._value <- lubridate::parse_date_time(
          df$endDate._value, "Y-m-d H:M:Sz!*"
          )

        df$endDate._datatype <- "POSIXct"

        df$legislature <- dplyr::bind_rows(df$legislature)

        df$legislature.prefLabel._value <- df$legislature$prefLabel._value

        df$legislature_about <- df$legislature$`_about`

        df$legislature_about <- stringi::stri_replace_all_fixed(
          df$legislature_about,
          "http://data.parliament.uk/terms/", "", vectorize_all = FALSE
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

            df <- tidyr::unnest_(df, "member")

            names(df)[names(df) == "_about1"] <- "member_about"

            names(df)[names(df) == "label._value"] <- "member_label_value"

            df$member_label_value <- stringi::stri_replace_all_fixed(
              df$member_label_value,
              "Biography information for ", "", vectorize_all = FALSE
              )

            df$member_about <- stringi::stri_replace_all_fixed(
              df$member_about, "http://data.parliament.uk/terms/", "",
              vectorize_all = FALSE)

            df <- tibble::as.tibble(df)

        }

    }

    df <- hansard_tidy(df, tidy_style)

    df

}
