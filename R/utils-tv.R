
tv_tidy <- function(df){

  if(nrow(df)>0){

    df$startDate._value <- gsub("T", " ", df$startDate._value)

    df$startDate._value <- lubridate::parse_date_time(df$startDate._value, "Y-m-d H:M:Sz!*")

    df$startDate._datatype <- "POSIXct"

    df$endDate._value <- gsub("T", " ", df$endDate._value)

    df$endDate._value <- lubridate::parse_date_time(df$endDate._value, "Y-m-d H:M:Sz!*")

    df$endDate._datatype <- "POSIXct"

    df$legislature <- dplyr::bind_rows(df$legislature)

    df$legislature.prefLabel._value <- df$legislature$prefLabel._value

    df$legislature_about <- df$legislature$`_about`

    df$legislature_about <- gsub("http://data.parliament.uk/terms/", "", df$legislature_about)

    df$legislature <- NULL

  }

    df

}


tv_tidy2 <- function(df){

  if(nrow(df)>0){

    df <- tidyr::unnest_(df, "member")

    names(df)[names(df)=="_about1"] <- "member_about"

    names(df)[names(df)=="label._value"] <- "member_label_value"

    df$member_label_value <- gsub("Biography information for ", "", df$member_label_value)

    df$member_about <- gsub("http://data.parliament.uk/terms/", "", df$member_about)

    df <- tibble::as.tibble(df)

    }

  df

}
