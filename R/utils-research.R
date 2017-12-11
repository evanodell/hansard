
# Tidying research briefing retrievals
research_tidy <- function(df){

          df$date._value <- gsub("T", " ", df$date._value)

            df$date._value <- lubridate::parse_date_time(df$date._value, "Y-m-d H:M:Sz!*")

            df$date._datatype <- "POSIXct"

            df$description <- as.character(df$description)

            df$description[df$description == "NULL"] <- NA

            for (i in 1:nrow(df)) {

                if (is.null(df$section[[i]]) == FALSE) {

                  df$section[[i]] <- hansard_tidy(df$section[[i]], tidy_style)

                }
            }

            df <- hansard_tidy(df, tidy_style)

}
