
# A function to make the results of calls to the API easier to work with.

hansard_tidy <- function(df, tidy_style) {
    
    if (nrow(df) > 0) {
        
        names(df) <- gsub("\\.", "_", names(df), perl = TRUE)
        
        names(df) <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", names(df), perl = TRUE)
        
        names(df) <- gsub("__", "_", names(df), perl = TRUE)
        
        names(df) <- gsub("^_", "", names(df), perl = TRUE)
        
        names(df) <- tolower(names(df))
        
        names(df)[names(df) == "x_about"] <- "about"
        
        names(df)[names(df) == "x_value"] <- "value"
        
        if (tidy_style == "camelCase") {
            
            names(df) <- gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", names(df), 
                perl = TRUE)
            
            substr(names(df), 1, 1) <- tolower(substr(names(df), 1, 1))
            
        } else if (tidy_style == "period.case") {
            
            names(df) <- gsub("_", "\\.", names(df), perl = TRUE)
            
        }
        
    }
    
    df
    
}
