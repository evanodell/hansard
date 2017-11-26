
## Tidy function for constituency calls

cons_tidy <- function(df, current, tidy_style) {
    
    if (nrow(df) > 0) {
        
        if (is.null(current) || current == FALSE) {
            
            df$endedDate._value <- as.POSIXct(df$endedDate._value)
            
            df$endedDate._datatype <- "POSIXct"
            
        }
        
        df$startedDate._value <- as.POSIXct(df$startedDate._value)
        
        df$startedDate._datatype <- "POSIXct"
        
        df <- hansard_tidy(df, tidy_style)
        
    }
    
    df
    
}
