
# Creating research_types

x <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtypes.json?")

research_types_list <- as.list(x$result$items$prefLabel$`_value`)


devtools::use_data(research_types_list)
