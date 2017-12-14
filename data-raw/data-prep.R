
# Bill Publication Types -----------------------------------------------------------



x <- bill_publications(start_date="2015-01-01", verbose = T)

bill_publication_types <- unique(x$publication_type_value)

bill_publication_types <- bill_publication_types[!is.na(bill_publication_types)]

bill_publication_types

devtools::use_data(bill_publication_types, overwrite = T)
