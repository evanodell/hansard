
# Code for creating the `research_topics_list` file.


x <- jsonlite::fromJSON("http://lda.data.parliament.uk/researchbriefingtopics.json?", flatten = TRUE)

y <- x$result$items$prefLabel._value

research_topics_list <- as.list(y)

research_subtopics_list <- list()

for (i in y) {

    i <- URLencode(i)

    g <- jsonlite::fromJSON(paste0("http://lda.data.parliament.uk/researchbriefingsubtopics/", i, ".json?"), flatten = TRUE)


    i <- URLdecode(i)

    research_subtopics_list[[i]] <- g$result$items$prefLabel._value

}

research_subtopics_list
