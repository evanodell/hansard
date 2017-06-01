## ---- echo=TRUE, message=TRUE, warning=FALSE-----------------------------
library(hansard)
library(tibble)##for the `glimpse()` function
z <- mp_vote_record(172, "aye", start_date = "2017-01-01", end_date = "2017-05-03")
glimpse(z)


## ---- echo=TRUE, message=TRUE, warning=FALSE-----------------------------
x <- commons_divisions(722300)
glimpse(x)

## ---- echo=TRUE, message=TRUE, warning=FALSE-----------------------------
y <- commons_divisions(722300, summary=TRUE)
glimpse(y)

## ---- echo=TRUE, message=TRUE, warning=FALSE-----------------------------
research_topics_list <- research_topics_list()

research_subtopics_list <- research_subtopics_list()

research_types_list <- research_types_list()

research_topics_list[[7]]


research_subtopics_list[[7]][10]

research_types_list[[1]]



## ---- echo=TRUE, message=TRUE, warning=FALSE-----------------------------
a <- research_briefings(topic = research_topics_list[[7]])

b <- research_briefings(topic = research_topics_list[[7]], subtopic=research_subtopics_list[[7]][10])

c <- research_briefings(topic = "Defence")


## ---- echo=TRUE, message=TRUE, warning=FALSE-----------------------------

research_topics_list <- research_topics_list()

research_subtopics_list <- research_subtopics_list()

system.time(without_topic <- research_briefings(subtopic = research_subtopics_list[[7]][10]))

system.time(with_topic <- research_briefings(topic = research_topics_list[[7]], subtopic=research_subtopics_list[[7]][10]))

identical(with_topic, without_topic)


## ---- eval=FALSE---------------------------------------------------------
#  x <- hansard_generic("commonsansweredquestions.json")

## ----echo=TRUE-----------------------------------------------------------
library(hansard)
members_search("abbot")


## ---- echo=TRUE----------------------------------------------------------
library(hansard)
members_search("abbot", tidy = FALSE)

## ---- echo=TRUE----------------------------------------------------------
library(hansard)
members_search("abbot", tidy = TRUE, tidy_style = "period.case")

