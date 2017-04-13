## ----eval=FALSE----------------------------------------------------------
#  library(hansard)
#  members_search("abbot")
#  #> Connecting to API
#  #> Retrieving page 1 of 1
#  #>     mnis_id                         home_page additional_name_value                         constituency_about
#  #> 1     172     http://www.dianeabbott.org.uk                 Julie http://data.parliament.uk/resources/146966
#  #> 2    1651                              <NA>             Granville                                       <NA>
#  #> 3    4249 http://www.annemariemorris.co.uk/                  <NA> http://data.parliament.uk/resources/147092
#  #> 4    3827       http://www.judiciary.gov.uk                Edmond                                       <NA>
#  #>            constituency_label_value family_name_value                full_name_value gender_value
#  #> 1 Hackney North and Stoke Newington            Abbott                Ms Diane Abbott       Female
#  #> 2                              <NA>           Hodgson Lord Hodgson of Astley Abbotts         Male
#  #> 3                      Newton Abbot            Morris              Anne Marie Morris       Female
#  #> 4                              <NA>         Neuberger   Lord Neuberger of Abbotsbury         Male
#  #>   given_name_value                                              label_value  party_value
#  #> 1            Diane                Biography information for Ms Diane Abbott       Labour
#  #> 2            Robin Biography information for Lord Hodgson of Astley Abbotts         <NA>
#  #> 3       Anne Marie              Biography information for Anne Marie Morris Conservative
#  #> 4            David   Biography information for Lord Neuberger of Abbotsbury         <NA>
#  #>                       twitter_value
#  #> 1 https://twitter.com/HackneyAbbott
#  #> 2                              <NA>
#  #> 3    https://twitter.com/AMMorrisMP
#  #> 4                              <NA>

## ----eval=FALSE----------------------------------------------------------
#  library(hansard)
#  members_search("abbot", tidy = FALSE)
#  #> Connecting to API
#  #> Retrieving page 1 of 1
#  #>                                   _about                          homePage additionalName._value
#  #> 1  http://data.parliament.uk/members/172     http://www.dianeabbott.org.uk                 Julie
#  #> 2 http://data.parliament.uk/members/1651                              <NA>             Granville
#  #> 3 http://data.parliament.uk/members/4249 http://www.annemariemorris.co.uk/                  <NA>
#  #> 4 http://data.parliament.uk/members/3827       http://www.judiciary.gov.uk                Edmond
#  #>                          constituency._about         constituency.label._value familyName._value                fullName._value
#  #> 1 http://data.parliament.uk/resources/146966 Hackney North and Stoke Newington            Abbott                Ms Diane Abbott
#  #> 2                                       <NA>                              <NA>           Hodgson Lord Hodgson of Astley Abbotts
#  #> 3 http://data.parliament.uk/resources/147092                      Newton Abbot            Morris              Anne Marie Morris
#  #> 4                                       <NA>                              <NA>         Neuberger   Lord Neuberger of Abbotsbury
#  #>   gender._value givenName._value                                             label._value party._value
#  #> 1        Female            Diane                Biography information for Ms Diane Abbott       Labour
#  #> 2          Male            Robin Biography information for Lord Hodgson of Astley Abbotts         <NA>
#  #> 3        Female       Anne Marie              Biography information for Anne Marie Morris Conservative
#  #> 4          Male            David   Biography information for Lord Neuberger of Abbotsbury         <NA>
#  #>                      twitter._value
#  #> 1 https://twitter.com/HackneyAbbott
#  #> 2                              <NA>
#  #> 3    https://twitter.com/AMMorrisMP
#  #> 4                              <NA>

## ----eval = FALSE--------------------------------------------------------
#  x <- mp_vote_record(172, "aye")
#  #> Retrieving page 1 of 4
#  #> Retrieving page 2 of 4
#  #> Retrieving page 3 of 4
#  #> Retrieving page 4 of 4
#  #> head(x)
#  #> # A tibble: 6 × 5
#  #>                                         about                                                   title               uin
#  #>                                       <chr>                                                   <chr>             <chr>
#  #> 1 http://data.parliament.uk/resources/714865 Pension Schemes Bill [Lords]: Report Stage New Clause 2 CD:2017-03-29:260
#  #> 2 http://data.parliament.uk/resources/714866 Pension Schemes Bill [Lords]: Report Stage New Clause 1 CD:2017-03-29:259
#  #> 3 http://data.parliament.uk/resources/714868  Pension Schemes Bill [Lords]: Report Stage Amendment 1 CD:2017-03-29:261
#  #> 4 http://data.parliament.uk/resources/713962    Bus Services Bill [Lords]: Report Stage New Clause 1 CD:2017-03-27:255
#  #> 5 http://data.parliament.uk/resources/713963    Bus Services Bill [Lords]: Report Stage New Clause 2 CD:2017-03-27:256
#  #> 6 http://data.parliament.uk/resources/714005     Bus Services Bill [Lords]: Report Stage Amendment 1 CD:2017-03-27:257
#  #> # ... with 2 more variables: date_value <date>, date_datatype <fctr>
#  >
#  

## ----eval = FALSE--------------------------------------------------------
#  #> research_topics_list <- research_topics_list()
#  #>
#  #> research_subtopics_list <- research_subtopics_list()
#  #>
#  #> research_types_list <- research_types_list()
#  #>
#  #> research_topics_list[[7]]
#  #> [1] "Defence"
#  #>
#  #> research_subtopics_list[[7]][10]
#  #> [1] "Falkland Islands"
#  #>
#  #> research_types_list[[1]]
#  #> [1] "Lords Library notes"
#  

## ----eval = FALSE--------------------------------------------------------
#  #> x <- research_briefings(topic = research_topics_list[[7]])
#  #>
#  #> x <- research_briefings(topic = research_topics_list[[7]], subtopic=research_subtopics_list[[7]][10])
#  #>
#  #> x <- research_briefings(topic = "Defence")
#  

## ----eval = FALSE--------------------------------------------------------
#  #> x <- research_briefings(subtopic = research_subtopics_list[[7]][10])
#  #>
#  #> x <- research_briefings(subtopic = "Falkland Islands")
#  #>
#  #> system.time(without_topic <- research_briefings(subtopic = research_subtopics_list[[7]][10]))
#  #> Retrieving page 1 of 1
#  #>   user  system elapsed
#  #>   1.12    2.59    4.71
#  #>
#  #> system.time(with_topic <- research_briefings(topic = research_topics_list[[7]], subtopic=research_subtopics_list[[7]][10]))
#  #> Retrieving page 1 of 1
#  #>    user  system elapsed
#  #>    0.47    1.31    1.89
#  #>
#  #> all.equal(with_topic, without_topic)
#  #> [1] TRUE

## ---- eval=FALSE---------------------------------------------------------
#  x <- hansard_generic("commonsansweredquestions.json")

