


test <- commons_divisions(start_date="2015-03-26", end_date = "2016-03-09")


ggplot2::qplot(test$date_value)

library(lubridate)


test$month <- month(test$date_value)

test$month <- as.factor(test$month)
ggplot2::qplot(test2$month)


View(test)
