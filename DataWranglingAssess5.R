library(dslabs)
library(lubridate)
data(movielens)


dates<-as_datetime(movielens$timestamp)
years<-table(year(dates))
hours<-table(hour(dates))
hours
