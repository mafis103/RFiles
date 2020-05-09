library(rvest)
library(tidyverse)
library(stringr)
library(dplyr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
#polls<-polls%>%rename(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))

polls<- polls %>% rename(
  "dates" = "Date(s) conducted",
  "remain" =  "Remain",
  "leave" = "Leave",
  "undecided" = "Undecided",
  "lead" = "Lead",
  "samplesize" = "Sample",
  "pollster" = "Conducted by",
  "poll_type" = "Polling type",
  "notes" = "Notes"
  
)
names(polls)
#?grep
polls<-polls%>%filter(str_detect(remain, "%"))
#polls<-as.numeric(str_replace(polls$remain, "%", ""))/100
#polls<-str_replace(polls$undecided,"N/A","0")

head(polls$dates)
temp <- str_extract_all(polls$dates,"\\d+\\s[a-zA-Z]{3,5}", )
head(temp)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date