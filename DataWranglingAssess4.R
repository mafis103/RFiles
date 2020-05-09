library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits
data(brexit_polls)
k<-0
a<-character(0)
head(brexit_polls$startdate)
polls<-ymd(brexit_polls$startdate)
polls_end<-ymd(brexit_polls$enddate)
april <- str_extract(polls,"[0-9]{4}-04-\\d*")
#  for(i in 1:127){
#    if(is.na(april[i])==TRUE)
#    {
#      
#      i<-i+1
#    }
#    else{
#      k<-k+1
#      print(k)
#      i+1}
# }
week<-round_date(polls_end,unit="week", week_start="2016-06-12")
week<-str_extract(week,"2016-06-12")
# k<-0
#  for(i in 1:127){
#    if(is.na(week[i])==TRUE)
#    {
# 
#      i<-i+1
#    }
#    else{
#      k<-k+1
#      print(k)
#      i+1}
# }

day<-table(weekdays(polls_end))
day