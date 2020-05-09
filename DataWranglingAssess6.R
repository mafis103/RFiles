library(tidyverse)
library(gutenbergr)
library(tidytext)
library(dplyr)
options(digits = 3)

pride<-str_detect(gutenberg_metadata$title,"Pride and Prejudice",negate=FALSE)
# k<-0
#  for(i in 1:length(pride)){
#    if(is.na(pride[i])==TRUE | pride[i]==FALSE )
#    {
# 
#      i<-i+1
#    }
#    else{
#      k<-k+1
#      print(k)
#      print(i)
#      i+1}
# }
prej<-gutenberg_works(title=="Pride and Prejudice")
text<-gutenberg_download(1342)
words<-text %>% unnest_tokens(word, text)
words<- words %>% filter(!word %in% stop_words$word )
str_replace(words$word,"[a-zA-Z]*\\d+[a-zA-Z]*","")
head(words)
afinn <- get_sentiments("afinn")

sentiments<-words %>% inner_join(afinn, by = "word") %>% 
  select(word, value)
table(sentiments)
class(sentiments$value)
100*sum(sentiments$value>0)/6065
sum(sentiments$value==4)

