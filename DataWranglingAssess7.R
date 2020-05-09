library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
#system("cmd.exe", input = paste("start", fn))

text<-pdf_text(fn)
p9<-text[9]
x<-str_split(p9,"\n")
s<-x[[1]]
s<-str_trim(s,side="both")
header_index<-2
header<-s[header_index]
header <- str_split(header,"\\s+", simplify=TRUE)
month<-"SEPT"
header<-c("2015","2016","2017","2018")
tail_index<-35

n<-str_count(s,"\\d+")
s<-c(s[3:5],s[7],s[8],s[10:34])
s <- str_remove_all(s, "[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
tab<-s
tab<-as.numeric(tab)
tab<-matrix(data=tab, ncol=5, nrow=30)
colnames(tab) <- c("day", header)
tab<-data.frame(tab)
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

#?ggplot
tab %>% filter(year!="x2018") %>% 
  ggplot(aes(day,deaths,color=year)) + geom_line()
