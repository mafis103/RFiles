library(rvest)
library(tidyr)
library(dbplyr)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
#html_text(nodes[19:21])
#html_table(nodes[19:21])
#length(nodes)
tab1_s<-nodes[10] %>% html_table()
tab2_s<-html_table(nodes[19])
#tab1 %>% html_table
tab1 <- tab1_s[[1]] %>% select(2:4) %>% setNames(c("Team", "Payroll", "Average"))
tab2 <- tab2_s[[1]] %>% setNames(c("Team", "Payroll", "Average"))
tab1=tab1[-1,]
tab2=tab2[-1,]
bigtab<-full_join(tab1,tab2, by="Team")
bigtab


