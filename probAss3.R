library(tidyverse)
head(esoph)
nrow(esoph)

all_cases<-sum(esoph$ncases)
all_controls<-sum(esoph$ncontrols)

p<- esoph %>% filter(ncases!=0 & (tobgp=="30+" | alcgp=="120+"))
ca<-sum(p$ncases)/all_cases

p<- esoph %>% filter(ncontrols!=0 & (tobgp=="30+" | alcgp=="120+"))
co<-sum(p$ncontrols)/all_controls

ca/co