options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>% filter(!is.na(Sex) & !is.na(Age) & !is.na(Survived) & !is.na(Fare) & Fare>0)

titanic %>%ggplot(aes(as.numeric(Survived),Fare,group=Survived)) + 
  geom_boxplot() +  
  scale_y_continuous(trans = "log2") + 
  geom_point(alpha = 0.2) + geom_jitter(width = 0.1, alpha=0.2) 

leb<- titanic %>% filter (Survived==1)
stirb<-titanic %>% filter (Survived==0)
mean(leb$Fare)
mean(stirb$Fare)
quantile(leb$Fare)
quantile(stirb$Fare)