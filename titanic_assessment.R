options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>% filter(!is.na(Sex) & !is.na(Age)) %>%
  mutate(state = reorder(Age, Sex))

titanic

titanic %>% ggplot(aes(Age)) + 
  geom_density(alpha = 0.3, bw = 2) + facet_grid(. ~ Sex) 

females<-titanic_train %>%filter(!is.na(Sex) & Sex=="female" & Age<17)
#nrow(females)
proportion<-nrow(females)/314
proportion
males<-titanic_train %>%filter(!is.na(Sex) & Sex=="male" & Age<17)
#nrow(males)
proportionm<-nrow(males)/577
proportionm

titanic %>% filter (Age >=80)