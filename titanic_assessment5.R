options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>% filter(!is.na(Sex) & !is.na(Age) & !is.na(Survived) & !is.na(Pclass))

titanic %>% ggplot(aes(Age, y=..count..,fill=Survived)) + geom_density() +
  facet_grid(rows=vars(Sex),cols=vars(Pclass)) 
