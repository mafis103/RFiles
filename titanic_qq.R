options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>% filter(!is.na(Sex) & !is.na(Age)) %>%
  mutate(state = reorder(Age, Sex))
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
p<-titanic %>% ggplot(aes(sample=Age)) 
p+ geom_qq(dparams=params) + geom_abline()


