options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex)) %>% filter(!is.na(Sex) & !is.na(Age) & !is.na(Survived))

overall_pass<-sum(as.numeric(titanic$Survived))
pass_survive<-titanic %>% filter (Survived == 1)
pass_dead <- nrow(titanic$Survived) - nrow(pass_survive)


print("Overall passengers") 
overall_pass

print("Survivors: ") 
nrow(pass_survive)

print("Deaths:") 
overall_pass - nrow(pass_survive)


titanic %>% ggplot(aes(Age, fill=Survived)) + 
  geom_density(alpha = 0.2, bw = 2) 
