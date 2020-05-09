library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits
mean(stars$magnitude)
sd(stars$magnitude)

#stars %>% ggplot(aes(magnitude)) + geom_density()
#stars %>% ggplot(aes(temp)) + geom_boxplot()
stars%>%  
  ggplot(aes(temp,magnitude,col=type)) + geom_point() 
