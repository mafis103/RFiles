library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

p <- seq(.01, .03, .001)

pandemic_dead<-p[11]
loss <- -150000
gain <- 1150
avg <- 1000*(loss*pandemic_dead + gain*(1-pandemic_dead))
se <- sqrt(1000)*abs((loss-gain))*sqrt((pandemic_dead)*(1-pandemic_dead))
avg
se

pnorm(-1000000,avg,se)
p[11]