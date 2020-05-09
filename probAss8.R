library(tidyverse)
library(dslabs)
data(death_prob)

p<-0.015
loss <- -150000
premium<-3268.063
gain <- 1150
avg <- 1000*(loss*p_50_dead + gain*(1-p_50_dead))
se <- sqrt(1000)*abs((loss-gain))*sqrt((p_50_dead)*(1-p_50_dead))

n <- 1000
l <- loss
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/150000    # interest rate
x
l*p + x*(1-p)    # expected value of the profit per loan
n*(loss*p + x*(1-p))