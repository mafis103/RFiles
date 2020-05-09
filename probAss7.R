library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)


death_prob %>% filter(sex=="Male" & age==50)
p_50m_dead<-0.005013
p_50_dead<-0.003193
loss <- -150000
premium<-1459
gain <- 1150
avg <- 1000*(loss*p_50_dead + gain*(1-p_50_dead))
se <- sqrt(1000)*abs((loss-gain))*sqrt((p_50_dead)*(1-p_50_dead))
avgm <- 1000*(loss*p_50m_dead + premium*(1-p_50m_dead))
sem <- sqrt(1000)*abs((loss-premium))*sqrt((p_50m_dead)*(1-p_50m_dead))

pnorm(0,avgm,sem)

n <- 1000
l <- loss
z <- qnorm(0.019254)
x <- -l*( n*p_50m_dead - z*sqrt(n*p_50m_dead*(1-p_50m_dead)))/ ( n*(1-p_50m_dead) + z*sqrt(n*p_50m_dead*(1-p_50m_dead)))
x/150000    # interest rate
l*p_50m_dead + x*(1-p_50m_dead)    # expected value of the profit per loan
n*(loss*p_50m_dead + x*(1-p_50m_dead))