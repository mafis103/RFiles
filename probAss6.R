library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

set.seed(29)
n <- 1000
B <- 10000
p_loss <- 0.015
loss <- -150000
gain <- 3268.063



S<- replicate(B,{
  p_local<-p_loss + sample(seq(-0.01, 0.01, length = 100), 1)
  K<-sample(c(loss,gain),n,prob=c(p_local,1-p_local),replace=TRUE)
  sum(K)
  
})
L<-S< -1000000
sum(L)/length(S)
mean(L)

avg <- 1000*(loss*p_loss + gain*(1-p_50_dead))
se <- sqrt(1000)*abs((loss-gain))*sqrt((p_50_dead)*(1-p_50_dead))
avgm <- 1000*(loss*p_50m_dead + premium*(1-p_50m_dead))
sem <- sqrt(1000)*abs((loss-premium))*sqrt((p_50m_dead)*(1-p_50m_dead))


pnorm(-1000000,avg,se)

