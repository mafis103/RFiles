# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N<-1500

expected_remain<-N*p # expected remaining in 1500
se<-sqrt(p*(1-p)/N) # standard error Xhat
sed<-sqrt(N*p*(1-p)) # standard deviation
expd<- p-(1-p) #expected value of d


brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
brexit_polls<- brexit_polls %>% mutate(se_hat=2*sqrt(x_hat*(1-x_hat)/samplesize))

ci<-c(mean(brexit_polls$x_hat) - qnorm(0.975)*mean(brexit_polls$se_hat), mean(brexit_polls$x_hat) + qnorm(0.975)*mean(brexit_polls$se_hat))

ci2<-c(brexit_polls$x_hat[1] - qnorm(0.975)*brexit_polls$se_hat[1], brexit_polls$x_hat[1] + qnorm(0.975)*brexit_polls$se_hat[1])


