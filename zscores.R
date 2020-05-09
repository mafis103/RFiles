library(tidyverse)
set.seed(16)
act_scores<-rnorm(10000,20.9,5.7)
pnorm(10,mean(act_scores),sd(act_scores))


#x <- seq(1, 36)
#data.frame(x, f = dnorm(x,20.9,5.7)) %>%
#  ggplot(aes(x, f)) +
#  geom_line()

z_scores<-(act_scores-mean(act_scores))/sd(act_scores)

1-pnorm(2,mean(z_scores),sd(z_scores))

qnorm(0.975,mean(act_scores),sd(act_scores))

prob<-function(number){
  pnorm(number,mean(act_scores),sd(act_scores))
}

sapply(1:36,prob)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles<-quantile(act_scores,p)
theoretical_quantiles<-qnorm(p,20.9,5.7)
qqplot(theoretical_quantiles,sample_quantiles)