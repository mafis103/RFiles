# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit) 

table<-table(brexit_hit$poll_type,brexit_hit$hit)
chisq<-table %>%  chisq.test()
table
chisq$p.value
odds_online <- ((48 / 85) /  (37 / 85))
odds_phone <- ((10 / 42) /  (32 / 42))
#odds_online/odds_phone

brexit_polls%>%ggplot(aes(enddate,spread,colour = poll_type)) + geom_point() + geom_smooth(method = "loess",span=0.4) + geom_hline(yintercept=-0.038)
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long%>%ggplot(aes(enddate,proportion,colour=vote)) + geom_smooth(method = "loess",span=0.3)
