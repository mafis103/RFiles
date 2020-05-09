# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481
june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01")
nrow(june_polls)
june_polls <- june_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2*se_x_hat,
         ci_lower_spread = spread - qnorm(0.975)*se_spread,
         ci_upper_spread = spread + qnorm(0.975)*se_spread)
mean(june_polls$ci_lower_spread < 0 & june_polls$ci_upper_spread > 0)
mean(june_polls$ci_lower_spread > 0)
june_polls <- june_polls %>%
  mutate(hit = (2*p-1 > ci_lower_spread) & (2*p-1 < ci_upper_spread))
mean(june_polls$hit)

#june_polls%>%group_by(poll_type)%>%ggplot(aes(poll_type,spread)) + geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2) %>%
  mutate(se_x_hat = sqrt(p_hat*(1-p_hat)/N)) %>% 
  mutate(se_spread = 2*se_x_hat, ci_lower_spread = spread - qnorm(0.975)*se_spread,
         ci_upper_spread = spread + qnorm(0.975)*se_spread)
combined_by_type
#0.01815
0.03374