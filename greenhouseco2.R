library(tidyverse)
library(dslabs)
library(dplyr)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

greenhouse_gases %>% filter (year>1850) %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850), col = "blue") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#p<-temp_carbon %>% filter(!is.na(carbon_emissions)) %>%ggplot(aes(year,carbon_emissions)) + geom_line()
#p
names(historic_co2)

co2_time<-historic_co2 %>% filter (!is.na(co2) & !is.na(year) & !is.na(source)) %>%
  ggplot(aes(year,co2,col=source)) +
  geom_line() + scale_x_continuous(limits=c(-3000,2018))
co2_time