library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)


p<-temp_carbon %>% filter(!is.na(temp_anomaly) & !is.na(ocean_anomaly) & !is.na(land_anomaly)) %>%ggplot(aes(year,temp_anomaly)) + geom_line() + geom_line(aes(year,land_anomaly,col="land")) + geom_line(aes(year,ocean_anomaly, col="ocean"))
p <- p + geom_hline(aes(yintercept = 0), col = "blue") + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p