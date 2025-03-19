library(tidyverse)

df <- read_csv('titanic.csv')

titanic_fc <- filter(df, class=='First')

titanic_fc_fare <- arrange(titanic_fc, fare)
head(titanic_fc_fare,2)

#pipe

df %>%
  filter(class=='First') %>%
  arrange(fare) %>%
  head(2)

titanic_2 <- df %>%
  filter(class=='First') %>%
  arrange(fare) %>%
  head(2)