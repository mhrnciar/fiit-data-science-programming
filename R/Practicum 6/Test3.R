library(tidyverse)
library(magrittr)

setwd("/Users/mhrnciar/Desktop/School/FIIT/5. Semester/Programovanie pre dátovú vedu/Cvičenia/R/Practicum 6")
listOfFiles <- list.files(pattern = '.*\\.csv$')
data <- map(listOfFiles, read_csv)
names(data) <- listOfFiles

data <- map2(data, listOfFiles, ~ mutate(.x, season = .y))

# 1.
data %>% map(~ select(.x, short_name)) %>% reduce(intersect) %>% tally() #unlist() %>% length()

# 2.
data %>% map(~ filter(.x, short_name == 'L. Messi')) %>%
  map(~ select(.x, season, wage_eur, value_eur)) %>%
  reduce(rbind) %>% arrange(wage_eur)

# 3. & 4.
leagues <- c("Spain Primera Division", "German 1. Bundesliga", "English Premier League", "Italian Serie A")
data %>% map(~ filter(.x, league_name %in% leagues)) %>%
  map(~ select(.x, league_name, overall, potential, wage_eur, value_eur)) %>%
  reduce(rbind) %>%
  group_by(league_name) %>%
  summarise_if(is.double, mean) %>%
  mutate(wage_to_value = wage_eur / value_eur)