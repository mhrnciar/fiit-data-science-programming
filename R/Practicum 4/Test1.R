library(tidyverse)
data <- read_csv('Practicum 4/players_22.csv')
data %>% View()

# 1. What are the three least valuable Slovak goal keepers, or GKs, with their overall and potential ranks
data %>% filter(nationality == 'Slovakia', player_positions == 'GK')%>%
  arrange(value_eur) %>% select(long_name, player_positions, nationality, overall, potential, value_eur) %>%
  head(3)

# 2. Show players under 22 years of age, show the biggest gap between their overall
# and potential ranks and earn below 600 EUR/month
data %>% filter(age < 22, wage_eur < 600) %>%
  mutate(rank_diff = abs(potential - overall)) %>%
  arrange(desc(rank_diff)) %>%
  select(long_name, rank_diff, wage_eur, age)

# 3. Has Primera Division or Premier League more players
# playing dominantly with their left foot or their right foot?
data %>% filter(league_name %in% c('Spain Primera Division', 'English Premier League')) %>%
  group_by(league_name, preferred_foot) %>% tally()

# 4. What value distribution (min-max) and central tendency (mean or medium) should we expect
# for age of players playing in the national teams (i.e. having a nation_position or
# nation_jersey_number) with club contract valid until 2024?
data %>% drop_na(nation_jersey_number, nation_position) %>%
  filter(club_contract_valid_until <= 2024) %>%
  summarise(mean_age = mean(age),
            min_age = min(age),
            max_age = max(age),
            n = n())