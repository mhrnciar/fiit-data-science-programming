library(tidyverse)
data <- read_csv('../Skuska/players_22.csv')
data %>% View()

# 1. O koľko je nižší alebo vyšší minimálny defenzívny rating (defending) hráčov s výškou 170-185cm a s
# chudou stavbou tela (lean) oproti hráčom, ktorí majú unikátnu stavbu tela (unique)?
uniq <- data %>% filter(body_type == 'Unique') %>% summarise(min = min(defending, na.rm = TRUE))
lean <- data %>% filter(body_type == 'Lean (170-185)') %>% summarise(min = min(defending, na.rm = TRUE))
uniq - lean

# 2. Aké meno hráča FIFA 22 hrajúceho za FC Barcelonu, kto má najväčší rozdiel medzi trhovou hodnotou a mesačným platom?
data %>% filter(club_name == 'FC Barcelona') %>%
  mutate(difference = value_eur - wage_eur) %>%
  arrange(desc(difference)) %>% head(1)

# 3. Ktorý hráč z talianskej Serie A má najväčšie BMI (body mass index) a na akej pozícii hrá?
data %>% filter(league_name == 'Italian Serie A') %>%
  mutate(bmi = weight_kg / (height_cm / 100) ^ 2) %>%
  arrange(desc(bmi)) %>% select(long_name, bmi, player_positions)

# 4. Ktorí hráči z francúskej ligy 1 majú najnižšiu relatívnu výšku oproti celkovému priemeru všetkých hráčov?
data %>% filter(league_name == 'French Ligue 1') %>%
  mutate(norm_height = height_cm - mean(height_cm)) %>%
  arrange(norm_height) %>% select(long_name, norm_height)
