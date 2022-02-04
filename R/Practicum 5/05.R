library(tidyverse)
library(gmodels)
players <- read_csv('../Skuska/players_22.csv')
players %>% View()

# 1.
num <- dim(players)[1]
data <- map(1:20, ~ players[sort(sample(1:num, size = 0.1*num)),])

# 2.
models <- map(data, ~ lm(.x$overall ~ .x$value_eur))

# 3.
functions_list <- list(coeficients = coef, residuals = residuals)
f <- function (x) { sapply(functions_list, function (g) g(x)) }
extracted_data <- map(models, ~ f(.x))

# 4.
sd(map_dbl(models, ~ coef(.x)[1]))  # intercept
sd(map_dbl(models, ~ coef(.x)[2]))  # slope

# 5.
rss <- map_dbl(models, ~ sum(resid(.x) ^ 2))
rse <- map_dbl(rss, ~ sqrt(.x / 0.5 * num - 2))
boxplot(rss)
boxplot(rse)

