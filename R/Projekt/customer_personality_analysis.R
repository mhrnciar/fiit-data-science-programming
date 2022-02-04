# Title     : Customer Personality Analysis
# Objective : Analyse the impact of income on the amounts of money spent on wine, gold, sweets and meat
# Created by: Matej Hrniar, Zuzana Rohacova
# Created on: 20/10/2021

library(tidyverse)
library(ggpubr)
library(gmodels)

# Load dataset and open view in another window
df <- read_delim('../Projekt/marketing_campaign.csv')
df %>% View()

# Show the type of every column
map(df, ~ typeof(.x))

# Use summary to view basic information about every column. The only column with missing data
# is Income with 24 NA's
summary(df)

# Print all rows with missing data - 24 rows with missing Income
df[!complete.cases(df), ]

# Make a copy of dataset
data <- df

# Impute the missing Income with mean
data$Income[is.na(data$Income)] <- mean(data$Income, na.rm = TRUE)

# Now we have 0 rows with missing data
sum(!complete.cases(data))

# Print unique values of marital status and education
unique(data$Marital_Status)
unique(data$Education)

# Clean absurd and duplicated values
data$Education[data$Education == 'Graduation'] <- 'Bachelor'
data$Education[data$Education == '2n Cycle'] <- 'Master'

data$Marital_Status[data$Marital_Status == 'Alone'] <- 'Single'
data$Marital_Status[data$Marital_Status == 'YOLO'] <- 'Absurd'

# Print unique values again to see if the columns are cleaned
unique(data$Marital_Status)
unique(data$Education)

# Plot income by marital status and education level
ggplot(data, aes(ID, Income)) + geom_point(aes(colour = Marital_Status))
ggplot(data, aes(ID, Income)) + geom_line(aes(colour = Education))

# We can see some outliers in income which need to be removed before fitting a linear
# model, or it could return incorrect results. Outliers can be identified using interquantile
# range (IQR), which is a method used in boxplots: all observations above 75th quantile +
# 1.5 * IQR and below 25th quantile - 1.5 * IQR are treated as outliers and removed.
q <- quantile(data$Income, na.rm = TRUE)
iqr <- q[['75%']] - q[['25%']]
cutoff <- iqr * 1.5
data <- data %>% filter(between(Income, q[['25%']] - cutoff, q[['75%']] + cutoff))

# Distribution of education levels
edu <- data %>% group_by(Education) %>% summarise(n = n()) %>%
  ggplot(aes(x = '', y = n, fill = Education)) +
  geom_bar(stat = 'identity', width = 1) + coord_polar('y', start = 0) +
  labs(x = '', y = '', title = 'Education distribution') +
  theme(axis.ticks = element_blank())

# Distribution of marital status
marit <- data %>% group_by(Marital_Status) %>% summarise(n = n()) %>%
  ggplot(aes(x = '', y = n, fill = Marital_Status)) +
  geom_bar(stat = 'identity', width = 1) + coord_polar('y', start = 0) +
  labs(x = '', y = '', title = 'Marital status distribution') +
  theme(axis.ticks = element_blank())

# Distribution of kids at home
kids <- data %>% group_by(Kidhome) %>% summarise(n = n()) %>%
  ggplot(aes(x = '', y = n, fill = Kidhome)) +
  geom_bar(stat = 'identity', width = 1) + coord_polar('y', start = 0) +
  labs(x = '', y = '', title = 'Distribution of kids') +
  theme(axis.ticks = element_blank())

# Distribution of teens at home
teen <- data %>% group_by(Teenhome) %>% summarise(n = n()) %>%
  ggplot(aes(x = '', y = n, fill = Teenhome)) +
  geom_bar(stat = 'identity', width = 1) + coord_polar('y', start = 0) +
  labs(x = '', y = '', title = 'Distribution of teens') +
  theme(axis.ticks = element_blank())

# Density of income by number of teens
teen_density <- data %>% ggplot(aes(x = Income, group = Teenhome, colour = Teenhome)) +
  geom_density() + labs(title = 'Density of income by number of teens')

# Density of income by number of teens
kids_density <- data %>% ggplot(aes(x = Income, group = Kidhome, colour = Kidhome)) +
  geom_density() + labs(title = 'Density of income by number of kids')

# Put the graphs into one figure
ggarrange(edu, marit, kids, teen, kids_density, teen_density, ncol = 2, nrow = 3)

# Prepare linear models, we are going to analyse
models <- list(lm(MntWines ~ Income, data =  data),
               lm(MntGoldProds ~ Income, data =  data),
               lm(MntSweetProducts ~ Income, data = data),
               lm(MntMeatProducts ~ Income, data =  data))

# Helper function to plot the regression line from linear model on scatterplot
plotReg <- function (model) {
  ggplot(model$model, aes_string(x = names(model$model)[2], y = names(model$model)[1])) +
    geom_point() +
    geom_abline(slope = coef(model)[[2]], intercept = coef(model)[[1]], col = 'red') +
    labs(title = paste(names(model$model[1]),
                       ' Intercept =', signif(model$coef[[1]],4),
                       ' Slope =', signif(model$coef[[2]], 4)))
}

# Map the models on the function and create one figure from them
graphs <- map(models, ~ plotReg(.x))
ggarrange(graphs[[1]], graphs[[2]], graphs[[3]], graphs[[4]], ncol = 2, nrow = 2)

# Testing helper function using stat_smooth function to plot regression to determine if our function
# plotReg plots the regression correctly
plotSmooth <- function (model) {
  ggplot(model$model, aes_string(x = names(model$model)[2], y = names(model$model)[1])) +
  geom_point() + stat_smooth(method = 'lm', col = 'red') +
  labs(title = paste(names(model$model[1]),
                       ' Intercept =', signif(model$coef[[1]],4),
                       ' Slope =', signif(model$coef[[2]], 4)))
}

# Again, map the models on the function and compare with our function
graphs <- map(models, ~ plotSmooth(.x))
ggarrange(graphs[[1]], graphs[[2]], graphs[[3]], graphs[[4]], ncol = 2, nrow = 2)

# Helper function to plot grouped models by inserted 'y' and 'grouper'. If the grouper
# value is numeric, the color scale is changed to make it more visible
plotGrouped <- function (dataset, y, grouper) {
  plot <- ggplot(dataset, aes_string(x = 'Income', y = y, group = grouper)) +
    geom_point() + stat_smooth(method = 'lm', aes_string(col = grouper), se = FALSE) +
    labs(title = paste(y, ' grouped by ', grouper))

  if (is.numeric(dataset[[grouper]])) {
    plot + scale_color_gradient(low = "blue", high = "red")
  }
  else {
    plot
  }
}

# val is vactor of y values that will be passed to plotGrouped function
val <- c('MntWines', 'MntGoldProds', 'MntSweetProducts', 'MntMeatProducts')

# Create graphs from dataset grouped by number of kids or teens, or education level
graphs_kids <- map(val, ~ plotGrouped(data, .x, 'Kidhome'))
graphs_teen <- map(val, ~ plotGrouped(data, .x, 'Teenhome'))
graphs_edu <- map(val, ~ plotGrouped(data, .x, 'Education'))

# Put all graphs into one figure
ggarrange(graphs_kids[[1]], graphs_teen[[1]], graphs_edu[[1]],
          graphs_kids[[2]], graphs_teen[[2]], graphs_edu[[2]],
          graphs_kids[[3]], graphs_teen[[3]], graphs_edu[[3]],
          graphs_kids[[4]], graphs_teen[[4]], graphs_edu[[4]],
          ncol = 3, nrow = 4)

# Perform cross-validation:
# Get the number of observations
num <- dim(data)[1]

# Create 40 samples, each with randomly sampled 50% of rows from data
samples <- map(1:40, ~ data[sort(sample(1:num, size = 0.5*num)),])

# Train model for every sample
models <- list(map(samples, ~ lm(.x$MntWines ~ .x$Income)),
               map(samples, ~ lm(.x$MntGoldProds ~ .x$Income)),
               map(samples, ~ lm(.x$MntSweetProducts ~ .x$Income)),
               map(samples, ~ lm(.x$MntMeatProducts ~ .x$Income)))

# Create and map a function to return coeficients and residuals for every model in each category
functions_list <- list(coeficients = coef, residuals = residuals)
f <- function (x) { map(x, ~ sapply(functions_list, function (g) g(.x))) }
extracted_data <- map(models, ~ f(.x))

# Helper mapping functions for list of lists of models
intercept <- function (x) { sd(map_dbl(x, ~ coef(.x)[1])) }
slope <- function (x) { sd(map_dbl(x, ~ coef(.x)[2])) }

# Calculate standard deviation of intercept and slope for every model in each category
map(models, ~ intercept(.x))  # intercept
map(models, ~ slope(.x))  # slope

# Helper functions to calculate residual sum of squares (RSS) and residual mean square error
# (RMSE) for list of lists of models
f_rss <- function (x) { map_dbl(x, ~ sum(resid(.x) ^ 2)) }
f_rmse <- function (x) { map_dbl(x, ~ sqrt(.x / 0.5 * num - 2)) }
rss <- map(models, ~ f_rss(.x))
rmse <- map(rss, ~ f_rmse(.x))

# Plot the results - error is fairly large for all regression models, but it seems that 
# regressions between income and gold and sweets performed slightly better
# columns: 1: wine, 2: gold, 3: sweets, 4: meat
boxplot(rss)
boxplot(rmse)

# Perform t-test for every model - every t-test rejects the H0 hypothesis in favor of
# alternative H1 hypothesis: customers with higher income do tend to spend more on all
# selected products
f_cfs <- function (x) { map_dbl(x, ~ coef(.x)[2]) }
cfs <- map(models, ~ f_cfs(.x))
map(cfs, ~ t.test(.x, mu=0))
