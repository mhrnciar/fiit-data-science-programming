library(tidyverse)

# 1. map(1:3, ~ runif(2)) is a useful pattern for generating random numbers, but map(1:3, runif(2)) is not.
# Why not? Can you explain why it returns the result that it does?
# ~ successfully uses the formula interface and the runif() function is passed to as_mapper(), in the second
# map(), the result of runif() is passed to as_mapper() which creates an extractor function (pluck()), which
# returns 3 NULLs, because no values corresponding to the index can be found
map(1:3, ~ runif(2))  # Use of lambda function in purrr format, creates list of 3 vectors of 2 random numbers
map(1:3, runif(2))    # Returns NULLs

# 2. Use the appropriate map() function to:
# a. compute the standard deviation of every column in a numeric data frame
# Use the rnorm function to generate one with 100 values in each of five columns
df <- as_tibble(matrix(rnorm(100*5), 100, 5))
df %>% map_dbl(sd)

# b. compute the standard deviation of every numeric column in a mixed data frame/tibble nycflights13 from practicum 2
library(nycflights13)
num <- flights %>% map_lgl(is.numeric)
map_dbl(flights[num], sd, na.rm = TRUE)

# c. compute the number of levels for every factor in a data frame/tibble nycflights13 from practicum 2
fac <- flights %>% map_lgl(is.factor)
map_int(flights[fac], ~ length(levels(.x)))

# 3. The following code uses a map nested inside another map to apply a function to every element of a nested list.
# Why does it fail, and what do you need to do to make it work?
# triple() is passed as a function to outer map() and unnamed map is considered an argument for triple()
x <- list(list(1, c(3, 9)),
          list(c(3, 6), 7, c(4, 7, 6)))

triple <- function(x) x * 3
map(x, map, .f = triple)
map(x, ~ map(.x, triple))

# 4. modify() is a shortcut for x[[i]] <- f(x[[i]]); return(x), try to explain the result of modify(mtcars, 1)
# It extracts the first element of every column, but since modify() returns the same structure, the row is recycled
modify(mtcars, 1)

