library(tidyverse)

# 1. Explain the relationship between objects a, b, c and d. Use tracemem() to check your answer
a <- 1:10
b <- a
tracemem(a); tracemem(b) # same
a[2] <- 4
tracemem(a); tracemem(b) # different, because a has been modified
c <- b
tracemem(b); tracemem(c) # same
d <- 1:10
tracemem(d)

# 2. Write a R program to add two vectors of integers with length 3
x <- c(1, 2, 3)
y <- c(4, 5, 6)
(xy <- x + y)

# 3. Use the following code to create vector x. For a given vector x, is.na(x) returns logical vector with TRUE for
# each position that contains NA. Use is.na() together with knowledge you gathered about atomic vector subsetting to
# convert all NAs in x to 4
x <- 1:10 ; x[c(1,4,8)] <- NA
x
x[is.na(x)] <- 4
x

# 4. Consider the following code, at which row and column of a matrix will the number 8 occur?
x <- 1:10 ; matrix(x, 2, 5)  # row 2, col 4

# 5. Let’s use pseudo-random number generator to create to vectors x and y. How many rows in a column-bound matrix xy
# contain values that are higher than 5 in x and less than 2 in y?
set.seed(123) # Sets an initial value for pseudo random generator of numbers
x <- rnorm(20, mean = 10, sd = 5) # Generates 20 random values that have mean of 10 and standard deviation of 5
y <- rnorm(20, mean = 5, sd = 15) # Generates another 20 random values that have mean of 5 and standard deviation of 15
m <- cbind(x, y)
subset(m, x > 5 & y < 2)  # 10

# 6. In mtcars:
# a. find all car models with motor that has 6 cylinders
View(mtcars)
subset(mtcars, cyl == 6)

# b. find the car model with the highest horsepower, but limited to transmission with only three gears
subset(mtcars, gear == 3) %>% arrange(desc(hp))   # Duster 360

# c. find how many cars have cylinder displacement between 100 and 120 cubic inches and have 2 carburetors
subset(mtcars, disp %between% c(100, 120) & carb == 2)  # None

# d. find cars that can do 1/4 mile under 17 seconds and weight less than 1.5K lbs?
subset(mtcars, qsec < 17 & wt < 1.5)  # None

sessionInfo()
