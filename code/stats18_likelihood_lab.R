##Likelihood laboratory
pacman::p_load(tidyverse,
               patchwork)

# Binomial Distribution ---------------------------------------------------

#Exercise 1: probability of observing a vector of values

y <- c(2, 2, 0, 0, 3, 1, 3, 3, 4, 3)
p <- seq(0, 1.0, by = .01)

?dbinom()

(pr_binom <- dbinom(x = y, size = 10, prob = p))

