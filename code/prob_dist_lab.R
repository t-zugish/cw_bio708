##probability lab


# Normal distribution (n = 50)-----------------------------------------------------

pacman::p_load(tidyverse,
               patchwork)
?rnorm()

#step 1: show the distribution as a histogram.
#I tried running this without making a data frame first, which was a mistake. 
df_1 <- data.frame(sample_50 = rnorm(50, mean = 25, sd = 4)) 
df_1 %>% 
  ggplot(aes(x = sample_50)) + 
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_vline(aes(xintercept = 25))
             

df_1
#looks a little funny but we have a histogram!

#step 2: define the probability distribution
#just following from yesterday

# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_1), max(df_1), length = 100)

# calculate probability density
pd <- dnorm(x, mean = 25, sd = 4) #did not assign enw objects bc i already know what the values are; I picked them


# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density") # re-label

#I did get a line! that's great!

#step 3: time to integrate

x_min <- floor(min(df_1)) # floor takes the integer part of the value
x_max <- ceiling(max(df_1)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1)

p <- NULL 
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = 25, sd = 4) - pnorm(bin[i], mean = 25, sd = 4)
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin
df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * length(df_1))

df_prob

#step 4: plot

df_1 %>% 
  ggplot(aes(x = sample_50)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) + # bin's center position
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")

#it looks alright. i can maybe adjust the bin size (but that requires a lot of back tracking)
#once again humbled by efficiency of the in-class code

# Poisson Distribution ( n = 1000) ----------------------------------------

#step one: make a distribution (and learn from what i did less efficiently in the first section)

x <- rpois(
  n = 1000,
  lambda = 50
)

lambda <- mean(x) #only need sd or mean for the poisson
#define the bins
bin <- seq(min(x), max(x), by = 1)
           
df_x <- tibble(x = x)

df_x %>% 
  ggplot(aes(x =x)) +
  geom_histogram (binwidth = 0.5, 
                  center = 0)

#this looks so weird
?rpois()
#okay, changed mean to lambda in first argument and assigned 50 to the mu since i picked 50 as the mean
#but it looks like a normal distribution
#at least it's not continuous any more

#step 2: probability

# calculate probability mass
lambda_hat <- mean(x)
pm <- dpois(x = bin, lambda = lambda)

#no errors yet. cool. 

# figure
tibble(y = pm, x = bin) %>%
  ggplot(aes(x = bin, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label

#what in the world
#changed vector to 1:1000
#hmmmmmmm
#changed until reasonable (min and max values)
#if poisson, why look so normal?

#step 3: overlay

df_prob <- tibble(x = bin, y = pm) %>% 
  mutate(freq = y * length(bin))

 df_x %>% 
  ggplot(aes(x = x)) +
   geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                  center = 0) +
   geom_line(data = df_prob,
             aes(x = bin,
               y = freq),
             linetype = "dashed") +
   geom_point(data = df_prob,
              aes(
                x = bin, 
                y = freq)
              )

#oh man, the scale seems to be off in a bad way, but at least i don't have errors
#changed something and got an error:
# Error in `geom_line()`:
#   ! Problem while computing aesthetics.
# ℹ Error occurred in the 2nd layer.
# Caused by error in `check_aesthetics()`:
#   ! Aesthetics must be either length 1 or the same as the data (49).
# ✖ Fix the following mappings: `x`.





  






