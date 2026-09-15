# Sampling (learning theory before lab exercise on Thurs)

# to remove objects from previous sessions, start by running this line of code:
rm(list = ls())

# double colon means pulling a function without calling from library. 
pacman::p_load(tidyverse,
               patchwork)


# Individual samples (n = 10) ---------------------------------------------

#data frame 1
h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)
df_h <- tibble(plant_id = 1:length(h),
                height = h, 
                unit = "cm")
df_h

df_h1 <- df_h %>% 
  mutate(mu_height = mean(h),
         var_height = sum((h-mean(h))^2) / nrow(.))
df_h1

# data frame 2
h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)


# Population (n = 1000) ---------------------------------------------------

# parameter: un-measurable, constant attribute that describes the population. we have to make inferences about the true value of the parameter from what we observe in our samples.

# load csv (spreadsheet) data on R: found in the "data_src" folder in the bio708 directory
df_h0 <- read_csv("data_src/data_plant_height.csv")

# show the first 10 rows
print(df_h0)

#for reference: df_h1 and df_h2 are samples from our df_h0 population

mu <- mean(df_h0$height) #true mean
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0) #true variance

#here, mu and sigma2 are examples of parameters

#for loop: what if we want to calculate something many times?
#selecting a random sample from a data set

df_i <- df_h0 %>% 
  sample_n(size=10)

mu_i <- mean(df_i$height)
var_i <- sum((df_i$height - mu_i)^2) / nrow(df_i)

#doing that many times: 

mu_i <- var_i <- NULL 
for (i in 1:1000) {
  df_i <- df_h0 %>% 
    sample_n(size=10)
  
  mu_i <- mean(df_i$height)
  var_i <- sum((df_i$height - mu_i)^2) / nrow(df_i)
}

#this code makes a single value for mu_i and var_i, but that is not what we want.
#code from textbook (online) has bracketed [i] and a set.seed(3) line
# YOU NEED TO INDLUDE []

# for reproducibility
set.seed(3)

mu_i <- var_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 1000
for (i in 1:1000) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
}

#now, mu_i and var_i are a set of values 
mu_i
var_i


# Using patchwork ---------------------------------------------------------

library(patchwork)
#visualizing the sampled mean/variance

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_mu

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

g_var

#how patchwork works: combines multiple figures into 1
#horizontal: 

g_hor <- g_mu + g_var

# layout vertically
# possible only if "patchwork" is loaded
g_ver <- g_mu / g_var

#R's built-in stdev and var functions are bias-corrected
#what happens if we use the bias corrected versions? 


# Bias corrected, built-in functions --------------------------------------


mu_i <- var_i <- var_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 1000
for (i in 1:1000) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
  var_ub_i[i] <- var(df_i$height)
}

df_sample <- df_h0 %>% 
  mutate(var_ub_hat = var_ub_i)
df_sample

g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline (xintercept = sigma2)
g_var_ub

#comparison
g_mu / g_var / g_var_ub



