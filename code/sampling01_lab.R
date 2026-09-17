# Laboratory exercise: practice with sampling

# Question 1 --------------------------------------------------------------

#using the same .csv as on Tuesday's class: 
pacman::p_load(tidyverse,
               patchwork)

df_h0 <- read_csv("data_src/data_plant_height.csv")

#use for loop to create multiple random samples of 50 individuals: 

mu_i <- var_i <- NULL 

for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 50)
  
  mu_i[i] <- mean(df_i$height)
  
  var_i[i] <- var(df_i$height)
  
}

mu_i
var_i

#histograms of the sample mean and variance for the 50 individual samples: 

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram(bins = 10) 

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram(bins = 10) 

g_mu + g_var

#now, we want to repeat the same process for a sample of 100 individuals. I will call this df_100 to differentiate. 

mu_100 <- var_100 <- NULL 

for (i in 1:100) {
  
  df_100 <- df_h0 %>% 
    sample_n(size = 100)
  
  mu_100[i] <- mean(df_100$height)
  
  var_100[i] <- var(df_100$height) 
  
}

mu_100
var_100

#histograms!

df_sample_100 <- tibble(mu_hat_100 = mu_100, var_hat_100 = var_100)

# histogram for mean
g_mu_100 <- df_sample_100 %>% 
  ggplot(aes(x = mu_hat_100)) +
  geom_histogram(bins = 10) 

# histogram for variance
g_var_100 <- df_sample_100 %>% 
  ggplot(aes(x = var_hat_100)) +
  geom_histogram(bins = 10) 

g_mu_100 + g_var_100


# Question #2: non-random sampling (exclusion) ----------------------------

#to create a dataset where only plants with a height greater than 10 cm are included:

df_h10 <- df_h0 %>% 
  filter(height >= 10)

#now, re-do Question 1 with df_h10. 
#I pick the 100 individual sample. (learned that I did not have to do both). 
#call this df_100_b
#use unbiased variance function again


mu_100_b <- var_100_b <- NULL 

for (i in 1:100) {
  
  df_100_b <- df_h10 %>% 
    sample_n(size = 100)
  
  mu_100_b[i] <- mean(df_100_b$height)
  
  var_100_b[i] <- var(df_100_b$height) 
  
}

mu_100_b
var_100_b

#histogram! 

df_sample_100_b <- tibble(mu_hat_100_b = mu_100_b, var_hat_100_b = var_100_b)

# histogram for mean
g_mu_100_b <- df_sample_100_b %>% 
  ggplot(aes(x = mu_hat_100_b)) +
  geom_histogram(bins = 10) 

g_mu_100_b

# histogram for variance
g_var_100_b <- df_sample_100_b %>% 
  ggplot(aes(x = var_hat_100_b)) +
  geom_histogram(bins = 10) 

g_mu_100_b + g_var_100_b

#to compare: i will use patchwork to display side-by-side: 

g_mu_100 + g_mu_100_b
g_var_100 + g_var_100_b

?stat_bin()
#changed the bins for all histograms, just to see how this works


# Exercise takeaways: -----------------------------------------------------

#going over in class: I made this more complicated than it needed to be
#I guess you can create samples of both sizes (n-50 and n=100) in the same loop: do not need to be done separately:
#which is to say: although my approach is not technically wrong, there is another, straighter path
#additionally, you can create a tibble with values from both samples as well
#I need to define my individual objects more clearly so that I can keep track of them intuitively
#patchwork lets you put four graphs into 1! 

(g_mu / g_mu_100) | (g_var / g_var_100)
(g_mu_100 / g_mu_100_b) | (g_var_100 / g_var_100_b)

#scale_x_continuous lets you set limits for the x axis for easier comparison between different data sets
#word to the wise, though: it will cut off values that exist outside the values you set
#overall, we observe that larger sample sizes are more consistent (less variability) = increases precision
#attempt to stack 8 graphs: 

((g_mu / g_mu_100) | (g_var / g_var_100)) / ((g_mu_100 / g_mu_100_b) | (g_var_100 / g_var_100_b))

#cool

(g_mu / g_mu_100 / g_mu_100_b) | (g_var / g_var_100 / g_var_100_b)

#no redundant graphs


# Extra -------------------------------------------------------------------

#patchwork is most helpful for combining different types of figures
#facetwrap does something different
 # df_h10 %>% 
 #  pivot_longer(
 #    cols = everything() , 
 #    names_to = "measure" , 
 #    values_to = "value"
 #  ) %>% 
 #  ggplot(aes(x = value)) +
 #  geom_histogram() +
 #  facet_wrap(facets =~ measure)

#got an error saying Can't combine `plant_id` <double> and `unit` <character>."






