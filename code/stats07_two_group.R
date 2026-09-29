# Two group comparisons: t-tests

pacman::p_load(tidyverse)
rm(list = ls())

df_fl <- read_csv("data_src/data_fish_length.csv")

unique(df_fl$lake) #base R, returns vector
distinct(df_fl, lake) #unique but the dplyr version, returns df or tibble

#tells us that there are two different groups under "lake" 

#mean and standard deviation
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_1 = mean(length),
            sd_1 = sd(length)
            )
df_fl %>% 
  ggplot(
    aes(x = lake,
        y = length)
  ) +
  geom_jitter( #geom_jitter adds random noise
    width = 0.1,
    height = 0,
    alpha = 0.25
  ) +
  geom_segment( #adds a line to the plot with a certain range
    data = df_fl_mu,
    aes(
      x = lake, 
      xend = lake, #x values are constant
      y = mu_1 - sd_1, #have to define the vertical boundaries of the line
      yend = mu_1 + sd_1 #this line is defined by the mean +/- sd, which we will later use to create a probability distribution
    )
  ) +
  geom_point(
    data = df_fl_mu,
    aes(x = lake,
        y = mu_1)
  )

#probability distribution almost always shows up on the y axis


# T-test (equal variance)-----------------------------------------------------------------

x <- df_fl %>%
  filter (lake == "a") %>% 
  pull(length)

y <- df_fl %>%
  filter (lake == "b") %>% 
  pull(length)

t.test(x, y, var.equal = TRUE) #note: default is false
#p value is the probability of observing the difference between two means under random assumptions
#small value means the chance is very low = "significance"

#t value: it actually means something
#getting the t value:
v_mu <- df_fl_mu %>% 
  pull(mu_1)

v_mu[1] - v_mu[2] #calculates difference in mean values. v_mu is a vector. 

df_t <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_1 = mean(length),
            var_1 = var(length),
            n = n()
  )

v_mu <- pull(df_t, mu_1) #mean vector
v_var <- pull(df_t, var_1) #variance vector
v_n <- pull(df_t, n) #sample size vector

#weighted average: proportion of overall information coming from each group = pooled variance
var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1) / (sum(v_n) - 2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / (sqrt(var_p * (1 / v_n[1]) + (1 / v_n[2])))

print(t_value)

#MEMORIZE: NUMERATOR IS DIFFERENCE BETWEEN MEANS DIVIDED BY VARIATION. t-value increases when the magnitude of difference is larger or the variance is smaller (or both)
#t-value helps assess uncertainty. difference between means is only meaningful if we incorporate information about how much the population varies.

#null hypothesis: there is no difference between the means of two groups
#basis for calculating p value

#getting p-value:

x <- seq(-5, 5, length = 500)

#probability density of t-statistics with df = 98
y <- dt(x, df = sum(v_n) - 2)
y1 <- dt(x, df = 10 - 2)

tibble (x, y, y1) %>% 
  ggplot(
    aes(
      x = x, 
      y = y
    )
  ) +
  geom_line() +
  geom_line (aes (y = y1),
             color = "red") +
  geom_vline (xintercept = abs(t_value)) +
  geom_vline(xintercept = t_value) +
  labs (y = "Probability density",
        x = "t-statistic")

#p-value is likelihood of observing a difference above or below the t-value


# T-test (unequal variance) -----------------------------------------------

x <- df_fl %>%
  filter (lake == "a") %>% 
  pull(length)

y <- df_fl %>%
  filter (lake == "b") %>% 
  pull(length)

t.test(x, y, var.equal = FALSE) #welch two sample t-test: more robust; safer to use when the variance is unknown


