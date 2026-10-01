
#class note: poisson dist. resembles a normal distribution when the mean is large. still discrete, but can be approximated as normal.
#degrees of freedom = number of total sample minus 
# Influence of Sample size ------------------------------------------------


xs <- rnorm(n = 10, mean = 10, sd = 5)
xl <- rnorm(n = 100, mean = 10, sd = 5)

ys <- rnorm(n = 10, mean = 12, sd = 5)
yl <- rnorm(n = 100, mean = 12, sd = 5)

#t-test, smnall sample size

t.test(xs, ys, var.equal = TRUE)

#degrees of freedom = 18
#p-value = 0.5363

#t-test, large sample size

t.test(xl, yl, var.equal = TRUE)

#degrees of freedopm = 198
#p-value = 0.02931

#comparison: smaller p-value in the larger sample size and fewer degrees of freedom


# Effects of uncertainty --------------------------------------------------

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)


pacman::p_load(tidyverse)

my_data <- tibble(
  group = c(rep("a1", length(a1)), 
            rep("a2", length(a2)),
            rep("b1", length (b1)),
            rep("b2", length (b2))), 
  value = c(a1, a2, b1, b2)
)

my_data
#need to make tibble, but once tibble is made, this is how I would go about making the figure from 10.1:
#tibble columns will be named group and include 
data_mu <- my_data %>% 
  group_by(group) %>% 
  summarize(mu = mean(value),
            sd = sd(value)
  )

data_mu #success! a table with mean and sd for each vector!
#plotting: 

my_data %>% 
  filter(group %in% c("a1", "a2")) %>% 
  ggplot(
    aes(x = group,
        y = value
        )
  ) +
  geom_jitter( 
    width = 0.1,
    height = 0,
    alpha = 0.5
  ) +
  geom_segment(
    data = data_mu %>% 
      filter(group %in% c("a1", "a2")),
    aes(
      y = mu - sd,
      yend = mu + sd 
    )
  ) +
  geom_point(
    data = data_mu %>% 
      filter(group %in% c("a1", "a2")),
    aes(x = group,
        y = mu)
  )

#need to filter in every plot argument, not just the first one. otherwise, b1 abnd b2 are included sequentially.

#ttest; don't have to specify FALSE bc false is the default (Welch)

t.test(a1, a2) #very very small p-value
t.test(b1, b2) 


# Simulate null hypothesis ------------------------------------------------

df_fl <- read_csv("data_src/data_fish_length.csv")
 mu <- mean(df_fl$length)
 sig <- sd(df_fl$length)

x <- rnorm(n = 50, mean = mu, sd = sig)
y <- rnorm(n = 50, mean = mu, sd = sig)

t.test (x, y, var.equal = TRUE)$statistic

#for loop attempt: 

# mu_i <- sig_i <- NULL 
# 
# for (i in 1:100) {
#   
#   df_i <- df_fl %>% 
#     sample_n(size = 50)
#   
#   mu_i[i] <- mean(df_fl$length)
#   sig_i[i] <- sd(df_fl$length)
#   t_value_i[i] <- t.test(df_i[i], df_i[i+1], var.equal = TRUE)$statistic
#   
# }

#i got no errors but i see no results
#wow its not that complicated

v <- NULL
R <- 100
for( i in 1:R) {
  x <- rnorm(n = 50, mean = mu, sd = sig)
  y <- rnorm(n = 50, mean = mu, sd = sig)
  v[i] <- t.test (x, y, var.equal = TRUE)$statistic
}

a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake == "b") %>%
  pull(length)

t_obs <- t.test(a, b, var.equal = TRUE)$statistic

tibble(v = v) %>% 
  ggplot(aes(x = v)) +
  geom_histogram() +
  geom_vline(xintercept = t_obs) +
  geom_vline(xintercept = -t_obs)

#why is my histogram not displayed?
  
mean(abs(v) > abs(t_obs))
  

  

