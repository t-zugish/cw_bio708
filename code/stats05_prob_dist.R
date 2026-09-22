#Probability distribution
#two distributions: density function (PDF) and mass function (PMF)
#is the variable continuous? use PDF. discrete? PMF.


# PDF (continuous)---------------------------------------------------------------------
pacman::p_load(tidyverse,
               patchwork)

df_h0 <- read_csv("data_src/data_plant_height.csv")

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width, which means each bin = 1 value (tried this on 9/17)
                 center = 0.5) + # bin's center specification (what does this do?)
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

#probability dist. should reflect the histogram of the population values
#the dist. is the distribution of probability of a variable having a certain value
#simplification of data structure
#normal dist. has two numbers; mean and variance

#draw probability distribution

x <- seq(min(df_h0$height) ,
    max(df_h0$height) ,
    length = 100)

mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

tibble(y = pd, 
       x = x) %>% 
  ggplot(
    aes(x = x, 
        y = y)
  ) +
  geom_line() + 
  labs(y = "Probability density",
       x = "Plant height")

#to compare to histogram, we need to make our values more comparable (density to frequency)
#we have to integrate (the area under the curve!)

p10 <- pnorm(q = 10, mean = mu, sd = sigma)
#output is the probability of finding a value less than 10

p20 <- pnorm(q = 20, mean = mu, sd = sigma)
p10 - p20

x_min <- floor(min(df_h0$height)) #rounds down to nearest integer
x_max <- ceiling(max(df_h0$height)) #rounds up
bin <- seq(x_min, x_max, by = 1)

p <- NULL 
for (i in 1:(length(bin) - 1)) {
  p_up <- pnorm(bin[i + 1], mean = mu, sd = sigma) #probability up to bin[i + 1]
  p_low <- pnorm(bin[i], mean = mu, sd = sigma) #probability up to bin[i]
  p[i] <- p_up - p_low #difference represents probability between bin[i] and bin[i + 1]
  #need to do this because of overlap
}

#combine the data
df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

df_h0 %>% 
  ggplot(aes(x = height)) +
  geom_histogram(
    binwidth = 1,
    center = 0.5
  ) +
  geom_point(
    data = df_prob,
    aes(x = bin,
        y = freq),
    color = "salmon"
  ) +
  geom_line(
    data = df_prob,
    aes(x = bin,
        y = freq),
    color = "salmon"
  )
#there is a difference between binwidth and bin


# PMF (discrete)---------------------------------------------------------------------
df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(
    binwidth = 0.5, 
    center = 0
    )

#poisson! c'est la creature magnifique!
#only need the mean

x <- seq(0, 10, by = 1)
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

tibble (x = x, 
        y = pm) %>% 
  ggplot(
    aes(x = x, 
        y = y)
  ) +
  geom_line(linetype = "dashed") +
  geom_point() +
  labs(y = "Probability",
       x = "Count")

df_prob2 <- tibble (x = x, 
                    y = pm) %>% 
  mutate(freq = y * nrow(df_count))

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, 
                 center = 0) +
  geom_line(data = df_prob2,
            aes(x = x,
                y = freq) ,
            linetype = "dashed",
            color = "steelblue"
            ) +
  geom_point(data = df_prob2,
             aes(x = x,
                 y = freq) ,
             color = "steelblue"
             )
  




