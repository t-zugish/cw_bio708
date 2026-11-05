##generalized linear model

rm(list=ls())
pacman::p_load(tidyverse)

##Poisson and binomial distributions (remember: poisson is discrete)

df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)

##figure of individuals in nitrate

df_count %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point()

#fit LM to data and draw predicted line

m_lm <- lm(count ~ nitrate, 
   data = df_count)

df_count %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point() +
  geom_abline(
    intercept = coef(m_lm)[1],
                     slope = coef(m_lm)[2]
  )

#this is not a good figure
#yintercept of predicted fit is negative...count data cannot be negative
#alternative: poisson regression for count data

m_pois <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")
summary(m_pois)

df_count %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point() +
  geom_abline(
    intercept = coef(m_pois)[1],
    slope = coef(m_pois)[2]
  )

#this figure also does not make sense
#link functions: variable transformation to accommodate requirements of the poisson distribution

#log(lambda) = alpha + beta*x for poisson

x <- seq(min(df_count$nitrate),
         max(df_count$nitrate),
         length = 100)

a <- coef(m_pois)[1] #intercept in log scale
b <- coef(m_pois)[2] #slope in log scale

log_y_pred <- a + b * x
y_pred <- exp(log_y_pred)

df_pred <- tibble(y_pred = y_pred,
                  nitrate = x)

df_count %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point() +
  geom_line(
    data = df_pred,
    aes(y = y_pred)
  )

#compare lm and glm for differences:

summary(m_lm)
summary(m_pois)

#in this case, both models yeild significant results
#to determine which kind of model is appropriate

#binomials: 

df_mussel <- read_csv("data_src/data_mussel.csv")

##visualization

df_mussel %>% 
  mutate (p = n_fertilized / n_examined) %>% #p cannot exceed 1 b/c proportional data
  ggplot(
    aes(x = density,
        y = p)
  ) +
  geom_point() 

#proportional data needs a different link function: exp(a + bx) does not restrict p to [0, 1]

#binomial regression
#n_examined is the possible maximum success
#n_fertilized is the success numbers
#binomial distributions take two parameters
#Bin(N, p) where N is number of trials and p is the success probability

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
    data = df_mussel,
    family = "binomial")
summary(m_binom)

z <- seq(min(df_mussel$density),
         max(df_mussel$density),
         length = 100)
a_mussel <- coef(m_binom)[1]
b_mussel <- coef(m_binom)[2]

logit_y_pred <- a_mussel + b_mussel * z
y_pred_binom <- exp(logit_y_pred)/(1 + exp(logit_y_pred))

df_pred_binom <- tibble(y_pred_binom = y_pred_binom,
                  density = z)

df_mussel %>% 
  mutate(p = n_fertilized / n_examined) %>% 
  ggplot(
    aes(x = density,
        y = p)
  ) +
  geom_point() +
  geom_line(
    data = df_pred_binom,
    aes(y = y_pred_binom)
  )







