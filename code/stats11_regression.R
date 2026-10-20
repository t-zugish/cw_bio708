## linear regression

rm(list = ls())
pacman::p_load(tidyverse)



# Data structure ----------------------------------------------------------

df_algae <- read_csv("data_src/data_algae.csv")

#always start with visualization!

df_algae %>% 
  ggplot(
    aes(x = conductivity,
        y = biomass)
  ) +
  geom_point()

#is the positive linear relationship significant?
#least-of-squares method

m <- lm(biomass ~ conductivity,
   data = df_algae)
summary(m)

#alpha = yint, beta = slope of best fit line. "residual std error" = deviation of normal dist. that maps to y

##extract intercept and slope info:

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(
    aes(x = conductivity,
        y = biomass)
  ) +
  geom_point() +
  geom_abline(
    intercept = alpha,
    slope = beta
  )

#interpreting t-values and standard errors in this context:

summary(m)

#null hypothesis: no relationship (beta = 0) (there is no slope)

0.50355/0.02568

#t-value is estimated value/std. error and then evaluate the probability of observing a value more extreme than that

theta <- coef(m)
se <- sqrt(diag(vcov(m)))

t_value <- theta/se

#most important takeaway: how we determine best fit (vertical distance) and null hypothesis (beta = 0)


# Unexplained Variation ---------------------------------------------------

##extract residuals (manually and built-in)
##residuals = deviations from the best fit line

eps0 <- df_algae$biomass - (alpha + beta * df_algae$conductivity) #observed value minus predicted value. eps = epsilon which is the symbol used for error.

#function: 
eps <- resid(m)

plot(eps0 ~ eps)

##visualize error

df_algae <- df_algae %>% 
  mutate (eps = eps) ##add new column with error

df_algae %>% 
  ggplot(
    aes(x = conductivity,
        y = biomass)
  ) +
  geom_point() + 
  geom_abline (
    intercept = alpha,
    slope = beta
  ) +
  geom_segment(
    aes(
      x = conductivity,
      xend = conductivity,
      y = biomass,
      yend = biomass - eps
    ) ,
    linetype = "dashed"
  )
  
#what did we do? displayed the vertical dist. from each point to the best fit line. 
##original variation in y
y <- df_algae$biomass
ss0 <- sum((y - mean(y))^2) #exponent needs to be inside the sum function. variation ONLY within y.
ss1 <- sum((eps^2))

ss1/ss0 #variation that remains unexplained after accounting for conductivity (x)

rsq <- 1 - (ss1/ss0) #variation that IS explained after accounting for x. ##corresponds to "multiple r squared" value in the summary output




