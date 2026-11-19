#model comparison

#review of likelihood: least squares method is a special case
#the equation is a lot simpler than it seems at first glance. summation of two constants. only the sum of least squares really matters. 
#likelihood can be applied to any kind of distribution


# Model Fit and Complexity ------------------------------------------------

#making a "perfect model" is easy, but making a "perfect" and generalizeable model is hard. 
#a good model explains the data and allows you to make predictions outside of your data set

pacman::p_load(tidyverse,
               patchwork)

set.seed(1) #generates the same set of numbers from a random number generator
 n <- 100 #sample size
 b <- c(0.1, 0.5) #intercept and slope
 x1 <- rnorm(n = n, mean = 0, sd = 1) #hypothetical predictors
 X <- model.matrix(~x1) #design matrix
 y_hat <- X %*% b #simulated y (response variables)
 
# %*% signifies matrix multiplication. this function allowed us to create the linear relationshiop between y and x. 
 
 y <- rnorm(n = n, mean = y_hat, sd = 0.5)
 
 X
 
df0 <- tibble (y = y, x1 = x1)

df0 %>% 
  ggplot(
    aes(x = x1,
        y = y)
  ) +
  geom_point()


# Linear model fit --------------------------------------------------------
#one way to compare models: adjusted r squared values with additional predictors
#ONLY NORMAL DISTRIBUTIONS BC SUM OF SQUARES
m1 <- lm(y ~ x1, df0)
summary(m1)

# adding unrelated variables (remember how this increases r^2??)

df0 <- df0 %>% 
  mutate(x2 = rnorm(n=n))

m2 <- lm(y ~ x1 + x2, df0)
summary(m2)

#multiple r^2 increased slightly, but adjusted r^2 decreased
#recall that "adjusted" assigns a penalty for additional predictors

#extracting r^2 values:
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared


# GLM fit -----------------------------------------------------------------
#comparing non-normal cases
#metric of comparison: "deviance"

df_count <- read_csv("data_src/data_garden_count.csv") %>% 
  mutate(moisture = runif(nrow(.), 0, 100))

glm1 <- glm(count ~ nitrate,
    data = df_count,
    family = poisson)
s_pois0 <- summary(glm1)

glm2 <- glm(count ~ nitrate + moisture,
            data = df_count,
            family = poisson)
s_pois1 <- summary(glm2)

#look at "null deviance" and "residual deviance"

pseudoR <- 1 - (s_pois0$deviance / s_pois0$null.deviance) #just nitrate
pseudoR1 <- 1 - (s_pois1$deviance/ s_pois1$null.deviance) #nitrate and moisture. slightly higher than nitrate alone
#does not account for model complexity (ie, number of predictors)

adj.pseudoR <- 1 - ((s_pois0$deviance - 1) / s_pois0$null.deviance)
adj.pesudoR1 <- 1 - ((s_pois1$deviance - 2) / s_pois1$null.deviance)


# Likelihood Ratio Test ---------------------------------------------------
#ESSENTIAL ASSUMPTION: MODELS ARE NESTED AND HAVE THE SAME KIND OF DISTRIBUTION
# LR = -2(lnL1 - lnL0) 

#normal distribution case: (using models from earlier)
anova(m1, m2, test = "Chisq") #compares the likelihood ratios and then determines if the improvement is due to chance alone (look at the p value!)

#poisson distribution case:
anova(glm1, glm2, test = "Chisq") #p value is not significant


# Out of sample prediction comparison: AIC------------------------------------

#select a model based on its capacity to predict an omitted data point, does not allow for causal inference
#models with high adj. R^2 values usually align with this capacity

#metric: AIC (lower value = better)
#AIC = 2k - 2 ln L  #k is number of parameters in the model 

#get AIC value: generalizeable to non-normal distributions

#normal case
AIC(m1)
AIC(m2)

#poisson case:
AIC(glm1)
AIC(glm2)


# Summary -----------------------------------------------------------------

#Adj. R^2, LRT, and AIC values are three ways to compare models
#LRT must be nested. AIC does not need to be nested. 
#When you see |AIC1 - AIC2| > 2, models predictive capabilities are meaningfully different
#AIC is useful in ecology bc there are lots of potential models; may be less useful in cell bio




