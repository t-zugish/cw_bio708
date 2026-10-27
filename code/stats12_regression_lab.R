##Regression Lab

rm(list = ls())
pacman::p_load(tidyverse)

#create three data frames by species

df_setosa <- iris %>% 
  filter(Species == "setosa")

df_versicolor <- iris %>% 
  filter(Species == "versicolor")

df_virginica <- iris %>% 
  filter(Species =="virginica")

#regression model is the function lm()

m_setosa <- lm(Sepal.Width ~ Petal.Width,
               data = df_setosa)

m_versicolor <- lm(Sepal.Width ~ Petal.Width,
                   data = df_versicolor)

m_virginica <- lm(Sepal.Width ~ Petal.Width,
                  data = df_virginica)

#lapply() = "least apply" and is the path of least resistance. 

#additional explanatory variables
#not sure about the syntax #just add +! 
?lm()

m2_setosa <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
                            data = df_setosa)

m2_versicolor <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
                   data = df_versicolor)

m2_virginica <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
                   data = df_virginica)

#comparisons: 
summary(m2_setosa) 
summary(m2_versicolor)
summary(m2_virginica)

#adjusted r^2 is lower than mult. r^2
#difference between mult. and adjusted: more variables always increases mult. because you are adding predictors that will explain some variance, but the observed improvement may be due to chance. "adjusted" applies a penalty for each added variable to help account for increases due to chance. (source: StackOverflow: https://stackoverflow.com/questions/2870631/what-is-the-difference-between-multiple-r-squared-and-adjusted-r-squared-in-a-si) 

summary(m_setosa)
summary(m_versicolor)
summary(m_virginica)

#there is probably a more efficient way to do this, but I like being able to track of what I'm doing
#reminder: mult. r^2 is coefficient of determination
#values in the "estimate" column correspond to beta (slope) for explanatory variables and the y-intercept (alpha) for the (intercept)

iris_x <- iris %>% 
  mutate(useless = rnorm(150))

m0 <- lm(Sepal.Width ~ Petal.Width,
         data = iris_x)

m1 <- lm(Sepal.Width ~ Petal.Width + useless,
         data = iris_x)

summary(m0)
summary(m1)

#mult. R increased even though it was USELESS


