#Linear model

rm(list=ls())
pacman::p_load(tidyverse)


# Categorical and Continuous predictors -------------------------------------------------------------
#t-test (two groups), anovas (multiple groups): predictors are categorical (think about the example of the lakes)

df_fl <- read_csv("data_src/data_fish_length.csv")

m_fl <- lm(length ~ lake,
   data = df_fl)
summary(m_fl)

#mean fish lengths for lake a and b:
df_fl %>% 
  group_by(lake) %>% 
  summarize (mu = mean(length)) %>% 
  pull(mu)

#t-test

x <- df_fl %>%
  filter (lake == "a") %>% 
  pull(length)

y <- df_fl %>%
  filter (lake == "b") %>% 
  pull(length)

t.test(x, y, var.equal = TRUE)

#t-test and lm can do the same thing under an identical setup

#anova:
df_anova <- read_csv("data_src/data_fish_length_anova.csv")
aov(length ~ lake, data = df_anova)

m_anova <- lm(length ~ lake, data = df_anova)
summary(m_anova)

#lake a is the baseline group

mu_aov <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean (length)) %>% 
  pull(mu)

mu_aov

mu_aov[2] - mu_aov[1]
mu_aov[3] - mu_aov[1]

m_aov <- aov(length ~ lake, data = df_anova)
summary(m_aov)

#modifying group order (so that lake a isn't the default baseline)

df_anova <- df_anova %>% 
  mutate(lake_factor = factor(lake, levels = c("c", "a", "b")))
unique(df_anova$lake)
unique(df_anova$lake_factor)

lm(length ~ lake_factor, data = df_anova)

# Mix of Continuous and categorical predictors ------------------------------------

iris <- as_tibble(iris)
distinct(iris, Species)

m_iris <- lm(Petal.Length ~ Petal.Width + Species,
   data = iris)
summary(m_iris)

#visualizing combined continuous and categorical data:

n_rep <- 100
df_pred <- iris %>% 
  group_by(Species) %>% 
  reframe(
    Petal.Width = seq(min(Petal.Width),
                      max(Petal.Width),
                      length = n_rep))
view(df_pred)
#use df_pred to generate predicted petal lengths

y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

iris %>% 
  ggplot(aes(
    x = Petal.Width,
    y = Petal.Length,
    color = Species
  )) +
  geom_point(alpha = 0.5) +
  geom_line(
    data = df_pred,
    aes(y = y_pred)
  )
  
#interaction: 

lm(Petal.Length ~ Petal.Width + Species + Petal.Width:Species,
   data = iris)

#shortcut:
m_iris_int <- lm(Petal.Length ~ Petal.Width * Species,
   data = iris)
summary(m_iris_int)

y_pred_int <- predict(m_iris_int,
        newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred_int = y_pred_int)

iris %>% 
  ggplot(aes(
    x = Petal.Width,
    y = Petal.Length,
    color = Species
  )) +
  geom_point(alpha = 0.5) +
  geom_line (data = df_pred, 
             aes(y = y_pred_int))