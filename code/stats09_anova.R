#Multi-group comparisons: ANOVA
#Anova = ANalysis Of VAriance (even though it compares means)

# Partitioning variability ------------------------------------------------
pacman::p_load(tidyverse)

df_anova <- read_csv("data_src/data_fish_length_anova.csv")
distinct(df_anova, lake)

#violin plots: 
df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2) +
  geom_jitter(
    height = 0, 
    width = 0.1,
    alpha = 0.2
  )

#is there a difference between a, b, and c?
#evaluate between and within group variation

#anova = aov()

aov(length ~ lake, #formular: put variable you want to compare on the left, and the predictor/explanatory variable on the right. DO NOT SWAP.
    data = df_anova)

#HOW TO INTERPRET: 
#sum of squares/squared sum: between groups: compares the mean of each group (in this case, lake a, b, and c) and compares it to the overall mean
#within group variability: distance of each measurement from the mean

#between-group variability:
#overall mean: 

mu <- mean(df_anova$length)

#group-specific means: 

df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize (mu_g = mean (length),
             dev_g = (mu_g - mu)^2,
             n = n())

#now that we have means, we need to sum: 

ss_b <- df_g %>% 
  mutate (ss_g = dev_g * n) %>% 
  pull(ss_g) %>% 
  sum()

aov(length ~ lake,
    data = df_anova)

#manual calculation matches the anova SoS output!

#within group variability

ss_w <- df_anova %>% 
  group_by(lake) %>% #recall: use group_by with summarize or mutate. they are not the same!
  mutate (mu_g = mean(length)) %>% 
  ungroup() %>%  #EVERY TIME YOU PAIR GROUP_BY AND MUTATE
  mutate(dev_i = (length - mu_g)^2) %>% 
  pull(dev_i) %>% 
  sum()

#matches the "SoS "residuals" output in anova!

#overall variability: 
ss_o <- sum((df_anova$length - mu)^2)
#equals the sum of the SoS of between and within group variability
ss_b + ss_w #for proof of concept

summary(aov(length ~ lake, df_anova))

#converting variability to variance

sig_b <- ss_b / 2
sig_w <- ss_w / (nrow (df_anova) - n_distinct(df_anova$lake))

(f_value <- sig_b / sig_w)

#T-test uses t-statistic (can be positive or negativ. not a ratio.)
#anova uses f-statistic: ratio of variance between groups: within groups (only positive values)

#anova null hypothesis: there is no difference in the means between groups
#anova cannot tell us which groups have a significant difference
#post-hoc analysis helps us figure out exactly which groups are significantly different

#null distribution
f <- seq(0, 10, by = 0.01)
pd <- df(f, df1 = 1, df2 = 147) #common between t and anova: using degrees of freedom to define samples
#unlike t-test, where more samples makes it more likely to detect significance, more groups makes it harder to detect significance in anova because you need more information


tibble(x = f, y = pd) %>% 
  ggplot(aes(x = x,
             y = y)) + 
  geom_line() +
  geom_vline(xintercept = f_value,
            color = "chocolate") +
  labs(x= "F statistic",
       y = "probability density")

#function to calculate p value:

pf(f_value, df1 = 2, df2 = 147) #cumulative probability up to the value observed
#need to subtract this value form 1 to get the p-value

p_value <- 1 - pf(f_value, df1 = 2, df2 = 147)
#matches the anova!


# Post-hoc tests ----------------------------------------------------------

#interpreting p-values in multiple contexts



