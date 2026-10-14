#ANOVA laboratory

pacman::p_load(tidyverse)
# install.packages("pwr")
library(pwr)


# Exercise 1: Plant Growth ------------------------------------------------

#Part 1: re-creating violin plots of weight vs group
PlantGrowth %>% 
  ggplot(aes(
    x = group,
    y = weight)) +
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2) +
  geom_jitter(alpha = 0.2)
#to change the group labels, use mutate (group_label = case_when(group == "current label" ~ "new label")) before ggplot. don't forget to pipe!

#Part 2: comparing differences in weight between groups
#a: overall mean:
mu <- mean(PlantGrowth$weight)

#b: group means:
df_pg <- PlantGrowth %>% 
  group_by(group) %>% 
  summarize(mu_g = mean(weight),
            dev_g = (mu_g -mu)^2,
            n = n())
#just realized that I don't have to do this manually

#ANOVA:
pg <- aov(formula = weight ~ group,
    data = PlantGrowth)
summary(pg)

#Part 3: Terms to include in a paper:
#I would assume that the F-statistic and corresponding p-value are important, as well as the residual (within group) and group (between group) means/sum of squares
#what would we not include in a paper?
#typically: at least the F-value and the p-value, likely the DFs

# Exercise 2: Power Analysis ----------------------------------------------
?pwr

pwr::pwr.anova.test(f=0.5, k=3, sig.level=0.05, power=0.8)

#leaving one parameter out will automatically calculate it in the output
#According to the output, we need a group size of n = 13.89 per group, or, rounding up, 14 individuals
#what does it mean to have 80% power? #means an 80% likelihood of observing a difference if one exists

#takeaway: fewer groups but more individuals per group is better!

pwr::pwr.anova.test( #how does decreasing significance affect power?
  k = 10,
  n = 10,
  sig.level = 0.5,
  f = 0.5
)

pwr::pwr.anova.test( #how does a larger sample per group affect power?
  k = 3,
  n = 100,
  sig.level = 0.05,
  f = 0.5
)

pwr::pwr.anova.test( 
  k = 3,
  n = 5,
  sig.level = 0.05,
  f = 0.5
) #power is much lower with fewer individuals in each group


pwr::pwr.anova.test( #how does changing the power affect the effect size?
  k = 2,
  n = 50,
  sig.level = 0.05,
  power = 0.5
)

pwr::pwr.anova.test(
  k = 3,
  n = 50,
  sig.level = 0.05,
  power = 0.8
) #higher power = larger effect size

#number of groups?
# pwr::pwr.anova.test(
#   f = 0.5,
#   n = 30,
#   sig.level = 0.05,
#   power = 0.8
# ) #leaving out k yields error "f values at end points not of opposite signs"

