##Linear model lab

pacman::p_load(tidyverse)


# Normality assumption ----------------------------------------------------

#linear model: dose and supp. are the explanatory variables; length is the response variable
#want to see if there is an interaction between dose and supp

m_tooth <- lm(len ~ dose * supp,
   data = ToothGrowth)

#output: in this case, suppOJ is our baseline. the suppVC  output is relative to suppOJ
summary(m_tooth)

#normality: shapiro test
?shapiro.test()
#in this case, we should test the residual values
shapiro.test(m_tooth$residuals)

# Model Interpretation ----------------------------------------------------

#visualization

df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_tooth 

