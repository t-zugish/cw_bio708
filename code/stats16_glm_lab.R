## GLM lab

pacman::p_load(tidyverse,
               patchwork)

# GLM Exercise ------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

#response variable (number of species) is a count variable. we cannot have 1.001, 1.02, etc. number of species. therefore, poisson would be appropriate here. 

glm_dist <- glm(n_sp ~ distance,
              data = df_fish,
              family = "poisson")
summary(glm_dist)

glm_cat <- glm(n_sp ~ cat_area,
               data = df_fish,
               family = "poisson")
summary(glm_cat)

glm_hull <- glm(n_sp ~ hull_area,
    data = df_fish,
    family = "poisson")
summary(glm_hull)

#foiled by the plus sign again

m_glm <- glm(n_sp ~ distance + cat_area + hull_area,
    data = df_fish,
    family = "poisson")
summary(m_glm)

#part 2: binomials

glm_mpg <- glm(am ~ mpg,
               data = mtcars,
               family = "binomial")
summary(glm_mpg)

glm_hp <- glm(am ~ hp,
              data = mtcars,
              family = "binomial")
summary(glm_hp)

glm_wt <- glm(am ~ wt,
              data = mtcars,
              family = "binomial")
summary(glm_wt)

#or, more efficiently:

cars_glm <- glm(am ~ mpg + hp + wt,
    data = mtcars,
    family = "binomial")
summary(cars_glm)

#weight seems to predict a car's transmission
# what abaut a "gaussian" distribution?

gauss_glm_mpg <- glm(am ~ mpg,
               data = mtcars,
               family = "gaussian")
summary(gauss_glm_mpg)

gauss_glm_hp <- glm(am ~ hp,
              data = mtcars,
              family = "gaussian")
summary(gauss_glm_hp)

gauss_glm_wt <- glm(am ~ wt,
              data = mtcars,
              family = "gaussian")
summary(gauss_glm_wt)

#gaussian means normal

cars_gauss <- glm(am ~ mpg + hp + wt,
                data = mtcars,
                family = "gaussian")
summary(cars_gauss)


# Effect size -------------------------------------------------------------
#important to compare predictors at the same scale! otherwise, hard to interpret

df_fish <- df_fish %>% 
  mutate(
    std_dist = c(scale(distance)),
    std_cat = c(scale(cat_area)),
    std_hull = c(scale(hull_area))
  )

std_glm <- glm(n_sp ~ std_dist + std_cat + std_hull,
             data = df_fish,
             family = "poisson")
summary(std_glm)

#getting an error that the predictor variables (std_dist, std_cat, std_hull) are not found? 
#fixed! had to overwrite the old df_fish!


# Offset term -------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_offset.csv"
df_offset <- read_csv(url)

df_offset %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point() #all over the place. really putting the "scatter" in scatterplot. 

df_offset %>% 
  ggplot(
    aes(x = area,
        y = count)
  ) +
  geom_point() #seems like a positive relationship?

df_offset %>% 
  ggplot(
    aes(x = nitrate,
        y = (count/area))
  ) +
  geom_point() #not as defined as area and count, but slightly less scattered than count and nitrate

#let's see if this gives a warning:

# df_offset <- df_offset %>% 
#   mutate(density = count / area)
# 
# glm(density ~ nitrate,
#     data = df_offset,
#     family = "poisson")

#there are in fact "50 or more warnings"

m_glm <- glm(count ~ nitrate + offset(log(area)),
                  data = df_offset,
                  family = "poisson")
summary(m_glm)

# g_glm <- glm(density ~ nitrate,
#              data = df_offset,
#              family = "gaussian")
# summary(g_glm)


# Overdispersion ----------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_tadpole.csv"
df_tadpole <- read_csv(url)


df_tadpole %>% 
  ggplot(
    aes(x = aqveg,
        y = tadpole)
  ) +
  geom_point() 

df_tadpole %>% 
  ggplot(
    aes(x = permanence,
        y = tadpole)
  ) +
  geom_point()

#use poisson and see how it goes!

tadpole_glm <- glm(tadpole ~ aqveg + permanence,
                   data = df_tadpole,
                   family = "poisson")
summary(tadpole_glm)

#visualize the data: 

df_tadpole %>% 
  ggplot(
    aes(x = permanence,
        y = tadpole)
  ) +
  geom_point() +
  geom_abline(
    intercept = coef(tadpole_glm)[1],
    slope = coef(tadpole_glm)[2]
  )

mean(df_tadpole$tadpole)
sd(df_tadpole$tadpole)
var(df_tadpole$tadpole)

MASS::glm.nb(tadpole ~ aqveg + permanence,
             data = df_tadpole) %>% 
  summary()

