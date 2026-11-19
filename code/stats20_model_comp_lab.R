## Model Comparison Lab

# install.packages("palmerpenguins")
pacman::p_load(tidyverse,
               patchwork,
               palmerpenguins,
               janitor,
               MuMIn)

# Formatting penguin data -------------------------------------------------
penguins_raw #messy data set
?clean_names

#convert characters to lower case, replace spaces with underscores, and remove parentheses
#idk what the right case is, so i'm going to experiment with all of the options
penguins_clean <- clean_names(penguins_raw, case = "snake") #alright well this one was the right one. no need to continue
clean_names(penguins_raw, case = "small_camel")
clean_names(penguins_raw, case = "big_camel")
clean_names(penguins_raw, case = "screaming_snake") #same as snake, but all caps (hence the "screaming")
clean_names(penguins_raw, case = "parsed")

#class notes: 
df_pen <- penguins_raw %>% 
  clean_names

#convert yes/no in "clutch completion" to binary values for easier analysis:
?ifelse()
#we want to identify if a value is a yes and change it to a 1; if not yes, then 0
#because we are changing information in the columns, we need to use mutate

# df_pen <- df_pen %>% 
#   mutate(clutch_completion = ifelse(clutch_completion == "Yes", 1, 0))
# 
# #change species names to just adelie, chinstrap, gentoo
# 
# df_pen <- df_pen %>% 
#   mutate(species = case_when(species == sp[1] ~ "adelie",
#                              species == sp[2] ~ "gentoo",
#                              species == sp[3] ~ "chinstrap") 
         # ) commenting out because running this code twice creates problems later in the model due to all 0s in clutch completion!!!
#you can combine the mutate operations from these two exercises!

sp <- unique(df_pen$species)
df_pen <- df_pen %>% 
  mutate(
    clutch_completion = ifelse(clutch_completion == "Yes", 1, 0),
         species = case_when(species == sp[1] ~ "adelie",
                             species == sp[2] ~ "gentoo",
                             species == sp[3] ~ "chinstrap")
    ) %>% 
  drop_na( #remove rows with missing data
    culmen_length_mm,
    culmen_depth_mm,
    flipper_length_mm,
    body_mass_g
  ) 

# Analyzing penguin data (and selecting the appropriate model) --------------------------------------------------
#Predictors: Species, Culmen Length (mm), Culmen Depth (mm), Flipper Length (mm), Body Mass (g), and Sex. 
#before we start: clutch completion (our response variable) is binomial. 

# glm_pen <- glm(clutch_completion ~ species + culmen_length_mm + culmen_depth_mm + flipper_length_mm + body_mass_g + sex,
#                family = "binomial",
#                data = df_pen) #got an error that says error in contrasts? maybe need to do each variable one at a time?
# 
# glm_length <- glm(clutch_completion ~ culmen_length_mm,
#                   data = df_pen,
#                   family = "binomial") #no error, but a warning that "algorithm did not converge"
# summary(glm_length)
# 
# glm_flipper <- glm(clutch_completion ~ flipper_length_mm,
#                    data = df_pen,
#                    family = "binomial") #same warning

##class notes: 

m <- glm(clutch_completion ~ 
           species + 
           culmen_length_mm + 
           culmen_depth_mm + 
           flipper_length_mm + 
           body_mass_g + 
           sex,
         data = df_pen,
         family = "binomial")
summary(m) #I am getting an error in na.fail.default?

#model selection:
options(na.action = "na.fail")
m_set <- dredge(m, rank = "AIC")
m_sub <- subset(m_set, delta < 2)


