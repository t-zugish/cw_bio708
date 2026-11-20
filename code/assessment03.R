# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.

library(tidyverse)

# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################

# Q1
# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`

df_co2 <- as_tibble(CO2)
data.class(df_co2) #just checking

# Q2
# Convert column names to lowercase and reassign to `df_co2`

library(janitor)
df_co2 <- df_co2 %>% 
  clean_names()

df_co2

# Q3
# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# The figure should satisfy the following:
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment.
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots.

pacman::p_load(patchwork)

#starting with all data:
df_co2 %>% 
  ggplot(
    aes(x = conc,
        y = uptake,
        color = treatment)
  ) +
  geom_point()

#By plant:

df_quebec <- df_co2 %>% 
  filter(type == "Quebec")

q_plot <- df_quebec %>% 
  ggplot(
    aes(x = conc,
        y = uptake,
        color = treatment)
  ) +
  geom_point() +
  labs(x = "Ambient CO2 Concentration",
       y = "CO2 Assimilation Rate",
       title = "Quebec CO2 Data")

q_plot

df_ms <- df_co2 %>% 
  filter(type == "Mississippi")

m_plot <- df_ms %>% 
  ggplot(
    aes(x = conc,
        y = uptake,
        color = treatment)
  ) +
  geom_point() +
  labs(x = "Ambient CO2 Concentration",
       y = "CO2 Assimilation Rate",
       title = "Mississippi CO2 Data")

m_plot

##combined plots: 

q_plot + m_plot #horizontal
q_plot / m_plot #vertical


# Q4
# The df_co2 dataset contains the following variables:
# - CO₂ assimilation rate
# - ambient CO₂ concentration
# - treatment: chilled vs nonchilled
# - type: plant origin (Quebec vs Mississippi)
# 
# Develop a statistical model that examines the following effects simultaneously:
#   
# - The main effect of ambient CO₂ concentration (conc)
# - The main effect of treatment
# - The interaction between concentration and treatment
# 
# Fit these models separately for each plant origin.

#MOST IMPORTANT PART: picking the right data distribution:
#"assimilation" is not count data, so poisson is probably not the right choice
#assimilation (uptake) IS continuous (NOT discrete)
#we aren't asking if carbon was assimilated or not (yes/no), so binomial is also not the right choice.
#following the GLM flow chart, the response variable is NOT discrete and does NOT have an upper bound
#therefore, a normal dist. is the right choice (I hope)

#Quebec data: 
glm_q <- glm(uptake ~ conc + treatment + conc * treatment,
             data = df_quebec,
             family = gaussian) 
summary(glm_q)

lm_q <- lm(uptake ~ conc + treatment + conc * treatment,
           data = df_quebec) 
summary(lm_q) #as expected, identical to glm output. wanted to trust but verify.


#Mississippi data: 
glm_ms <- glm(uptake ~ conc + treatment + conc * treatment,
              data = df_quebec,
              family = gaussian) 
summary(glm_ms)

lm_ms <- lm(uptake ~ conc + treatment + conc * treatment,
            data = df_ms) 
summary(lm_ms) #trust but verify!

# Q5
# Based on the models fitted in Q4 for Quebec and Mississippi plants,
# describe how CO2 assimilation rate responded to ambient CO2
# concentration under different treatments (chilled vs non-chilled) 
# for each plant origin. Highlight the differences between Quebec and 
# Mississippi plants, and use the model results to support your answers.

# ENTER YOUR ANSWER HERE as COMMENT:

#General interpretation of coefficients: 

#Quebec data:
#CO2 assimilation shows a significant positive correlation with ambient CO2 
#the interaction term between concentration and treatment is not significant
#there is not a significant difference between treatment groups
#meaning that differences between treatment groups at the same CO2 conc may be due to chance alone

#Mississippi data: VERY DIFFERENT FROM QUEBEC
#CO2 assimilation shows a significant positive correlation with ambient CO2
#the chilled treatment shows a significant difference relative to the nonchilled treatment
#the interaction between concentration and treatment is significant



# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
# if(!require(vegan)) install.packages("vegan") #commenting to prevent issues later.
library(vegan)
data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots 
# on Barro Colorado Island (Panama). Each row represents a plot, and each 
# column represents a tree species. Entries are counts of individuals of each 
# species in that plot. This dataset is often used to study species richness, 
# community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)

## DATA DESCRIPTION END ####################################################

# Q6
# Convert column names of `df_env` to lowercase and reassign to `df_env`

df_env <- df_env %>% 
  clean_names()

df_env

# Q7
# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.

#precipitation, elevation, age_cat, and geology do not change
#these aren't NA columns, so we can't use the drop_na function like in the model comp lab
#use select!

df_env_sub <- select(df_env, -c(precipitation, elevation, age_cat, geology))

# Q8
# Calculate summary statistics for each plot using `df_bci`.
# Compute the following quantities by plot as columns:
# - n_sum: total count of all individuals across species
# - n1: count of the most abundant species (maximum count among species for each plot)
# - p: proportion of the most abundant species (n1 / n_sum)
#
# Ensure that each plot has one row representing a data point of the dominant species.
#
# Assign the resulting dataframe to `df_n`.

df_n <- df_bci %>% 
  group_by(plot) %>% 
  summarize(n_sum = sum(count),
            n1 = max(count),
            p = (n1/n_sum))
  
df_n

# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.

df_m <- left_join(x = df_env_sub,
          y = df_n,
          by = "plot")
df_m

# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species with a suitable error distribution. 
# - Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# - Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.

#preparation: response variable is p
#because p is a proportion, it IS bounded (somewhere between 0 and 1)
#this is like the mussel fertilization example

#visualizing the information:

df_m %>% 
  ggplot(
    aes(x = env_het,
        y = p)
  ) +
  geom_point()

df_m %>% 
  ggplot(
    aes(x = stream,
        y = p)
  ) +
  geom_point()

df_m %>% 
  ggplot(
    aes(x = habitat,
        y = p)
  ) +
  geom_point()


#let's try the binomial distribution

# glm_bci <- glm(p ~ env_het + stream + habitat,
#                data = df_m,
#                family = "binomial") #got a warning
# summary(glm_bci)

#need to combine successes and failures for binomial analysis: 
#combine the number of dominant species with the total number minus dominant (n1 and n_sum)

glm_bci <- glm(cbind(n1, n_sum - n1) ~ 
                 env_het + 
                 stream + 
                 habitat,
               data = df_m, 
               family = binomial) #no warning!
summary(glm_bci)

#model selection and predictability: AIC 
library(MuMIn)
options(na.action = "na.fail")
m_set <- dredge(glm_bci, rank = "AIC")
m_sub <- subset(m_set, delta < 2)

m_set
m_sub

#The best predictive model includes environmental heterogeneity and habitat (but not stream). 
#In this model, env_het has a negative effect on the proportion of dominant species (p)
#the second best model includes all three predictors (heterogeneity, habitat, and stream)
#this model also shows that env_het has a negative effect on p. 
#that being said, both of these models are imperfect. the predicted intercept is negative.
#we do not expect proportional data to ever be negative

#Also, the final paper we discussed in EHS1 with Dr. Komatsu was about niches using data from BCI!
#This was a cool connection between classes



