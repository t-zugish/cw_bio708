library(tidyverse)

## point figure
iris %>% 
  ggplot(
    mapping = aes(x = Sepal.Length,
                  y = Sepal.Width)
  ) +
  geom_point()

##plot shows up in lower right corner

##remember to use aes(), otherwise there will be an error message! aes allows you to use the column names from the data frame
##short cut for commenting out: ctr + Shift + C (allows you to include bad code as an example without running into issues when making reports)
# for example: 

## point figure
# iris %>% 
#   ggplot(
#     mapping = x = Sepal.Length,
#               y = Sepal.Width)
#   ) +
#   geom_point()

#ctrl shift R = section label: helps you keep track of different parts of a longer code

# Point Figure ------------------------------------------------------------

## point figure: color by species
# change color by "Species" column
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point()

##without pipe: 

ggplot(
  data = iris, 
       mapping = aes(x = Sepal.Length,
                     y = Sepal.Width, 
                     color = Species)
) + 
  geom_point()

# color must be inside the aes() argument, unless you want a uniform color:

ggplot(
  data = iris, 
  mapping = aes(x = Sepal.Length,
                y = Sepal.Width)
) + 
  geom_point(color = "red")

ggplot(
  data = iris, 
  mapping = aes(x = Sepal.Length,
                y = Sepal.Width)
) + 
  geom_point(color = "salmon")

ggplot(
  data = iris, 
  mapping = aes(x = Sepal.Length,
                y = Sepal.Width)
) + 
  geom_point(color = "steelblue")


# Line figure -------------------------------------------------------------

## reminder: rep = replicate, so we are replicating 1 through 50 3 times in this argument
df0 <- tibble(
  x=rep(1:50, 3),
  y = x * 2
)

df0 %>% 
  ggplot(
    mapping = aes(x = x,
                  y = y)
  ) +
  geom_line()


# Histogram ---------------------------------------------------------------

iris %>% 
  ggplot(mapping = aes(x = Sepal.Length)) +
  geom_histogram()

# side quest

iris %>% 
  ggplot(mapping = aes(x = Sepal.Length)) +
  geom_histogram(color = "salmon")

##changed the color of the outline of each bar, but not the color of the bars themselves. interesting.

iris %>% 
  ggplot(mapping = aes(x = Sepal.Length,
                       color = Species)) +
  geom_histogram(color = "salmon")

##that did nothing, but maybe it would be different if it was a bee swarm or similar plot. there isn't any species-specific point in the histogram. 
# command option o collapses all sections (keeps it tidy)


# Box Plot ----------------------------------------------------------------

iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length)
  ) +
  geom_boxplot()

# color

iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length,
                  color = Species)
  ) +
  geom_boxplot()

# changes border color! for inside box: fill ()

iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length,
                  fill = Species)
  ) +
  geom_boxplot()


# Ridgeline plot (Data to Viz) --------------------------------------------------

# library(tidyverse)
# library(ggtext)
# library(ggdist)
# library(glue)
# library(patchwork)
# 
# install.packages("ggtext")
# 
# library(tidyverse)
# library(ggtext)
# library(ggdist)
# library(glue)
# library(patchwork)
# 
# install.packages("ggdist", "glue", "patchwork")
# 
# library(tidyverse)
# library(ggtext)
# library(ggdist)
# library(glue)
# library(patchwork)

##picking a different plot that I can make without downloading more packages


# Lollipop Plot -----------------------------------------------------------

# Libraries
library(ggplot2)

# Plot
# iris %>% 
#   ggplot(
#     mapping = aes( x = Sepal.Length,
#                    y = Sepal.Width)
#   ) + geom_point () + 
#   geom_segment( aes(x = Sepal.Length, xend = x, y = 0, yend = y))

# okay so that was a lot of errors... I am going to try first with the code from data to viz and then with the Iris set. 

# Libraries
library(ggplot2)

# Create data
data <- data.frame(x=seq(1,30), y=abs(rnorm(30)))

# Plot
ggplot(data, aes(x=x, y=y)) +
  geom_point() + 
  geom_segment( aes(x=x, xend=x, y=0, yend=y))

# i am running into issues translating the "x" and "y" syntax in the geom_segment argument tp yhe Iris equivalent

#already recalled ggplot2
#already have data
#trying plot without pipe: 

# ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length))+
#   geom_point() +
#   geom_segment( aes(x = Sepal.Width, xend=x, y=0, yend=y))

#more errors! 




