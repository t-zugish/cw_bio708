# descriptive stats exercises


# Central Tendency --------------------------------------------------------
 z <- c(exp(rnorm(n = 100, mean = 0, sd = 0.1)))

 #arithmetic mean
 n_z <- length(z) 
 sum_z <- sum(z) 
 mu_z <- sum_z / n_z 
 print(mu_z)
 
 #geometric mean
 mu_z_ge <- prod(z)^(1 / length(z))
 print(mu_z_ge)
 
 #median
 (median(z))
 
 #plot: histogram
 library(tidyverse)
 
tibble(z) %>% 
  ggplot(mapping = aes(z)) +
  geom_histogram()
 
?geom_vline()

tibble(z) %>% 
  ggplot(mapping = aes(z)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z, color = "red") +
  geom_vline(xintercept = mu_z_ge, color = "blue") +
  geom_vline(xintercept = median(z), color = "yellow")

#visual comparison: arithmetic is the largest (makes sense; less robust) and the median is the smallest. 

# z_rev: 

z_rev <- c(-z + max(z) + 0.5)

n_z_rev <- length(z_rev) 
sum_z_rev <- sum(z_rev) 
mu_z_rev <- sum_z_rev / n_z_rev
print(mu_z_rev) #arithmetic

mu_z_rev_ge <- prod(z_rev)^(1 / length(z_rev))
print(mu_z_rev_ge) #geometric

median(z_rev)

tibble(z_rev) %>% 
  ggplot(mapping = aes(z_rev)) +
  geom_histogram()

tibble(z_rev) %>% 
  ggplot(mapping = aes(z_rev)) +
  geom_histogram() +
  geom_vline(xintercept = mu_z_rev, color = "red") +
  geom_vline(xintercept = mu_z_rev_ge, color = "blue") +
  geom_vline(xintercept = median(z_rev), color = "yellow")

#visual comparison: this time, the median is the highest. arithmetic mean is still greater than the geopmetric mean. 

 
# Variation ---------------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

# #changing units
# ?unit()
# 
# install.packages( "grid" )
# library(grid)

#ignore my attempt to actually assign a unit to the vector

m <- c(w/1000) 

# standard deviation
#part 1: variance 

# for w
sqd_w <- (w - mean(w))^2 
sum_sqd_w <- sum(sqd_w)
var_w <- sum_sqd_w / length(w)
print(var_w)

# for m
sqd_m <- (m - mean(m))^2 # sqared deviance
sum_sqd_m <- sum(sqd_m)
var_m <- sum_sqd_m / length(m)
print(var_m)

#part 2: standard dev

#for w:
(sd_w <- sqrt(var_w))

#for m:
(sd_m <- sqrt(var_m))

#MAD:
#for w:
ad_w <- abs(w - median(w))
(mad_w <- median(ad_w))

#for m:
ad_m <- abs(m - median(m))
(mad_m <- median(ad_m))

#Coefficient of Variation

# for w 
(cv_w <- sd_w / mean(w))

#for m:
# for x 
(cv_m <- sd_m / mean(m))

#MAD/median

# for w:
(mm_w <- mad_w / median(w))

#for m:
(mm_m <- mad_m / median(m))

 




