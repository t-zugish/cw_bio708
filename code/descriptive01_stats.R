#Descriptive statistics! 

#call packages every single time
library(tidyverse)


# Central tendency --------------------------------------------------------


#important definitions:
#arithmetic mean: what I usually associate with an average (sum and divide)
#geometric mean: multiply all numbers and take the root of the product of the number of elements
#median (50th percentile)

# construct vectors x and y (examples for calculating different summary statistics)
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)
x
y

#arithmetic mean: 
sum(x)
#length() = number of elements in vector

(mu_x <-sum(x) / length(x))
mu_x
mean(x)

mu_y <- sum(y) / length(y)
mu_y
mean(y)
#to print value when running code: put in another set of parentheses

#geometric mean (no built-in function). prod() is product of all elements
mu_x_ge <- prod(x)^(1 / length(x))

#geometric mean in log scale is more stable (and faster!) in R

exp(sum(log(x)) / length(x))

(mu_y_ge <- prod(y)^(1 / length(y)))
exp(sum(log(y)) / length(y))

#comparing means: arithmetic mean is always larger than geometric mean because it is more sensitive to outliers 
c(mu_x, mu_y)
c(mu_x_ge, mu_y_ge)

# median
med_x <- median(x)
med_y <- median(y)
med_x
med_y

# Variation ---------------------------------------------------------------

#variance: sum of squares. sensitive to outliers just like arithmetic mean
(var_x <- sum((x - mean(x))^2)/length(x))
(var_y <- sum((y - mean(y))^2)/length(y))

#standard deviation: variance ^ 1/2. reminder that units of stddev are not the same as variance!

sqrt(var_x)
sqrt(var_y)

# interquartile range (IQR): more robust (robust means less sensitive to outliers)
(x_l <- quantile(x, 0.25))
(x_h <- quantile(x, 0.75))
(iqr_x <- x_h - x_l)

#alt method: using vector elements [n] to calculate IQR. the number in the square bracket is the 1st, 2nd, 3rd, etc. element in the vector. 
(y_q <- quantile(y, c(0.25, 0.75)))
(iqr_y <- y_q[2] - y_q[1])

#MAD: another robust deviation measure (Median of Absolute Deviation (from the median))
(ad_x <- abs(x - median(x)))
median(ad_x)

(ad_y <- abs(y - median(y)))
median(ad_y)

#coefficient of variation (NO UNITS: good for comparing between datasets: standard deviation/mean)
(sd_x <- sqrt(var_x))
(sd_y <- sqrt(var_y))

(cv_x <- sd_x / mu_x)
(cv_y <- sd_y / mu_y)


# Bonus content! ----------------------------------------------------------
#defining your own function: function()
#defining a function to calculate CoV:

mycv <- function(v) {
  mu <- mean(v)
  s <- sd(v)
  cv <- s / mu
  
  return(cv)
}

mycv

mycv(x)

#we get a different value from mycv compared to the manual calculation because the built-in sd(x) calculates standard deviation differently than our manual stdev calculation




