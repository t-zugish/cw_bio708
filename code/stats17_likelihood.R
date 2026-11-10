##Likelihood
##VERY IMPORTANT FUNDAMENTAL CONCEPT

pacman::p_load(tidyverse,
               patchwork)

#using poisson dist. to explain likelihood

(pr3 <- dpois(x = 3, lambda = 3.5))
#reminder: x must be discrete, but lambda can take any value
#output: probability of observing value x given the mean of a distribution lambda

# find the best value of lambda that maximizes pr(x = 3), changing lambda from 0 to 10 by 0.1
lambda <- seq(0.1, 10, by = 0.1)
pr <- dpois(3, lambda = lambda)

df_pois <- tibble(lambda = lambda,
       pr = dpois(3, lambda = lambda)) %>% 
  arrange(desc(pr))

##what about vectors? y = {3, 2, 5}
#probability of observing three together

y <- c(3, 2, 5)
(prv <- dpois(y, lambda = 3.5))
lik <- prv[1] * prv[2] * prv[3] #manual calc
lik <- prod(pr) #product function

lambda <- seq(0.1, 10, by = 0.01)
pr <- dpois(y, lambda = lambda)

#likelihood of observing each lambda value
#probability of observing y
lik_i <- sapply(lambda,
       function(x) {
         prod(dpois(y, lambda = x))
       })

df_pois_v <- tibble(lambda = lambda,
                 lik = lik_i) %>% 
  arrange(desc(lik))
df_pois_v

#maximum likelihood value matches sample mean
mean(y)

#why??
dpois(3, lambda = 3.5)
(3.5^3 * exp(-3.5))/factorial(3)

#visualization

df_pois_v %>% 
  ggplot(
    aes(x = lambda,
        y = log(lik)) #very small values; taking the log makes figure more readable
  ) +
  geom_line() +
  labs(x = "lambda value",
       y = "likelihood")

##general case: GLM and likelihood

df_count <- read_csv("data_src/data_garden_count.csv")

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

logLik(m_pois) #this value should not be positive!
summary(m_pois)

#likelihood cannot be compared across data sets!!!!!
#assumption of data point independence comes from likelihood. multiplication is only valid if samples are independent from deach other!







