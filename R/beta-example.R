
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose of script:
#  1. Demonstrate how to work with the Bernoulli model with a beta prior
#  2. Illustrate how to summarize the posterior
#  3. Illustrate how to work with posterior simulations
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages
library(tidyverse)

# data
N <- 150
k <- 8

# prior
alpha_star <- 3
beta_star <- 15

# posterior
alpha_prime <- alpha_star + k
beta_prime <- beta_star + (N - k)

# plot posterior pdf
ggplot() + 
  xlim(0, 1) + 
  stat_function(fun = dbeta, n = 1001,
                args = list(shape1 = alpha_prime, shape2 = beta_prime)) + 
  labs(x = "Pi",
       y = "Posterior Density") + 
  theme_bw()
ggsave("figs/beta-posterior-density.png", height = 3, width = 4)

# find posterior mean
alpha_prime/(alpha_prime + beta_prime)

# 90% percentile credible interval
qbeta(c(0.05, 0.95), alpha_prime, beta_prime)

# posterior simulations
post_sims <- rbeta(10000, alpha_prime, beta_prime)

# plot histogram
gg_data <- tibble(post_sims) 
ggplot(gg_data, aes(x = post_sims)) + 
  geom_histogram(bins = 100)

# posterior mean
mean(post_sims)

# 95% credible interval
quantile(post_sims, c(0.05, 0.95))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# a technical piece for the slides
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# figure to illustrate 90% credible interval.
ggplot(data = NULL, aes(x = c(0, 1))) +
  geom_area(stat = "function", fun = dbeta, 
            args = list(shape1 = alpha_prime, shape2 = beta_prime),
            fill = "#1b9e77", 
            xlim = qbeta(c(0.05, 0.95), alpha_prime, beta_prime)) +
  geom_area(stat = "function", fun = dbeta, 
            args = list(shape1 = alpha_prime, shape2 = beta_prime),
            fill = "grey80", 
            xlim = qbeta(c(0.00, 0.05), alpha_prime, beta_prime)) +
  geom_area(stat = "function", fun = dbeta, 
            args = list(shape1 = alpha_prime, shape2 = beta_prime),
            fill = "grey80", 
            xlim = qbeta(c(0.95, 1.00), alpha_prime, beta_prime)) +
  labs(x = "Pi", y = "Posterior Density") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = round(qbeta(c(0.05, 0.95), alpha_prime, beta_prime), 3), 
                     minor_breaks = NULL,
                     limits = c(0, .2)) + 
  theme_bw() +
  labs(title = "Constructing a Credible Interval with the Percentile Method")
ggsave("figs/beta-posterior-90pct-credible-interval.png", height = 3, width = 4, scale = 1.5)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
