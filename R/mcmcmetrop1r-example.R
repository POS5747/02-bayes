
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose of script:
#  1. Illustrate how to use MCMCmetrop1R()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages
library(MCMCpack)
library(tidyverse)

# target distribution is t2 (heavy tails)
log_posterior <- function(theta) {
  dt(theta, df = 2, log = TRUE)
}

# metropolis algorithm
post_sims <- MCMCmetrop1R(fun = log_posterior, 
                          theta.init = 0, 
                          burnin = 5000, 
                          mcmc = 50000)

# plot draws (burn-in already removed via `burnin` argument)
gg_data <- tibble(post_sim = as.numeric(post_sims))
ggplot(gg_data) +
  geom_histogram(aes(x = post_sim, y = ..density..), bins = 100, color = "black", fill = "grey90") + 
  stat_function(fun = dt, n = 1001, args = list(df = 2), color = "#d95f02", size = 1) + 
  theme_bw() + 
  labs(title = "Metropolis Samples from t2 Distribution",
       subtitle = "50,000 iterations total; 5,000 iteration burnin")
ggsave("figs/metrop1r-example.png", height = 3, width = 4, dpi = 300, scale = 2)

