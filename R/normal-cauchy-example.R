
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose of script:
#  1. Illustrate the normal-cauchy model with MCMCmetrop1R()
#  2. Illustrate the normal-cauchy model with stan()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages
library(tidyverse)
library(MCMCpack)
library(rstan)

# toy data
y <- c(1, 2, 3, 4, 5)


# using MCMCmetrop1R() 
# --------------------

# log-posterior
log_posterior <- function(theta, y) {
  mu <- theta[1]
  log_sigma <- theta[2]
  lp <- sum(dnorm(y, mu, exp(log_sigma), log = TRUE)) + dcauchy(mu, 0, 1 ,log = TRUE) + dcauchy(log_sigma, 0, 1, log = TRUE)
  return(lp)
}

# metropolis algorithm
post_sims <- MCMCmetrop1R(fun = log_posterior, y = y,
                          theta.init = c(0, 0), 
                          burnin = 5000, 
                          mcmc = 50000)

# compute "effective sample size" for comparison with Stan below
coda::effectiveSize(post_sims)

# plot draws
gg_data <- as_tibble(post_sims) %>%
  rename(mu_tilde = V1, log_sigma_tilde = V2) %>%
  mutate(iter = 1:n()) %>%
  glimpse()
ggplot(gg_data, aes(x = mu_tilde, y = log_sigma_tilde)) + 
  geom_point(alpha = 0.1) + 
  theme_bw() + 
  labs(title = "Metropolis Samples from Posterior for Normal Model",
       subtitle = "50,000 iterations total; 5,000 iteration burnin")
ggsave("figs/metrop1r-example-normal.png", height = 3, width = 4, dpi = 300, scale = 2)

# summarize draws
gg_data %>%
  summarize(mean_mu = mean(mu_tilde),
            mean_log_sigma = mean(log_sigma_tilde),
            mu05 = quantile(mu_tilde, 0.05),
            mu95 = quantile(mu_tilde, 0.95),
            log_sigma05 = quantile(log_sigma_tilde, 0.05),
            log_sigma95 = quantile(log_sigma_tilde, 0.95))


# using stan()
# ------------

# fit model
stan_data <- list(y = y, N = length(y))
fit <- stan(file = "R/normal.stan", 
            data = stan_data, 
            chains = 4, 
            cores = 4, 
            iter = 2000,  
            warmup = 1000)

# rstan's default summary
print(fit)

# plot of draws
gg_data <- as.data.frame(fit)
ggplot(gg_data, aes(x = mu, y = log_sigma)) + 
  geom_point(alpha = 0.1) + 
  theme_bw() + 
  labs(title = "Stan/HMC Samples from Posterior for Normal Model",
       subtitle = "2,000 iterations total; 1,000 iteration burnin ('warmup')")
# summarize draws
gg_data %>%
  summarize(mean_mu = mean(mu),
            mean_log_sigma = mean(log_sigma),
            mu05 = quantile(mu, 0.05),
            mu95 = quantile(mu, 0.95),
            log_sigma05 = quantile(log_sigma, 0.05),
            log_sigma95 = quantile(log_sigma, 0.95))
