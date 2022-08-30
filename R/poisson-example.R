
# load packages
library(tidyverse)
library(rstan)

# load data
hks <- read_csv("data/hks.csv")

# fit stan model
stan_data <- list(y = hks$osvAll, 
                  N = length(hks$osvAll), 
                  cauchy_prior_scale = 100)

# fit model
fit <- stan(file = "R/poisson.stan", 
            data = stan_data, 
            chains = 4, 
            cores = 4, 
            iter = 2000,  
            warmup = 1000)

# summarize fit
print(fit)

# transform sims of lambda into sims of SD
post_sims <- as.data.frame(fit) %>%
  mutate(sd = sqrt(lambda))

# numerical summary of posterior
summary(post_sims$sd)

# quick histogram
qplot(post_sims$sd)

# posterior predictive distribution
post_sims <- as.data.frame(fit) %>%
  mutate(y_tilde = rpois(n(), lambda))

# numerical summary of posterior predictive distribution
summary(post_sims$y_tilde)

# numerical summary of observed outcome
summary(hks$osvAll)



