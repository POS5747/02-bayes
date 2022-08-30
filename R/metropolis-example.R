
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose of script:
#  1. Show examples of Metropolis sampler.
#  2. Make GIFs showing how the sampler works.
#  3. Illustrate what happens with a too-narrow 
#     too-wide proposal distribution.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# load packages
library(tidyverse)
library(gganimate)
library(ggforce)

# set seed for reproducibility
set.seed(1234)

# simple metropolis example to generate samples from N(0, 1) = dnorm(x), 
# with a uniform proposal density conditional on current location.
n_iter <- 500000
values <- numeric(n_iter)
values[1] <- 10  # arbitrary starting point, choose a bad one on purpose
for (t in 1:(n_iter - 1)) {
  # generate proposals close to current location so chain wanders slowly
  proposal <- runif(1, values[t] - 0.5, values[t] + 0.5)
  # density ratio
  r <- dnorm(proposal)/dnorm(values[t])
  # accept always
  if (r >= 1) {
    values[t + 1] <- proposal
  }
  # accept probabilistically
  if (r < 1) {
    accept <- rbinom(1, size = 1, prob = r)
    values[t + 1] <- ifelse(accept == 1, proposal, values[t])
  }
}

# histogram of draws (dropping first 5,000 for burn-in)
gg_data <- tibble(value = values[5001:n_iter])
ggplot(gg_data, aes(x = value)) + 
  geom_histogram()

# numerical summary of draws (dropping first 5,000 for burn-in)
summary(gg_data$value)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# a technical pieces for the slides
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# trace plot of first 500 draws
mc_sims <- tibble(value = values[1:500]) %>% 
  mutate(iter = 1:n()) %>%
  glimpse()
anim <- ggplot(mc_sims, aes(x = iter, y = value)) +
  #geom_path(alpha = 0.05) +
  geom_line(alpha = 1) +
  transition_manual(iter, cumulative = TRUE) + 
  labs(title = "Metropolis Samples from Standard Normal Distribution",
       subtitle = "500 Iterations", 
       x = "Iteration",
       y = "Value") + 
  theme_bw()
scale <- 2
animate(anim, duration = 10, height = 3*scale, width = 4*scale, units = "in", res = 300)
anim_save("figs/metropolis-example-ts-500.gif")

# histogram of first 500 draws
bin_width <- 0.2
count_data <- mc_sims %>%
  mutate(x = plyr::round_any(value, bin_width)) %>%
  group_by(x) %>%
  mutate(y = seq_along(x))
plot <- ggplot(count_data, aes(group = iter, x, y, fill = iter)) + # group by index is important
  geom_ellipse(aes(group = iter, x0 = x, y0 = y, a = bin_width/2, b = 0.5, angle = 0), color = "black") + 
  coord_equal(bin_width) + 
  scale_fill_gradient2(low = "#d95f02", high = "#7570b3", mid = "#1b9e77", midpoint = nrow(mc_sims)/2) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank()) + 
  labs(x = "Values"); plot # to make the dots look nice and round
ggsave("figs/metropolis-example-hist-500.png", height = 3, width = 4, dpi = 300, scale = 2)

# trace plot of first 5,000 draws
mc_sims <- tibble(value = values[1:5000]) %>% 
  mutate(iter = 1:n(), 
         anim_group = iter %% 10 - iter) %>%
  glimpse()
anim <- ggplot(mc_sims, aes(x = iter, y = value)) +
  #geom_path(alpha = 0.05) +
  geom_line(alpha = 1) +
  transition_manual(iter, cumulative = TRUE) + 
  labs(title = "Metropolis Samples from Standard Normal Distribution",
       subtitle = "5,000 Iterations", 
       x = "Iteration",
       y = "Value") + 
  theme_bw()
scale <- 2
animate(anim, duration = 10, height = 3*scale, width = 4*scale, units = "in", res = 300)
anim_save("figs/metropolis-example-ts-5k.gif")

# histogram of all draws
mc_sims <- tibble(value = values) %>% 
  mutate(iter = 1:n()) %>%
  filter(iter > 50000) %>%
  glimpse()
ggplot(mc_sims) +
  geom_histogram(aes(x = value, y = ..density..), bins = 100, color = "black", fill = "grey90") + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color = "#d95f02", size = 1) + 
  theme_bw() + 
  labs(title = "Metropolis Samples from Stanard Normal Distribution",
       subtitle = "500,000 iterations total; 50,000 iteration burnin")
ggsave("figs/metropolis-example-hist-500k.png", height = 3, width = 4, dpi = 300, scale = 2)


# ---- experiment with different proposal distributions

# -- very wide proposal distribution

# metropolis sampler
n_iter <- 5000
values <- numeric(n_iter)
values[1] <- 10  # arbitrary starting point, choose a bad one on purpose
for (t in 1:(n_iter - 1)) {
  # generate proposals close to current location so chain wanders slowly
  proposal <- runif(1, values[t] - 50, values[t] + 50)
  # density ratio
  r <- dnorm(proposal)/dnorm(values[t])
  # accept always
  if (r >= 1) {
    values[t + 1] <- proposal
  }
  # accept probabilistically
  if (r < 1) {
    accept <- rbinom(1, size = 1, prob = r)
    values[t + 1] <- ifelse(accept == 1, proposal, values[t])
  }
}

# trace plot of first 5,000 draws
mc_sims <- tibble(value = values[1:5000]) %>% 
  mutate(iter = 1:n(), 
         anim_group = iter %% 10 - iter) %>%
  glimpse()
anim <- ggplot(mc_sims, aes(x = iter, y = value)) +
  #geom_path(alpha = 0.05) +
  geom_line(alpha = 1) +
  transition_manual(iter, cumulative = TRUE) + 
  labs(title = "Metropolis Samples from Standard Normal Distribution",
       subtitle = "Very Wide Proposal Distribution; 5,000 Iterations", 
       x = "Iteration",
       y = "Value") + 
  theme_bw()
scale <- 2
animate(anim, duration = 10, height = 3*scale, width = 4*scale, units = "in", res = 300)
anim_save("figs/metropolis-example-ts-5k-wide.gif")

# -- very narrow proposal distribution

# metropolis sampler
n_iter <- 10000
values <- numeric(n_iter)
values[1] <- 10  # arbitrary starting point, choose a bad one on purpose
for (t in 1:(n_iter - 1)) {
  # generate proposals close to current location so chain wanders slowly
  proposal <- runif(1, values[t] - 0.05, values[t] + 0.05)
  # density ratio
  r <- dnorm(proposal)/dnorm(values[t])
  # accept always
  if (r >= 1) {
    values[t + 1] <- proposal
  }
  # accept probabilistically
  if (r < 1) {
    accept <- rbinom(1, size = 1, prob = r)
    values[t + 1] <- ifelse(accept == 1, proposal, values[t])
  }
}

# trace plot of first 10,000 draws
mc_sims <- tibble(value = values[1:10000]) %>% 
  mutate(iter = 1:n(), 
         anim_group = iter %% 100 - iter) %>%
  glimpse()
anim <- ggplot(mc_sims, aes(x = iter, y = value)) +
  #geom_path(alpha = 0.05) +
  geom_line(alpha = 1) +
  transition_manual(iter, cumulative = TRUE) + 
  labs(title = "Metropolis Samples from Standard Normal Distribution",
       subtitle = "Very Narrow Proposal Distribution; 10,000 Iterations", 
       x = "Iteration",
       y = "Value") + 
  theme_bw()
scale <- 2
animate(anim, duration = 10, height = 3*scale, width = 4*scale, units = "in", res = 300)
anim_save("figs/metropolis-example-ts-10k-narrow.gif")
