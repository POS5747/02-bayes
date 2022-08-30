data {
  int<lower = 1> N;  // number of obervations
  vector[N] y;       // observed outcomes
}

parameters {
  real mu;
  real log_sigma;
}

transformed parameters {
  real<lower = 0> sigma;
  sigma = exp(log_sigma);
}

model {
  // prior 
  mu ~ cauchy(0, 1);
  log_sigma ~ cauchy(0, 1);
  // posterior
  y ~ normal(mu, sigma);
} 
