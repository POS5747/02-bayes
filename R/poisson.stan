data {
  int<lower = 1> N;     // number of obervations
  int<lower = 0> y[N];  // observed outcomes
  real cauchy_prior_scale;
}

parameters {
  real lambda;
}

model {
  // prior 
  lambda ~ cauchy(0, cauchy_prior_scale);
  // posterior
  y ~ poisson(lambda);
} 
