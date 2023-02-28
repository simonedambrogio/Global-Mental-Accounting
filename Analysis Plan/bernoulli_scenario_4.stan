data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  
  int prior_only;  // should the likelihood be ignored?
}

parameters {
  vector[K] b;  // population-level effects
}

transformed parameters {
}

model {
  // likelihood including constants
  if (!prior_only) {
    vector[N] mu = X * b;
    target += bernoulli_logit_lpmf(Y | mu);
  }
  // priors including constants
  target += normal_lpdf(b | 0, 5);
}

