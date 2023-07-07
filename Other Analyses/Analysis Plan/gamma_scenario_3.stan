data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  
  int prior_only;  // should the likelihood be ignored?
}

parameters {
  vector[K] b;  // population-level effects
  real<lower=0> shape;  // shape parameter
}

transformed parameters {
}

model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = X * b;
    for (n in 1:N) {
      // apply the inverse link function
      mu[n] = shape * exp(-(mu[n]));
    }
    target += gamma_lpdf(Y | shape, mu);
  }
  // priors including constants
  target += normal_lpdf(b | 0, 5);
  target += gamma_lpdf(shape | 0.01, 0.01);
}
 