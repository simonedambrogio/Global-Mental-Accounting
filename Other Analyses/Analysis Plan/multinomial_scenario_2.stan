
data {
  int<lower=1> N;  // total number of observations
  int<lower=2> ncat;  // number of categories
  int Y[N];  // response variable
  int<lower=1> K_5;  // number of population-level effects
  matrix[N, K_5] X_5;  // population-level design matrix
  int<lower=1> K_10;  // number of population-level effects
  matrix[N, K_10] X_10;  // population-level design matrix
  int<lower=1> K_Other;  // number of population-level effects
  matrix[N, K_Other] X_Other;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}

parameters {
  vector[K_5] b_5;  // population-level effects
  vector[K_10] b_10;  // population-level effects
  vector[K_Other] b_Other;  // population-level effects
}
transformed parameters {
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu5 = X_5 * b_5;
    // initialize linear predictor term
    vector[N] mu10 = X_10 * b_10;
    // initialize linear predictor term
    vector[N] muOther = X_Other * b_Other;
    // linear predictor matrix
    vector[ncat] mu[N];
    for (n in 1:N) {
      mu[n] = transpose([0, mu5[n], mu10[n], muOther[n]]);
    }
    for (n in 1:N) {
      target += categorical_logit_lpmf(Y[n] | mu[n]);
    }
  }
  // priors including constants
  target += normal_lpdf(b_5 | 0, 5);
  target += normal_lpdf(b_10 | 0, 5);
  target += normal_lpdf(b_Other | 0, 5);
}

