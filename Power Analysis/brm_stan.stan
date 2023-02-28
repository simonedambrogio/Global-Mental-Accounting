data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_sbj;  // number of subject
  int<lower=1> J_sbj[N];  // grouping indicator per observation

  int prior_only;  // should the likelihood be ignored?
}

parameters {
  vector[K] b;  // population-level effects
  real<lower=0> shape;  // shape parameter
  vector<lower=0>[1] sd_sbj;  // group-level standard deviations
  vector[N_sbj] z_sbj[1];  // standardized group-level effects
}
transformed parameters {
  vector[N_sbj] r_sbj;  // actual group-level effects
  r_sbj = (sd_sbj[1] * (z_sbj[1]));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = X * b;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_sbj[J_sbj[n]];
    }
    for (n in 1:N) {
      // apply the inverse link function
      mu[n] = shape * exp(-(mu[n]));
    }
    target += gamma_lpdf(Y | shape, mu);
  }
  // priors including constants
  target += normal_lpdf(b | 0, 5);
  target += gamma_lpdf(shape | 0.01, 0.01);
  target += gamma_lpdf(sd_sbj | 2, 0.1); 
  target += std_normal_lpdf(z_sbj[1]);
}
