functions {
  /* 
   * Args: 
   *   y:  response value 0: A, 1: B, 2: No difference
   *   ds: different (0|1) - same (2) probability
   *   cA: conditional (different) A probability
   * Returns:  
   *   a scalar to be added to the log posterior 
   */ 
   real mental_accounting_lpdf(real y,  real ds, real cA) {
     if (y == 0) { // Stay (Explore)
       return bernoulli_lpmf(1 | ds) + bernoulli_lpmf(1 | cA); 
     } else if (y == 1) { // Switch (Explore)
       return bernoulli_lpmf(1 | ds) + bernoulli_lpmf(0 | cA);
     } else { // Select
       return bernoulli_lpmf(0 | ds);
     }
   }
}

data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_ds;  // number of population-level effects
  matrix[N, K_ds] X_ds;  // population-level design matrix
  int<lower=1> K_cA;  // number of population-level effects
  matrix[N, K_cA] X_cA;  // population-level design matrix
  
  int prior_only;  // should the likelihood be ignored?
}
parameters {
  vector[K_ds] b_ds;  // population-level effects
  vector[K_cA] b_cA;  // population-level effects
}

transformed parameters {
}

model {
  // likelihood including constants
  if (!prior_only) {
    // initialize different - same linear predictor term
    vector[N] ds = X_ds * b_ds;
    // initialize linear predictor term
    vector[N] cA = X_cA * b_cA;
    
    for (n in 1:N) {
      // apply the inverse link function
      ds[n] = inv_logit(ds[n]);
    }
    for (n in 1:N) {
      // apply the inverse link function
      cA[n] = inv_logit(cA[n]);
    }
    for (n in 1:N) {
      target += mental_accounting_lpdf(Y[n] | ds[n], cA[n]);
    }
  }
  // priors including constants
  target += normal_lpdf(b_ds | 0, 5);
  target += normal_lpdf(b_cA | 0, 5);
}

