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
  
  int<lower=1> N_sbj;  // number of subject
  int<lower=1> J_sbj[N];  // grouping indicator per observation
  
  int prior_only;  // should the likelihood be ignored?
}
parameters {
  vector[K_ds] b_ds;  // population-level effects
  vector[K_cA] b_cA;  // population-level effects
  vector<lower=0>[1] sd_ds_sbj;  // group-level standard deviations
  vector[N_sbj] z_ds_sbj[1];  // standardized group-level effects
  vector<lower=0>[1] sd_cA_sbj;  // group-level standard deviations
  vector[N_sbj] z_cA_sbj[1];  // standardized group-level effects
}

transformed parameters {
  vector[N_sbj] r_ds_sbj;  // actual group-level effects
  vector[N_sbj] r_cA_sbj;  // actual group-level effects
  r_ds_sbj = (sd_ds_sbj[1] * (z_ds_sbj[1]));
  r_cA_sbj = (sd_cA_sbj[1] * (z_cA_sbj[1]));
}

model {
  // likelihood including constants
  if (!prior_only) {
    // initialize different - same linear predictor term
    vector[N] ds = X_ds * b_ds;
    // initialize linear predictor term
    vector[N] cA = X_cA * b_cA;
    
    for (n in 1:N) {
      // add more terms to the linear predictor
      ds[n] += r_ds_sbj[J_sbj[n]];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      cA[n] += r_cA_sbj[J_sbj[n]];
    }
    
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
  target += gamma_lpdf(sd_ds_sbj | 2, 0.1); 
  target += std_normal_lpdf(z_ds_sbj[1]);
  target += gamma_lpdf(sd_cA_sbj | 2, 0.1); 
  target += std_normal_lpdf(z_cA_sbj[1]);
}

