
data {
  int<lower=1> N;  // total number of observations
  int<lower=2> ncat;  // number of categories
  int Y[N];  // response variable
  
  int<lower=1> K_2;  // number of population-level effects
  matrix[N, K_2] X_2;  // population-level design matrix
  int<lower=1> K_3;  // number of population-level effects
  matrix[N, K_3] X_3;  // population-level design matrix
  
  // data for group-level effects of ID 1
  
  int<lower=1> N_sbj;  // number of subject
  int<lower=1> J_sbj[N];  // grouping indicator per observation
  // int<lower=1> N_3_sbj;  // number of subject
  // int<lower=1> J_3_sbj[N];  // grouping indicator per observation
  
  int prior_only;  // should the likelihood be ignored?
  
}

parameters {
  
  vector[K_2] b_2;  // population-level effects
  vector[K_3] b_3;  // population-level effects
  
  vector<lower=0>[1] sd_sbj;  // group-level standard deviations
  vector[N_sbj] z_sbj[1];  // standardized group-level effects
  // 
  // vector<lower=0>[1] sd3_sbj;  // group-level standard deviations
  // vector[N_3_sbj] z3_sbj[1];  // standardized group-level effects
}

transformed parameters {
  
  vector[N_sbj] r_sbj;  // actual group-level effects
  // vector[N_3_sbj] r_sbj3;  // actual group-level effects
  // 
  r_sbj = (sd_sbj[1] * (z_sbj[1]));
  // r_sbj3 = (sd3_sbj[1] * (z3_sbj[1]));
}

model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu2 = X_2 * b_2;
    vector[N] mu3 = X_3 * b_3;
    vector[ncat] mu[N];
    
    // for (n in 1:N) {
    //   mu2[n] += r_sbj2[J_2_sbj[n]];
    // }
    // for (n in 1:N) {
    //   mu3[n] += r_sbj3[J_3_sbj[n]];
    // }
    
    for (n in 1:N) {
      mu[n] = transpose([0, mu2[n], mu3[n]]);
    }
    for (n in 1:N) {
      mu[n] += r_sbj[J_sbj[n]];
    }
    
    for (n in 1:N) {
      target += categorical_logit_lpmf(Y[n] | mu[n]);
    }
  }
  // priors including constants

  target += normal_lpdf(b_2 | 0, 5);
  target += normal_lpdf(b_3 | 0, 5);
  
  target += gamma_lpdf(sd_sbj | 2, 0.1);
  target += std_normal_lpdf(z_sbj[1]);
  // 
  // target += gamma_lpdf(sd3_sbj | 2, 0.1); 
  // target += std_normal_lpdf(z3_sbj[1]);
  
}
