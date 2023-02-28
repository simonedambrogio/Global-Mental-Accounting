data {
  int<lower = 1> N_opt;
  int<lower = 1> N_sbj;
  int<lower = 0,upper = N_sbj> ans[N_opt];
}
parameters {
  simplex[N_opt] theta;
}
model {
  target += dirichlet_lpdf(theta | rep_vector(2, N_opt));
  target += multinomial_lpmf(ans | theta);
}