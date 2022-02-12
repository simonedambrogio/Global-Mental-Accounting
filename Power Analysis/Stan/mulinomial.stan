data {
  int<lower = 1> N_sbj;
  int<lower = 0,upper = N_sbj> ans[3];
}
parameters {
  simplex[3] theta;
}
model {
  target += dirichlet_lpdf(theta | rep_vector(2, 3));
  target += multinomial_lpmf(ans | theta);
}