
data {
  int<lower = 1> N_sbj;
  vector[N_sbj] x;
  int<lower=0,upper=1> ans[N_sbj];
}
parameters {
  real beta0;
  real beta1;
}
model {
  target += student_t_lpdf(beta0 | 3, 0, 2.5);
  target += student_t_lpdf(beta1 | 3, 0, 2.5);
  target += bernoulli_logit_lpmf(ans | beta0 + beta1 * x);
}

