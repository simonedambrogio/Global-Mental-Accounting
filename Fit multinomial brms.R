library(brms)

N <- 15
dat <- data.frame(
  y1 = rbinom(N, 10, 0.3), y2 = rbinom(N, 10, 0.5), 
  y3 = rbinom(N, 10, 0.7), x = rnorm(N)
)

dat$size <- with(dat, y1 + y2 + y3)
dat$y <- with(dat, cbind(y1, y2, y3))

prior <- prior(normal(0, 10), "b", dpar = muy2) +
               prior(cauchy(0, 1), "Intercept") +
               prior(normal(0, 2), "Intercept", dpar = muy3)

fit <- brm(bf(y | trials(size)  ~ 1, muy2 ~ x), data = dat, 
           family = multinomial(), prior = prior)

make_stancode(bf(y | trials(size)  ~ 1, muy2 ~ x), data = dat, 
              family = multinomial(), prior = prior)



# ----- 
library(brms); library(dplyr); library(purrr)
options( mc.cores = 4 )
library( rstan ); 
rstan_options(auto_write = TRUE)
library(ggplot2); theme_set(ggpubr::theme_pubr())
library(gridExtra)
# First, generate data assuming a multinomial distribution. 
# The outcomes will be determined by a vector  θ (called true_theta 
# below in the R code) that indicates the probability of each outcome:
  
(true_theta <- tibble(theta_NR = .2, 
                      theta_Neologism = .1,
                      theta_Formal = .2,
                      theta_Mixed = .08,
                      theta_Correct =  1 - 
                        (theta_NR + theta_Neologism + theta_Formal + theta_Mixed)))


# Given this vector of probabilities θ, generate values assuming a 
# multinomial distribution of responses in 100 trials:
N_trials <- 100
(ans_mn <- rmultinom(1, N_trials, true_theta))

# Create standata:
# c(ans_mn) makes a vector out of the matrix ans_mn
data_mn <-  list(N_trials = N_trials,
                 ans = c(ans_mn)) 
# Upload Stancode:
stan_mn <- stan_model(file = "mulinomial.stan")


# Fit model
res<-sampling(stan_mn, iter = 2000, cores=4, data=data_mn)


