make_stancode(formula = response ~ price, 
              family = bernoulli(link = "logit"),
              data = data_s4,
              refresh = 0, 
              cores = 4)

# Create Stan Data
X <- model.matrix(~ price, data_s4)

#Stan Data
stan_data <- list(
  N = nrow(data_s4),
  Y = data_s4$response,
  X = X,
  K = ncol(X),
  
  prior_only = 0
)
bernoulli_scenario_4 <- rstan::stan_model(file = 'bernoulli_scenario_4.stan')

# Fit the model
fit_S4 <- rstan::sampling(bernoulli_scenario_4, 
                          iter = 2000, 
                          cores = 4, 
                          save_warmup = FALSE,
                          data = stan_data, 
                          save_warmup = FALSE)

draws <- extract(fit_S4)

draws$b %>%
  melt(value.name = 'post') %>% 
  mutate(Var2=ifelse(Var2==1, 'Intercept', '')) %>% 
  filter(Var2!='Intercept') %>%
  ggplot(aes(x = post, y = Var2)) + 
  stat_halfeye(fill = jcolors::jcolors(palette = "pal8")[6], 
               color = jcolors::jcolors(palette = "pal8")[12]) + 
  mytheme() + geom_vline(xintercept = 0, linetype = 2) + 
  labs(y = NULL, x = "Posterior Probability", title='Low - High')
