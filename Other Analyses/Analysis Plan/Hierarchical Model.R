data_s3 <- data_short_MA %>% 
  select(subject, contains('C', ignore.case = FALSE)) %>% 
  melt(id.var='subject',
       variable.name='store',
       value.name='response') %>% 
  mutate(store=ifelse(store=='C1_1', 'Resort Hotel', 'Grocery Store'),
         response=as.numeric(response))


# Create Stan Data
X <- model.matrix(~ store, data_s3)

#Stan Data
stan_data <- list(
  N        = nrow(data_s3),
  Y        = data_s3$response,
  X        = X,
  K        = ncol(X),
  N_sbj    = n_sbj,
  J_sbj    = data_s3$subject,
  prior_only = 0
)

brm_stan <- rstan::stan_model(file = 'brm_stan.stan')

# Fit the model
fit1 <- rstan::sampling(brm_stan, 
                        iter = 2000, 
                        cores = 4, 
                        data = stan_data, 
                        save_warmup = FALSE)

draws <- extract(fit1)

u <- apply(draws$r_sbj, 2, mean) %>% exp()

fe_Intercept <- mean( exp(as.vector(draws$b[,1]) + as.vector(draws$b[,2])*0.5) )

data_s3 %>% 
  group_by(subject) %>%
  summarise(mean_sbj=mean(response)) %>% 
  ungroup() %>% 
  mutate(model=fe_Intercept+u) %>% 
  ggplot(aes(subject)) +
  geom_point(aes(y=mean_sbj)) +
  geom_point(aes(y=model), color='firebrick') +
  mytheme()
  

fit <- brm( 
  response ~ store + (1|subject),
  family = Gamma(link = "log"), 
  cores = 4, 
  data = data_s3
)

