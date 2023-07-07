


# ---------- 1. Mr. A vs Mr. B2 Scenario ---------- #
if( TRUE ){
  # Coding legend:
  # response: 0 (Happier/More upset);  
  #           1 (Less Happy/Less upset)
  #           2 (No difference)
  
  # Create dataframe scenario 1
  data_s1 <- data_short_MA %>% 
    select(subject, contains('A', ignore.case = FALSE)) %>% 
    # Recode response
    mutate(`gain-gain VS gain` = case_when(A1=='A'~0, A1=='B'~1, T~2),
           `loss-loss VS loss` = case_when(A2=='A'~0, A2=='B'~1, T~2),
           `gain-loss VS gain` = case_when(A3=='A'~0, A3=='B'~1, T~2),
           `loss-gain VS loss` = case_when(A4=='A'~0, A4=='B'~1, T~2)
    ) %>% 
    select(-contains('A', ignore.case=FALSE)) %>% 
    melt(id.var='subject',
         variable.name='scenario',
         value.name='response')
  
  data_s1 %>% 
    ggplot(aes(response, fill=as.factor(response))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) + 
    mytheme() + theme(legend.position = 'none') +
    scale_fill_brewer(palette = 'Set1') +
    scale_y_continuous(labels=scales::percent, guide = "prism_offset") +
    scale_x_continuous(guide = "prism_offset") +
    labs(y=NULL, x=NULL) +
    facet_wrap(~scenario)
  
  # ---- Fit Model ---- #
  library(rstan)
  library(tidybayes)
  # options(mc.cores = 4)
  rstan_options(auto_write = TRUE)
  
  # ------------- ~ scenario ------------- #
  # Create Stan Data
  X <- model.matrix(~ 0 + scenario, data_s1)
  
  #Stan Data
  stan_data <- list(
    N        = nrow(data_s1),
    Y        = data_s1$response,
    X_ds     = X,
    X_cA     = X,
    K_ds     = ncol(X),
    K_cA     = ncol(X),
    
    N_sbj    = n_sbj,
    J_sbj    = data_s1$subject,
    
    prior_only = 0
  )
  mental_accounting_stan <- rstan::stan_model(file = 'mental_accounting_nohier.stan')
  
  # Fit the model
  fitMAS <- rstan::sampling(mental_accounting, 
                            iter = 2000, 
                            cores = 2, 
                            chains = 2,
                            data = stan_data, 
                            save_warmup = FALSE)
  
  draws <- extract(fitMAS)
  colnames(draws$b_ds) <- words_replace(colnames(X), 'scenario')
  colnames(draws$b_cA) <- words_replace(colnames(X), 'scenario')
  
  rbind(draws$b_ds %>% melt() %>% mutate(type='P[Different vs Same]'),
        draws$b_cA %>% melt() %>% mutate(type='P[A vs B | Different]')) %>% 
    mutate(Var2 = factor(Var2, levels = rev(unique(.$Var2))),
           type = factor(type, levels = rev(unique(.$type)))) %>% 
    ggplot(aes(x = value, y = Var2)) + 
    stat_halfeye(fill = jcolors::jcolors(palette = "pal8")[6],
                 color = jcolors::jcolors(palette = "pal8")[12]) + 
    mytheme() + 
    geom_vline(xintercept = 0, linetype = 2) + 
    labs(y = NULL, x = "Posterior Probability") +
    facet_grid(~type)
  
  
  mean( plogis(draws$b_ds[,1]) )
  mean( plogis(draws$b_ds[,2]) )
  mean( plogis(draws$b_ds[,3]) )
  mean( plogis(draws$b_ds[,4]) )
  
  data_s1 %>% 
    group_by(scenario) %>% 
    mutate(resp=ifelse(response==2, 0, 1)) %>% 
    summarise(mean(resp))
    
  # ---- Power Calculation ---- #
  # Calculate the probability of responding 'different'
  
  nModels <- 3
  nChains <- 2
  nClusters <- nModels*nChains
  
  n_sim <- 100
  n_sbj_range <- c(10, 50, 100, 150, 200, 250, 300)
  
  # Calculate the probability of responding 'different'
  logit = function(p) log(p/(1-p))
  # Effect found in the paper Thaler 1985
  n_sbj    = 87
  scenario = 1:4
  A        = c(56, 66, 22, 19)
  B        = c(16, 14, 61, 63)
  ds       = (A+B)/n_sbj # Probability to choose Different
  cA       = A/(A+B)
  
  effect_ds = logit(ds)
  effect_cA = logit(cA)
  
  sim_y <- function(sbj, mu_ds, mu_cA){
    
    
    n_sbj    = 87
    scenario = 1:4
    A        = c(56, 66, 22, 19)
    B        = c(16, 14, 61, 63)
    ds       = (A+B)/n_sbj # Probability to choose Different
    cA       = A/(A+B)
    
    std <- 0.5
    # Random Effect
    re_ds <- rnorm(1, 0, std)
    re_cA <- rnorm(1, 0, std)
    # Probability of 'Different' and Conditional Probability of 'A'
    p_ds <- plogis(mu_ds+re_ds)
    p_cA <- plogis(mu_cA+re_cA)
    
    response <- sapply( scenario, function(i){
      y_ds <- rbinom( 1, 1, p_ds[i])
      if( y_ds==1 ){
        response = rbinom(1, 1, (1-cA[i]) )
      } else {
        response = 2
      }
      response
    })
    
    return( data.frame(subject=sbj, response, scenario=paste0('s', scenario)) )
  }
  
  samplingfunction <- function(x){
    
    #Simulate data
    data_sim <- purrr::map_dfr(seq_len(n), 
                               sim_y, 
                               mu_ds=effect_ds, 
                               mu_cA=effect_cA)
    
    ## Create Stan Data
    X <- model.matrix(~ 0 + scenario, data_sim)
    
    #Stan Data
    stan_data <- list(
      N        = nrow(data_sim),
      Y        = data_sim$response,
      X_ds     = X,
      X_cA     = X,
      K_ds     = ncol(X),
      K_cA     = ncol(X),
      
      N_sbj    = n,
      J_sbj    = data_sim$subject,
      
      prior_only = 0
    )
    
    # Fit the model
    fit <- rstan::sampling(mixture_scenario_1, 
                           iter   = 2000, 
                           cores  = 2, 
                           chains = 2,
                           # pars   = c('b_ds', 'b_cA'),
                           data   = stan_data, 
                           refresh = 0,
                           save_warmup = FALSE)
    efit <- rstan::extract(fit)
    
    
    rbind(
      data.frame(
        estimate = apply(efit$b_ds, 2, mean),
        lwr      = apply(efit$b_ds, 2, quantile, probs=0.025),
        upr      = apply(efit$b_ds, 2, quantile, probs=0.975),
        type     = 'p_different',
        scenario = paste0('s', 1:4),
        N        = n
      ),
      
      data.frame(
        estimate = apply(efit$b_cA, 2, mean),
        lwr      = apply(efit$b_cA, 2, quantile, probs=0.025),
        upr      = apply(efit$b_cA, 2, quantile, probs=0.975),
        type     = 'p_A|different',
        scenario = paste0('s', 1:4),
        N        = n
      )
    )
    
  }
  
  
  n_sbj_range<-10
power <- map_dfr(n_sbj_range, function(n){
  message('\n\nN Subjects: ', n, '\n')
  
  message('Setting up Clusters...')
  cl <- parallel::makeCluster(nClusters, outfile = "")
  parallel::clusterExport(cl, c('mixture_scenario_1', 'n_sbj_range',
                                'sim_y', 'effect_ds', 'effect_cA', 
                                'n'))
  
  map_dfr(seq_len( round(n_sim/nModels) ), function(i){
    message( '\r', 'Simulation: ', i*nModels, '/', n_sim, appendLF = FALSE)
    parallel::parLapply(cl, 1:nModels, samplingfunction)
  })
  
})
  
  
  power %>% 
    mutate(
      is_significant = case_when(
        type=='p_different' & lwr<0 ~ FALSE,
        type=='p_different' & lwr>0 ~ TRUE,
        lwr<0 & scenario%in%c('s1', 's2') ~ FALSE,
        upr>0 & scenario%in%c('s3', 's4') ~ FALSE,
        T ~ TRUE ),
      type=ifelse(type=='p_different', 'P(Different)', 'P(A|Different)'),
      scenario = case_when(
        scenario=='s1'~ 'gain-gain VS gain',
        scenario=='s2'~ 'loss-loss VS loss',
        scenario=='s3'~ 'gain-loss VS gain',
        scenario=='s4'~ 'loss-gain VS loss'
      )) %>% 
    group_by(N, type, scenario) %>% 
    summarise(power = mean(is_significant)) %>% 
    ungroup() %>% 
    ggplot(aes(N, power, color=type)) +
    geom_line(size=1) +
    geom_point(size=3) +
    geom_hline(yintercept=0.8, linetype=2) +
    mytheme() +
    facet_wrap(~scenario) +
    scale_y_continuous(limits = c(0,1), labels=scales::percent) +
    scale_x_continuous(breaks = n_sbj_range) +
    labs(x='Sample Size', y='1-β') +
    scale_color_jcolors(palette = "pal6")
  
}



# ---------- 2. The sold-out ticket Scenario ---------- #
if( TRUE ){
  # --------- Coding legend: --------- #
  # -- Dependent Variable -- #
  # response: 0
  #           5 
  #           10 
  #           Other
  # -- Explanatory Variables -- #
  # cost: 0, 5, 10 (defined as p in Thaler 1985) 
  #      [cost influence the "fair price" (p_star), which is operationalized
  #       with the price asked to a friend]
  # market_value : 5, 10
  # buyer: friend (proxy for a fair price), stranger
  
  
  # Create dataframe scenario 2
  df <- data_short_MA %>% 
    select(contains('B', ignore.case =  F))
  
  col_names <- names(df)
  cost <- rep(c(0, 5, 10), each=2)
  market_value <- rep(c(5, 10), 3)
  col_idx <- seq(1,ncol(df),2)
  df1 <- map_df(seq_along(col_idx), function(i){
    
    data.frame( response = c(df[,col_names[col_idx[i]]], df[,col_names[col_idx[i]+1]]),
                cost = cost[i],
                market_value = market_value[i],
                buyer = c(rep('Friend', n_sbj), rep('Stranger', n_sbj)) )
    
  }) 
  
  data_s2 <- df1 %>% 
    mutate(response=ifelse(response%in%unique(cost), response, 'Other'),
           response = factor(response, levels = c('0', '5', '10', 'Other')))
    
  
  data_s2 %>% 
    group_by(cost, market_value, buyer) %>% 
    count(response, .drop = F) %>% 
    ggplot(aes(response, n, fill=buyer)) +
    geom_bar(stat="identity", position=position_dodge(), color="black") + 
    mytheme() + 
    scale_fill_jcolors(palette = 'pal6') +
    # scale_y_continuous(labels=scales::percent, guide = "prism_offset", breaks = ) +
    # scale_x_discrete(guide = "prism_offset", breaks) +
    # scale_x_continuous(breaks = 1:4) + 
    labs(y=NULL, x=NULL) +
    facet_grid(cost~market_value)
  
  # ---- Fit Model ---- #
  library(rstan)
  library(tidybayes)
  options(mc.cores = 4)
  rstan_options(auto_write = TRUE)
  
  
  multinomial_scenario_2 <- rstan::stan_model('multinomial_scenario_2.stan')
  
  data_s2 <- data_s2 %>% 
    mutate(response=as.numeric(response),
           cost=as.factor(cost),
           market_value=as.factor(market_value))
  
  X <- model.matrix(~ 0 + cost * market_value * buyer, data_s2)
  
  
  #Stan Data
  stan_data <- list(
    N       = nrow(data_s2),
    ncat    = max(data_s2$response),
    Y       = data_s2$response,
    X_5     = X,
    X_10    = X,
    X_Other = X,
    K_5     = ncol(X),
    K_10    = ncol(X),
    K_Other = ncol(X),
    
    prior_only = 0
  )
  
  psoftmax <- function(b) exp(c(0, b)) / sum(exp( c(0, b) ))
  
  # Fit the model
  fit_S2 <- rstan::sampling(multinomial_scenario_2, 
                            iter = 2000, 
                            cores = 4, 
                            data = stan_data, 
                            save_warmup = FALSE) 
  
  extr_fit <- extract(fit_S2)
  
  sfit <- summary(fit_S2)$summary; n_pars <- nrow(sfit)-1 
  B <- as.vector(sfit[1:n_pars,1])
  
  make_designmatrix <- function(cost, market_value, buyer){
    if(cost=='0'){
      cost = c(1, 0, 0)
    } else if( cost=='5' ){
      cost = c(0, 1, 0)
    } else if( cost=='10' ){
      cost = c(0, 0, 1)
    }
    
    market_value10 <- ifelse( market_value=='10', 1, 0 )
    buyerStranger  <- ifelse( buyer=='Stranger', 1, 0 )
    
    
    x <- c(
      cost[1],
      cost[2],
      cost[3],
      market_value10,
      buyerStranger,
      cost[2]*market_value10,
      cost[3]*market_value10,
      cost[2]*buyerStranger,
      cost[3]*buyerStranger,
      market_value10*buyerStranger,
      cost[2]*market_value10*buyerStranger,
      cost[3]*market_value10*buyerStranger
    )
    return(x)
  }
  
  
  cost_i = '0'
  market_value_i = '5'
  buyer_i = 'Friend'
  
  extracted_posterior <- map_dfr(c('0', '5', '10'), function(cost_i){
    cat( '\n\n\nExtracting Posterior for')
    cat(     '\n     - Cost:         ', cost_i)
    map_dfr(c('5', '10'), function(market_value_i){
      cat(     '\n     - Market Value: ', market_value_i)
      map_dfr(c('Friend', 'Stranger'), function(buyer_i){
        cat(     '\n     - Buyer:        ', buyer_i)
        
        b <- matrix(nrow = 4000, ncol = 3)
        # For each response (5, 10, Other) 
        for( i in 1:3 ){
          b[,i] <- extr_fit[[i]]%*% make_designmatrix(cost=cost_i, market_value=market_value_i, buyer=buyer_i)
        }
        
        # For each sample
        map_dfr(1:4000, function(j){ 
          data.frame(post=psoftmax(b[j,]),
                     response=c('0', '5', '10', 'Other'),
                     cost = cost_i,
                     market_value = market_value_i,
                     buyer = buyer_i)
        })
        
      })
    })
  })
  
  extracted_posterior %>% 
    mutate(market_value = factor(market_value, levels = c('5', '10')),
           cost = factor(cost, levels = c('0','5', '10')),
           response = factor(response, levels = c('0', '5', '10', 'Other'))) %>% 
    # filter(buyer=='Friend', market_value=='5', cost=='0') %>% 
    ggplot(aes(x = response, y = post, fill=buyer, color=buyer)) + 
    stat_gradientinterval()+
    geom_line( data = extracted_posterior %>% 
                 group_by(cost, market_value, buyer, response) %>% 
                 summarise(mean_post=mean(post)) ,
               aes(response, mean_post, group=buyer)
               
               
    ) +
    theme_pubr() + scale_y_continuous(limits = 0:1) +
    labs(y = NULL, x = "Posterior Probability") +
    scale_fill_jcolors(palette = "pal6") +
    scale_color_jcolors(palette = "pal6") +
    facet_grid(cost~market_value) 
  
  
  # -------------------------------------------------------------------- #
  # -------------------------- Power Analysis -------------------------- #
  # -------------------------------------------------------------------- #
  nModels <- 4
  nChains <- 2
  nClusters <- nModels*nChains
  
  n_sim <- 200
  n_sbj_range <- c(10, 50, 100, 150, 200, 250, 300) 
  
  
  sim_y <- function(cost, market_value, buyer, linear_effect){
    n_y <- rmultinom(1, n, utilities::softmax(linear_effect))
    y <- rep(1:4, n_y)
    
    stn_data <- list(
      N       = n,
      ncat    = 4,
      Y       = y,
      X_5     = matrix(1, ncol = 1, nrow = n),
      X_10    = matrix(1, ncol = 1, nrow = n),
      X_Other = matrix(1, ncol = 1, nrow = n),
      K_5     = 1,
      K_10    = 1,
      K_Other = 1,
      
      prior_only = 0
    )
    return(stn_data)
  }
  
  samplingfunction <- function(x){
    
    stan_data <- sim_y(cost, market_value, buyer, linear_effect)
    
    fit_S2 <- rstan::sampling(multinomial_scenario_2, 
                              iter = 2000,  cores = 2, chains=2, 
                              data = stan_data, 
                              save_warmup = FALSE) 
    
    
    efit      <- rstan::extract(fit_S2)
    estimate <- sapply(efit[1:3], mean)
    lwr      <- sapply(efit[1:3], quantile, probs=0.025)
    upr      <- sapply(efit[1:3], quantile, probs=0.975)
    
    data.frame(estimate, lwr, upr, 
               cost, market_value, buyer,
               N = n)
  }
  samplingfunction2 <- function(x){
    
    stan_data <- sim_y(cost, market_value, buyer, linear_effect/2)
    
    fit_S2 <- rstan::sampling(multinomial_scenario_2, 
                              iter = 2000,  cores = 2, chains=2, 
                              data = stan_data, 
                              save_warmup = FALSE) 
    
    
    efit      <- rstan::extract(fit_S2)
    estimate <- sapply(efit[1:3], mean)
    lwr      <- sapply(efit[1:3], quantile, probs=0.025)
    upr      <- sapply(efit[1:3], quantile, probs=0.975)
    
    data.frame(estimate, lwr, upr, 
               cost, market_value, buyer,
               N = n)
  }
  samplingfunction3 <- function(x){
    
    stan_data <- sim_y(cost, market_value, buyer, linear_effect/3)
    
    fit_S2 <- rstan::sampling(multinomial_scenario_2, 
                              iter = 2000,  cores = 2, chains=2, 
                              data = stan_data, 
                              save_warmup = FALSE) 
    
    
    efit      <- rstan::extract(fit_S2)
    estimate <- sapply(efit[1:3], mean)
    lwr      <- sapply(efit[1:3], quantile, probs=0.025)
    upr      <- sapply(efit[1:3], quantile, probs=0.975)
    
    data.frame(estimate, lwr, upr, 
               cost, market_value, buyer,
               N = n)
  }
  samplingfunction4 <- function(x){
    
    stan_data <- sim_y(cost, market_value, buyer, linear_effect/4)
    
    fit_S2 <- rstan::sampling(multinomial_scenario_2, 
                              iter = 2000,  cores = 2, chains=2, 
                              data = stan_data, 
                              save_warmup = FALSE) 
    
    
    efit      <- rstan::extract(fit_S2)
    estimate <- sapply(efit[1:3], mean)
    lwr      <- sapply(efit[1:3], quantile, probs=0.025)
    upr      <- sapply(efit[1:3], quantile, probs=0.975)
    
    data.frame(estimate, lwr, upr, 
               cost, market_value, buyer,
               N = n)
  }
  
  
  
  # Cost: 0, 5, 10
  # Market Value: 5, 10
  # Buyer: Friend, Stranger
  
  # 1. Cost: 0, Market Value: 5, Buyer: Friend
  cost <- 0
  market_value <- 5
  buyer <- 'Friend'
  
  p_paper <- c(.68, .26, .03, .03)
  linear_effect <- utilities::softmaxinv(p_paper)
    
  power <- data.frame()
  for (n in n_sbj_range) {
    message("\n\nN Subjects: ", n, "\n")
    
    message("Setting up Clusters...")
    cl <- parallel::makeCluster(nClusters, outfile = "")
    parallel::clusterExport(cl, c("multinomial_scenario_2", "n_sbj_range", "sim_y", 
                                  "cost", "market_value", "buyer",  
                                  "linear_effect", "n"))
    
    out <- map_dfr(seq_len(round(n_sim/nModels)), function(i) {
      message("\r", "Simulation: ", i * nModels, "/", n_sim, appendLF = FALSE)
      parallel::parLapply(cl, 1:nModels, samplingfunction)
    })
    power <- rbind(power, out)
    
    message("\nShutting down Clusters...")
    parallel::stopCluster(cl)
  }
  
  
}



# ---------- 3. Beer on the beach ---------- #
if( TRUE ){
  # --------- Coding legend: --------- #
  # -- Dependent Variable -- #
  # response 
  # -- Explanatory Variables -- #
  # store: Resort hotel vs Grocery store
  
  # Create dataframe scenario 3
  data_s3 <- data_short_MA %>% 
    select(subject, contains('C', ignore.case = FALSE)) %>% 
    melt(id.var='subject',
         variable.name='store',
         value.name='response') %>% 
    mutate(store=ifelse(store=='C1_1', 'Resort Hotel', 'Grocery Store'),
           response=as.numeric(response))
  
  data_s3 %>% 
    ggplot(aes(response, fill=store, color=store)) +
    geom_density(alpha=.4, adjust=1.5) + 
    mytheme() + 
    theme(legend.position = c(0.7, 0.8)) +
    scale_fill_jcolors(palette = 'pal8') +
    scale_color_jcolors(palette = 'pal8') +
    # scale_y_continuous(labels=scales::percent, guide = "prism_offset") +
    scale_x_continuous(breaks = seq(0, 40, 10), limits = c(0, 40) ) +
    labs(y=NULL, x=NULL) 
  
  data_s3 %>% 
    group_by(store) %>% 
    summarise(mean(response))
  
  gamma_scenario_3 <- rstan::stan_model('gamma_scenario_3.stan')
  
  # Create Stan Data
  X <- model.matrix(~ store, data_s3)
  
  #Stan Data
  stan_data <- list(
    N = nrow(data_s3),
    Y = data_s3$response,
    X = X,
    K = ncol(X),
    
    N_sbj    = n_sbj,
    J_sbj    = data_s3$subject,
    
    prior_only = 0
  )
  
  # Fit the model
  fit_S3 <- rstan::sampling(gamma_scenario_3, 
                            iter = 2000, 
                            cores = 4, 
                            data = stan_data, 
                            save_warmup = FALSE)
  
  draws <- extract(fit_S3)
  
  mean( exp(draws$b[,1]) )
  
  draws$b %>%
    melt(value.name = 'post') %>%
    mutate(Var2=ifelse(Var2==1, 'Intercept', '')) %>%
    filter(Var2!='Intercept') %>%
    ggplot(aes(x = post, y = Var2)) +
    stat_halfeye(fill = jcolors::jcolors(palette = "pal8")[6],
                 color = jcolors::jcolors(palette = "pal8")[12]) +
    mytheme() + geom_vline(xintercept = 0, linetype = 2) +
    labs(y = NULL, x = "Posterior Probability", title='Resort Hotel - Grocery Store') 
  
  
  # ---- Power Calculation ---- #
  # Calculate the probability of responding 'different'
  
  inv_link = function(linear_predictor, shape) shape*exp(-linear_predictor)
  
  # Effect found in the paper Thaler 1985
  # n_sbj_paper    = 87
  store   = c('Resort Hotel', 'Grocery Store')
  resort  = 2.65
  grocery = 1.5
  
  linear_effect <- c( log(resort), log(grocery) )
  assumed_sd = 0.5
  
  sim_y <- function(sbj, linear_effect, std){
    # Random Effect
    re <- rnorm(1, 0, std)
    # Population Effect
    shape_gamma <- 25
    rate_gamma  <- inv_link(linear_effect, shape_gamma) 
    rate_gamma  <- rate_gamma + re
    
    data.frame(subject=sbj, store, response = rgamma(2, shape_gamma, rate_gamma))
  }
  
  get_estimate <- function(n_sbj, linear_effect, std){
    
    #Simulate data
    data_sim <- map_dfr(seq_len(n_sbj), sim_y, linear_effect=linear_effect, std=std)
    
    ## Create Stan Data
    X <- model.matrix(~ store, data_sim)
    
    #Stan Data
    stan_data <- list(
      N = nrow(data_sim),
      Y = data_sim$response,
      X = X,
      K = ncol(X),
      
      N_sbj    = n_sbj,
      J_sbj    = data_sim$subject,
      
      prior_only = 0
    )
    
    # Fit the model
    library(parallel)
    library(doParallel)
    library(rstan)
    nParModels <- 4; nChains <- 2
    nClasters <- nParModels*nChains
    cl <- makeCluster(nClasters)
    registerDoParallel(cl)
    
    out <- foreach(i=1:nParModels, .export=c("optimizing")) %dopar% {
      fit <- rstan::sampling(gamma_scenario_3, 
                             iter = 2000, 
                             cores = 2, 
                             chain = 2,
                             refresh = 0,
                             data = stan_data, 
                             save_warmup = FALSE)
    }
    
    
       
    
    efit <- extract(fit)
    
    data.frame(
      estimate = apply(efit$b, 2, mean)[2],
      lwr      = apply(efit$b, 2, quantile, probs=0.025)[2],
      upr      = apply(efit$b, 2, quantile, probs=0.975)[2],
      store    = paste0(store[1], ' - ', store[2])
    )
    
  }
  
  n_sim <- 20
  n_sim_range <- seq(1, n_sim, nParModels)
  actual_n_sim <- round(n_sim/nParModels) 
  n_sbj_range  <- c( 10 )
  
  out <- vector(mode = 'list', length = length(n_sbj_range))
  names(out) <- n_sbj_range
  
  start_time <- Sys.time()
  for( n in seq_along(n_sbj_range) ){
    #Simulate data
    data_sim <- purrr::map_dfr(seq_len(n_sbj_range[n]), sim_y, linear_effect=linear_effect, std=std)
    
    ## Create Stan Data
    X <- model.matrix(~ store, data_sim)
    
    #Stan Data
    stan_data <- list(
      N = nrow(data_sim),
      Y = data_sim$response,
      X = X,
      K = ncol(X),
      
      N_sbj    = n_sbj,
      J_sbj    = data_sim$subject,
      
      prior_only = 0
    )
    
    samplingfunction<-function(x){
      fit <- rstan::sampling(gamma_scenario_3, 
                             iter = 2000, 
                             cores = 2,
                             chains = 2,
                             refresh = 0,
                             data = stan_data, 
                             save_warmup = FALSE)
    }
    
    
    # cl <- makeCluster(nClasters)
    # registerDoParallel(cl)
    stopCluster(cl)
    cl <- makeCluster(nClasters)
    clusterExport(cl,c('gamma_scenario_3','stan_data'))
    result <- parLapply(cl, c(1:4),samplingfunction)
    
    out[[n]] <- append(out[[n]], result)
    }
    end_time <- Sys.time()
    
    end_time - start_time
    
  }
 


# ---------- 4. Jacket-Calculator ---------- #
if( TRUE ){
  # --------- Coding legend: --------- #
  # -- Dependent Variable -- #
  # response 
  # -- Explanatory Variables -- #
  # store: Resort hotel vs Grocery store
  
  # Create dataframe scenario 3
  data_s4 <- data_short_MA %>% 
    select(subject, contains('D', ignore.case = FALSE)) %>% 
    rename(low_price2 = D3, low_price1 = D2_1,
           high_price2 = D4, high_price1 = D2_2) %>% 
    melt(id.var='subject',
         variable.name='price',
         value.name='response') %>% 
    mutate(price=ifelse(str_detect(price, 'low'), 'low', 'high'),
           response=ifelse(response=='No', 0, 1))
  
  data_s4 %>% 
    filter(response==1) %>% 
    group_by(price) %>%
    count(response, .drop = F) %>% 
    ungroup() %>% 
    ggplot(aes(price, n, fill=price)) +
    geom_bar(stat = "identity", 
             # aes(y = (..count..)/sum(..count..)),
             position = position_dodge()) +
    mytheme() + theme(legend.position = 'none') +
    scale_fill_jcolors(palette = 'pal8') +
    scale_color_jcolors(palette = 'pal8') +
    # scale_y_continuous(labels=scales::percent, guide = "prism_offset") +
    labs(y='Number of "Yes"', x='Price') 
  
  
  # ---- Fit Model ---- #
  library(rstan)
  library(tidybayes)
  options(mc.cores = 4)
  rstan_options(auto_write = TRUE)
  
  
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
    
    N_sbj    = max(data_s4$subject),
    J_sbj    = data_s4$subject,
    
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
  
  mean(plogis( draws$b[,1] ))
  
  data_s4 %>% 
    group_by(price) %>% 
    summarise(mean(response))
  
  draws$b %>%
    melt(value.name = 'post') %>% 
    mutate(Var2=ifelse(Var2==1, 'Intercept', '')) %>% 
    filter(Var2!='Intercept') %>%
    ggplot(aes(x = post, y = Var2)) + 
    stat_halfeye(fill = jcolors::jcolors(palette = "pal8")[6], 
                 color = jcolors::jcolors(palette = "pal8")[12]) + 
    mytheme() + geom_vline(xintercept = 0, linetype = 2) + 
    labs(y = NULL, x = "Posterior Probability", title='Low - High')
  
  # ---- Power Calculation ---- #
  # Calculate the probability of responding 'different'
  
  logit = function(p) log(p/(1-p))
  
  # Effect found in the paper Tversky, Kahneman, 1981
  n_sbj_paper = 88
  price       = c('High', 'Low')
  p_effect    = c(0.29, 0.68)
  
  linear_effect <- c( logit(p_effect) )
  assumed_sd = 0.5
  
  sim_y <- function(sbj, linear_effect, std){
    # Random Effect
    re <- rnorm(1, 0, std)
    # Population Effect
    theta  <- plogis(linear_effect+re)
    response = rbinom(2, 1, theta)

    data.frame(subject=sbj, price, response)
  }
  
  get_estimate <- function(n_sbj, linear_effect, std){
    
    #Simulate data
    data_sim <- map_dfr(seq_len(n_sbj), sim_y, linear_effect=linear_effect, std=std)
    
    ## Create Stan Data
    X <- model.matrix(~ price, data_sim)
    
    #Stan Data
    stan_data <- list(
      N = nrow(data_sim),
      Y = data_sim$response,
      X = X,
      K = ncol(X),
      
      N_sbj    = n_sbj,
      J_sbj    = data_sim$subject,
      
      prior_only = 0
    )
    
    # Fit the model
    fit <- rstan::sampling(bernoulli_scenario_4, 
                           iter = 2000, 
                           cores = 4, 
                           refresh = 0,
                           data = stan_data, 
                           save_warmup = FALSE)
    
    
    efit <- extract(fit)
    
    data.frame(
      estimate = apply(efit$b, 2, mean)[2],
      lwr      = apply(efit$b, 2, quantile, probs=0.025)[2],
      upr      = apply(efit$b, 2, quantile, probs=0.975)[2],
      store    = paste0(price[2], ' - ', store[1]),
      effsize  = linear_effect[2]-linear_effect[1]
    )
    
  }
  
  n_sim <- 100
  n_sbj_range <- c(10, 15, 20, 25, 30, 50, 87)
  power <- map_dfr(n_sbj_range, function(n_sbj){
    cat('\n\nN Subjects: ', n_sbj, '\n')
    map_dfr(1:n_sim, function(i){
      cat('Simulation: ', i, '\r')
      get_estimate(n_sbj, linear_effect, assumed_sd) %>%
        mutate(simulation=i, N=n_sbj)
    })
  })
  
  power %>% 
    mutate( is_significant = ifelse(lwr>0, TRUE, FALSE) ) %>% 
    group_by(N) %>% 
    summarise(power = mean(is_significant)) %>% 
    ungroup() %>% 
    ggplot(aes(N, power)) +
    geom_line(size=1) +
    geom_point(size=3) +
    mytheme() +
    scale_y_continuous(limits = c(0,1), labels=scales::percent) +
    scale_x_continuous(breaks = n_sbj_range) +
    labs(x='Sample Size', y='1-β') +
    scale_color_jcolors(palette = "pal6")
  
}



# ---------- 5. Lost ticket ---------- #
if( TRUE ){
  # --------- Coding legend: --------- #
  # -- Dependent Variable -- #
  # response 
  # -- Explanatory Variables -- #
  # loss: ticket vs cash
  
  # Create dataframe scenario 3
  data_s5 <- data_short_MA %>% 
    select(subject, contains('E', ignore.case = FALSE)) %>%
    rename(ticket = E1, 
           cash = E2) %>% 
    melt(id.var='subject',
         variable.name='loss',
         value.name='response') %>% 
    mutate(response=ifelse(response=='No', 0, 1))
  
  data_s5 %>% 
    filter(response==1) %>% 
    group_by(loss) %>%
    count(response, .drop = F) %>% 
    ungroup() %>% 
    ggplot(aes(loss, n, fill=loss)) +
    geom_bar(stat = "identity", 
             # aes(y = (..count..)/sum(..count..)),
             position = position_dodge()) +
    mytheme() + theme(legend.position = 'none') +
    scale_fill_jcolors(palette = 'pal8') +
    scale_color_jcolors(palette = 'pal8') +
    # scale_y_continuous(labels=scales::percent, guide = "prism_offset") +
    labs(y='Number of "Yes"', x='Price') 
  
  
  # ---- Fit Model ---- #
  library(rstan)
  library(tidybayes)
  options(mc.cores = 4)
  rstan_options(auto_write = TRUE)
  
  
  # Create Stan Data
  X <- model.matrix(~ loss, data_s5)
  
  #Stan Data
  stan_data <- list(
    N = nrow(data_s5),
    Y = data_s5$response,
    X = X,
    K = ncol(X),
    
    prior_only = 0
  )
  bernoulli_scenario_4 <- rstan::stan_model(file = 'bernoulli_scenario_4.stan')
  
  # Fit the model
  fit_S5 <- rstan::sampling(bernoulli_scenario_4, 
                            iter = 2000, 
                            cores = 4, 
                            save_warmup = FALSE,
                            data = stan_data, 
                            save_warmup = FALSE)
  
  extract(fit_S5)$b %>%
    melt(value.name = 'post') %>% 
    mutate(Var2=ifelse(Var2==1, 'Intercept', '')) %>% 
    filter(Var2!='Intercept') %>%
    ggplot(aes(x = post, y = Var2)) + 
    stat_halfeye(fill = jcolors::jcolors(palette = "pal8")[6], 
                 color = jcolors::jcolors(palette = "pal8")[12]) + 
    mytheme() + geom_vline(xintercept = 0, linetype = 2) + 
    labs(y = NULL, x = "Posterior Probability", title='Ticket - Cash')
  
}



# ---------- 6. Membership gym ---------- #
if( TRUE ){
  # --------- Coding legend: --------- #
  # -- Dependent Variable -- #
  # response 
  # -- Explanatory Variables -- #
  # loss: ticket vs cash
  
  answers = c(
    'I feel like I wasted $20',
    'I feel like I wasted something but no specific amount or measure comes to mind',
    'I feel like I wasted nothing, since my visit had already been paid for'
  )
  # Create dataframe scenario 3
  data_s6 <- data_short_MA %>% 
    select(subject, contains('F', ignore.case = FALSE)) %>%
    mutate(
      F1 = case_when( F1==answers[1]~1, F1==answers[2]~0, F1==answers[3]~0),
      F2 = case_when( F2==answers[1]~1, F2==answers[2]~0, F2==answers[3]~0)
      ) %>% 
    rename(`Per-session` = F1, 
           Yearly = F2) %>% 
    melt(id.var='subject',
         variable.name='frame',
         value.name='response') %>% 
    mutate(frame=factor(frame, levels = c('Per-session','Yearly') ))
  
  data_s6 %>% 
    filter(response==1) %>% 
    group_by(frame) %>%
    count(response, .drop = F) %>% 
    ungroup() %>% 
    ggplot(aes(frame, n, fill=frame)) +
    geom_bar(stat = "identity", 
             position = position_dodge()) +
    mytheme() + theme(legend.position = 'none') +
    scale_fill_jcolors(palette = 'pal8') +
    scale_color_jcolors(palette = 'pal8') +
    # scale_y_continuous(labels=scales::percent, guide = "prism_offset") +
    labs(y='Number of "Wasted $20"', x='Frame')
  
  data_s6 %>% 
    group_by(frame) %>% 
    summarise(mean=mean(response))
  
  # ---- Fit Model ---- #
  library(rstan)
  library(tidybayes)
  options(mc.cores = 4)
  rstan_options(auto_write = TRUE)
  
  # Create Stan Data
  X <- model.matrix(~ frame, data_s6 %>% mutate(frame=ifelse(frame=='Per-session', 1, 0 )))
  
  #Stan Data
  stan_data <- list(
    N = nrow(data_s6),
    Y = data_s6$response,
    X = X,
    K = ncol(X),
    
    N_sbj    = n_sbj,
    J_sbj    = data_s6$subject,
    
    prior_only = 0
  )
  
  # Fit the model
  bernoulli_scenario_6 <- rstan::stan_model('bernoulli_scenario_6.stan')
  fit_S6 <- rstan::sampling(bernoulli_scenario_6, 
                            iter = 2000, 
                            cores = 4, 
                            pars    = c('b'),
                            save_warmup = FALSE,
                            data = stan_data, 
                            save_warmup = FALSE)
  
  
  extract(fit_S6)$b %>%
    as.data.frame() %>% 
    mutate(post=V2,
           type = '') %>% 
    ggplot(aes(x = post, y = type)) + 
    stat_halfeye(fill = jcolors::jcolors(palette = "pal8")[6], 
                 color = jcolors::jcolors(palette = "pal8")[12]) + 
    mytheme() + geom_vline(xintercept = 0, linetype = 2) + 
    labs(y = NULL, x = "Posterior Probability", title='Per-session - Yearly')
  
}



# ---------- 6. Airplanes coupons scenario ---------- #
if( TRUE ){
  # --------- Coding legend: --------- #
  # -- Dependent Variable -- #
  # response 
  # -- Explanatory Variables -- #
  # coupon: purchased vs free
  
  answers = c(
    'Pay your friend $35 for the coupon.',
    'Pay some, but not the full amount for the coupon (for example, half the price).',
    'Consider it a gift and not pay for the coupon.'
  )
  # Create dataframe scenario 3
  data_s7 <- data_short_MA %>% 
    select(subject, contains('G', ignore.case = FALSE)) %>%
    mutate(
      G1 = case_when( G1==answers[1]~1, G1==answers[2]~0, G1==answers[3]~0),
      G2 = case_when( G2==answers[1]~1, G2==answers[2]~0, G2==answers[3]~0)
    ) %>% 
    rename(purchased = G1, 
           free = G2) %>% 
    melt(id.var='subject',
         variable.name='coupon',
         value.name='response')
  
  data_s7 %>% 
    filter(response==1) %>% 
    group_by(coupon) %>%
    count(response, .drop = F) %>% 
    ungroup() %>% 
    ggplot(aes(coupon, n, fill=coupon)) +
    geom_bar(stat = "identity", 
             position = position_dodge()) +
    mytheme() + theme(legend.position = 'none') +
    scale_fill_jcolors(palette = 'pal8') +
    scale_color_jcolors(palette = 'pal8') +
    # scale_y_continuous(labels=scales::percent, guide = "prism_offset") +
    labs(y='Pay your friend $35 for the coupon', x='')
  
  
  # ---- Fit Model ---- #
  library(rstan)
  library(tidybayes)
  options(mc.cores = 4)
  rstan_options(auto_write = TRUE)
  
  # Create Stan Data
  X <- model.matrix(~ coupon, data_s7)
  
  #Stan Data
  stan_data <- list(
    N = nrow(data_s7),
    Y = data_s7$response,
    X = X,
    K = ncol(X),
    
    N_sbj    = n_sbj,
    J_sbj    = data_s7$subject,
    
    prior_only = 0
  )
  
  # Fit the model
  bernoulli_scenario_6 <- rstan::stan_model('bernoulli_scenario_6.stan')
  fit_S7 <- rstan::sampling(bernoulli_scenario_7, 
                            iter = 2000, 
                            cores = 4, 
                            pars    = c('b'),
                            save_warmup = FALSE,
                            data = stan_data, 
                            save_warmup = FALSE)
  
  
  extract(fit_S7)$b %>%
    as.data.frame() %>% 
    mutate(post=V2,
           type = '') %>% 
    ggplot(aes(x = post, y = type)) + 
    stat_halfeye(fill = jcolors::jcolors(palette = "pal8")[6], 
                 color = jcolors::jcolors(palette = "pal8")[12]) + 
    mytheme() + geom_vline(xintercept = 0, linetype = 2) + 
    labs(y = NULL, x = "Posterior Probability", title='Purchased - Free')
  
}
