setwd("/Volumes/PROJECTS/Ongoing/MMAR/MMAR GitHub/Study/Analysis")

# Load libraries
library(dplyr); library(purrr); library(stringr)
library(ggplot2); library(ggpubr); library(jcolors)
library(cowplot); library(ggprism); library(tidybayes)
library(reshape2); library(parallel); library(rstan)
library(brms); library(plotly); library(knitr);
library(kableExtra); library(tidyverse)

lazyLoad("index_cache/html/Code-to-load-data-and-libraries_ac2dcc77804d4e19a61ad58c57f9e0c7")


mMrAB1 <- brm(response ~ condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) +
                (condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) | Country),
              data = MrAB %>% 
                # Tidy
                mutate(Age=as.numeric(Age)) %>%
                filter(Age>0 & Age<99) %>% 
                mutate(Education=as.numeric(Education)) %>% 
                filter(Education<100) %>% 
                filter(Gender%in%c("Male", "Female")) %>% 
                mutate(condition_group=ifelse(
                  condition %in% c("gain-gain VS gain", "gain-loss VS gain"),
                  "gain",
                  "loss"
                )) %>% 
                filter(condition_group=="gain") %>% 
                mutate(condition=factor(condition, levels = c("gain-loss VS gain", "gain-gain VS gain") )), 
              iter = 20000, refresh = 1, cores = 2, chains = 2,
              family="bernoulli")

saveRDS(mMrAB1, file = "index_cache/brms_exploratory/mMrAB1.rds")



mMrAB2 <- brm(response ~ condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) + (condition+FinancialLiteracy+Age+Education+FinancialLiteracy+Gender+numeric_income | Country),
              data = MrAB %>% 
                # Tidy
                mutate(Age=as.numeric(Age)) %>%
                filter(Age>0 & Age<99) %>% 
                mutate(Education=as.numeric(Education)) %>% 
                filter(Education<100) %>% 
                filter(Gender%in%c("Male", "Female")) %>% 
                mutate(condition_group=ifelse(
                  condition %in% c("gain-gain VS gain", "gain-loss VS gain"),
                  "gain",
                  "loss"
                )) %>% 
                filter(condition_group=="loss") %>% 
                mutate(condition=factor(condition, levels = c("loss-gain VS loss", "loss-loss VS loss") )), 
              
              iter = 20000, refresh = 1, cores = 2, chains = 2,
              family="bernoulli")

saveRDS(mMrAB2, file = "index_cache/brms_exploratory/mMrAB2.rds")

mGame <- brm(response ~ condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) + 
               (condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) | Country),
               data = Game %>% 
                 # Tidy
                 mutate(Age=as.numeric(Age)) %>%
                 filter(Age>0 & Age<99) %>% 
                 mutate(Education=as.numeric(Education)) %>% 
                 filter(Education<100) %>% 
                 filter(Gender%in%c("Male", "Female")), 
             
             iter = 20000, refresh = 1, chains = 2, cores = 2,
             family="bernoulli")

saveRDS(mGame, file = "index_cache/brms_exploratory/mGame.rds")

mDrink <- brm(response ~ condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) + 
                (condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) | Country),
                data = Drink %>% 
                  # Tidy
                  mutate(Age=as.numeric(Age)) %>%
                  filter(Age>0 & Age<99) %>% 
                  mutate(Education=as.numeric(Education)) %>% 
                  filter(Education<100) %>% 
                  filter(Gender%in%c("Male", "Female")),
              iter = 20000, refresh = 1, chains = 2, cores = 2)
saveRDS(mDrink, file = "index_cache/brms_exploratory/mDrink.rds")


mJacket <- brm(response ~ condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) + 
                 (condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) | Country),
                 data = Jacket %>% 
                   # Tidy
                   mutate(Age=as.numeric(Age)) %>%
                   filter(Age>0 & Age<99) %>% 
                   mutate(Education=as.numeric(Education)) %>% 
                   filter(Education<100) %>% 
                   filter(Gender%in%c("Male", "Female")), 
               
                 iter = 20000, refresh = 1, chains = 2, cores = 2,
                 family="bernoulli")

saveRDS(mJacket, file = "index_cache/brms_exploratory/mJacket.rds")

mPlay <- brm(response ~ condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) + 
               (condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) | Country),
               data = Play %>% 
                 # Tidy
                 mutate(Age=as.numeric(Age)) %>%
                 filter(Age>0 & Age<99) %>% 
                 mutate(Education=as.numeric(Education)) %>% 
                 filter(Education<100) %>% 
                 filter(Gender%in%c("Male", "Female")) %>% 
                 mutate(condition=factor(condition, levels = c("ticket", "cash"))), 
               
               iter = 20000, refresh = 1, chains = 2, cores = 2,
               family="bernoulli")
saveRDS(mPlay, file = "index_cache/brms_exploratory/mPlay.rds")


mGym <- brm(response ~ condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) + 
              (condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) | Country),
            
              data = Gym %>% 
                # Tidy
                mutate(Age=as.numeric(Age)) %>%
                filter(Age>0 & Age<99) %>% 
                mutate(Education=as.numeric(Education)) %>% 
                filter(Education<100) %>% 
                filter(Gender%in%c("Male", "Female")),
            iter = 20000, refresh = 1, chains = 2, cores = 2)
saveRDS(mGym, file = "index_cache/brms_exploratory/mGym.rds")

mPlane <- brm(response ~ condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) + 
                (condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) | Country),
              
                data = Plane %>% 
                  # Tidy
                  mutate(Age=as.numeric(Age)) %>%
                  filter(Age>0 & Age<99) %>% 
                  mutate(Education=as.numeric(Education)) %>% 
                  filter(Education<100) %>% 
                  filter(Gender%in%c("Male", "Female")), 
                
                iter = 20000, refresh = 1, chains = 2, cores = 2,
                 family="bernoulli")

saveRDS(mPlane, file = "index_cache/brms_exploratory/mPlane.rds")
