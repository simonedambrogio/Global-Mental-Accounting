# Load libraries
library(dplyr); library(purrr); library(stringr)
library(ggplot2); library(ggpubr); library(jcolors)
library(cowplot); library(ggprism); library(tidybayes)
library(reshape2); library(parallel); library(rstan)
library(brms)
library(plotly)

# Run first chunk
lazyLoad("index_cache/html/Code-to-load-data-and-libraries_fdfddf6fbc1f4f007cbfc5779e882289")


# Chunk containing brm model
lazyLoad("index_cache/html/fit-MrAB-unpooled-fin_lit-preregistered-exclusion_3a994d384e989739662a094dba97f9e9")

predictor_of_interest <- "FinancialLiteracy"
post <- prepare_predictions(mMrAB1)$dpars$mu$fe$b %>% 
  as.data.frame() %>% 
  select(contains("scenario") & contains(":") & contains(predictor_of_interest))


mMrAB1 <- glm(response ~ (scenario)*FinancialLiteracy * Country,
              data = data_MrAB %>% filter(response!=2) %>%
                filter(scenario_group=="gain") %>% 
                # EXCLUSION: Full Exclusion
                filter( !(Country %in% countries2remove) ) %>% 
                filter( attention_check_grater_than_3 ) %>% 
                mutate(scenario=factor(scenario, levels = c("gain-loss VS gain", "gain-gain VS gain") )),
              family=binomial())

m <- coef(mMrAB1) %>% 
  matrix(nrow = 1)
colnames(m) <- names(coef(mMrAB1))  

post <- m %>% as.data.frame() %>%
  select(contains("scenario") & contains(":") & contains(predictor_of_interest))


mMrAB1 <- glm(response ~ (Age + scenario)*FinancialLiteracy * Country,
              data = data_MrAB %>% filter(response!=2) %>%
                filter(scenario_group=="gain") %>% 
                # EXCLUSION: Full Exclusion
                filter( !(Country %in% countries2remove) ) %>% 
                filter( attention_check_grater_than_3 ) %>% 
                mutate(scenario=factor(scenario, levels = c("gain-loss VS gain", "gain-gain VS gain") )),
              family=binomial())


m <- coef(mMrAB1) %>% 
  matrix(nrow = 1)
colnames(m) <- names(coef(mMrAB1))  

post <- m %>% as.data.frame() %>%
  select(contains("scenario") & contains(":") & contains(predictor_of_interest))

library(ggbeeswarm)
nCountries <- 21
data_MrAB %>% filter(response!=2) %>%
  filter(scenario_group=="gain") %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  # filter(Country=='Austria') %>% 
  group_by(Country, subject) %>% 
  filter(row_number()==1) %>% 
  select(Age) %>% 
  ungroup() %>% 
  mutate(Age=as.numeric(Age)) %>%
  filter(Age>0 & Age<99) %>% 
  ggplot(aes(Age, Country, color=Country)) +
  geom_boxplot() +
  theme_pubr() + 
  guides(size = "none") + 
  theme(text = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        legend.position = "none") +
  scale_color_manual(values = viridis::viridis_pal()(nCountries))

  
data_MrAB %>% filter(response!=2) %>%
  filter(scenario_group=="gain") %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  # filter(Country=='Austria') %>% 
  group_by(Country, subject) %>% 
  filter(row_number()==1) %>% 
  # select(Gender) %>% 
  ungroup() %>% 
  mutate(Age=as.numeric(Age)) %>%
  filter(Age>0 & Age<99) %>% 
  filter(Gender!="Prefer not to say")


# ---- Plot Gender ---- #
data_MrAB %>% filter(response!=2) %>%
  filter(scenario_group=="gain") %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  # filter(Country=='Austria') %>% 
  group_by(Country, subject) %>% 
  filter(row_number()==1) %>% 
  # select(Gender) %>% 
  ungroup() %>% 
  mutate(Age=as.numeric(Age)) %>%
  filter(Age>0 & Age<99) %>% 
  filter(Gender!="Prefer not to say") %>% 
  group_by(Country) %>% 
  summarise(pMale = mean(ifelse(Gender=='Male', 1, 0))) %>% 
  ggplot(aes(pMale, Country, fill=Country)) +
  geom_bar(stat = "identity") +
  theme_pubr() + 
  guides(size = "none") + 
  theme(text = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none") +
  scale_fill_manual(values = viridis::viridis_pal()(nCountries)) +
  labs(y="Proportion of Male")


# ---- Plot Income ---- #
data_MrAB %>% filter(response!=2) %>%
  filter(scenario_group=="gain") %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  # filter(Country=='Austria') %>% 
  group_by(Country, subject) %>% 
  filter(row_number()==1) %>% 
  # select(Gender) %>% 
  ungroup() %>% 
  mutate(Age=as.numeric(Age)) %>%
  filter(Age>0 & Age<99) %>% 
  filter(Gender!="Prefer not to say") %>% 
  group_by(Country) %>% 
  summarise(pMale = mean(ifelse(Gender=='Male', 1, 0))) %>% 
  ggplot(aes(pMale, Country, fill=Country)) +
  geom_bar(stat = "identity") +
  theme_pubr() + 
  guides(size = "none") + 
  theme(text = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none") +
  scale_fill_manual(values = viridis::viridis_pal()(nCountries)) +
  labs(y="Proportion of Male")


