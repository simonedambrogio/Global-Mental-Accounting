# Load libraries
library(dplyr); library(purrr); library(stringr)
library(ggplot2); library(ggpubr); library(jcolors)
library(cowplot); library(ggprism); library(tidybayes)
library(reshape2); library(parallel); library(rstan)
library(brms)
library(plotly)

color_21countries <- c(
  "#440154FF", "#481467FF", "#482576FF", "#463480FF", 
  "#414487FF", "#3B528BFF", "#35608DFF", "#2F6C8EFF",
  "#2A788EFF", "#25848EFF", "#21908CFF", "#1E9C89FF",
  "#22A884FF", "#2FB47CFF", "#43BF71FF", "#5DC863FF",
  "#7AD151FF", "#9AD93CFF", "#BBDF27FF", "#DEE318FF",
  "#FDE725FF"
)

# Run first chunk
lazyLoad("index_cache/html/Code-to-load-data-and-libraries_fdfddf6fbc1f4f007cbfc5779e882289")


# Chunk containing brm model
lazyLoad("index_cache/html/fit-MrAB-unpooled-fin_lit-preregistered-exclusion_3a994d384e989739662a094dba97f9e9")

predictor_of_interest <- "FinancialLiteracy"
post <- prepare_predictions(mMrAB1)$dpars$mu$fe$b %>% 
  as.data.frame() %>% 
  select(contains("scenario") & contains(":") & contains(predictor_of_interest))


mMrAB1 <- glm(response ~ condition * (Age+Education+FinancialLiteracy+Gender+numeric_income) * Country,
              data = MrAB %>% 
                # Tidy
                mutate(Age=as.numeric(Age)) %>%
                filter(Age>0 & Age<99) %>% 
                mutate(Education=as.numeric(Education)) %>% 
                filter(Education<100) %>% 
                mutate(condition_group=ifelse(
                  condition %in% c("gain-gain VS gain", "gain-loss VS gain"),
                  "gain",
                  "loss"
                )) %>% 
                filter(condition_group=="gain") %>% 
                mutate(condition=factor(condition, levels = c("gain-loss VS gain", "gain-gain VS gain") )),
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


MrAB <- read.csv("../Data/Preregistered/MrAB.csv")
Game <- read.csv("../Data/Preregistered/Game.csv")
Drink <- read.csv("../Data/Preregistered/Drink.csv")
Jacket <- read.csv("../Data/Preregistered/Jacket.csv")
Play <- read.csv("../Data/Preregistered/Play.csv")
Gym <- read.csv("../Data/Preregistered/Gym.csv")
Plane <- read.csv("../Data/Preregistered/Plane.csv")
nCountries <- length(unique(MrAB$Country))

# ---- Plot Age ---- #
Jacket %>% 
  filter(condition==first(Jacket$condition)) %>% 
  mutate(Age=as.numeric(Age)) %>%
  filter(Age>18 & Age<99) %>% 
  ggplot(aes(Age, Country, color=Country)) +
  geom_boxplot() +
  theme_pubr() + 
  guides(size = "none") + 
  theme(text = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none") +
  scale_color_manual(values = viridis::viridis_pal()(nCountries))

# ---- Plot Education ---- #
Jacket %>% 
  # Tidy
  filter(condition==first(Jacket$condition)) %>% 
  mutate(Age=as.numeric(Age)) %>%
  filter(Age>0 & Age<99) %>% 
  mutate(Education=as.numeric(Education)) %>% 
  filter(Education<100) %>% 
  # Plot
  ggplot(aes(Education, Country, color=Country)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(alpha=0.1, width = 0.3, height = 0.2) +
  scale_color_manual(values = color_21countries) +
  theme_pubr() +
  guides(size = "none") + 
  theme(text = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none") +
  labs(y=NULL) 
  

# ---- Plot Gender ---- #
Jacket %>% 
  # Tidy
  filter(condition==first(Jacket$condition)) %>% 
  mutate(Age=as.numeric(Age)) %>%
  filter(Age>0 & Age<99) %>% 
  mutate(Education=as.numeric(Education)) %>% 
  filter(Education<100) %>% 
  filter(Gender!="Prefer not to say") %>% 
  group_by(Country) %>% 
  summarise(pMale = mean(ifelse(Gender=='Male', 1, 0))) %>% 
  # Plot
  ggplot(aes(pMale, Country, fill=Country)) +
  geom_bar(stat = "identity", width=0.8) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = color_21countries) +
  theme_pubr() +
  guides(size = "none") + 
  theme(text = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none") +
  labs(x="Proportion of Male", y=NULL)


# ---- Plot Income ---- #
Jacket %>% 
  # Tidy
  filter(condition==first(Jacket$condition)) %>% 
  mutate(Age=as.numeric(Age)) %>%
  filter(Age>0 & Age<99) %>% 
  mutate(Education=as.numeric(Education)) %>% 
  filter(Education<100) %>% 
  # Plot
  ggplot(aes(numeric_income, Country, color=Country)) +
  # geom_boxplot(width = 0.5) +
  geom_jitter(alpha=0.1, width = 0.2, height = 0.2) +
  scale_color_manual(values = color_21countries) +
  theme_pubr() +
  guides(size = "none") + 
  theme(text = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none") +
  labs(y=NULL, x="Income") +
  scale_x_continuous(breaks = 0:5)

