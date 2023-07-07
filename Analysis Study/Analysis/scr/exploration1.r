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
lazyLoad("index_cache/html/Code-to-load-data-and-libraries_ac2dcc77804d4e19a61ad58c57f9e0c7")


# Chunk containing brms model
lazyLoad("index_cache/html/fit-MrAB-unpooled-fin_lit-preregistered-exclusion_3a994d384e989739662a094dba97f9e9")
lazyLoad("index_cache/html/fit-MrAB1-unpooled-exploratory-preregistered-exclusion_23ae1410c20cf24a11d0309a609b8e39")

predictor_of_interest <- "GenderMale"
post <- prepare_predictions(mMrAB1)$dpars$mu$fe$b %>% 
  as.data.frame() %>% 
  select(contains("condition") & contains(":") & contains(predictor_of_interest))

names(post) <- c("Austria", sapply(2:length(post), function(i) strsplit( names(post)[i], "Country")[[1]][2]))
all_countries <- names(post)
post_plot1 <- map_dfr(all_countries, function(country){
  if(country=="Austria"){
    theta <- post[,"Austria"]
  } else {
    theta <- post[,"Austria"] + post[,country]
  }
  data.frame(theta, Country=country, study="MrAB", family="binomial", x=1.2) %>% 
    mutate(lower = HDInterval::hdi( theta )["lower"],
           upper = HDInterval::hdi( theta )["upper"],
           credible = ifelse(lower<0, "no", "higher"),
           credible = ifelse(upper>0, credible, "lower"))
})

postMrAB1 <- post_plot1 %>% 
  group_by(Country) %>% 
  mutate(theta=mean(theta)) %>% 
  filter(row_number()==1)

post_plot1 %>% 
  ggplot(aes(x = theta, y = Country)) +
  stat_halfeye(aes(fill=credible, color=credible)) +
  geom_vline(xintercept = 0, linetype=2) +
  theme_pubr() + 
  labs(x=expression(log(OR)), y=NULL) +
  scale_y_discrete(guide = "prism_offset") + 
  scale_x_continuous(guide = "prism_offset") + 
  scale_fill_manual(values = c("#228B8DFF", "#D5006A", "gray"), breaks = c("higher", "lower","no")) +
  scale_color_manual(values = c("#33628DFF", "#560f56", "gray"), breaks = c("higher", "lower","no")) +
  guides(size = "none") + 
  theme(text = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        legend.position = "none")


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
  mutate(condition=factor(condition, levels = c("gain-loss VS gain", "gain-gain VS gain") )) %>% 
  group_by(subject) %>% filter(row_number()==1) %>% ungroup()


library(lme4)
mMrAB1 <- glmer(response ~ condition*(Age+Education+FinancialLiteracy+Gender+numeric_income) + (condition+FinancialLiteracy+Age+Education+FinancialLiteracy+Gender+numeric_income | Country),
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
                family=binomial())

summary(mMrAB1)

# --------- Fit Exploratory ------------- #
lazyLoad("index_cache/html/fit-exploratory-MrAB1_42ce1c34eb34f7ae6bfd5c14b3c59062")
lazyLoad("index_cache/html/fit-exploratory-MrAB2_bcad070ed4ced86a27180d8bf0b81443")
lazyLoad("index_cache/html/fit-exploratory-Game_bc35536c9e4cdd60d9ba27bd9431c1b3")
lazyLoad("index_cache/html/fit-exploratory-Drink_cd4cfd16f5ee20061da6082805aa1027")
lazyLoad("index_cache/html/fit-exploratory-Jacket_a3db8e695fd35ca1b62b16e2a8080354")
lazyLoad("index_cache/html/fit-exploratory-Play_bf57d38fb040e05d0551d18e9039eaf5")
lazyLoad("index_cache/html/fit-exploratory-Gym_3b0b26e020bc0cc530c51a585df3e403")
lazyLoad("index_cache/html/fit-exploratory-Plane_2f20bf143b0f934dd0469fab1a08198c")

mMrAB1 <- readRDS("index_cache/brms_exploratory/mMrAB1.rds")
mMrAB2 <- readRDS("index_cache/brms_exploratory/mMrAB2.rds")
mGame <- readRDS("index_cache/brms_exploratory/mGame.rds")
mDrink <- readRDS("index_cache/brms_exploratory/mDrink.rds")
mJacket <- readRDS("index_cache/brms_exploratory/mJacket.rds")
mPlay <- readRDS("index_cache/brms_exploratory/mPlay.rds")
mGym <- readRDS("index_cache/brms_exploratory/mGym.rds")
mPlane <- readRDS("index_cache/brms_exploratory/mPlane.rds")

summary(mMrAB1)
summary(mMrAB2)
summary(mGame)
summary(mJacket)
summary(mPlay)
summary(mPlane)
summary(mDrink)
summary(mGym)

library(stringi)


m <- mMrAB1

df_plot <- function(m, x, study_name){
  fe <- fixef(m) %>% as.data.frame()
  fe <- data.frame(beta=fe[,1], lower=fe[,3], upper=fe[,4], var=row.names(fe))
  
  fe %>% 
    filter(stri_detect_fixed(var, ":")) %>% 
    mutate(var=sapply(str_split(var, ":"), function(x) x[2])) %>% 
    mutate(
      x=seq(x-0.2, x+0.2, length=5), 
      study=study_name,
      var=ifelse(var=="FinancialLiteracy", "Financial\nLiteracy",var),
      var=ifelse(var=="GenderMale", "Gender",var),
      var=ifelse(var=="numeric_income", "Income",var)
    ) %>% 
    mutate(is_sign=ifelse(sign(lower)==sign(upper),"yes", "no"))
    
}


df <- rbind(
  df_plot(mMrAB1, 1.2, "MrAB1"),
  df_plot(mMrAB2, 1.8, "MrAB2"),
  df_plot(mGame, 3, "Game"),
  df_plot(mJacket, 4, "Jacket"),
  df_plot(mPlay, 5, "Play"),
  df_plot(mPlane, 6, "Plane"),
  df_plot(mDrink, 7, "Drink"),
  df_plot(mGym, 8, "Gym")
) 


df %>% 
  mutate(is_sign=factor(is_sign, levels=c("yes", "no"))) %>% 
  mutate(study=ifelse( (study=="MrAB1" | study=="MrAB2"), "MrAB", study)) %>% 
  # Age
  mutate(beta=ifelse(var=="Age", beta*sd(data$Age), beta)) %>% 
  mutate(lower=ifelse(var=="Age", lower*sd(data$Age), lower)) %>% 
  mutate(upper=ifelse(var=="Age", upper*sd(data$Age), upper)) %>% 
  # Education
  mutate(beta=ifelse(var=="Education", beta*sd(data$Education), beta)) %>% 
  mutate(lower=ifelse(var=="Education", lower*sd(data$Education), lower)) %>% 
  mutate(upper=ifelse(var=="Education", upper*sd(data$Education), upper)) %>% 
  # Financial Literacy
  mutate(beta=ifelse(var=="Financial\nLiteracy", beta*sd(data$FinancialLiteracy), beta)) %>% 
  mutate(lower=ifelse(var=="Financial\nLiteracy", lower*sd(data$FinancialLiteracy), lower)) %>% 
  mutate(upper=ifelse(var=="Financial\nLiteracy", upper*sd(data$FinancialLiteracy), upper)) %>% 
  # Income
  mutate(beta=ifelse(var=="Income", beta*sd(data$numeric_income, na.rm=T), beta)) %>% 
  mutate(lower=ifelse(var=="Income", lower*sd(data$numeric_income, na.rm=T), lower)) %>% 
  mutate(upper=ifelse(var=="Income", upper*sd(data$numeric_income, na.rm=T), upper)) %>% 
  
  mutate(x = x+rep(c(0.2, 0.1, 0, -0.1, -0.2), 8)) %>% 
  
  mutate(study=factor(study, labels = c("MrAB", "Game", "Jacket", "Play", "Plane", "Drink", "Gym"), 
                      levels = c("MrAB", "Game", "Jacket", "Play", "Plane", "Drink", "Gym"), 
                      )) %>% 
  ggplot(aes(x, beta, fill=var, color=study, linetype=is_sign)) +
  geom_hline(yintercept = 0, linewidth=0.2) +
  geom_segment(aes(x = x, xend=x, y=lower, yend=upper)) +
  geom_point(stat="identity", size=3) +
  theme_cowplot() +
  scale_x_continuous(breaks = c(1.5, 3:8), 
                     labels = c("MrAB", "Game", "Jacket", "Play", 
                                "Plane", "Drink", "Gym"),
                     guide = "prism_offset") +
  scale_y_continuous(guide = "prism_offset") + 
  labs(fill=NULL, x=NULL, y=expression(Standardized~beta), color="Study") +
  theme(legend.position = c(0.85, 0.25), 
        text = element_text(size = 15, family="Arial"), 
        # strip.background = element_rect(fill="#E2E2E2"),
        strip.background = element_rect(fill="#F9F9F9"),
        # axis.ticks.x = element_blank(), 
        axis.text.x = element_blank()) +
  guides(fill="none", alpha="none", shape="none", linetype="none") +
  facet_wrap(~var) +
  scale_color_manual(values = colorspace::qualitative_hcl(7, "Dark 3")[c(5, 2, 4, 7, 6, 1, 3)])

ggsave("./index_files/figure-html/exploratory.svg", device = "svg", width = 8, height = 4.5)

ggsave("test/exploratory.svg", device = "svg", width = 8, height = 4.5)
ggsave("test/exploratory.png", device = "png", width = 8, height = 4.5)

