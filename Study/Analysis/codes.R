
# --- Hierarchical Bayesian Meta-Analysis  --- #
library(knitr)
library(kableExtra)
library(tidyverse)

lazyLoad('index_cache/html/Code-to-load-data-and-libraries_d4033ba30588e0897d19923a4badd205')

# Participants from 21 countries
df <- data_Plane %>% 
  filter(coupon=='free') %>% 
  # EXCLUSION: Preregistered
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 )


df_egypt <- df %>% filter(coupon=="free") %>% filter(Country=='Egypt')
convert_farsi_to_arabic <- function(x) {
  if(x=="٠"|x=="۰"){
    return( "0" )
  } else if (x=="۱" | x=="١"){
    return( "1" )
  } else if (x=="۲" | x=="٢"){
    return( "2" )
  } else if (x=="۳"|x=="٣"){
    return( "3" )
  } else if (x=="۴" | x=="٤"){
    return( "4" )
  } else if (x=="۵" | x=="٥"){
    return( "5" )
  } else if (x=="۶"|x=="٦"){
    return( "6" )
  } else if (x=="۷" | x=="٧"){
    return( "7" )
  } else if (x=="۸" | x=="٨"){
    return( "8" )
  } else if (x=="۹" | x=="٩"){
    return( "9" )
  } else if( any(x %in% as.character(0:9) ) ){
    return( x )
  } else {
    warning( paste("Persian number", x, "not found!!!") )
  }
}

convert <- function(x){
  apply(
    str_split(string = x, pattern = "", simplify = T),
    2,
    convert_farsi_to_arabic
  ) %>% paste0(collapse = "")
}

df[df$Country=='Egypt', "Age"] <-  sapply(as.list(df_egypt$Age), convert)

rbind(
  df %>% 
    mutate(Age=as.numeric(Age)) %>% 
    filter(Age>0 & Age<99) %>% 
    summarise(
      Country="pooled",
      Language=names(which.max(table(NativeLanguage))),
      n = n(),
      `% female` = round(mean(Gender=='Female')*100,2),
      `Age, median (IQR) (yr)` = str_c(median(Age), " (", quantile(Age,probs = .25), "-", quantile(Age,probs = .75), ")")
    ),
  df %>% 
    group_by(Country) %>% 
    mutate(Age=round(as.numeric(Age)),0) %>% 
    filter(Age>0 & Age<99) %>% 
    summarise(
      Country=names(which.max(table(Country))),
      Language=names(which.max(table(NativeLanguage))),
      n = n(),
      `% female` = round(mean(Gender=='Female')*100,2),
      `Age, median (IQR) (yr)` = str_c(round(median(Age)), " (", round(quantile(Age,probs = .25)), "-", round(quantile(Age,probs = .75)), ")")
    ) 
) %>% 
  kbl(caption="<b>Table 1 | </b> Demographics", align=rep('l', 5),
      format = "html", table.attr = "style='width:40%;'") %>% 
  kable_classic(html_font = "Cambria")


theta_fullExclusion$MrAB %>% filter(condition=="gain")

lazyLoad("Report_cache/html/plot-MrAB-unpooled-posteriors-full-exclusion_c7bc9f4c7adbf512247dd9a4889ed25f")

post_plot1

table_unpooled <- post_plot1 %>% 
  group_by(Country) %>% mutate(mu=mean(theta)) %>% 
  filter(row_number()==1) %>% 
  select(Country, mu, lower, upper) %>% 
  ungroup() %>% 
  mutate(`CIs (95%)`=str_c(round(lower,2), round(upper,2), sep = ' - '),
         mu=round(mu,2)) %>% 
  rename(Estimate = mu) %>% 
  mutate(` ` = "") %>% 
  .[,c(6,1,2,5)] 


empty <- function(x){ 
  if(x==''){
    return("MrAB1")
    } else {
      return("")
    } 
  }

table_unpooled <- rbind(apply(table_unpooled[1,], 2, empty), table_unpooled)

table_unpooled |>
  kbl(caption="<b>Table 2 | </b> Hierarchical Bayesian Meta-Analysis",
      format = "html", table.attr = "style='width:90%;'") %>% 
  kable_classic(html_font = "Cambria")



fixef(mMrAB2) %>% as.data.frame(row.names = '') %>% 
  mutate(Study="MrAB1", 
         `$\\hat{\\beta}$` = round(Estimate, 2),
         `CIs (95%)`=str_c(round(Q2.5,2), round(Q97.5,2), sep = ' - '),
         ) %>% 
  select(-c(Est.Error, Q2.5, Q97.5, Estimate))

post %>% filter(family=="binomial") %>% 
  filter(Country!="all") %>% 
  ggplot(aes(x, post, color=Country)) +
  geom_jitter( width = 0.1, alpha=0.7, color="gray", size=2 ) +
  # geom_point(data = data_paper, aes(x, theta), color="#472E7CFF", size=30, shape="-") +

  geom_segment(data = data_paper, linewidth=1.8,
               aes(x=x-0.2, xend=x+0.2, y=theta, yend=theta), color="#472E7CFF") +
  geom_point(data = post %>% filter(family=="binomial") %>%
               filter(Country=="all"), color="#228B8DFF", size=5) +
  geom_point(data = post %>% filter(family=="binomial") %>% 
               filter(Country=="all"), color="white", size=3) +
  geom_hline(yintercept = 0, linetype=2, size=1) +
  theme_pubr() + theme(legend.position = "right") +
  labs(x=NULL, y=expression(Posterior~log(OR))) +
  scale_y_continuous(guide = "prism_offset", limits = c(-1,5.5), breaks = -1:5) + 
  scale_size(range = c(3, 8)) +
  scale_x_continuous(breaks =c(1.5,3:6), 
                     labels = post %>% filter(family=="binomial") %>% 
                       .[,"study",drop=T] %>% unique(),
                     guide = "prism_offset") + 
  guides(size = "none") + 
  theme(text = element_text(size = 15), legend.position = "none")

lazyLoad("Report_cache/html/fit-MrAB-finantial-literacy_3c6336b4c6cc2482adb5f563c9e628d8")

# Sample Size #
data_MrAB %>% 
  group_by(subject) %>% 
  filter(row_number()==1) %>% 
  group_by(Country) %>% 
  mutate(sample_size=n()) %>% 
  filter(sample_size>=250) %>% 
  # Apply new criterion
  # A
  filter(!loi_lower_than_loiX0_33) %>%
  # B
  filter(native_language_is_country_language) %>%
  # C
  filter(attention_check_grater_than_3) %>%
  # D
  # filter(attention_check_grater_than_2) %>%
  # Result
  nrow()

data_MrAB %>% 
  group_by(subject) %>% 
  filter(row_number()==1) %>% 
  # Apply new criterion
  # A
  filter(!loi_lower_than_loiX0_33) %>%
  # B
  filter(native_language_is_country_language) %>%
  # C
  filter(attention_check_grater_than_3) %>%
  # D
  # filter(attention_check_grater_than_2) %>%
  group_by(Country) %>% 
  summarise(sample_size=n()) %>% 
  filter(sample_size>=250) %>% 
  # Result
  nrow()



data_MrAB %>%
  filter(attention_check_grater_than_2) %>% 
  group_by(subject,Country) %>% 
  filter(row_number()==1) %>% 
  group_by(Country) %>% 
  mutate(sample_size=n()) %>% 
  ungroup() %>% #nrow() %>% 
  filter(sample_size>=250) %>% nrow()
  
  


summary( mMrAB1 )

mGame <- brm(response ~ buyer*FinancialLiteracy*Country,
             data = data_Game %>% 
               # EXCLUSION: Preregistered Exclusion
               filter( !(Country %in% countries2remove) ) %>% 
               filter( attention_check_grater_than_3 ),
             iter = 1000, refresh = 0, family="bernoulli")

post <- prepare_predictions(mGame)$dpars$mu$fe$b %>% 
  as.data.frame() %>% 
  select(contains(":") & contains("FinancialLiteracy"))

names(post)[1] <- "Austria"
names(post) <- str_remove( names(post), "b_buyerStranger:FinancialLiteracy:Country" )
post <- post %>% select(!contains(":"))
all_countries <- names(post)

post_plot <- map_dfr(all_countries, function(country){
  if(country=="Austria"){
    theta <- post[,"Austria"]
  } else {
    theta <- post[,"Austria"] + post[,country]
  }
  data.frame(theta, Country=country, study="MrAB", family="binomial", x=1.2) %>% 
    mutate(lower = HDInterval::hdi( theta )["lower"],
           upper = HDInterval::hdi( theta )["upper"],
           credible = ifelse(lower<0, "no", "yes"))
})

postMrAB1 <- post_plot1 %>% 
  group_by(Country) %>% 
  mutate(theta=mean(theta)) %>% 
  filter(row_number()==1)

plMrAB1 <- post_plot1 %>% 
  ggplot(aes(x = theta, y = Country)) +
  stat_halfeye(aes(fill=credible, color=credible)) +
  geom_vline(xintercept = 0, linetype=2) +
  theme_pubr() + 
  labs(x=expression(log(OR)), y=NULL) +
  scale_y_discrete(guide = "prism_offset") + 
  scale_x_continuous(guide = "prism_offset") + 
  scale_fill_manual(values = c("#bf7fbf", "gray"), breaks = c("yes", "no")) +
  scale_color_manual(values = c("#560f56", "gray"), breaks = c("yes", "no")) +
  guides(size = "none") + 
  theme(text = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        legend.position = "none")


plot(effects::allEffects(mGame), multiline=T)

as.data.frame(effects::allEffects(mGame))[[1]] %>% 
  group_by(Country, FinancialLiteracy) %>% 
  mutate(theta = fit[buyer=='Stranger']-fit[buyer=='Friend']) %>% 
  filter(row_number()==1) %>% mutate(FinancialLiteracy=as.numeric(FinancialLiteracy)) %>% 
  ggplot(aes(FinancialLiteracy, theta, color=Country)) +
  geom_line()


data.frame( b = as.vector( coef(mGame)[c(35, 98:128)] ),
            country = unique(data_Game$Country) ) %>% 
  ggplot(aes(1, b, color=country)) +
  geom_jitter() +
  geom_hline(yintercept = 0)

data_MrAB %>% 
  group_by(Country, subject) %>% 
  filter(row_number()==1) %>% 
  group_by(Country) %>% 
  summarise(mean(FinancialLiteracy))

dat <- data_MrAB %>% 
  group_by(Country, subject) %>% 
  filter(row_number()==1) %>% 
  group_by(Country) %>%
  mutate(N=n()) %>% 
  group_by(Country, FinancialLiteracy) %>% 
  summarise(fqFinLit = n()/N)

nCountries <- length(unique(dat$Country))

ggplot(dat, aes(FinancialLiteracy, fqFinLit, color=Country)) +
  geom_line(size=1) +
  theme_pubr() +
  scale_color_manual(values = viridis::viridis_pal()(nCountries)) +
  theme(text = element_text(size = 20), legend.position = "none") +
  scale_x_continuous(guide = "prism_offset") + 
  scale_y_continuous(guide = "prism_offset") +
  labs(y='Frequency', x='Financial Literacy')

mDrink <- lm(response ~ store * Country,
             data = data_Drink %>% 
               # EXCLUSION: Full Exclusion
               filter( !(Country %in% countries2remove) ) %>% 
               filter( attention_check_grater_than_3 ) %>% 
               # Remove really really extreme outliers
               filter(response<10000 & response>=0) %>% 
               # filter(Country=='Vietnam') %>% 
               mutate(response=response+1, logResp=log(response)) %>% 
               group_by(Country) %>%
               mutate(response=as.vector(scale(logResp))) %>% 
               ungroup())

summary(mDrink)

lazyLoad("Report_cache/html/fit-Drink-unpooled-full-exclusion_476ea8268cf38131304840ccbd9f2044")

post <- prepare_predictions(mDrink)$dpars$mu$fe$b %>% 
  as.data.frame()

mean( post$b_storeResortHotel + post$`b_storeResortHotel:CountryVietnam` )

names(post)[1] <- "Austria"
names(post)[2] <- "theta"
names(post) <- str_remove( names(post), "b_" )
names(post) <- str_remove( names(post), "store" )
names(post) <- str_remove( names(post), "Country" )
all_countries <- names(post)[-2]
all_countries <- all_countries[-grep(":", all_countries)]

post_plot <- map_dfr(all_countries, function(country){
  if(country=="Austria"){
    theta <- post[,"theta"]
  } else {
    theta <- post[,"theta"] + post[,grep(paste0(":",country), names(post))]
  }
  data.frame(theta, Country=country, study="Drink", family="gaussian", x=1) %>% 
    mutate(lower = HDInterval::hdi( theta )["lower"],
           upper = HDInterval::hdi( theta )["upper"],
           credible = ifelse(lower<0, "no", "yes"))
})


mean( with( post_plot, theta[Country=='Vietnam'] ) )
postDrink <- post_plot %>% 
  group_by(Country) %>% 
  mutate(theta=mean(theta)) %>% 
  filter(row_number()==1)

post_plot %>%   
  ggplot(aes(x = theta, y = Country)) +
  stat_halfeye(aes(fill=credible, color=credible)) +
  geom_vline(xintercept = 0, linetype=2) +
  theme_pubr() + 
  labs(x=expression(theta[SMD]), y=NULL) +
  scale_y_discrete(guide = "prism_offset") + 
  scale_x_continuous(guide = "prism_offset") + 
  scale_fill_manual(values = c("#bf7fbf", "gray"), breaks = c("yes", "no")) +
  scale_color_manual(values = c("#560f56", "gray"), breaks = c("yes", "no")) +
  guides(size = "none") + 
  theme(text = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        legend.position = "none")

data_Drink %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  # Remove really really extreme outliers
  filter(response<10000 & response>=0) %>% 
  mutate(response=response+1, logResp=log(response)) %>% 
  # Compute effect size as Standardized Mean Difference
  group_by(Country) %>% 
  # Calculate Mean Difference 
  mutate(md=mean(logResp[store=="Resort Hotel"])-mean(logResp[store=="Grocery Store"])) %>% 
  # Calculate effect size
  mutate(theta=md/sd(logResp)) %>% 
  group_by(Country, store) %>% filter(row_number()==1) %>% 
  group_by(Country) %>% 
  
  select(-c(md, response, logResp)) %>% 
  filter(row_number()==1) %>% ungroup() %>% 
  filter(Country=='Vietnam') %>% 
  select(theta)


data_MrAB %>% 
  filter(attention_check_grater_than_3) %>% 
  group_by(subject) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  # group_by(Country) %>%
  summarise(m = paste( round((1-mean(Country==Residence))*100,2),  "%") )
  summarise(m = paste( round((1-mean( (Country==Residence) | native_language_is_country_language ))*100,2),  "%") )
  

data_MrAB %>% 
  filter(attention_check_grater_than_3) %>% 
  group_by(subject) %>% 
  filter(row_number()==1) %>% 
  group_by(Country) %>% 
  summarise(sample_size=n()) %>% 
  filter(sample_size<=250) %>% 
  .[,"Country", drop=TRUE]

lazyLoad("Report_cache/html/")
post <- prepare_predictions(mDrink)$dpars$mu$fe$b %>% 
  as.data.frame()

names(post)[1] <- "Austria"
names(post)[2] <- "theta"
names(post) <- str_remove( names(post), "b_" )
names(post) <- str_remove( names(post), "store" )
names(post) <- str_remove( names(post), "Country" )
all_countries <- names(post)[-2]
all_countries <- all_countries[-grep(":", all_countries)]


post_plot <- map_dfr(all_countries, function(country){
  if(country=="Austria"){
    theta <- post[,"Austria"] + post[,"theta"]
  } else {
    theta <- post[,"Austria"] + post[,country] + post[,"theta"] + post[,grep(paste0(":",country), names(post))]
  }
  data.frame(theta, Country=country, study="Drink", family="gaussian", x=1) %>% 
    mutate(lower = HDInterval::hdi( theta )["lower"],
           upper = HDInterval::hdi( theta )["upper"],
           credible = ifelse(lower<0, "no", "yes"))
})

post_plot %>%   
  ggplot(aes(x = theta, y = Country)) +
  stat_halfeye(aes(fill=credible, color=credible)) +
  geom_vline(xintercept = 0, linetype=2) +
  theme_pubr() + 
  labs(x=expression(theta[SMD]), y=NULL) +
  scale_y_discrete(guide = "prism_offset") + 
  scale_x_continuous(guide = "prism_offset") + 
  scale_fill_manual(values = c("#bf7fbf", "gray"), breaks = c("yes", "no")) +
  scale_color_manual(values = c("#560f56", "gray"), breaks = c("yes", "no")) +
  guides(size = "none") + 
  theme(text = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        legend.position = "none")

post_plot %>% 
  select(Country, theta, lower, upper) %>% 
  group_by(Country) %>% filter(row_number()==1) %>% 
  ungroup()

data_Drink %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  # Remove really really extreme outliers
  filter(response<10000 & response>=0) %>% 
  mutate(response=response+1, logResp=log(response)) %>% 
  filter(Country=="Austria") %>% 
  mutate(response=as.vector(scale(response))) %>% 
  ggplot(aes(response, fill=store)) +
  geom_density(alpha=.5)

glm( response ~ coupon, 
    data = data_Plane %>% 
      # EXCLUSION: Full Exclusion
      filter( !(Country %in% countries2remove) ) %>% 
      filter( attention_check_grater_than_3 ), 
    family="binomial"
    
    ) %>% summary()

lm( response ~ frame, 
     data = data_Gym %>% 
       # EXCLUSION: Full Exclusion
       filter( !(Country %in% countries2remove) ) %>% 
       filter( attention_check_grater_than_3 )
) %>% summary()


glm(response ~ scenario + Country,
    data = data_MrAB %>% filter(response!=2) %>%
      filter(scenario_group=="loss") %>% 
      # EXCLUSION: Full Exclusion
      filter( !(Country %in% countries2remove) ) %>% 
      filter( attention_check_grater_than_3 ) %>% 
      mutate(scenario=factor(scenario, levels = c("loss-gain VS loss", "loss-loss VS loss") )), 
family="binomial")

lazyLoad("Report_cache/html/fit-Game-unpooled-full-exclusion_7498e593588ae4fe1c415435b3ed5597")

summary( mGame )
sjPlot::tab_model(mGame)

library(tidybayes)

mMrAB1 <- brm(response ~ scenario + Country,
              data = data_MrAB %>% filter(response!=2) %>%
                filter(scenario_group=="gain") %>% 
                # EXCLUSION: Full Exclusion
                filter( !(Country %in% countries2remove) ) %>% 
                filter( attention_check_grater_than_3 ) %>% 
                mutate(scenario=factor(scenario, levels = c("gain-loss VS gain", "gain-gain VS gain") )), 
              iter = 10000, refresh = 0, family="bernoulli")

post <- prepare_predictions(mMrAB1)$dpars$mu$fe$b %>% 
  as.data.frame()

names(post)[1] <- "Austria"
names(post)[2] <- "theta"
names(post) <- str_remove( names(post), "b_Country" )
all_countries <- names(post)[-2]

post_plot <- map_dfr(all_countries, function(country){
  if(country=="Austria"){
    theta <- post[,"Austria"] + post[,"theta"]
  } else {
    theta <- post[,"Austria"] + post[,country] + post[,"theta"]
  }
  data.frame(theta, Country=country, study="MrAB", family="binomial", x=1.2) %>% 
    mutate(lower = HDInterval::hdi( theta )["lower"],
           credible = ifelse(lower<0, "no", "yes"))
})
  

post_plot %>% 
  ggplot(aes(x = theta, y = Country)) +
  stat_halfeye(aes(fill=credible, color=credible)) +
  geom_vline(xintercept = 0) +
  theme_pubr() + 
  labs(x=expression(theta[OR]), y=NULL) +
  scale_y_discrete(guide = "prism_offset") + 
  scale_x_continuous(guide = "prism_offset") + 
  scale_fill_manual(values = c("#bf7fbf", "gray"), breaks = c("yes", "no")) +
  scale_color_manual(values = c("#560f56", "gray"), breaks = c("yes", "no")) +
  guides(size = "none") + 
  theme(text = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        legend.position = "none")


postMrAB1 <- post_plot %>% 
  group_by(Country) %>% 
  mutate(theta=mean(theta)) %>% 
  filter(row_number()==1)

lazyLoad("Report_cache/html/plot-unpooled-posteriors-fill-exclusion_85e48329d645837824049c9986be42e8")

plotOR <- post %>% filter(family=="binomial") %>% 
  ggplot(aes(x, theta, color=credible)) +
  geom_jitter( width = 0.1, alpha=0.7, size=2 ) +
  geom_hline(yintercept = 0, linetype=2, size=1) +
  theme_pubr() + theme(legend.position = "right") +
  labs(x=NULL, y=expression(Posterior~theta[OR])) +
  scale_y_continuous(guide = "prism_offset", limits = c(-1,5.5), breaks = -1:5) + 
  scale_size(range = c(3, 8)) +
  scale_x_continuous(breaks =c(1.5, 3:6), 
                     labels = post %>% filter(family=="binomial") %>% 
                       .[,"study",drop=T] %>% unique(),
                     guide = "prism_offset") + 
  scale_color_manual(values = c("#560f56", "gray"), breaks = c("yes", "no")) +
  guides(size = "none") + 
  theme(text = element_text(size = 20), legend.position = "none")

plotSMD <- post %>% filter(family=="gaussian") %>% 
  ggplot(aes(x, theta, color=credible)) +
  geom_jitter( width = 0.1, alpha=0.7, size=2 ) +
  geom_hline(yintercept = 0, linetype=2, size=1) +
  theme_pubr() + theme(legend.position = "right") +
  labs(x=NULL, y=expression(Posterior~theta[SMD])) +
  scale_y_continuous(guide = "prism_offset", limits = c(-1,5.5), breaks = -1:5) + 
  scale_size(range = c(3, 8)) +
  scale_x_continuous(breaks = 1:2, limits = c(0.5, 2.5),
                     labels = post %>% filter(family=="gaussian") %>% 
                       .[,"study",drop=T] %>% unique(),
                     guide = "prism_offset") + 
  scale_color_manual(values = c("#560f56", "gray"), breaks = c("yes", "no")) +
  guides(size = "none") + 
  theme(text = element_text(size = 20), legend.position = "none")

