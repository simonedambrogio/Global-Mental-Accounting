library(ggplot2); library(dplyr); library(purrr)
library(stringr); library(ggpubr); library(ggprism)
remove_character <- function(x, pattern){
  x_split <- strsplit(x, "")[[1]]
  paste(x_split[x_split!=pattern], collapse = "")
}

keep_character <- function(x, pattern){
  x_split <- strsplit(x, "")[[1]]
  paste(x_split[x_split%in%pattern], collapse = "")
}

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
  } else if (x=="۸"){
    return( "8" )
  } else if (x=="۹"){
    return( "9" )
  } else {
    warning( paste("Persian number", x, "not found!!!") )
  }
}

Countries <- c("Austria", "Brazil", "Canada", "China", "Colombia",
               "Denmark", "Egypt", "France", "Germany", "Ghana", 
               "Hungary", "India", "Indonesia", "Iran", "Israel", 
               "Italy", "Lithuania", "Morocco", "Netherlands", "Portugal", 
               "Romania", "Russia", "Singapore", "South Africa", 
               "South Korea", "Spain", "Sweden", "Switzerland", "Turkey", 
               "UK", "USA", "Vietnam")

Links <- c(
  "https://www.dropbox.com/s/v9sh2dv9l1l67u1/Austria.csv?dl=1",
  "https://www.dropbox.com/s/na0ucludleiwd9e/Brazil.csv?dl=1",
  "https://www.dropbox.com/s/9fvbn3qbffs62t8/Canada.csv?dl=1",
  "https://www.dropbox.com/s/8mdqx4kfop45upq/China.csv?dl=1",
  "https://www.dropbox.com/s/gnztue9vg0imo1f/Colombia.csv?dl=1",
  "https://www.dropbox.com/s/1htul957n715jl3/Denmark.csv?dl=1",
  "https://www.dropbox.com/s/dj15su5wg9j653f/Egypt.csv?dl=1",
  "https://www.dropbox.com/s/qhawfac17hiqrgp/France.csv?dl=1",
  "https://www.dropbox.com/s/a420drk3dx19yod/Germany.csv?dl=1",
  "https://www.dropbox.com/s/q9c0uxflfrpp4n7/Ghana.csv?dl=1",
  "https://www.dropbox.com/s/2ubkegux1c89q5m/Hungary.csv?dl=1",
  "https://www.dropbox.com/s/byn8phl29sszgef/India.csv?dl=1",
  "https://www.dropbox.com/s/slw1ccllbhziw0c/Indonesia.csv?dl=1",
  "https://www.dropbox.com/s/l73rvrsu4ush5ht/Iran.csv?dl=1",
  "https://www.dropbox.com/s/2y6ln88xc3ytbre/Israel.csv?dl=1",
  "https://www.dropbox.com/s/53ezpib85ca57nj/Italy.csv?dl=1",
  "https://www.dropbox.com/s/rx8sbhyfffny7an/Lithuania.csv?dl=1",
  "https://www.dropbox.com/s/0pwvk8tmojk60ac/Morocco.csv?dl=1",
  "https://www.dropbox.com/s/mheh3e5orryeqzx/Netherlands.csv?dl=1",
  "https://www.dropbox.com/s/opmi43jq6nrqsrg/Portugal.csv?dl=1",
  "https://www.dropbox.com/s/suy9ue8olqy27zg/Romania.csv?dl=1",
  "https://www.dropbox.com/s/z7lujkbfkb5u9z7/Russia.csv?dl=1",
  "https://www.dropbox.com/s/7ou3ecdb8kho1p2/Singapore.csv?dl=1",
  "https://www.dropbox.com/s/5xkt29f2j9i6aon/South%20Africa.csv?dl=1",
  "https://www.dropbox.com/s/9zm5kpk08st8lfu/South%20Korea.csv?dl=1",
  "https://www.dropbox.com/s/ouy0fk88kucnllr/Spain.csv?dl=1",
  "https://www.dropbox.com/s/2hk0pf0jheg1qxp/Sweden.csv?dl=1",
  "https://www.dropbox.com/s/a38v4k3bg6jx2je/Switzerland.csv?dl=1",
  "https://www.dropbox.com/s/fn14j02la64x1yl/Turkey.csv?dl=1",
  "https://www.dropbox.com/s/8zph24u5rdp1irm/UK.csv?dl=1",
  "https://www.dropbox.com/s/y7x86s82hpjbtb0/USA.csv?dl=1",
  "https://www.dropbox.com/s/f5mm1m45dix0hhh/Vietnam.csv?dl=1"
)

# Control variables
control_variables <- c("Gender", "Age", "Income", "Education", 
                       "Residence", "Country", "NativeLanguage", "FinancialLiteracy",
                       "attention_check_grater_than_3",
                       "attention_check_grater_than_2",
                       "loi_lower_than_loiX0_33",
                       "native_language_is_country_language")

# Extract Native Language for each Country
NativeLanguageList=lapply(Countries, function(Country){
  cat("\n# --------------------- #\n")
  cat(Country, "\n")
  lCountry <- Links[Countries==Country]
  dat <- read.csv(lCountry) %>% filter(Native.language!="")
  tb <- table(dat$Native.language)
  language = names(which.max(tb))
  cat(language, "\n")
  cat("# --------------------- #\n")
  if(Country=="Switzerland"){
    return(c("Italian", "French", "German"))
  } else {
    return(names(which.max(tb)))
  }
}); names(NativeLanguageList) <- Countries 

income <- read.csv("Raw Qualtrics Data/Income.csv")
  
################################################################
# ---------------------- Mr. A vs Mr. B ---------------------- #
################################################################
MrAB <- TRUE
if( MrAB ){
  data_MrAB <- map_dfr(Countries, function(Country){
    
    print(paste("Processing", Country))
    # Link for Austria
    lCountry <- Links[Countries==Country]
    dat <- read.csv(lCountry) %>% mutate(subject=ResponseId, Country, NativeLanguage=Native.language)
    dat$native_language_is_country_language = dat$NativeLanguage%in%NativeLanguageList[[Country]]
    
    # Convert "Residence" labels so that they match the labels used for the variable "Country"
    if(Country=="Russia"){
      dat$Residence <- with( dat, ifelse(Residence=='Russian Federation', Country, Residence) )
    } else if( Country=="UK" ){
      dat$Residence <- with( dat, ifelse(Residence=='United Kingdom of Great Britain and Northern Ireland', Country, Residence) )
    } else if( Country=="USA" ){
      dat$Residence <- with( dat, ifelse(Residence=='United States of America', Country, Residence) )
    } else if( Country=="Vietnam" ){
      dat$Residence <- with( dat, ifelse(Residence=='Viet Nam', Country, Residence) )
    } 
    
    # Remove first two rows. The first row indicate text presented, the second 
    # contains Qualtrics relevant variables (e.g. {"ImportId":"QID15_1"})
    dat <- dat[3:nrow(dat),]
    
    # Filter subject that finish the experiment and allow to 
    # use the responses provided in the study
    allow <- "I allow you to use the responses I provided in this study."
    dat <- dat %>% filter(Debriefing==allow) %>% select(-Debriefing)
    
    names(dat) <- sapply(names(dat), remove_character, pattern=".")
    
    # Create columns indicating attention check
    acg3 <- c("Absolutely serious", "Very serious")
    acg2 <- c("Absolutely serious", "Very serious", "Moderately serious")
    
    dat <- dat %>% mutate(
      attention_check_grater_than_3=ifelse(Att_check%in%acg3, T, F),
      attention_check_grater_than_2=ifelse(Att_check%in%acg2, T, F)
    ) %>% select(-Att_check) %>% 
    #Create column indicating whether LOI (Length of Interview) of each subjects lower
    # than a third of the median value of LOI 
    mutate(LOI=as.numeric(Durationinseconds),
           loi_lower_than_loiX0_33 = LOI < median( LOI )*0.33) %>% 
    select(-LOI) %>% 
    # Calculate score Financial Literacy
    mutate(fin_lit1 = ifelse(str_detect(fin_lit1, "More than"), 1, 0),
           fin_lit2 = ifelse(str_detect(fin_lit2, "Less than today"), 1, 0),
           fin_lit3 = ifelse(str_detect(fin_lit3, "False"), 1, 0),
           fin_lit4 = ifelse(str_detect(fin_lit4, "They collapse"), 1, 0),
           FinancialLiteracy =  fin_lit1+fin_lit2+fin_lit3+fin_lit4)
      
    
    # Create MrAB dataseta
    d <- dat %>%
      select(subject, contains("MrAB", ignore.case = FALSE), all_of(control_variables)) %>%
      # Re-code response
      mutate(`gain-gain VS gain` = case_when(MrAB1 == "A" ~ 1, 
                                             MrAB1 == "B" ~ 0, 
                                             T ~ 2), 
             `loss-loss VS loss` = case_when(MrAB2 =="A" ~ 1, 
                                             MrAB2 == "B" ~ 0, 
                                             T ~ 2),
             `gain-loss VS gain` = case_when(MrAB3 == "A" ~ 1,
                                             MrAB3 == "B" ~ 0, 
                                             T ~ 2), 
             `loss-gain VS loss` = case_when(MrAB4 == "A" ~ 1, 
                                             MrAB4 == "B" ~ 0, 
                                             T ~ 2)) %>%
      select(-contains("MrAB", ignore.case = FALSE)) %>%
      reshape2::melt(id.var = c("subject", control_variables), 
                     variable.name = "scenario", value.name = "response") %>%
      mutate(scenario_group = ifelse(scenario == "gain-gain VS gain" | scenario == "gain-loss VS gain", "gain", "loss"))
      
  })
  
  get_numeric_income <- TRUE
  if(get_numeric_income){
    
    # -------- Income conversion ------- #
    numeric_income <- c()
    
    # "Austria"
    d <- data_MrAB %>% filter(Country==Countries[1])
    inc <- income[income$Country==Countries[1],-1] %>% unlist() %>% as.numeric()
    
    list_string <- str_split(d$Income, pattern = ' ')
    
    num_income <- sapply(list_string, function(ls){
      any(ls=="Less") && return(0)
      any(ls=="More") && return(5)
      any(ls=="Prefer") && return(NA)
      return( which(as.numeric(ls[1]) == inc) )
    })
    
    numeric_income <- c(numeric_income, num_income)
    
    # "Brazil"
    d <- data_MrAB %>% filter(Country==Countries[2])
    inc <- c(1100, 2200, 3300, 4400, 5500, 6600, 7700, 9900)
    
    list_string <- str_split(d$Income, pattern = ' ')
    
    num_income <- sapply(list_string, function(ls){
      any(ls=="less") && return(0)
      any(ls=="more") && return(8)
      any(ls=="Prefer") && return(NA)
      return( which(as.numeric(ls[2])-1 == inc) )
    })
    num_income <- num_income/8*5
    
    numeric_income <- c(numeric_income, num_income)
    
    
    # "Canada"
    d <- data_MrAB %>% filter(Country==Countries[3])
    inc <- income[income$Country==Countries[3],-1] %>% unlist() %>% as.numeric()
    
    list_string <- str_split(d$Income, pattern = ' ')
    # Remove $ and , symbol
    list_string <- lapply(list_string, function(ls){ 
      ls[1] %>% 
      str_remove(pattern = '[$]') %>% 
      str_remove(pattern = '[,]')
    })
  
    ls <- list_string[[3]]
    num_income <- sapply(list_string, function(ls){
      any(ls=="Less") && return(0)
      any(ls=="More") && return(5)
      any(ls=="Prefer") && return(NA)
      return( which(as.numeric(ls) == inc) )
    })
    numeric_income <- c(numeric_income, num_income)
    
    
    # ---- "China" ---- #
    d <- data_MrAB %>% filter(Country==Countries[4])
    inc <- income[income$Country==Countries[4],-1] %>% unlist() %>% as.numeric()
    
    list_string <- str_split(d$Income, pattern = ' ')
    
    # Remove $ and , symbol
    list_string <- lapply(list_string, function(ls){ 
      ls[1] %>% 
        str_remove(pattern = '[￥]')
    })
    
    ls <- list_string[[3]]
    num_income <- sapply(list_string, function(ls){
      any(ls=="Less") && return(0)
      any(ls=="More") && return(5)
      any(ls=="Prefer") && return(NA)
      return( which(as.numeric(ls) == inc) )
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Colombia" ---- #
    d <- data_MrAB %>% filter(Country==Countries[5]) # table(d$Income) 
    inc <- income[income$Country==Countries[5],-1] %>% unlist() %>% as.numeric()
    
    list_string <- str_split(d$Income, pattern = ' ')
    
    # Remove ´ and . symbol
    list_string <- lapply(list_string, function(ls){ 
      (ls[1]!='$') && return(ls[1])
      ls[2] %>% 
        str_remove(pattern = '[´]') %>% 
        str_remove(pattern = '[.]')
    })
    
    num_income <- sapply(list_string, function(ls){
      any(ls=="Less") && return(0)
      any(ls=="More") && return(5)
      any(ls=="Prefer") && return(NA)
      return( which(as.numeric(ls) == inc) )
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Denmark" ---- #
    d <- data_MrAB %>% filter(Country==Countries[6]); table(d$Income) 
    inc <- income[income$Country==Countries[6],-1] %>% unlist() %>% as.numeric()
    
    list_string <- str_split(d$Income, pattern = ' ')
    
    # Remove symbol
    list_string <- lapply(list_string, function(ls){ 
      ls[1] %>% 
        str_remove(pattern = '[.]')
    })
    
    num_income <- sapply(list_string, function(ls){
      any(ls=="Less") && return(0)
      any(ls=="More") && return(5)
      any(ls=="Prefer") && return(NA)
      return( which(as.numeric(ls) == inc) )
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Egypt" ---- #
    d <- data_MrAB %>% filter(Country==Countries[7]); table(d$Income) 
    
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than EGP 3400") && return(0)
      (ls=="EGP 3400 - 5000") && return(1)
      (ls=="EGP 5000 - 6700") && return(2)
      (ls=="EGP 6700 -  8400") && return(3)
      (ls=="EGP 8400 - 10000") && return(4)
      (ls=="More than EGP 10000") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    
    
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "France" ---- #
    d <- data_MrAB %>% filter(Country==Countries[8]); table(d$Income) 
    inc <- income[income$Country==Countries[8],-1] %>% unlist() %>% as.numeric()
    
    list_string <- str_split(d$Income, pattern = ' ')
    
    # Remove symbol
    list_string <- lapply(list_string, function(ls){ 
      ls[1] %>% 
        str_remove(pattern = '[€]')
    })
    
    num_income <- sapply(list_string, function(ls){
      any(ls=="Less") && return(0)
      any(ls=="More") && return(5)
      any(ls=="Prefer") && return(NA)
      return( which(as.numeric(ls) == inc) )
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Germany" ---- #
    d <- data_MrAB %>% filter(Country==Countries[9]); table(d$Income) 
    inc <- income[income$Country==Countries[9],-1] %>% unlist() %>% as.numeric()
    
    list_string <- str_split(d$Income, pattern = ' ')
    
    # Remove symbol
    list_string <- lapply(list_string, function(ls){ 
      ls[1] %>% 
        str_remove(pattern = '[€]')
    })
    
    num_income <- sapply(list_string, function(ls){
      any(ls=="Less") && return(0)
      any(ls=="More") && return(5)
      any(ls=="Prefer") && return(NA)
      return( which(as.numeric(ls) == inc) )
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Ghana" ---- #
    d <- data_MrAB %>% filter(Country==Countries[10]); table(d$Income) 
    inc <- income[income$Country==Countries[10],-1] %>% unlist() %>% as.numeric()
    
    list_string <- str_split(d$Income, pattern = ' ')
    
    # Remove symbol
    list_string <- lapply(list_string, function(ls){ 
      (ls[1]!="GHS") && return(ls[1])
      paste0(c(ls[2], ls[3]), collapse = "")
    })
    
    num_income <- sapply(list_string, function(ls){
      any(ls=="Less") && return(0)
      any(ls=="More") && return(5)
      any(ls=="Prefer") && return(NA)
      return( which(as.numeric(ls) == inc) )
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Hungary" ---- #
    d <- data_MrAB %>% filter(Country==Countries[11]); table(d$Income) 
    
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 190 000 forint") && return(0)
      (ls=="190 000 – 280 000 forint") && return(1)
      (ls=="280 000 – 375 000 forint") && return(2)
      (ls=="375 000 – 470 000 forint") && return(3)
      (ls=="470 000 – 560 000 forint") && return(4)
      (ls=="More than 560 000 forint") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "India" ---- #
    d <- data_MrAB %>% filter(Country==Countries[12]); table(d$Income) 
    
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than Rs 210000") && return(0)
      (ls=="Rs 210000 - Rs 315000") && return(1)
      (ls=="Rs 315000 - Rs 420000") && return(2)
      (ls=="Rs 420000- Rs 525000") && return(3)
      (ls=="Rs 525000- Rs 630000") && return(4)
      (ls=="More than Rs 630000") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "India" ---- #
    d <- data_MrAB %>% filter(Country==Countries[13]); table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls==uIncome[1]) && return(0)
      (ls==uIncome[4]) && return(1)
      (ls==uIncome[5]) && return(2)
      (ls==uIncome[6]) && return(3)
      (ls==uIncome[7]) && return(4)
      (ls==uIncome[2]) && return(5)
      (ls==uIncome[3]) && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Iran" ---- #
    d <- data_MrAB %>% filter(Country==Countries[14]); Countries[14]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 3,100,000 Tomans") && return(0)
      (ls=="3,100,000 - 4,700,000 Tomans") && return(1)
      (ls=="4,700,000 - 6,200,000 Tomans") && return(2)
      (ls=="6,200,000 - 7,700,000 Tomans") && return(3)
      (ls=="7,700,000 - 9,300,000 Tomans") && return(4)
      (ls=="More than 9,300,000 Tomans") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Iran" ---- #
    d <- data_MrAB %>% filter(Country==Countries[15]); Countries[15]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 8,100") && return(0)
      (ls=="8,100 - 12,500") && return(1)
      (ls=="12,500 - 16,300") && return(2)
      (ls=="16,300 - 20,800") && return(3)
      (ls=="20,800 - 25,000") && return(4)
      (ls=="More than 25,000") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Italy" ---- #
    d <- data_MrAB %>% filter(Country==Countries[16]); Countries[16]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 15.000 €") && return(0)
      (ls=="15.000 € - 22.000 €") && return(1)
      (ls=="22.000 € - 30.000 €") && return(2)
      (ls=="30.000 € - 38.000 €") && return(3)
      (ls=="38.000 € - 45.000 €") && return(4)
      (ls=="More than 45.000 €") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Lithuania" ---- #
    d <- data_MrAB %>% filter(Country==Countries[17]); Countries[17]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 600 Eur") && return(0)
      (ls=="600 Eur – 1000 Eur") && return(1)
      (ls=="1000 Eur - 1300 Eur") && return(2)
      (ls=="1300 Eur - 1600 Eur") && return(3)
      (ls=="1600 Eur - 2000 Eur") && return(4)
      (ls=="More than 2000 Eur") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Morocco" ---- #
    d <- data_MrAB %>% filter(Country==Countries[18]); Countries[18]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 30000 Dirhams") && return(0)
      (ls=="30000 Dirhams - 45000 Dirhams") && return(1)
      (ls=="45000 Dirhams - 60000 Dirhams") && return(2)
      (ls=="60000 Dirhams - 75000 Dirhams") && return(3)
      (ls=="75000 Dirhams - 90000 Dirhams") && return(4)
      (ls=="More than 90000 Dirhams") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Netherlands" ---- #
    d <- data_MrAB %>% filter(Country==Countries[19]); Countries[19]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than €17000,-") && return(0)
      (ls=="€17000,- – €26000,-") && return(1)
      (ls=="€26000,- – €35000,-") && return(2)
      (ls=="€35000,- – €44000,-") && return(3)
      (ls=="€44000,- – €53000,-") && return(4)
      (ls=="More than €53000,-") && return(5)
      (ls=="Prefer not to say") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Portugal" ---- #
    d <- data_MrAB %>% filter(Country==Countries[20]); Countries[20]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 5500 €") && return(0)
      (ls=="5500 € – 8200 €") && return(0.71)
      (ls=="8200 € – 11 000 €") && return(1.43)
      (ls=="11 000 € – 13 700 €") && return(2.14)
      (ls=="13 700 € – 16 500 €") && return(2.85)
      (ls=="16 500 € – 22 000 €") && return(3.57)
      (ls=="22 000 € – 27 000 €") && return(4.29)
      (ls=="More than 27 000 €") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Romania" ---- #
    d <- data_MrAB %>% filter(Country==Countries[21]); Countries[21]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 1900 lei") && return(0)
      (ls=="1900  - 2800 lei") && return(1)
      (ls=="2800 - 3800 lei") && return(2)
      (ls=="3800 - 4800 lei") && return(3)
      (ls=="4800 - 5700 lei") && return(4)
      (ls=="More than 5700 lei") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Russia" ---- #
    d <- data_MrAB %>% filter(Country==Countries[22]); Countries[22]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 17 000 ₽") && return(0)
      (ls=="17 000 ₽ - 34 000 ₽") && return(1)
      (ls=="34 000 ₽ - 51 000 ₽") && return(2)
      (ls=="51 000 ₽ - 68 000 ₽") && return(3)
      (ls=="68 000 ₽ - 85 000 ₽") && return(4)
      (ls=="More than 85 000 ₽") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
  
    # ---- "Singapore" ---- #
    d <- data_MrAB %>% filter(Country==Countries[23]); Countries[23]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than $60000") && return(0)
      (ls=="$60000 - $90000") && return(1)
      (ls=="$90000 - $120000") && return(2)
      (ls=="$120000 - $150000") && return(3)
      (ls=="$150000 - $180000") && return(4)
      (ls=="More than $180000") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "South Africa" ---- #
    d <- data_MrAB %>% filter(Country==Countries[24]); Countries[24]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than R75 000") && return(0)
      (ls=="R75 000 - R110 000") && return(1)
      (ls=="R110 000 - R150 000") && return(2)
      (ls=="R150 000 - R190 000") && return(3)
      (ls=="R190 000 - R225 000") && return(4)
      (ls=="More than R225 000") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "South Korea" ---- #
    d <- data_MrAB %>% filter(Country==Countries[25]); Countries[25]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than ₩14,000,000") && return(0)
      (ls=="₩14,000,000 - ₩21,000,000") && return(1)
      (ls=="₩21,000,000 - ₩28,000,000") && return(2)
      (ls=="₩28,000,000 -\b ₩35,000,000") && return(3)
      (ls=="₩35,000,000 - ₩42,000,000") && return(4)
      (ls=="More than ₩42,000,000") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Spain" ---- #
    d <- data_MrAB %>% filter(Country==Countries[26]); Countries[26]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 8000€") && return(0)
      (ls=="8000€ - 12000€") && return(1)
      (ls=="12000€ - 16000€") && return(2)
      (ls=="16000€ - 20000€") && return(3)
      (ls=="20000€ - 24000€") && return(4)
      (ls=="More than 24000€") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Sweden" ---- #
    d <- data_MrAB %>% filter(Country==Countries[27]); Countries[27]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 130000 SEK") && return(0)
      (ls=="130000 SEK - 195000 SEK") && return(1)
      (ls=="195000 SEK - 260000 SEK") && return(2)
      (ls=="260000 SEK - 325000 SEK") && return(3)
      (ls=="325000 SEK - 390000 SEK") && return(4)
      (ls=="More than 390000 SEK") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Switzerland" ---- #
    d <- data_MrAB %>% filter(Country==Countries[28]); Countries[28]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than CHF 25'000") && return(0)
      (ls=="CHF 25'000 - CHF 38'000") && return(1)
      (ls=="CHF 38'000 - CHF 50'000") && return(2)
      (ls=="CHF 50'000 - CHF 62'000") && return(3)
      (ls=="CHF 62'000 - CHF 75'000") && return(4)
      (ls=="More than CHF 75'000") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Turkey" ---- #
    d <- data_MrAB %>% filter(Country==Countries[29]); Countries[29]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 45000 TL") && return(0)
      (ls=="45000 TL – 68000 TL") && return(1)
      (ls=="68000 TL – 90000 TL") && return(2)
      (ls=="90000 TL – 112000 TL") && return(3)
      (ls=="112000 TL – 135000 TL") && return(4)
      (ls=="More than  135000 TL") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "UK" ---- #
    d <- data_MrAB %>% filter(Country==Countries[30]); Countries[30]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than £15,000") && return(0)
      (ls=="£15,000 - £22,000") && return(1)
      (ls=="£22,000 - £30,000") && return(2)
      (ls=="£30,000 - £38,000") && return(3)
      (ls=="£38,000 - £45,000") && return(4)
      (ls=="More than £45,000") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "USA" ---- #
    d <- data_MrAB %>% filter(Country==Countries[31]); Countries[31]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than $35,000") && return(0)
      (ls=="$35,000 - $52,000") && return(1)
      (ls=="$52,000 - $70,000") && return(2)
      (ls=="$70,000 - $87,000") && return(3)
      (ls=="$87,000 - $105,000") && return(4)
      (ls=="More than $105,000") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
    
    # ---- "Vietnam" ---- #
    d <- data_MrAB %>% filter(Country==Countries[32]); Countries[32]; table(d$Income) 
    uIncome <- table(d$Income) %>% names()
    num_income <- sapply(as.list(d$Income), function(ls){
      (ls=="Less than 5.000.000Đ") && return(0)
      (ls=="5.000.000Đ -  7.500.000Đ") && return(1)
      (ls=="7.500.000Đ - 10.000.000Đ") && return(2)
      (ls=="10.000.000Đ - 12.500.000Đ") && return(3)
      (ls=="12.500.000Đ - 15.000.000Đ") && return(4)
      (ls=="More than 15.000.000Đ") && return(5)
      (ls=="Prefer not to answer") && return(NA)
    })
    numeric_income <- c(numeric_income, num_income)
  }
  
  data_MrAB$numeric_income <- numeric_income
  
  write.csv(data_MrAB, file = "data_MrAB.csv")
  
  # Effect found in the paper Thaler 1985
  n_sbj    = 87
  A        = c(56, 66, 22, 19)
  B        = c(16, 14, 61, 63)
  
  data_paper_MrAB <- data.frame(
    x=c(1.2, 1.8), study="MrAB", family="binomial",
    # se_theta = c( sqrt(1/A[1]+1/A[3]+1/B[1]+1/B[3]),
    #               sqrt(1/A[2]+1/A[4]+1/B[2]+1/B[4])),
    theta=c(log( (A[1]/A[3]) / (B[1]/B[3]) ),
            log( (A[2]/A[4]) / (B[2]/B[4]) ) )
  )

}

###############################################################
# ------------------- The Sold Out Ticket ------------------- #
###############################################################

Game <- TRUE
if( Game ){
  
  data_Game <- map_dfr(Countries, function(Country){
    
    print(paste("Processing", Country))
    # Link for Austria
    lCountry <- Links[Countries==Country]
    dat <- read.csv(lCountry) %>% mutate(subject=ResponseId, Country, NativeLanguage=Native.language)
    dat$native_language_is_country_language = dat$NativeLanguage%in%NativeLanguageList[[Country]]
    
    if(Country=="Russia"){
      dat$Residence <- with( dat, ifelse(Residence=='Russian Federation', Country, Residence) )
    } else if( Country=="UK" ){
      dat$Residence <- with( dat, ifelse(Residence=='United Kingdom of Great Britain and Northern Ireland', Country, Residence) )
    } else if( Country=="USA" ){
      dat$Residence <- with( dat, ifelse(Residence=='United States of America', Country, Residence) )
    } else if( Country=="Vietnam" ){
      dat$Residence <- with( dat, ifelse(Residence=='Viet Nam', Country, Residence) )
    } 
    
    text <- dat[1,]
    # Remove first two rows. The first row indicate text presented, the second 
    # contains Qualtrics relevant variables (e.g. {"ImportId":"QID15_1"})
    dat <- dat[3:nrow(dat),]
    
    # Filter subject that finish the experiment and allow to 
    # use the responses provided in the study
    allow <- "I allow you to use the responses I provided in this study."
    dat <- dat %>% filter(Debriefing==allow) %>% select(-Debriefing)
    
    names(dat) <- sapply(names(dat), remove_character, pattern=".")
    
    # Create columns indicating attention check
    acg3 <- c("Absolutely serious", "Very serious")
    acg2 <- c("Absolutely serious", "Very serious", "Moderately serious")
    
    dat <- dat %>% mutate(
      attention_check_grater_than_3=ifelse(Att_check%in%acg3, T, F),
      attention_check_grater_than_2=ifelse(Att_check%in%acg2, T, F)
    ) %>% select(-Att_check) %>% 
      #Create column indicating whether LOI (Length of Interview) of each subjectis lower
      # than a third of the median value of LOI 
      mutate(LOI=as.numeric(Durationinseconds),
             loi_lower_than_loiX0_33 = LOI < median( LOI )*0.33) %>% 
      select(-LOI) %>% 
      # Calculate score Financial Literacy
      mutate(fin_lit1 = ifelse(str_detect(fin_lit1, "More than"), 1, 0),
             fin_lit2 = ifelse(str_detect(fin_lit2, "Less than today"), 1, 0),
             fin_lit3 = ifelse(str_detect(fin_lit3, "False"), 1, 0),
             fin_lit4 = ifelse(str_detect(fin_lit4, "They collapse"), 1, 0),
             FinancialLiteracy =  fin_lit1+fin_lit2+fin_lit3+fin_lit4)
    
    # Create dataframe scenario 2
    df <- dat %>% 
      select(contains('Game', ignore.case =  F))
    
    # ---- Game1_1_1 ---- #
    # You are going to a sold-out game of your favorite local 
    # sports team and you have an extra ticket. The price marked 
    # on the ticket is x$ but this ticket has been 
    # offered to you by a friend for free.
    # You want to get rid of the extra ticket and you know the 
    # ticket’s face value price is x$. You find someone 
    # who wants the ticket. There is no law against charging more 
    # than the price marked on the ticket.   
    # What price do you ask for selling the extra ticket if the 
    # person you sell it to... ? - Is a friend
    
    text_near_mv <- strsplit(text$Game1_1_1, " ")[[1]][24:33]
    mark_val <- sapply(text_near_mv, keep_character, pattern=as.character(0:9))
    market_value <- as.numeric(mark_val[mark_val!=""])
    
    # For Egypt
    df$Game1_1_1[df$Game1_1_1=="٠"] <- "0"
    df$Game1_1_1[df$Game1_1_1=="٠٠"] <- "0"
    df$Game1_1_1[df$Game1_1_1=="۰"] <- "0"
    df$Game1_1_1[df$Game1_1_1=="١"] <- "1"
    df$Game1_1_1[df$Game1_1_1=="٢"] <- "2"
    df$Game1_1_1[df$Game1_1_1=="١٠"] <- "10"
    df$Game1_1_1[df$Game1_1_1=="١٥"] <- "15"
    df$Game1_1_1[df$Game1_1_1=="۱۵"] <- "15"
    df$Game1_1_1[df$Game1_1_1=="٢٠"] <- "20"
    df$Game1_1_1[df$Game1_1_1=="٢٥"] <- "25"
    df$Game1_1_1[df$Game1_1_1=="٣٠"] <- "30"
    df$Game1_1_1[df$Game1_1_1=="٣٥"] <- "35"
    df$Game1_1_1[df$Game1_1_1=="٤٠"] <- "40"
    df$Game1_1_1[df$Game1_1_1=="٤٥"] <- "45"
    df$Game1_1_1[df$Game1_1_1=="٥٠"] <- "50"
    df$Game1_1_1[df$Game1_1_1=="٥٥"] <- "55"
    df$Game1_1_1[df$Game1_1_1=="٦٠"] <- "60"
    df$Game1_1_1[df$Game1_1_1=="٧٠"] <- "70"
    df$Game1_1_1[df$Game1_1_1=="٨٠"] <- "80"
    df$Game1_1_1[df$Game1_1_1=="١٠٠"] <- "100"
    df$Game1_1_1[df$Game1_1_1=="١٥٠"] <- "150"
    
    game1 <- data.frame( buyer = 'Friend',
                         response = as.numeric(df$Game1_1_1),
                         market_value )
    
    if( any( is.na( as.numeric(df$Game1_1_1) ) ) ) print( "Check Game1_1_1!") 
    # ---- Game1_1_2 ---- #
    # You are going to a sold-out game of your favorite local sports team and 
    # you have an extra ticket. The price marked on the ticket is x$
    # but this ticket has been offered to you by a friend for free.       
    # You want to get rid of the extra ticket and you know the ticket’s face 
    # value price is x$. You find someone who wants the ticket. There is no 
    # law against charging more than the price marked on the ticket.   
    # What price do you ask for selling the extra ticket if the person you 
    # sell it to... ? - Is a stranger
    
    # For Egypt
    df$Game1_1_2[df$Game1_1_2=="٠"] <- "0"
    df$Game1_1_2[df$Game1_1_2=="٠٠"] <- "0"
    df$Game1_1_2[df$Game1_1_2=="١"] <- "1"
    df$Game1_1_2[df$Game1_1_2=="٢"] <- "2"
    df$Game1_1_2[df$Game1_1_2=="١٠"] <- "10"
    df$Game1_1_2[df$Game1_1_2=="١٥"] <- "15"
    df$Game1_1_2[df$Game1_1_2=="۱۵"] <- "15"
    df$Game1_1_2[df$Game1_1_2=="٢٠"] <- "20"
    df$Game1_1_2[df$Game1_1_2=="٢٥"] <- "25"
    df$Game1_1_2[df$Game1_1_2=="٣٠"] <- "30"
    df$Game1_1_2[df$Game1_1_2=="٣٥"] <- "35"
    df$Game1_1_2[df$Game1_1_2=="٤٠"] <- "40"
    df$Game1_1_2[df$Game1_1_2=="٤٥"] <- "45"
    df$Game1_1_2[df$Game1_1_2=="٥٠"] <- "50"
    df$Game1_1_2[df$Game1_1_2=="٥٥"] <- "55"
    df$Game1_1_2[df$Game1_1_2=="٦٠"] <- "60"
    df$Game1_1_2[df$Game1_1_2=="٧٠"] <- "70"
    df$Game1_1_2[df$Game1_1_2=="٨٠"] <- "80"
    df$Game1_1_2[df$Game1_1_2=="١٠٠"] <- "100"
    df$Game1_1_2[df$Game1_1_2=="١٥٠"] <- "150"
    
    game2 <- data.frame( buyer = 'Stranger',
                         response = as.numeric(df$Game1_1_2),
                         market_value )
    
    if( any( is.na( as.numeric(df$Game1_1_2) ) ) ) print( "Check Game1_1_2!") 
    # ---- Game1_2_1 ---- #
    # What price would you have asked for if you found out that the ticket’s 
    # face value price was y$ instead? - If they are a friend
    
    # For Egypt
    df$Game1_2_1[df$Game1_2_1=="٠"] <- "0"
    df$Game1_2_1[df$Game1_2_1=="۰"] <- "0"
    df$Game1_2_1[df$Game1_2_1=="٠٠"] <- "0"
    df$Game1_2_1[df$Game1_2_1=="١"] <- "1"
    df$Game1_2_1[df$Game1_2_1=="٢"] <- "2"
    df$Game1_2_1[df$Game1_2_1=="١٠"] <- "10"
    df$Game1_2_1[df$Game1_2_1=="١٥"] <- "15"
    df$Game1_2_1[df$Game1_2_1=="٢٠"] <- "20"
    df$Game1_2_1[df$Game1_2_1=="٢٥"] <- "25"
    df$Game1_2_1[df$Game1_2_1=="٣٠"] <- "30"
    df$Game1_2_1[df$Game1_2_1=="۳۰"] <- "30"
    df$Game1_2_1[df$Game1_2_1=="٣٥"] <- "35"
    df$Game1_2_1[df$Game1_2_1=="٤٠"] <- "40"
    df$Game1_2_1[df$Game1_2_1=="٤٥"] <- "45"
    df$Game1_2_1[df$Game1_2_1=="٥٠"] <- "50"
    df$Game1_2_1[df$Game1_2_1=="٥٥"] <- "55"
    df$Game1_2_1[df$Game1_2_1=="٦٠"] <- "60"
    df$Game1_2_1[df$Game1_2_1=="٧٠"] <- "70"
    df$Game1_2_1[df$Game1_2_1=="٨٠"] <- "80"
    df$Game1_2_1[df$Game1_2_1=="١٠٠"] <- "100"
    df$Game1_2_1[df$Game1_2_1=="١٥٠"] <- "150"
    
    
    text_near_mv <- strsplit(text$Game1_2_1, " ")[[1]]
    mark_val <- sapply(text_near_mv, keep_character, pattern=as.character(0:9))
    market_value <- as.numeric(mark_val[mark_val!=""])
    if(Country=="Hungary") market_value<-16000
    
    game3 <- data.frame( buyer = 'Friend',
                         response = as.numeric(df$Game1_2_1),
                         market_value )
    if( any( is.na( as.numeric(df$Game1_2_1) ) ) ) print( "Check Game1_2_1!") 
    # ---- Game1_2_2 ---- #
    # What price would you have asked for if you found out that the ticket’s 
    # face value price was y$ instead? - If they are a stranger
    
    # For Egypt
    df$Game1_2_2[df$Game1_2_2=="٠"] <- "0"
    df$Game1_2_2[df$Game1_2_2=="٠٠"] <- "0"
    df$Game1_2_2[df$Game1_2_2=="١"] <- "1"
    df$Game1_2_2[df$Game1_2_2=="٢"] <- "2"
    df$Game1_2_2[df$Game1_2_2=="١٠"] <- "10"
    df$Game1_2_2[df$Game1_2_2=="١٥"] <- "15"
    df$Game1_2_2[df$Game1_2_2=="٢٠"] <- "20"
    df$Game1_2_2[df$Game1_2_2=="٢٥"] <- "25"
    df$Game1_2_2[df$Game1_2_2=="٣٠"] <- "30"
    df$Game1_2_2[df$Game1_2_2=="۳۰"] <- "30"
    df$Game1_2_2[df$Game1_2_2=="٣٥"] <- "35"
    df$Game1_2_2[df$Game1_2_2=="٤٠"] <- "40"
    df$Game1_2_2[df$Game1_2_2=="٤٥"] <- "45"
    df$Game1_2_2[df$Game1_2_2=="٥٠"] <- "50"
    df$Game1_2_2[df$Game1_2_2=="٥٥"] <- "55"
    df$Game1_2_2[df$Game1_2_2=="٦٠"] <- "60"
    df$Game1_2_2[df$Game1_2_2=="٧٠"] <- "70"
    df$Game1_2_2[df$Game1_2_2=="٨٠"] <- "80"
    df$Game1_2_2[df$Game1_2_2=="١٠٠"] <- "100"
    df$Game1_2_2[df$Game1_2_2=="١٥٠"] <- "150"
    
    game4 <- data.frame( buyer = 'Stranger',
                         response = as.numeric(df$Game1_2_2),
                         market_value )
    if( any( is.na( as.numeric(df$Game1_2_2) ) ) ) print( "Check Game1_2_2!") 
    # ---- Game2_1_1 ---- #
    # Imagine you are going to a sold-out game of your favorite local sports
    # team, and you have an extra ticket. The price marked on the ticket is 
    # x$ which is what you paid for each ticket.    
    # You want to get rid of the extra ticket and you know the ticket's face 
    # value price is x$. You find someone who wants the ticket. There is no 
    # law against charging more than the price marked on the ticket.    
    # What price do you ask for selling the extra ticket if the person you
    # sell it to...? - Is a friend
    text_near_mv <- strsplit(text$Game2_1_1, " ")[[1]][24:33]
    mark_val <- sapply(text_near_mv, keep_character, pattern=as.character(0:9))
    market_value <- as.numeric(mark_val[mark_val!=""])
    
    # For Egypt
    df$Game2_1_1[df$Game2_1_1=="۰"] <- "0"
    df$Game2_1_1[df$Game2_1_1=="٠"] <- "0"
    df$Game2_1_1[df$Game2_1_1=="٠٠"] <- "0"
    df$Game2_1_1[df$Game2_1_1=="١"] <- "1"
    df$Game2_1_1[df$Game2_1_1=="٢"] <- "2"
    df$Game2_1_1[df$Game2_1_1=="١٠"] <- "10"
    df$Game2_1_1[df$Game2_1_1=="١٥"] <- "15"
    df$Game2_1_1[df$Game2_1_1=="۱۵"] <- "15"
    df$Game2_1_1[df$Game2_1_1=="٢٠"] <- "20"
    df$Game2_1_1[df$Game2_1_1=="٢٥"] <- "25"
    df$Game2_1_1[df$Game2_1_1=="٣٠"] <- "30"
    df$Game2_1_1[df$Game2_1_1=="۳۰"] <- "30"
    df$Game2_1_1[df$Game2_1_1=="٣٥"] <- "35"
    df$Game2_1_1[df$Game2_1_1=="٤٠"] <- "40"
    df$Game2_1_1[df$Game2_1_1=="٤٥"] <- "45"
    df$Game2_1_1[df$Game2_1_1=="٥٠"] <- "50"
    df$Game2_1_1[df$Game2_1_1=="٥٥"] <- "55"
    df$Game2_1_1[df$Game2_1_1=="٦٠"] <- "60"
    df$Game2_1_1[df$Game2_1_1=="٧٠"] <- "70"
    df$Game2_1_1[df$Game2_1_1=="٨٠"] <- "80"
    df$Game2_1_1[df$Game2_1_1=="١٠٠"] <- "100"
    df$Game2_1_1[df$Game2_1_1=="١٥٠"] <- "150"
    df$Game2_1_1[df$Game2_1_1=="۱۵۰۰۰"] <- "1500"
    
    game5 <- data.frame( buyer = 'Friend',
                         response = as.numeric(df$Game2_1_1),
                         market_value )
    if( any( is.na( as.numeric(df$Game2_1_1) ) ) ) print( "Check Game2_1_1!") 
    # ---- Game2_1_2 ---- #
    # Imagine you are going to a sold-out game of your favorite local sports 
    # team, and you have an extra ticket. The price marked on the ticket is 
    # x$ which is what you paid for each ticket.    
    # You want to get rid of the extra ticket and you know the ticket's face 
    # value price is x$ You find someone who wants the ticket. 
    # There is no law against charging more than the price marked on the ticket. 
    # What price do you ask for selling the extra ticket if the person you sell 
    # it to...? - Is a stranger
    
    # For Egypt
    
    df$Game2_1_2[df$Game2_1_2=="۰"] <- "0"
    df$Game2_1_2[df$Game2_1_2=="٠"] <- "0"
    df$Game2_1_2[df$Game2_1_2=="٠٠"] <- "0"
    df$Game2_1_2[df$Game2_1_2=="١"] <- "1"
    df$Game2_1_2[df$Game2_1_2=="٢"] <- "2"
    df$Game2_1_2[df$Game2_1_2=="١٠"] <- "10"
    df$Game2_1_2[df$Game2_1_2=="١٥"] <- "15"
    df$Game2_1_2[df$Game2_1_2=="۱۵"] <- "15"
    df$Game2_1_2[df$Game2_1_2=="٢٠"] <- "20"
    df$Game2_1_2[df$Game2_1_2=="٢٥"] <- "25"
    df$Game2_1_2[df$Game2_1_2=="٣٠"] <- "30"
    df$Game2_1_2[df$Game2_1_2=="٣٥"] <- "35"
    df$Game2_1_2[df$Game2_1_2=="٤٠"] <- "40"
    df$Game2_1_2[df$Game2_1_2=="٤٥"] <- "45"
    df$Game2_1_2[df$Game2_1_2=="٥٠"] <- "50"
    df$Game2_1_2[df$Game2_1_2=="٥٥"] <- "55"
    df$Game2_1_2[df$Game2_1_2=="٦٠"] <- "60"
    df$Game2_1_2[df$Game2_1_2=="٧٠"] <- "70"
    df$Game2_1_2[df$Game2_1_2=="٨٠"] <- "80"
    df$Game2_1_2[df$Game2_1_2=="١٠٠"] <- "100"
    df$Game2_1_2[df$Game2_1_2=="١٥٠"] <- "150"
    df$Game2_1_2[df$Game2_1_2=="۲۵۰۰۰"] <-  "2500"
    
    game6 <- data.frame( buyer = 'Stranger',
                         response = as.numeric(df$Game2_1_2),
                         market_value )
    if( any( is.na( as.numeric(df$Game2_1_2) ) ) ) print( "Check Game2_1_2!") 
    # ---- Game2_2_1 ---- #
    # What price would you have asked for if you found out that the ticket’s 
    # face value price was y$ instead? - If they are a friend
    
    # For Egypt
    df$Game2_2_1[df$Game2_2_1=="۰"] <- "0"
    df$Game2_2_1[df$Game2_2_1=="٠"] <- "0"
    df$Game2_2_1[df$Game2_2_1=="٠٠"] <- "0"
    df$Game2_2_1[df$Game2_2_1=="١"] <- "1"
    df$Game2_2_1[df$Game2_2_1=="٢"] <- "2"
    df$Game2_2_1[df$Game2_2_1=="١٠"] <- "10"
    df$Game2_2_1[df$Game2_2_1=="١٥"] <- "15"
    df$Game2_2_1[df$Game2_2_1=="٢٠"] <- "20"
    df$Game2_2_1[df$Game2_2_1=="٢٥"] <- "25"
    df$Game2_2_1[df$Game2_2_1=="٣٠"] <- "30"
    df$Game2_2_1[df$Game2_2_1=="۳۰"] <- "30"
    df$Game2_2_1[df$Game2_2_1=="٣٥"] <- "35"
    df$Game2_2_1[df$Game2_2_1=="٤٠"] <- "40"
    df$Game2_2_1[df$Game2_2_1=="٤٥"] <- "45"
    df$Game2_2_1[df$Game2_2_1=="٥٠"] <- "50"
    df$Game2_2_1[df$Game2_2_1=="٥٥"] <- "55"
    df$Game2_2_1[df$Game2_2_1=="٦٠"] <- "60"
    df$Game2_2_1[df$Game2_2_1=="٧٠"] <- "70"
    df$Game2_2_1[df$Game2_2_1=="٨٠"] <- "80"
    df$Game2_2_1[df$Game2_2_1=="١٠٠"] <- "100"
    df$Game2_2_1[df$Game2_2_1=="١٥٠"] <- "150"
    df$Game2_2_1[df$Game2_2_1=="۳۰۰۰۰"] <- "30000"
    
    text_near_mv <- strsplit(text$Game2_2_1, " ")[[1]]
    mark_val <- sapply(text_near_mv, keep_character, pattern=as.character(0:9))
    market_value <- as.numeric(mark_val[mark_val!=""])
    if(Country=="Hungary") market_value<-16000
    
    if( any( is.na( as.numeric(df$Game2_2_1) ) ) ) print( "Check Game2_2_1!") 
    
    game7 <- data.frame( buyer = 'Friend',
                         response = as.numeric(df$Game2_2_1),
                         market_value )
    
    # ---- Game2_2_2 ---- #
    # What price would you have asked for if you found out that the ticket’s
    # face value price was y$ instead? - If they are a stranger
    
    # For Egypt
    df$Game2_2_2[df$Game2_2_2=="۰"] <- "0"
    df$Game2_2_2[df$Game2_2_2=="٠"] <- "0"
    df$Game2_2_2[df$Game2_2_2=="٠٠"] <- "0"
    df$Game2_2_2[df$Game2_2_2=="١"] <- "1"
    df$Game2_2_2[df$Game2_2_2=="٢"] <- "2"
    df$Game2_2_2[df$Game2_2_2=="١٠"] <- "10"
    df$Game2_2_2[df$Game2_2_2=="١٥"] <- "15"
    df$Game2_2_2[df$Game2_2_2=="۱۵"] <- "15"
    df$Game2_2_2[df$Game2_2_2=="٢٠"] <- "20"
    df$Game2_2_2[df$Game2_2_2=="٢٥"] <- "25"
    df$Game2_2_2[df$Game2_2_2=="٣٠"] <- "30"
    df$Game2_2_2[df$Game2_2_2=="۳۰"] <- "30"
    df$Game2_2_2[df$Game2_2_2=="٣٥"] <- "35"
    df$Game2_2_2[df$Game2_2_2=="٤٠"] <- "40"
    df$Game2_2_2[df$Game2_2_2=="٤٥"] <- "45"
    df$Game2_2_2[df$Game2_2_2=="٥٠"] <- "50"
    df$Game2_2_2[df$Game2_2_2=="٥٥"] <- "55"
    df$Game2_2_2[df$Game2_2_2=="٦٠"] <- "60"
    df$Game2_2_2[df$Game2_2_2=="٧٠"] <- "70"
    df$Game2_2_2[df$Game2_2_2=="٨٠"] <- "80"
    df$Game2_2_2[df$Game2_2_2=="١٠٠"] <- "100"
    df$Game2_2_2[df$Game2_2_2=="١٥٠"] <- "150"
    df$Game2_2_2[df$Game2_2_2=="۵۰۰۰۰"] <- "50000"
    df$Game2_2_2[df$Game2_2_2=="۳۰۰۰۰"] <- "30000"
    
    if( any( is.na( as.numeric(df$Game2_2_2) ) ) ) print( "Check Game2_2_2!") 
    
    game8 <- data.frame( buyer = 'Stranger',
                         response = as.numeric(df$Game2_2_2),
                         market_value )
    
    rbind( 
      cbind(dat %>% select(subject, all_of(control_variables)),game1),
      cbind(dat %>% select(subject, all_of(control_variables)),game2),
      cbind(dat %>% select(subject, all_of(control_variables)),game3),
      cbind(dat %>% select(subject, all_of(control_variables)),game4),
      cbind(dat %>% select(subject, all_of(control_variables)),game5),
      cbind(dat %>% select(subject, all_of(control_variables)),game6),
      cbind(dat %>% select(subject, all_of(control_variables)),game7),
      cbind(dat %>% select(subject, all_of(control_variables)),game8)
    ) %>% mutate(response = ifelse(response >= market_value, 1, 0))
    
    
  })

  # ------ Create Numeric Income ------ #
  data_Game$numeric_income <- NA
  print("Converting Income into numbers...")
  for( sbj in unique(data_Game$subject) ){
    data_Game[data_Game$subject==sbj, "numeric_income"] <- 
      unique(data_MrAB[data_MrAB$subject==sbj, "numeric_income"])
  }
  
  write.csv(data_Game, file = "data_Game.csv")
  
  # Effect found in the original paperå
  data_paper <- rbind(
    # Buyer
    rbind(
      # Market Value = 5
      data.frame(
        market_value = 5, buyer = 'Friend',
        response = c(rep(0, round(31*0.68)), rep(5, round(31*0.26)), rep(10, round(31*0.03)))
      ),
      data.frame(
        market_value = 5, buyer = 'Friend',
        response = c(rep(0, round(28*0.14)), rep(5, round(28*0.79)), rep(10, round(28*0.0)))
      ),
      data.frame(
        market_value = 5, buyer = 'Friend',
        response = c(rep(0, round(26*0.0)), rep(5, round(26*0.69)), rep(10, round(26*0.23)))
      ),
      # Market Value = 10
      data.frame(
        market_value = 10, buyer = 'Friend',
        response = c(rep(0, round(31*0.66)), rep(5, round(31*0.26)), rep(10, round(31*0.06)))
      ),
      data.frame(
        market_value = 10, buyer = 'Friend',
        response = c(rep(0, round(28*0.07)), rep(5, round(28*0.79)), rep(10, round(28*0.04)))
      ),
      data.frame(
        market_value = 10, buyer = 'Friend',
        response = c(rep(0, round(26*0.0)), rep(5, round(26*0.15)), rep(10, round(26*0.69)))
      )
    ),
    # Stranger
    rbind(
      # Market Value = 5
      data.frame(
        market_value = 5, buyer = 'Stranger',
        response = c(rep(0, round(31*0.06)), rep(5, round(31*0.77)), rep(10, round(31*0.10)))
      ),
      data.frame(
        market_value = 5, buyer = 'Stranger',
        response = c(rep(0, round(28*0.0)), rep(5, round(28*0.79)), rep(10, round(28*0.07)))
      ),
      data.frame(
        market_value = 5, buyer = 'Stranger',
        response = c(rep(0, round(26*0.0)), rep(5, round(26*0.42)), rep(10, round(26*0.46)))
      ),
      # Market Value = 10
      data.frame(
        market_value = 10, buyer = 'Stranger',
        response = c(rep(0, round(31*0.06)), rep(5, round(31*0.16)), rep(10, round(31*0.58)))
      ),
      data.frame(
        market_value = 10, buyer = 'Stranger',
        response = c(rep(0, round(28*0.0)), rep(5, round(28*0.14)), rep(10, round(28*0.57)))
      ),
      data.frame(
        market_value = 10, buyer = 'Stranger',
        response = c(rep(0, round(26*0.0)), rep(5, round(26*0.0)), rep(10, round(26*0.73)))
      )
    )
  ) %>% 
    mutate(response = ifelse(response >= market_value, 1, 0)) %>% 
    # Calculate ODD RATIO
    group_by(buyer, response) %>%
    mutate(n = n()) %>% filter(row_number()==1) %>%
    group_by(buyer) %>%
    mutate(Odds = n[response==1]/n[response==0]) %>%
    group_by(buyer) %>%
    # filter(Country=="Austria", scenario_group=="gain") %>%
    mutate(se_theta = sqrt(sum(1/n))) %>%
    filter(row_number()==1) %>% select(-response) %>%
    ungroup() %>%
    mutate(OR = Odds[buyer=="Stranger"]/Odds[buyer=="Friend"],
           theta = log(OR)) %>%
    select(-c(OR, Odds, n)) %>% ungroup()
  
  data_paper_Game <- data_paper %>% 
    mutate(x=3, study="Game", family="binomial") %>% 
    select(-c(market_value, buyer, se_theta))
  
  # ggplot(data_Game_theta, aes(1, theta, color=Country)) +
  #   geom_jitter( aes(size=1/se_theta), width = 0.1, alpha=0.7) +
  #   geom_point(data = data_paper, color="firebrick", size=35, shape="-") +
  #   geom_hline(yintercept = 0, linetype=2) +
  #   theme_pubr() + theme(legend.position = "right") +
  #   labs(x=NULL, y=expression(theta)) +
  #   scale_color_manual(values = viridis::viridis_pal()(length(unique(data_MrAB$Country)))) +
  #   scale_y_continuous(guide = "prism_offset") +
  #   scale_size(range = c(3, 8)) +
  #   scale_x_continuous(limits = c(0.6,1.4), breaks =c(0.6,1,1.4),guide = "prism_offset") + 
  #   guides(size = "none") + 
  #   theme(text = element_text(size = 20), legend.position = "none",
  #         axis.text.x=element_blank())
}

################################################################
# -------------------------- Drink --------------------------- #
################################################################

Drink <- TRUE
if( Drink ){
  data_Drink <- map_dfr(Countries, function(Country){
    
    print(paste("Processing", Country))
    # Link for Austria
    lCountry <- Links[Countries==Country]
    dat <- read.csv(lCountry) %>% mutate(subject=ResponseId, Country, NativeLanguage=Native.language)
    dat$native_language_is_country_language = dat$NativeLanguage%in%NativeLanguageList[[Country]]
    
    if(Country=="Russia"){
      dat$Residence <- with( dat, ifelse(Residence=='Russian Federation', Country, Residence) )
    } else if( Country=="UK" ){
      dat$Residence <- with( dat, ifelse(Residence=='United Kingdom of Great Britain and Northern Ireland', Country, Residence) )
    } else if( Country=="USA" ){
      dat$Residence <- with( dat, ifelse(Residence=='United States of America', Country, Residence) )
    } else if( Country=="Vietnam" ){
      dat$Residence <- with( dat, ifelse(Residence=='Viet Nam', Country, Residence) )
    } 
    
    # Remove first two rows. The first row indicate text presented, the second 
    # contains Qualtrics relevant variables (e.g. {"ImportId":"QID15_1"})
    dat <- dat[3:nrow(dat),]
    
    # Filter subject that finish the experiment and allow to 
    # use the responses provided in the study
    allow <- "I allow you to use the responses I provided in this study."
    dat <- dat %>% filter(Debriefing==allow) %>% select(-Debriefing)
    
    names(dat) <- sapply(names(dat), remove_character, pattern=".")
    
    # Create columns indicating attention check
    acg3 <- c("Absolutely serious", "Very serious")
    acg2 <- c("Absolutely serious", "Very serious", "Moderately serious")
    
    dat <- dat %>% mutate(
      attention_check_grater_than_3=ifelse(Att_check%in%acg3, T, F),
      attention_check_grater_than_2=ifelse(Att_check%in%acg2, T, F)
    ) %>% select(-Att_check) %>% 
      #Create column indicating whether LOI (Length of Interview) of each subjectis lower
      # than a third of the median value of LOI 
      mutate(LOI=as.numeric(Durationinseconds),
             loi_lower_than_loiX0_33 = LOI < median( LOI )*0.33) %>% 
      select(-LOI)  %>% 
      # Calculate score Financial Literacy
      mutate(fin_lit1 = ifelse(str_detect(fin_lit1, "More than"), 1, 0),
             fin_lit2 = ifelse(str_detect(fin_lit2, "Less than today"), 1, 0),
             fin_lit3 = ifelse(str_detect(fin_lit3, "False"), 1, 0),
             fin_lit4 = ifelse(str_detect(fin_lit4, "They collapse"), 1, 0),
             FinancialLiteracy =  fin_lit1+fin_lit2+fin_lit3+fin_lit4)
    
    
    
    # Create Drink dataseta
    dat1 <- dat %>%
      select(subject, contains("Drink", ignore.case = FALSE), all_of(control_variables)) %>%
      reshape2::melt(id.var=c('subject', control_variables),
           variable.name='store',
           value.name='response')
    
    # Convert Numbers For Egypt and Iran
    persian_position <- is.na(as.numeric(dat1$response)) %>% which()
    
    for( i in persian_position ){
      not_collapsed <- sapply(strsplit(dat1$response[i], "")[[1]], 
                              convert_farsi_to_arabic)
      dat1$response[i] <- paste(not_collapsed, collapse = "")
    }
    
    if( any( is.na(as.numeric(dat1$response)) ) ) print( paste("Check", Country))
    
    dat1 %>% 
      mutate(store=ifelse(store=='Drink1', 'Resort Hotel', 'Grocery Store'),
             response=as.numeric(response))
  })
  
  # The 2 warnings: In which(.) : NAs introduced by coercion are fine
  # They indicate the non-Arabic numbers coerced in NAs
  
  data_Drink$numeric_income <- NA
  print("Converting Income into numbers...")
  for( sbj in unique(data_Drink$subject) ){
    data_Drink[data_Drink$subject==sbj, "numeric_income"] <- 
      unique(data_MrAB[data_MrAB$subject==sbj, "numeric_income"])
  }
  
  
  write.csv(data_Drink, file = "data_Drink.csv")
}

################################################################
# ------------------------- Jacket --------------------------- #
################################################################

Jacket <- TRUE
if( Jacket ){
  data_Jacket <- map_dfr(Countries, function(Country){
    
    print(paste("Processing", Country))
    # Link for Austria
    lCountry <- Links[Countries==Country]
    dat <- read.csv(lCountry) %>% mutate(subject=ResponseId, Country, NativeLanguage=Native.language)
    dat$native_language_is_country_language = dat$NativeLanguage%in%NativeLanguageList[[Country]]
    
    if(Country=="Russia"){
      dat$Residence <- with( dat, ifelse(Residence=='Russian Federation', Country, Residence) )
    } else if( Country=="UK" ){
      dat$Residence <- with( dat, ifelse(Residence=='United Kingdom of Great Britain and Northern Ireland', Country, Residence) )
    } else if( Country=="USA" ){
      dat$Residence <- with( dat, ifelse(Residence=='United States of America', Country, Residence) )
    } else if( Country=="Vietnam" ){
      dat$Residence <- with( dat, ifelse(Residence=='Viet Nam', Country, Residence) )
    } 
    
    # Remove first two rows. The first row indicate text presented, the second 
    # contains Qualtrics relevant variables (e.g. {"ImportId":"QID15_1"})
    dat <- dat[3:nrow(dat),]
    
    # Filter subject that finish the experiment and allow to 
    # use the responses provided in the study
    allow <- "I allow you to use the responses I provided in this study."
    dat <- dat %>% filter(Debriefing==allow) %>% select(-Debriefing)
    
    names(dat) <- sapply(names(dat), remove_character, pattern=".")
    
    # Create columns indicating attention check
    acg3 <- c("Absolutely serious", "Very serious")
    acg2 <- c("Absolutely serious", "Very serious", "Moderately serious")
    
    dat <- dat %>% mutate(
      attention_check_grater_than_3=ifelse(Att_check%in%acg3, T, F),
      attention_check_grater_than_2=ifelse(Att_check%in%acg2, T, F)
    ) %>% select(-Att_check) %>% 
      #Create column indicating whether LOI (Length of Interview) of each subjectis lower
      # than a third of the median value of LOI 
      mutate(LOI=as.numeric(Durationinseconds),
             loi_lower_than_loiX0_33 = LOI < median( LOI )*0.33) %>% 
      select(-LOI) %>% 
      # Calculate score Financial Literacy
      mutate(fin_lit1 = ifelse(str_detect(fin_lit1, "More than"), 1, 0),
             fin_lit2 = ifelse(str_detect(fin_lit2, "Less than today"), 1, 0),
             fin_lit3 = ifelse(str_detect(fin_lit3, "False"), 1, 0),
             fin_lit4 = ifelse(str_detect(fin_lit4, "They collapse"), 1, 0),
             FinancialLiteracy =  fin_lit1+fin_lit2+fin_lit3+fin_lit4)
    
    
    
    # Create Jacket dataseta
    dat %>%
      select(subject, contains("Jacket", ignore.case = FALSE), all_of(control_variables)) %>%
      reshape2::melt(id.var=c('subject', control_variables),
                     variable.name='price',
                     value.name='response') %>% 
      mutate(price=ifelse(price=="Jacket1", "low", "high"),
             response=ifelse(response=="Yes", 1, 0))
    
  })
  
  # ------ Create Numeric Income ------ #
  data_Jacket$numeric_income <- NA
  print("Converting Income into numbers...")
  for( sbj in unique(data_Jacket$subject) ){
    data_Jacket[data_Jacket$subject==sbj, "numeric_income"] <- 
      unique(data_MrAB[data_MrAB$subject==sbj, "numeric_income"])
  }
  
  write.csv(data_Jacket, file = "data_Jacket.csv")
  
  # Paper: Kahneman, D., & Tversky, A. (1984). Choices, values, and frames. 
  # American Psychologist, 39(4), 341–350. https://doi.org/10.1037/0003-066X.39.4.341

  n_sbj = 88
  a     = round(n_sbj*0.68)
  b     = n_sbj-a
  odds1 = a/b
  n_sbj = 93
  c     = round(n_sbj*0.29)
  d     = n_sbj-c
  odds2 = c/d
  OR = odds1/odds2
  theta = log(OR)
  
  data_paper_Jacket <- data.frame(
    x=4, study="Jacket", family="binomial", theta
  )
  
  # ggplot(data_Jacket_theta, aes(1, theta, color=Country)) +
  #   geom_jitter( aes(size=1/se_theta), width = 0.1, alpha=0.7) +
  #   # geom_point(data = data_paper, color="firebrick", size=35, shape="-") +
  #   geom_hline(yintercept = 0, linetype=2) +
  #   theme_pubr() + theme(legend.position = "right") +
  #   labs(x=NULL, y=expression(theta)) +
  #   scale_color_manual(values = viridis::viridis_pal()(length(unique(data_MrAB$Country)))) +
  #   scale_y_continuous(guide = "prism_offset") + 
  #   scale_size(range = c(3, 8)) +
  #   scale_x_continuous(limits = c(0.6,1.4), breaks =c(0.6,1,1.4),guide = "prism_offset") + 
  #   guides(size = "none") + 
  #   theme(text = element_text(size = 20), legend.position = "none",
  #         axis.text.x=element_blank())
}

################################################################
# -------------------------- Play ---------------------------- #
################################################################

Play <- TRUE
if( Play ){
  data_Play <- map_dfr(Countries, function(Country){
    
    print(paste("Processing", Country))
    # Link for Austria
    lCountry <- Links[Countries==Country]
    dat <- read.csv(lCountry) %>% mutate(subject=ResponseId, Country, NativeLanguage=Native.language)
    dat$native_language_is_country_language = dat$NativeLanguage%in%NativeLanguageList[[Country]]
    
    if(Country=="Russia"){
      dat$Residence <- with( dat, ifelse(Residence=='Russian Federation', Country, Residence) )
    } else if( Country=="UK" ){
      dat$Residence <- with( dat, ifelse(Residence=='United Kingdom of Great Britain and Northern Ireland', Country, Residence) )
    } else if( Country=="USA" ){
      dat$Residence <- with( dat, ifelse(Residence=='United States of America', Country, Residence) )
    } else if( Country=="Vietnam" ){
      dat$Residence <- with( dat, ifelse(Residence=='Viet Nam', Country, Residence) )
    } 
    
    # Remove first two rows. The first row indicate text presented, the second 
    # contains Qualtrics relevant variables (e.g. {"ImportId":"QID15_1"})
    dat <- dat[3:nrow(dat),]
    
    # Filter subject that finish the experiment and allow to 
    # use the responses provided in the study
    allow <- "I allow you to use the responses I provided in this study."
    dat <- dat %>% filter(Debriefing==allow) %>% select(-Debriefing)
    
    names(dat) <- sapply(names(dat), remove_character, pattern=".")
    
    # Create columns indicating attention check
    acg3 <- c("Absolutely serious", "Very serious")
    acg2 <- c("Absolutely serious", "Very serious", "Moderately serious")
    
    dat <- dat %>% mutate(
      attention_check_grater_than_3=ifelse(Att_check%in%acg3, T, F),
      attention_check_grater_than_2=ifelse(Att_check%in%acg2, T, F)
    ) %>% select(-Att_check) %>% 
      #Create column indicating whether LOI (Length of Interview) of each subjectis lower
      # than a third of the median value of LOI 
      mutate(LOI=as.numeric(Durationinseconds),
             loi_lower_than_loiX0_33 = LOI < median( LOI )*0.33) %>% 
      select(-LOI) %>% 
      # Calculate score Financial Literacy
      mutate(fin_lit1 = ifelse(str_detect(fin_lit1, "More than"), 1, 0),
             fin_lit2 = ifelse(str_detect(fin_lit2, "Less than today"), 1, 0),
             fin_lit3 = ifelse(str_detect(fin_lit3, "False"), 1, 0),
             fin_lit4 = ifelse(str_detect(fin_lit4, "They collapse"), 1, 0),
             FinancialLiteracy =  fin_lit1+fin_lit2+fin_lit3+fin_lit4)
    
    # Create Play data set
    dat %>%
      select(subject, contains("Play", ignore.case = FALSE), all_of(control_variables)) %>%
      reshape2::melt(id.var=c('subject', control_variables),
                     variable.name='loss',
                     value.name='response') %>% 
      mutate(loss=ifelse(loss=="Play1", "ticket", "cash"),
             response=ifelse(response=="Yes", 1, 0))
    
  })
  
  # ------ Create Numeric Income ------ #
  data_Play$numeric_income <- NA
  print("Converting Income into numbers...")
  for( sbj in unique(data_Play$subject) ){
    data_Play[data_Play$subject==sbj, "numeric_income"] <- 
      unique(data_MrAB[data_MrAB$subject==sbj, "numeric_income"])
  }
  
  
  write.csv(data_Play, file = "data_Play.csv")
  
  # Tversky, A., & Kahneman, D. (1981). The framing of decisions and the psychology of choice. 
  # Science, 211(4481), 453–458. https://doi.org/10.1126/science.7455683
  n_sbj = 183
  a     = round(n_sbj*0.88)
  b     = n_sbj-a
  odds1 = a/b
  n_sbj = 200
  c     = round(n_sbj*0.46)
  d     = n_sbj-c
  odds2 = c/d
  OR = odds1/odds2
  theta = log(OR)
  
  data_paper_Play <- data.frame(
    x=5, study="Play", family="binomial", theta
  )
}

################################################################
# -------------------------- Gym ----------------------------- #
################################################################

Gym <- TRUE
if( Gym ){
  data_Gym <- map_dfr(Countries, function(Country){
    
    print(paste("Processing", Country))
    # Link for Austria
    lCountry <- Links[Countries==Country]
    dat <- read.csv(lCountry) %>% mutate(subject=ResponseId, Country, NativeLanguage=Native.language)
    dat$native_language_is_country_language = dat$NativeLanguage%in%NativeLanguageList[[Country]]
    
    if(Country=="Russia"){
      dat$Residence <- with( dat, ifelse(Residence=='Russian Federation', Country, Residence) )
    } else if( Country=="UK" ){
      dat$Residence <- with( dat, ifelse(Residence=='United Kingdom of Great Britain and Northern Ireland', Country, Residence) )
    } else if( Country=="USA" ){
      dat$Residence <- with( dat, ifelse(Residence=='United States of America', Country, Residence) )
    } else if( Country=="Vietnam" ){
      dat$Residence <- with( dat, ifelse(Residence=='Viet Nam', Country, Residence) )
    } 
    
    # Remove first two rows. The first row indicate text presented, the second 
    # contains Qualtrics relevant variables (e.g. {"ImportId":"QID15_1"})
    dat <- dat[3:nrow(dat),]
    
    # Filter subject that finish the experiment and allow to 
    # use the responses provided in the study
    allow <- "I allow you to use the responses I provided in this study."
    dat <- dat %>% filter(Debriefing==allow) %>% select(-Debriefing)
    
    names(dat) <- sapply(names(dat), remove_character, pattern=".")
    
    # Create columns indicating attention check
    acg3 <- c("Absolutely serious", "Very serious")
    acg2 <- c("Absolutely serious", "Very serious", "Moderately serious")
    
    dat <- dat %>% mutate(
      attention_check_grater_than_3=ifelse(Att_check%in%acg3, T, F),
      attention_check_grater_than_2=ifelse(Att_check%in%acg2, T, F)
    ) %>% select(-Att_check) %>% 
      #Create column indicating whether LOI (Length of Interview) of each subjectis lower
      # than a third of the median value of LOI 
      mutate(LOI=as.numeric(Durationinseconds),
             loi_lower_than_loiX0_33 = LOI < median( LOI )*0.33) %>% 
      select(-LOI) %>% 
      # Calculate score Financial Literacy
      mutate(fin_lit1 = ifelse(str_detect(fin_lit1, "More than"), 1, 0),
             fin_lit2 = ifelse(str_detect(fin_lit2, "Less than today"), 1, 0),
             fin_lit3 = ifelse(str_detect(fin_lit3, "False"), 1, 0),
             fin_lit4 = ifelse(str_detect(fin_lit4, "They collapse"), 1, 0),
             FinancialLiteracy =  fin_lit1+fin_lit2+fin_lit3+fin_lit4)
    
    # Create Play data set
    dat %>%
      select(subject, contains("Gym1_1", ignore.case = FALSE), 
             contains("Gym2_1", ignore.case = FALSE), all_of(control_variables)) %>%
      
      reshape2::melt(id.var=c('subject', control_variables),
                     variable.name='frame',
                     value.name='response') %>% 
      
      mutate(frame=ifelse(frame=="Gym1_1", "Per-session", "Yearly"),
             response=ifelse(grepl("Strongly agree", response), "1", response),
             response=ifelse(grepl("Strongly disagree", response), "5", response),
             response=as.numeric(response))
    })
  
  # ------ Create Numeric Income ------ #
  data_Gym$numeric_income <- NA
  print("Converting Income into numbers...")
  for( sbj in unique(data_Gym$subject) ){
    data_Gym[data_Gym$subject==sbj, "numeric_income"] <- 
      unique(data_MrAB[data_MrAB$subject==sbj, "numeric_income"])
  }
  
  write.csv(data_Gym, file = "data_Gym.csv")
}

################################################################
# -------------------------- Plane ---------------------------- #
################################################################

Plane <- TRUE
if( Plane ){
  data_Plane <- map_dfr(Countries, function(Country){
    
    print(paste("Processing", Country))
    # Link for Austria
    lCountry <- Links[Countries==Country]
    dat <- read.csv(lCountry) %>% mutate(subject=ResponseId, Country, NativeLanguage=Native.language)
    dat$native_language_is_country_language = dat$NativeLanguage%in%NativeLanguageList[[Country]]
    
    if(Country=="Russia"){
      dat$Residence <- with( dat, ifelse(Residence=='Russian Federation', Country, Residence) )
    } else if( Country=="UK" ){
      dat$Residence <- with( dat, ifelse(Residence=='United Kingdom of Great Britain and Northern Ireland', Country, Residence) )
    } else if( Country=="USA" ){
      dat$Residence <- with( dat, ifelse(Residence=='United States of America', Country, Residence) )
    } else if( Country=="Vietnam" ){
      dat$Residence <- with( dat, ifelse(Residence=='Viet Nam', Country, Residence) )
    } 
    
    # Remove first two rows. The first row indicate text presented, the second 
    # contains Qualtrics relevant variables (e.g. {"ImportId":"QID15_1"})
    dat <- dat[3:nrow(dat),]
    
    # Filter subject that finish the experiment and allow to 
    # use the responses provided in the study
    allow <- "I allow you to use the responses I provided in this study."
    dat <- dat %>% filter(Debriefing==allow) %>% select(-Debriefing)
    
    names(dat) <- sapply(names(dat), remove_character, pattern=".")
    
    # Create columns indicating attention check
    acg3 <- c("Absolutely serious", "Very serious")
    acg2 <- c("Absolutely serious", "Very serious", "Moderately serious")
    
    dat <- dat %>% mutate(
      attention_check_grater_than_3=ifelse(Att_check%in%acg3, T, F),
      attention_check_grater_than_2=ifelse(Att_check%in%acg2, T, F)
    ) %>% select(-Att_check) %>% 
      #Create column indicating whether LOI (Length of Interview) of each subjectis lower
      # than a third of the median value of LOI 
      mutate(LOI=as.numeric(Durationinseconds),
             loi_lower_than_loiX0_33 = LOI < median( LOI )*0.33) %>% 
      select(-LOI) %>% 
      # Calculate score Financial Literacy
      mutate(fin_lit1 = ifelse(str_detect(fin_lit1, "More than"), 1, 0),
             fin_lit2 = ifelse(str_detect(fin_lit2, "Less than today"), 1, 0),
             fin_lit3 = ifelse(str_detect(fin_lit3, "False"), 1, 0),
             fin_lit4 = ifelse(str_detect(fin_lit4, "They collapse"), 1, 0),
             FinancialLiteracy =  fin_lit1+fin_lit2+fin_lit3+fin_lit4)
    
    
    
    answers = c(
      'Pay your friend',
      'Pay some, but not',
      'Consider it a gift'
    )
    
    # Create Play data set
    dat <- dat %>%
      select(subject, contains("Plane", ignore.case = FALSE), all_of(control_variables))
    dat$Plane1[grepl(answers[1],dat$Plane1)] <- "1"
    dat$Plane1[grepl(answers[2],dat$Plane1)] <- "0"
    dat$Plane1[grepl(answers[3],dat$Plane1)] <- "0"
    dat$Plane2[grepl(answers[1],dat$Plane2)] <- "1"
    dat$Plane2[grepl(answers[2],dat$Plane2)] <- "0"
    dat$Plane2[grepl(answers[3],dat$Plane2)] <- "0"
    
    dat %>% 
      rename(purchased = Plane1, 
             free = Plane2) %>% 
      reshape2::melt(id.var=c('subject', control_variables),
                     variable.name='coupon',
                     value.name='response') %>% 
      mutate(response=as.numeric(response))
    
  })
  
  # ------ Create Numeric Income ------ #
  data_Plane$numeric_income <- NA
  print("Converting Income into numbers...")
  for( sbj in unique(data_Plane$subject) ){
    data_Plane[data_Plane$subject==sbj, "numeric_income"] <- 
      unique(data_MrAB[data_MrAB$subject==sbj, "numeric_income"])
  }
  
  write.csv(data_Plane, file = "data_Plane.csv")
  
  # Shafir, E., & Thaler, R. H. (2006). Invest now, drink later, 
  # spend never: On the mental accounting of delayed consumption. 
  # Journal of economic psychology, 27(5), 694-712.
  n_sbj = 57
  a     = round(n_sbj*0.25)
  b     = n_sbj-a
  odds1 = a/b
  n_sbj = 58
  c     = round(n_sbj*0.05)
  d     = n_sbj-c
  odds2 = c/d
  OR = odds1/odds2
  theta = log(OR)
  
  data_paper_Plane <- data.frame(
    x=6, study="Plane", family="binomial", theta
  )
}

################################################################
# ----------- Effect Size of the original papers ------------- #
################################################################

original_theta <- list(
  MrAB = data_paper_MrAB,
  Game = data_paper_Game,
  Jacket = data_paper_Jacket,
  Play = data_paper_Play,
  Plane = data_paper_Plane
)

saveRDS(original_theta, file = "original_theta.rds")


######################################################################################
# ----------- Cleaned data set using Preregistered exclusion criterion ------------- #
######################################################################################

countries2remove <- data_MrAB %>% 
  filter(attention_check_grater_than_3) %>% 
  group_by(subject) %>% 
  filter(row_number()==1) %>% 
  group_by(Country) %>% 
  summarise(sample_size=n()) %>% 
  filter(sample_size<250) %>% 
  .[,"Country", drop=TRUE]

# ---------------------- Mr. A vs Mr. B ---------------------- #
dMrAB <- data_MrAB %>% filter(response!=2) %>%
  # filter(scenario_group=="gain") %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  # mutate(scenario=factor(scenario, levels = c("gain-loss VS gain", "gain-gain VS gain") )) %>% 
  select(subject, Gender, Age, Income, numeric_income, Education, Country, FinancialLiteracy, 
         condition=scenario, response) 

write.csv(dMrAB, file = "./Preregistered/MrAB.csv", row.names = FALSE)

# ------------------- The Sold Out Ticket ------------------- #
dGame <- data_Game %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  select(subject, Gender, Age, Income, numeric_income, Education, Country, FinancialLiteracy, 
         condition=buyer, response) 

write.csv(dGame, file = "./Preregistered/Game.csv", row.names = FALSE)

# -------------------------- Drink --------------------------- #
dDrink <- data_Drink %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  # Remove really really extreme outliers
  filter(response<10000 & response>=0) %>% 
  mutate(response=response+1, logResp=log(response)) %>% 
  group_by(Country) %>%
  mutate(response=as.vector(scale(logResp))) %>% 
  ungroup() %>% 
  select(subject, Gender, Age, Income, numeric_income, Education, Country, FinancialLiteracy, 
         condition=store, response)

write.csv(dDrink, file = "./Preregistered/Drink.csv", row.names = FALSE)

# ------------------------- Jacket --------------------------- #
dJacket <- data_Jacket %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  select(subject, Gender, Age, Income, numeric_income, Education, Country, FinancialLiteracy, 
         condition=price, response)

write.csv(dJacket, file = "./Preregistered/Jacket.csv", row.names = FALSE)

# -------------------------- Play ---------------------------- #
dPlay <- data_Play %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  mutate(loss=factor(loss, levels = c("ticket", "cash"))) %>% 
  select(subject, Gender, Age, Income, numeric_income, Education, Country, FinancialLiteracy, 
         condition=loss, response)

write.csv(dPlay, file = "./Preregistered/Play.csv", row.names = FALSE)

# -------------------------- Gym ----------------------------- #
dGym <- data_Gym %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  group_by(Country) %>%
  mutate(response=as.vector(scale(response))) %>% 
  ungroup() %>% 
  select(subject, Gender, Age, Income, numeric_income, Education, Country, FinancialLiteracy, 
         condition=frame, response)

write.csv(dGym, file = "./Preregistered/Gym.csv", row.names = FALSE)

# -------------------------- Plane ---------------------------- #
dPlane <- data_Plane %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  select(subject, Gender, Age, Income, numeric_income, Education, Country, FinancialLiteracy, 
         condition=coupon, response)

write.csv(dPlane, file = "./Preregistered/Plane.csv", row.names = FALSE)

# -------------------------- All ---------------------------- #

MrAB <- read.csv("./Preregistered/MrAB.csv")
Game <- read.csv("./Preregistered/Game.csv")
Drink <- read.csv("./Preregistered/Drink.csv")
Jacket <- read.csv("./Preregistered/Jacket.csv")
Play <- read.csv("./Preregistered/Play.csv")
Gym <- read.csv("./Preregistered/Gym.csv")
Plane <- read.csv("./Preregistered/Plane.csv")

All <- rbind(
  MrAB %>% mutate(study="MrAB"),
  Game %>% mutate(study="Game"),
  Drink %>% mutate(study="Drink"),
  Jacket %>% mutate(study="Jacket"),
  Play %>% mutate(study="Play"),
  Gym %>% mutate(study="Gym"),
  Plane %>% mutate(study="Plane")
)

write.csv(All, file = "./Preregistered/All.csv", row.names = FALSE)
