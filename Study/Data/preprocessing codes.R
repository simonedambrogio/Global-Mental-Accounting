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
    dat %>%
      select(subject, contains("MrAB", ignore.case = FALSE), all_of(control_variables)) %>%
      # Recode response
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
  select(subject, Gender, Age, Income, Education, Country, FinancialLiteracy, 
         condition=scenario, response)

write.csv(dMrAB, file = "./Preregistered/MrAB.csv", row.names = FALSE)

# ------------------- The Sold Out Ticket ------------------- #
dGame <- data_Game %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  select(subject, Gender, Age, Income, Education, Country, FinancialLiteracy, 
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
  select(subject, Gender, Age, Income, Education, Country, FinancialLiteracy, 
         condition=store, response)

write.csv(dDrink, file = "./Preregistered/Drink.csv", row.names = FALSE)

# ------------------------- Jacket --------------------------- #
dJacket <- data_Jacket %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  select(subject, Gender, Age, Income, Education, Country, FinancialLiteracy, 
         condition=price, response)

write.csv(dJacket, file = "./Preregistered/Jacket.csv", row.names = FALSE)

# -------------------------- Play ---------------------------- #
dPlay <- data_Play %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  mutate(loss=factor(loss, levels = c("ticket", "cash"))) %>% 
  select(subject, Gender, Age, Income, Education, Country, FinancialLiteracy, 
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
  select(subject, Gender, Age, Income, Education, Country, FinancialLiteracy, 
         condition=frame, response)

write.csv(dGym, file = "./Preregistered/Gym.csv", row.names = FALSE)

# -------------------------- Plane ---------------------------- #
dPlane <- data_Plane %>% 
  # EXCLUSION: Full Exclusion
  filter( !(Country %in% countries2remove) ) %>% 
  filter( attention_check_grater_than_3 ) %>% 
  select(subject, Gender, Age, Income, Education, Country, FinancialLiteracy, 
         condition=coupon, response)

write.csv(dPlane, file = "./Preregistered/Plane.csv", row.names = FALSE)



