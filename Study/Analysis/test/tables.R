library(knitr)
library(kableExtra)
library(tidyverse)

# --- Table Demographics --- #
lazyLoad("index_cache/html/Code-to-load-data-and-libraries_ac2dcc77804d4e19a61ad58c57f9e0c7")

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



data_table <- rbind(
  df %>% 
    mutate(Age=as.numeric(Age)) %>% 
    filter(Age>0 & Age<99) %>% 
    summarise(
      Country="Pooled",
      Language="",
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
  mutate(
    Language=ifelse(Country=="Canada", "English, French", Language),
    Language=ifelse(Country=="India", "Hindi, Tamil, English", Language)
    )



data_table %>%
  kable(caption="<b>Table 1 | </b> Demographics", align=rep('l', 5),
        table.attr = "style='width:40%;'", booktabs = T) %>%
  kable_classic(html_font = "Cambria") %>%
  # kable_material(c("striped", "hover")) %>%
  row_spec(0, bold = TRUE) %>% 
  save_kable("tables/png/Demographics.png", zoom = 3)

data_table %>%
  kable(caption="<b>Table 1 | </b> Demographics", align=rep('l', 5),
        table.attr = "style='width:40%;'", booktabs = T) %>%
  kable_classic(html_font = "Cambria") %>%
  # kable_material(c("striped", "hover")) %>%
  row_spec(0, bold = TRUE) %>% 
  save_kable("tables/png/Demographics.png", zoom = 3)


