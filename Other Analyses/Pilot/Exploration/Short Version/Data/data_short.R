library(dplyr); library(purrr); library(stringr)
library(ggplot2); library(ggpubr)

dat <- read.csv('multicultural_full_nested_ShortForm_April+13,+2022_08.59.csv')
scenario <- dat[1,]
dat <- dat[3:nrow(dat),]

# Start to remove useless columns
dat <- dat[, -(1:19)]
scenario <- scenario[, -(1:19)]

# Remove time_.._Last.Click column
idx      <- str_detect(string = names(dat), pattern = "Last.Click")
dat      <- dat[,!idx]
scenario <- scenario[,!idx]

# Remove time_.._Page.Submit column
idx      <- str_detect(string = names(dat), pattern = "Page.Submit")
dat      <- dat[,!idx]
scenario <- scenario[,!idx]

# Remove time_.._Click.Count column
idx      <- str_detect(string = names(dat), pattern = "Click.Count")
dat      <- dat[,!idx]
scenario <- scenario[,!idx]

data_short <- list(data=dat, question=scenario)

saveRDS(data_short, file = 'data_short.rds')
