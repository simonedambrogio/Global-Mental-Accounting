library(dplyr); library(purrr); library(stringr)
library(ggplot2); library(ggpubr)

dat <- read.csv('Pilot/Long Version/Data/multicultural_full_nested_April+13,+2022_11.32.csv')
# scenario <- dat[1,]
# dat <- dat[3:nrow(dat),]

# Start to remove useless columns
dat <- dat[, -(1:19)]
# scenario <- scenario[, -(1:19)]

# Remove time_.._Last.Click column
idx      <- str_detect(string = names(dat), pattern = "Last.Click")
dat      <- dat[,!idx]

# Remove time_.._Page.Submit column
idx      <- str_detect(string = names(dat), pattern = "Page.Submit")
dat      <- dat[,!idx]

# Remove time_.._Click.Count column
idx      <- str_detect(string = names(dat), pattern = "Click.Count")
dat      <- dat[,!idx]

data_long <- list(data=dat)

saveRDS(data_long, file = 'Pilot/Long Version/Data/data_long.rds')
