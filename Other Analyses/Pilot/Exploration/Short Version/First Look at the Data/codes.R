# Load libraries
library(dplyr); library(purrr); library(stringr)
library(ggplot2); library(ggpubr); library(jcolors)
library(cowplot)

# Load data
data_short <- readRDS('Data/data_short.rds')$data
questions  <- readRDS('Data/data_short.rds')$question

# ----- Mental Accounting ----- #
# Remove final questionnaires
data_short_MA <- data_short[, -(48:75)]
# Remove response time
idx_time      <- str_detect( names(data_short_MA),  'time_')
data_short_MA <- data_short_MA[, !idx_time]

# Scenario 1
questions$A1 <- "Mr. A was given two tickets to the Regional lottery. He won $50 in one lottery 
and $25 in the other. 

Mr. B was given a ticket to a single, larger Regional lottery. He won $75.


Who is happier?"

data_short_MA %>% 
  ggplot(aes(A1, fill=A1)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(subtitle = questions$A1,y=NULL, x=NULL)

# Scenario 2
questions$B1 <- "You are going to a sold-out game of your favorite local sport-team, \nand you have an extra ticket. The price marked on the ticket is $5 \nbut you were given the tickets for free by a friend. \n\nYou want to get rid of the extra ticket and you know the going \nprice is $5. You find someone who wants the ticket. There is no\nlaw against charging more than the price marked on the ticket"

title <- ggdraw() + draw_label(questions$B1)

plot_b1_1 <- data.frame( resp = c(data_short_MA$B1_1_1,
                     data_short_MA$B1_1_2),
            ques = c(rep('He is a friend',   50),
                     rep('He is a stranger', 50))) %>% 
  mutate(resp=as.numeric(resp)) %>% 
  ggplot(aes(resp, fill=ques)) +
  geom_histogram(alpha=0.5) +
  theme_pubr() + labs(fill=NULL, x=NULL, y=NULL, 
                      subtitle = "What price do you ask for if...") +
  scale_fill_jcolors(palette = 'pal6') +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits = c(-1,21))

plot_b2_1 <- data.frame( resp = c(data_short_MA$B2_1_1,
                                  data_short_MA$B2_1_2),
                         ques = c(rep('He is a friend',   50),
                                  rep('He is a stranger', 50))) %>% 
  mutate(resp=as.numeric(resp)) %>% 
  ggplot(aes(resp, fill=ques)) +
  geom_histogram(alpha=0.5) +
  theme_pubr() + labs(fill=NULL, x=NULL, y=NULL, 
                      subtitle = "What would you have said if you found out that the going market price was $10 instead?") +
  scale_fill_jcolors(palette = 'pal6') +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits = c(-1,21))

plot_grid(title, plot_b1_1, plot_b2_1, ncol=1, rel_heights=c(0.9, 1, 1))


# Scenario 3
questions$C1 <- "You are on the beach on a hot day and you would really like a \ncold bottle of beer. A friend offers to go buy the beer from a fancy resort hotel. \nThey ask how much you are willing to pay for the beer: they will only buy\nit if it costs as much or less than the price you state. You trust your friend,\nand there is no possibility of bargaining with the bartender."

title <- ggdraw() + draw_label(questions$C1)

plot_c1 <- data_short_MA %>% 
  mutate(resp=as.numeric(C1_1)) %>% 
  ggplot(aes(resp)) +
  geom_histogram(alpha=0.5) +
  theme_pubr() + labs(fill=NULL, x=NULL, y=NULL, 
                      subtitle = questions$C1_1) +
  scale_fill_jcolors(palette = 'pal6') +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits = c(-1,21))

plot_grid(title, plot_c1, ncol=1, rel_heights=c(0.3, 1))


# Scenario 4
data_short_MA %>% 
  mutate(F1=ifelse(F1=='I feel like I wasted something but no specific amount or measure comes to mind',
                   'I feel like I wasted something but no specific\namount or measure comes to mind', 
                   F1),
         F1=ifelse(F1=='I feel like I wasted nothing, since my visit had already been paid for',
                   'I feel like I wasted nothing, since my\nvisit had already been paid for', 
                   F1)
         ) %>% 
  ggplot(aes(F1, fill=F1)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none', 
        axis.text.x = element_text(angle = 15, hjust=1)) +
  labs(subtitle = questions$F1,y=NULL, x=NULL)

# Scenario 5
questions$D3 <- "Imagine that you are about to purchase a jacket for $125 and a calculator for $15.\nThe salesman informs you that the calculator you wish to buy is on sale for $10\nat the other branch store, located 20 minutes away."

title <- ggdraw() + draw_label(questions$D3)

plot_d3 <- data_short_MA %>% 
  ggplot(aes(d3., fill=d3.)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(subtitle = questions$d3.,y=NULL, x=NULL)

plot_grid(title, plot_d3, ncol=1, rel_heights=c(0.2, 1))

# Scenario 6
questions$B2 <- "Imagine you are going to a sold-out game of your favorite local sport-team,\nand you havean extra ticket. The price marked on the ticket is $5 which\nis what you paid for each ticket.

                You want to get rid of the extra ticket and you know the going price is $5.\nYou find someonewho wants the ticket. There is no law against charging\nmore than the price marked on the ticket."

title <- ggdraw() + draw_label(questions$B2)

plot_b1_2 <- data.frame( resp = c(data_short_MA$B1_2_1,
                                  data_short_MA$B1_2_2),
                         ques = c(rep('He is a friend',   50),
                                  rep('He is a stranger', 50))) %>% 
  mutate(resp=as.numeric(resp)) %>% 
  ggplot(aes(resp, fill=ques)) +
  geom_histogram(alpha=0.5) +
  theme_pubr() + labs(fill=NULL, x=NULL, y=NULL, 
                      subtitle = "What price do you ask for if...") +
  scale_fill_jcolors(palette = 'pal6') +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits = c(-1,21))

plot_b2_2. <- data.frame( resp = c(data_short_MA$B2_2._1,
                                  data_short_MA$B2_2._2),
                         ques = c(rep('He is a friend',   50),
                                  rep('He is a stranger', 50))) %>% 
  mutate(resp=as.numeric(resp)) %>% 
  ggplot(aes(resp, fill=ques)) +
  geom_histogram(alpha=0.5) +
  theme_pubr() + labs(fill=NULL, x=NULL, y=NULL, 
                      subtitle = "What would you have said if you found out that the going market price was $10 instead?") +
  scale_fill_jcolors(palette = 'pal6') +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits = c(-1,21))

plot_grid(title, plot_b1_2, plot_b2_2., ncol=1, rel_heights=c(0.9, 1, 1))


# Scenario 7
questions$A2 <- "Mr. A received a lett¯er from the IRS saying that he made a minor arithmetical
mistake on his tax return and owed $100. He received a similar letter the same 
day from his state income tax authority saying he owed $50. There were no other 
repercussions from either mistake.
                 
Mr. B received a letter from the IRS saying he made a minor arithmetical mistake
on his tax return and owed $150. There were no other repercussions from this mistake.

Who is more upset?\n"

data_short_MA %>% 
  ggplot(aes(A2, fill=A2)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(subtitle = questions$A2,y=NULL, x=NULL)

# Scenario 7
questions$E2q <- "Imagine you are on your way to a play, with the intention of buying two tickets 
worth $40 in total. On entering the theater, you realize that you have lost $40 in cash."

title <- ggdraw() + draw_label(questions$E2q)

plot_e2 <- data_short_MA %>% 
  ggplot(aes(E2, fill=E2)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(subtitle = questions$E2,y=NULL, x=NULL)

plot_grid(title, plot_e2, ncol=1, rel_heights=c(0.2, 1))

# Scenario 8
questions$G2q <- "You are flying with a friend who has upgraded their ticket to business 
class and has two coupons, each worth $35, which can be used to upgrade your 
ticket/seat from economy to business class. They purchased one coupon for the 
standard price of $35, and received the other one for free. Just one coupon is 
required to upgrade your class on the current flight and your friend offers you 
the coupon which he got for free so that you can upgrade your ticket as well."

title <- ggdraw() + draw_label(questions$G2q)

data_short_MA$G2 <- ifelse(data_short_MA$G2=="Pay some, but not the full amount for the coupon (for example, half the price).",
                           "Pay some, but not the full amount\nfor the coupon (for example, half the price).",
                           data_short_MA$G2)
plot_g2 <- data_short_MA %>% 
  ggplot(aes(G2, fill=G2)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 15, hjust=1)) +
  labs(subtitle = questions$G2,y=NULL, x=NULL)

plot_grid(title, plot_g2, ncol=1, rel_heights=c(0.4, 1))

# Scenarion 9
questions$D4 <- "Imagine that you are about to purchase a jacket for $15 and a calculator 
for $125. The salesman informs you that the calculator you wish to buy is on sale
for $120 at the other branch store, located 20 minutes away."


title <- ggdraw() + draw_label(questions$D4)

plot_d4 <- data_short_MA %>% 
  ggplot(aes(d4, fill=d4)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(subtitle = questions$d4,y=NULL, x=NULL)

plot_grid(title, plot_d4, ncol=1, rel_heights=c(0.2, 1))

# Scenario 10
questions$A3 <- "Mr. A bought his first National lottery ticket and won $100. Also, in a freak accident,
he damaged the rug in his apartment and had to pay the landlord $80. 

Mr. B bought his first National lottery ticket and won $20. 


Who is happier?"

data_short_MA %>% 
  ggplot(aes(A3., fill=A3.)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(subtitle = questions$A3,y=NULL, x=NULL)


# Scenario 10
questions$B3 <- "You are going to a sold-out game of your favorite local sport-team, and you have 
an extra ticket. The price marked on the ticket is $5 but you paid $10 for each 
ticket when you bought them from another person.
 
 You want to get rid of the extra ticket and you know the going price is $5. You 
find someone who wants the ticket. There is no law against charging more than the
price marked on the ticket."

plot_b1_3 <- data.frame( resp = c(data_short_MA$B1_3._1,
                                  data_short_MA$B1_3._2),
                         ques = c(rep('He is a friend',   50),
                                  rep('He is a stranger', 50))) %>% 
  mutate(resp=as.numeric(resp)) %>% 
  ggplot(aes(resp, fill=ques)) +
  geom_histogram(alpha=0.5) +
  theme_pubr() + labs(fill=NULL, x=NULL, y=NULL, 
                      subtitle = "What price do you ask for if...",
                      title = questions$B3) +
  scale_fill_jcolors(palette = 'pal6') +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits = c(-1,21))

plot_b2_3 <- data.frame( resp = c(data_short_MA$B2_3_1,
                                   data_short_MA$B2_3_2),
                          ques = c(rep('He is a friend',   50),
                                   rep('He is a stranger', 50))) %>% 
  mutate(resp=as.numeric(resp)) %>% 
  ggplot(aes(resp, fill=ques)) +
  geom_histogram(alpha=0.5) +
  theme_pubr() + labs(fill=NULL, x=NULL, y=NULL, 
                      subtitle = "What would you have said if you found out that the going market price was $10 instead?") +
  scale_fill_jcolors(palette = 'pal6') +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits = c(-1,21))

plot_grid(plot_b1_3, plot_b2_3, ncol=1, rel_heights=c(1.7, 1))

# Scenario 11
questions$D2 <- "You are about to purchase a jacket for $15 and a Bluetooth speaker for $125.
The salesman informs you that the speaker you wish to buy is on sale for $120 
at the other branch store, located 20 minutes away."

data_short_MA %>% 
  ggplot(aes(D2_2, fill=D2_2)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(subtitle = questions$D2_2,
       title = questions$D2,
       y=NULL, x=NULL)

# Scenario 12
questions$F2q <- "You have a membership to a gym in a nearby town that you travel to for business
on a regular basis. You go to this gym every single Monday night. The membership
costs you $1000 a year (i.e., roughly $20 per visit). One Monday, just after you 
have arrived and changed into your gym clothes, you receive a phone call that
requires you to leave and forego your exercise for that evening."


data_short_MA %>% 
  mutate(F2=ifelse(F2=='I feel like I wasted something but no specific amount or measure comes to mind',
                   'I feel like I wasted something but no specific\namount or measure comes to mind', 
                   F2),
         F2=ifelse(F2=='I feel like I wasted nothing, since my visit had already been paid for',
                   'I feel like I wasted nothing, since my\nvisit had already been paid for', 
                   F2)
  ) %>% 
  ggplot(aes(F2, fill=F2)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none', 
        axis.text.x = element_text(angle = 15, hjust=1)) +
  labs(subtitle = questions$F2,y=NULL, x=NULL,
       title = questions$F2q)

# Scenario 13 - Beer on the beach [2]
questions$C1_2q <- "You are on the beach on a hot day and you would really like a cold bottle 
of beer.  A friend offers to go buy the beer from a small, run-down grocery 
store. They ask how much you are willing to pay for the beer: they will only 
buy it if it costs as much or less than the price you state. You trust your 
friend, and there is no possibility of bargaining with the store owner. "

data_short_MA %>% 
  mutate(resp=as.numeric(C1_2)) %>% 
  ggplot(aes(resp)) +
  geom_histogram(alpha=0.5) +
  theme_pubr() + labs(fill=NULL, x=NULL, y=NULL, 
                      subtitle = questions$C1_2,
                      title = questions$C1_2q) +
  scale_fill_jcolors(palette = 'pal6') +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits = c(-1,21))

# Scenario 14 - Mr. A vs Mr. B [4]

questions$A4q <- "Mr. A’s car was damaged in a parking lot. He had to spend $200 to repair 
the damage. The same day the car was damaged, he won $25 in the office holiday raffle.

Mr. B’s car was damaged in a parking lot. He had to spend $175 to repair the damage.

Who is more upset? \n"

data_short_MA %>% 
  ggplot(aes(A4, fill=A4)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(title = questions$A4q,
       y=NULL, x=NULL)

# Scenario 15 - jacket-calculator [1]

questions$D2_1q <- "You are about to purchase a jacket for $125 and a Bluetooth speaker
for $15. The salesman informs you that the speaker you wish to buy is on sale 
for $10 at the other branch store, located 20 minutes away."

data_short_MA %>% 
  ggplot(aes(D2_1, fill=D2_1)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(subtitle = questions$D2_1,
       title = questions$D2_1q,
       y=NULL, x=NULL)

# Scenario 16 - lost ticket [1]

questions$E1q <- "Imagine you are on your way to a play with a pair of tickets for which
you have paid $40. On entering the theater, you discover that you have lost 
the tickets."

data_short_MA %>% 
  ggplot(aes(E1, fill=E1)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none') +
  labs(subtitle = questions$E1,
       title = questions$E1q,
       y=NULL, x=NULL)

# Scenario 17 - Airplanes coupons scenario [1]

questions$G1q <- "You are flying with a friend who has upgraded their ticket to business 
class and has two coupons, each worth $35, which can be used to upgrade 
your ticket/seat from economy to business class. They purchased one 
coupon for the standard price of $35, and received the other one for 
free. Just one coupon is required to upgrade your class on the current 
flight, and your friend offers you the coupon which he purchased so that 
you can upgrade as well.\n"
  
data_short_MA$G1 <- ifelse(data_short_MA$G1=="Pay some, but not the full amount for the coupon (for example, half the price).",
                           "Pay some, but not the full amount\nfor the coupon (for example, half the price).",
                           data_short_MA$G1)

data_short_MA %>% 
  ggplot(aes(G1, fill=G1)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_jcolors(palette = 'pal6') +
  theme_pubr() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 15, hjust=1)) +
  labs(subtitle = questions$G1,
       title = questions$G1q,
       y=NULL, x=NULL)

