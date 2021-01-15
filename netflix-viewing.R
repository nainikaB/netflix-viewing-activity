#Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)

# Reading data from downloaded CSV file of my netflix viewing activity 
netflixactivity <- read.csv("NetflixViewingHistory.csv")
netflixactivity
str(netflixactivity)
netflixactivity$Date <- dmy(netflixactivity$Date)

# A separate column that contains a title for season, title of the show and which episode
netflixactivity_title <- netflixactivity %>% separate (col = Title, into = c("title", "season", "episode_title"), sep = ': ')
netflixactivity_title <- netflixactivity_title[!is.na(netflixactivity_title$season),]
netflixactivity_title <- netflixactivity_title[!is.na(netflixactivity_title$episode_title),]


# Number of of episodes and series seen per day 
epsidoes_binged <- netflixactivity_title %>% count(title,Date)


# Assuming the number of episodes is equal to or greater than 4 is considered a binge watched series 
epsidoes_binged <- epsidoes_binged[epsidoes_binged$n >= 6,]
mynetflix <- epsidoes_binged 


# Analyzing the month where I binged waatched the most shows
mynetflix$month20 <- months(mynetflix$Date, abbreviate = T)


number_episodes_per_month_plot <- ggplot(aes(x = month20, y = n, color = n), data = mynetflix) +
  geom_col(fill = c("#800080")) +
  theme_minimal() +
  ggtitle("Episodes watched per month", "duration from 2020 to 2021") +
  labs(x = "Month", y = "Number of Episodes watched") 
number_episodes_per_day_plot

#converting dates into days 
number_episodes_per_day <- number_episodes_per_day[order(number_episodes_per_day$Date),]
number_episodes_per_day$false_day <- wday(number_episodes_per_day$Date)
number_episodes_per_day$days <- weekdays(number_episodes_per_day$Date, abbreviate = F)
number_episodes_per_day

number_episodes_per_day <- number_episodes_per_day[order(number_episodes_per_day$Date),]
number_episodes_per_day$false_day <- wday(number_episodes_per_day$Date)
number_episodes_per_day$days <- weekdays(number_episodes_per_day$Date, abbreviate = F)
number_episodes_per_day


