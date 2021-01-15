#Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(magrittr)

# Reading data from downloaded CSV file of my netflix viewing activity 
netflixactivity <- read.csv("NetflixViewingHistory.csv")
netflixactivity
str(netflixactivity)
netflixactivity$Date <- lubridate::dmy(netflixactivity$Date)

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
mynetflix$months <- months(mynetflix$Date, abbreviate = T)


# Plotting graph to anakyse which month I watched most shows  
number_episodes_per_month_plot <- ggplot(aes(x = months, y = n, fill = n), data = mynetflix) +
  scale_fill_gradient(low = "orange", high = "red")+
  geom_bar(width=0.7, stat = "identity")
  theme_minimal() +
  ggtitle("Episodes watched per month", "duration from 2020 to 2021") +
  labs(x = "Month", y = "Number of Episodes watched") 
number_episodes_per_month_plot


# Converting dates into days 
mynetflix <- mynetflix[order(mynetflix$Date),]
mynetflix$false_day <- wday(mynetflix$Date)
mynetflix$days <- weekdays(mynetflix$Date, abbreviate = F)
mynetflix

#plotting a graph to anakyse which days I watched most shows  
number_episodes_per_day <- mynetflix %>% ggplot(aes(x = days, y = n, fill = n)) +
  scale_fill_gradient(low = "blue", high = "red")+
  geom_bar(width=0.7, stat = "identity")
number_episodes_per_day

