library(tidyverse)
library(knitr)
library("readr")
library(ggplot2)
library(dplyr)
library(lubridate)
data <- read.csv(here::here("Tweets.csv"), na.strings=c(""," ","NA"))

glimpse(data)

data_clean <- data %>% as_tibble() %>%
  mutate(airline_sentiment = as.factor(airline_sentiment)) %>%
  mutate(airline = as.factor(airline)) %>%
  mutate(user_timezone = as.factor(user_timezone)) %>%
  mutate(negativereason = as.factor(negativereason)) %>%
  mutate(tweet_created = as.Date(tweet_created)) %>%
  select(-tweet_coord, -airline_sentiment_gold, -negativereason_gold, -negativereason_confidence)

na_count <-sapply(data_clean, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
print(na_count)
colSums(is.na(data_clean))

summary(data_clean)

ggplot(data_clean, aes(airline_sentiment, fill = airline_sentiment)) + geom_histogram(stat="count") + theme(legend.position = "none")

data_clean%>% filter(!is.na(negativereason)) %>% ggplot(aes(negativereason, fill = negativereason)) + 
  geom_histogram(stat="count") + coord_flip() + theme(legend.position = "none")

df <- data_clean%>%
  group_by(month=month(ydm(tweet_created), label = TRUE))%>%
  summarise()

ggplot(data_clean, aes(tweet_created, fill = airline_sentiment)) + geom_histogram(stat="count") + facet_wrap(~airline)
