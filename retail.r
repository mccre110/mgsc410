library(tidyverse)
library(knitr)
library("readr")
library(ggplot2)
library("rsample")
library('yardstick')
library(dplyr)
library(lubridate)
library(stringr)
library(RColorBrewer)
library(maps)
options(scipen = 100, digits = 4)

sales <- read.csv(here::here("salesNew.csv"))
features <- read.csv(here::here("features.csv"))
stores <- read.csv(here::here("stores.csv"))

stores <- stores %>% select(-X)
features <- features %>% mutate(Date = as.Date(Date,"%d/%m/%Y"))
sales <- sales %>% mutate(Date = as.Date(Date,"%d/%m/%Y"))

glimpse(sales)
glimpse(stores)
glimpse(features)

fullSales <- merge(sales,features,by=c("Store","Date"))
fullSales <- fullSales %>% mutate(Store = as.factor(Store))
sales_stores_features <- merge(fullSales,stores,by=c("Store"))
sales_stores_features <- sales_stores_features %>% mutate(DMA = as.factor(DMA))
sales_stores_features <- sales_stores_features %>% mutate(Type = as.factor(Type))
sales_stores_features <- sales_stores_features %>% mutate(DMA = reorder(DMA,-Weekly_Sales))

summary(sales_stores_features)

ggplot(sales_stores_features, aes(x=DMA, y=Weekly_Sales, fill = DMA))+ 
  geom_bar(stat = "summary", fun = "mean") + theme_minimal()+coord_flip()

ggplot(sales_stores_features, aes(x=Store, y=Weekly_Sales, fill = DMA))+ geom_bar(stat = "identity") +facet_wrap(~IsHoliday)
+ theme_minimal()
ggplot(sales_stores_features, aes(x=DMA, y=Weekly_Sales, fill = DMA))+ geom_boxplot() +coord_flip()
ggplot(sales_stores_features, aes(x=Date, y=Weekly_Sales, color = DMA))+ geom_point() + facet_wrap(~DMA)

msa <- read.csv(here::here("MSAsel.csv"))
dma <- read.csv(here::here("DMAraw.csv"))

msa <- msa %>% rename(DMA.Name = ï..Item)
msa <- msa %>% mutate(DMA.Name = str_replace_all(DMA.Name, "[-.\r\n]" , ""))
msa <- msa %>% mutate(DMA.Name = str_replace_all(DMA.Name, fixed(" "), ""))

dma <- dma %>% mutate(DMA.Name = str_replace_all(DMA.Name, "[-.\r\n]" , ""))
dma <- dma %>% mutate(DMA.Name = str_replace_all(DMA.Name, fixed(" "), ""))

msa_dma <- merge(msa,dma,by=c("DMA.Name"))
msa_dma <- msa_dma %>% mutate(Med.HHld.Income = parse_number(Med.HHld.Income))
msa_dma <- msa_dma %>% mutate(Name = reorder(Name,-Med.HHld.Income))

ggplot(msa_dma, aes(x=Name, y=Med.HHld.Income, fill = Name))+ geom_bar(stat = "identity") +coord_flip()









set.seed(1818)
s <-sales_stores_features %>% select(Weekly_Sales, DMA, Date)
s <-na.omit(s)
s_split <- initial_split(s, prop = .7)
s_train <- training(s_split)
s_test <- testing(s_split)

lm_mod3 <- lm(Weekly_Sales ~ .,
                     data = s_train)
summary(lm_mod3)

train_preds_lm <- predict(lm_mod3, newdata = s_train)
test_preds_lm <- predict(lm_mod3, newdata = s_test)
results_test <- data.frame(
  `true` = s_test$Weekly_Sales,
  `linear` = test_preds_lm)
rmse(results_test, true, linear)



















MainStates <- map_data("state")
# read the state population data
StatePopulation <- read.csv("https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv", as.is = TRUE)
p <- ggplot()
p <- p + geom_polygon( data=MainStates, 
                       aes(x=long, y=lat, group=group), 
                       color="white", size = 0.2) 
p <- p + geom_point(data=msa_dma, aes(x=Longitude, y=Latitude, size = Total.Relevant.Expeditures)) + 
  
  scale_size(name="Population")
p

