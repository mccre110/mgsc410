library(tidyverse)
library(knitr)
library("readr")
library(ggplot2)
library("rsample")
library('yardstick')
library(dplyr)
library(lubridate)
library(stringr)
library(cluster)
library(factoextra)
options(scipen = 100, digits = 4)

sub <- read.csv(here::here("Subscriber.csv"))
sub <- sub %>% mutate(Language = as.factor(Language)) %>% 
               mutate(Subscription.Type = as.factor(Subscription.Type)) %>% 
               mutate(Subscription.Event.Type = as.factor(Subscription.Event.Type)) %>% 
               mutate(Purchase.Store = as.factor(Purchase.Store)) %>% 
               mutate(Currency = as.factor(Currency)) %>% 
               mutate(Purchase.Amount = as.double(Purchase.Amount)) %>% 
               mutate(Demo.User = as.factor(Demo.User)) %>% 
               mutate(Free.Trial.User = as.factor(Free.Trial.User)) %>% 
               mutate(Auto.Renew = as.factor(Auto.Renew)) %>% 
               mutate(Country = as.factor(Country)) %>% 
               mutate(User.Type = as.factor(User.Type)) %>% 
               mutate(Lead.Platform = as.factor(Lead.Platform)) %>%
               mutate(Email.Subscriber = as.factor(Email.Subscriber)) %>%
               mutate(Push.Notifications = as.factor(Push.Notifications)) %>%
               mutate(Send.Count = as.integer(Send.Count)) %>%
               mutate(Unique.Open.Count = as.integer(Unique.Open.Count)) %>%
               mutate(Open.Count = as.integer(Open.Count)) %>%
               mutate(Click.Count = as.integer(Click.Count)) %>%
               mutate(Unique.Click.Count = as.integer(Unique.Click.Count)) %>%
               mutate(Subscription.Start.Date = as.Date(Subscription.Start.Date, "%m/%d/%Y")) %>%
               mutate(Subscription.Expiration = as.Date(Subscription.Expiration, "%m/%d/%Y")) %>% 
               mutate(Free.Trial.Start.Date = as.Date(Free.Trial.Start.Date, "%m/%d/%Y")) %>% 
               mutate(Free.Trial.Expiration = as.Date(Free.Trial.Expiration, "%m/%d/%Y"))


na_count_sub <-sapply(sub, function(y) sum(length(which(is.na(y)))))
na_count_sub <- data.frame(na_count_sub)
print(na_count_sub)
summary(sub)
glimpse(sub)



ggplot(sub, aes(Language, fill = Subscription.Type)) + geom_bar() + theme_minimal()+coord_flip()+ facet_wrap(~Country)
ggplot(sub, aes(Language, fill = Auto.Renew)) + geom_bar() + theme_minimal()+coord_flip()
ggplot(sub, aes(Language, fill = Lead.Platform)) + geom_bar() + theme_minimal()+coord_flip()

sub %>% filter(Auto.Renew != "NULL") %>% ggplot(aes(Auto.Renew, fill = Country)) + geom_bar() + theme_minimal()
ggplot(sub, aes(Auto.Renew)) + geom_bar() + theme_minimal() 

ggplot(sub,aes(Auto.Renew)) + geom_bar() + theme_minimal()+ facet_wrap(~Country)


sub %>% filter(complete.cases(.)) %>%
  ggplot(aes(Subscription.Start.Date, fill =Country)) + geom_bar() + theme_minimal()

sub %>% filter(!is.na(Purchase.Amount)) %>% ggplot(aes(Subscription.Type, Purchase.Amount)) + geom_bar(stat = "summary", fun = "mean") + theme_minimal()


sub %>% filter(Auto.Renew == "Off") %>% summary()





sub_nodat <-sub %>% select(-ID,
                           -Subscription.Start.Date,
                           -Subscription.Expiration,
                           -Free.Trial.Start.Date,
                           -Free.Trial.Expiration) 

gower.dist <- daisy(sub_nodat, metric = c("gower"))
divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")


sub_click_r <-sub %>% select(Send.Count,Open.Count,Click.Count,Unique.Open.Count,Unique.Click.Count,Country) %>% filter(complete.cases(.))
s <-sub_click_r %>% select(Weekly_Sales, DMA, Date)
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


sub_click <-sub %>% select(Send.Count,Open.Count,Click.Count,Unique.Open.Count,Unique.Click.Count) %>% filter(complete.cases(.))
k2 <- kmeans(sub_click, centers = 3, nstart = 25)
str(k2)
fviz_cluster(k2, data = sub_click)

sub_click %>% as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(sub_click)) %>%
  ggplot(aes(Send.Count, Open.Count, color = factor(cluster))) +
  geom_point()

act <- read.csv(here::here("Activity.csv"))
act <- act %>% mutate(App.Session.Platform = as.factor(App.Session.Platform)) %>% 
                rename(Platform = App.Session.Platform) %>% 
               mutate(App.Activity.Type = as.factor(App.Activity.Type)) %>% 
                rename(Type = App.Activity.Type) %>% 
               mutate(App.Session.Date = as.Date(App.Session.Date, "%m/%d/%Y")) %>% 
                rename(Date = App.Session.Date) %>% filter(complete.cases(.))

na_count_act <-sapply(act, function(y) sum(length(which(is.na(y)))))
na_count_act <- data.frame(na_count_act)
print(na_count_act)
