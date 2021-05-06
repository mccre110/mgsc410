library(tidyverse)


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