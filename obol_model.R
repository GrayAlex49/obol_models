library(tidyverse)
library(modelr)


### Pull Data -----------------------------------------------
library(crypto)

# This takes a while so I've saved it out 
raw.data <- getCoins()

saveRDS(raw.data, paste0('./data/', format(max(raw.data$date), "%Y_%m%d"), '_rawData.RDS' ))

info <- raw.data %>%
  distinct(symbol, name, ranknow)

write_csv(info, paste0('./data/', format(max(raw.data$date), "%Y_%m%d"), '_symbolInfo.csv' ))

# raw.data <- readRDS("./data/2018_0116_rawData.RDS")

data <- raw.data %>%
  mutate(date = as.Date(date)) %>% 
  filter(ranknow < 20) %>%
  select(-slug, -ranknow, -name) %>%
  gather(key, value, -date, -symbol) %>%
  unite(key, symbol, key) %>%
  spread(key, value) %>%
  arrange(date)

trends <- gtrendsR::gtrends(c("Bitcoin", "Ethereum", "Litecoin", "Coinbase", "Cryptocurrency")) 

test <- data.frame(trends$interest_over_time) %>% 
  mutate(date = as.Date(date)) %>% 
  select(date, keyword, hits) %>% 
  mutate(keyword = paste0('google_', tolower(keyword))) %>% 
  spread(keyword, hits) %>% 
  full_join(tibble(date = as.Date(data$date))) %>% 
  fill(.)


data <- left_join(data, trends, by = 'date')

saveRDS(data, paste0('./data/',format(max(data$date), "%Y_%m%d"), '_modelDataset.RDS' ))

### Load Data --------------------------------------------

data <- readRDS("./data/2018_0116_modelDataset.RDS")

test <- data[,c("date", str_subset(names(data), "ETH"))] %>% 
  na.omit(.)

### Simple Liniar -----------------------------------------

# I'm doing what I know how to do here but my thought is take the measures that 
# make sense to me for the currencies trading on coinbase (+ ripple) and throw them
# in simple liniar models and then do an ensemble.  Obviously a rolling average or
# something will be more robust but I figure this is a decent place to start.

# Simple one day lag close price of top currencies 
mod.close <-lm(ETH_close ~
           lag(ETH_close) +
           lag(BTC_close) +
           lag(XRP_close) +
           lag(BCH_close) +
           lag(LTC_close),
         data=data, na.action="na.omit")

summary(mod.close)

data <- data %>% 
  add_predictions(mod.close, var = 'pred.lm.close')

ggplot(data)+
  geom_line(aes(date, pred.lm.close), color = 'red') +
  geom_line(aes(date, ETH_close))

cor(data$ETH_close, data$pred.lm.close, use = 'complete.obs')

# We'll want to include volume somehow 
mod.vol <-lm(ETH_close ~
                 lag(ETH_volume) +
                 lag(BTC_volume) +
                 lag(XRP_volume) +
                 lag(BCH_volume) +
                 lag(LTC_volume),
               data=data, na.action="na.omit")

summary(mod.vol)

data <- data %>% 
  add_predictions(mod.vol, var = 'pred.lm.volume')

ggplot(data)+
  geom_line(aes(date, pred.lm.volume), color = 'red') +
  geom_line(aes(date, ETH_close))

cor(data$ETH_close, data$pred.lm.volume, use = 'complete.obs')

# This gets at the volitility information 
mod.spread <-lm(ETH_close ~
                lag(ETH_spread) +
                lag(BTC_spread) +
                lag(XRP_spread) +
                lag(BCH_spread) +
                lag(LTC_spread),
              data=data, na.action="na.omit")

summary(mod.spread)

data <- data %>% 
  add_predictions(mod.spread, var = 'pred.lm.spread')

ggplot(data)+
  geom_line(aes(date, pred.lm.spread), color = 'red') +
  geom_line(aes(date, ETH_close))

cor(data$ETH_close, data$pred.lm.spread, use = 'complete.obs')

# Ensemble 
ens.lm <- lm(ETH_close ~ 
               pred.lm.close +
               pred.lm.volume +
               pred.lm.spread,
             data = data, na.action = 'na.omit'
)

summary(ens.lm)

data <- data %>%
  add_predictions(ens.lm, var="pred.ens.lm")

model.predictions <- data %>% 
  select(date, ETH_close, contains("pred."))

cor(data$ETH_close, data$pred.ens.lm, use = 'complete.obs')

ggplot(data)+
  geom_line(aes(date, pred.ens.lm), color = 'red') +
  geom_line(aes(date, ETH_close))
