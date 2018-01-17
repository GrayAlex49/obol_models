library(tidyverse)


### Pull Data -----------------------------------------------
library(crypto)

# This takes a while so I've saved it out 
raw.data <- getCoins()

info <- raw.data %>%
  distinct(symbol, name, ranknow)

saveRDS(raw.data, paste0('./data/', format(max(raw.data$date), "%Y_%m%d"), '_rawData.RDS' ))

write_csv(info, paste0('./data/', format(max(raw.data$date), "%Y_%m%d"), '_symbolInfo.csv' ))

data <- raw.data %>%
  filter(ranknow < 20) %>%
  select(-slug, -ranknow, -name) %>%
  gather(key, value, -date, -symbol) %>%
  unite(key, symbol, key) %>%
  spread(key, value) %>%
  arrange(date)

saveRDS(data, paste0('./data/',format(max(data$date), "%Y_%m%d"), '_modelDataset.RDS' ))

### Load Data --------------------------------------------

data <- readRDS("./data/2018_0116_modelDataset.RDS")

### Simple Liniar -----------------------------------------

mod <-lm(ETH_close ~
           lag(ETH_close) +
           lag(BTC_close),
         data=data, na.action="na.omit")

summary(mod)
