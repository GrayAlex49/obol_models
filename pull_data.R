library(tidyverse)
library(crypto)
library(quantmod)

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

# Google Trends
trends <- gtrendsR::gtrends(c("Bitcoin", "Ethereum", "Litecoin", "Coinbase", "Cryptocurrency")) 

trends <- data.frame(trends$interest_over_time) %>% 
  mutate(date = as.Date(date)) %>% 
  select(date, keyword, hits) %>% 
  mutate(hits = as.numeric(hits)) %>% 
  mutate(keyword = paste0('google_', tolower(keyword))) %>% 
  spread(keyword, hits) %>% 
  full_join(tibble(date = seq.Date(min(.$date), max(.$date), "days"))) %>% 
  arrange(date) %>% 
  fill(contains("google"))

data <- left_join(data, trends, by = 'date')

# Market Data
getSymbols(c("^IXIC", "^GSPC"))

GSPC <- data.frame(date = index(GSPC), GSPC)
IXIC <- data.frame(date = index(IXIC), IXIC)

market <- GSPC %>% 
  full_join(IXIC, by = 'date') %>% 
  as.tibble() %>% 
  full_join(tibble(date = seq.Date(min(.$date), max(.$date), "days"))) %>% 
  arrange(date) %>% 
  fill(everything())

data <- left_join(data, market, by = 'date')

saveRDS(data, paste0('./data/',format(max(data$date), "%Y_%m%d"), '_modelDataset.RDS' ))
