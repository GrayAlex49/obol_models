library(tidyverse)
library(modelr)

sample.cutoff <- as.Date("2017-10-01")

### Load Data --------------------------------------------

data <- readRDS("./data/2018_0116_modelDataset.RDS")

eth.data <- data[,c("date", str_subset(names(data), "ETH"))] %>% 
  na.omit(.)

### Synthetic Variables ------------------------------------

temp <- data %>% 
  mutate_if(is.numeric, funs(zoo::rollmean(., k = 3, fill = NA, align = 'right'))) %>% 
  rename_if(is.numeric, funs(paste0(., '_roll3')))

data <- data %>% 
  left_join(temp)

data.direction <- data %>% 
  mutate_if(is.numeric, funs(c(NA, if_else(diff(ETH_close)>0, 1, 0))))
  
train.data <- data %>% 
  filter(date < sample.cutoff)

### Simple Liniar Components -------------------------------

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
         data=train.data, na.action="na.omit")

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
               data=train.data, na.action="na.omit")

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
              data=train.data, na.action="na.omit")

summary(mod.spread)

data <- data %>% 
  add_predictions(mod.spread, var = 'pred.lm.spread')

ggplot(data)+
  geom_line(aes(date, pred.lm.spread), color = 'red') +
  geom_line(aes(date, ETH_close))

cor(data$ETH_close, data$pred.lm.spread, use = 'complete.obs')

# Google trends
mod.trends <-lm(ETH_close ~
                  lag(google_bitcoin) +
                  lag(google_coinbase) +
                  lag(google_ethereum) +
                  lag(google_litecoin) +
                  lag(google_cryptocurrency),
                data=train.data, na.action="na.omit")

summary(mod.trends)

data <- data %>% 
  add_predictions(mod.trends, var = 'pred.lm.trends')

ggplot(data)+
  geom_line(aes(date, pred.lm.trends), color = 'red') +
  geom_line(aes(date, ETH_close))

cor(data$ETH_close, data$pred.lm.trends, use = 'complete.obs')


### Binomial Liniar Components ----------------------------------

# For right now, the most important metric is direction so we may as well
# model that directly

mod.close.direction <-glm(ETH_close ~
                 lag(ETH_close) +
                 lag(BTC_close) +
                 lag(XRP_close) +
                 lag(BCH_close) +
                 lag(LTC_close),
               data=data.direction, na.action="na.omit", family = "binomial")

summary(mod.close.direction)


### Ensembles ---------------------------------------------------
ens.lm <- lm(ETH_close ~ 
               pred.lm.close +
               pred.lm.volume +
               pred.lm.spread +
               pred.lm.trends,
             data = data, na.action = 'na.omit'
)

summary(ens.lm)

data <- data %>%
  add_predictions(ens.lm, var="pred.ens.lm")

cor(data$ETH_close, data$pred.ens.lm, use = 'complete.obs')

ggplot(data)+
  geom_line(aes(date, pred.ens.lm), color = 'red') +
  geom_line(aes(date, ETH_close))


### Model Evaluation --------------------------------------------

model.predictions <- data %>% 
  select(date, ETH_close, contains("pred."))

model.eval <- function(data, dv, pred){
  pred <- enquo(pred)
  dv <- enquo(dv)
  
  test <- data %>% 
    select(date, !!dv, !!pred) %>% 
    na.omit() %>% 
    mutate(base = c(NA, diff(!!dv))) %>% 
    mutate(model = c(NA, diff(!!pred))) %>% 
    mutate(direction.match = if_else(sign(base) == sign(model), 1, 0)) %>% 
    mutate(magnitude = abs(base - model))
  
  data.frame(
    model = quo_name(pred),
    direction.match = sum(test$direction.match, na.rm = T) / sum(!is.na(test$direction.match)),
    magnitude.avg = mean(test$magnitude, na.rm = T),
    magnitude.sd = sd(test$magnitude, na.rm = T),
    magnitude.max = max(test$magnitude, na.rm = T)
  )
  
}
# 
# test <- data.frame()
# for(i in str_subset(names(model.predictions), "lm")){
#   temp <- model.eval(model.predictions, ETH_close, UQE(i))
#   test <- rbind(test, temp)
# }
# 
# map(model.predictions, ~model.eval(model.predictions, ETH_close, .x))



### Trading Strategies -----------------------------------------

trade.direction <- function(x){
  # The simplest strategy I can think of is if the model is predicting lower
  # than it did the day before, be out of the market and if it is predicting
  # higher, be in the market.  
  
  trade <- c(NA, if_else(diff(x) > 0, 1, 0))
  
  return(trade)
}

trade.macd <- function(x){
  # This does not really use the models, its a strategy typically applied to 
  # the raw series.  I've taken the lag out though so if it is not applied
  # to a prediction it would need to be lagged.
  temp <-  data.frame(MACD(x))
  signal <- ifelse(temp$macd < temp$signal, 0, 1)
  return(signal)
}

### Return Evaluation ------------------------------------------
library(quantmod)
library(TTR)

# These calculate the base return and then the return for one trading 
# strategy.  We'll want to put it in a function so each strategy can
# be applied to each model.

test <- model.predictions %>% 
  filter(date > sample.cutoff) %>% 
  mutate(base.returns = ROC(ETH_close)) %>% 
  replace_na(list(base.returns = 0)) %>% 
  mutate(base.returns = exp(cumsum(base.returns)))

test <- model.predictions %>% 
  filter(date > sample.cutoff) %>% 
  mutate(ens.lm.return = trade.direction(pred.ens.lm)) %>% 
  mutate(ens.lm.return = ROC(ETH_close)*ens.lm.return) %>% 
  replace_na(list(ens.lm.return = 0)) %>% 
  mutate(ens.lm.return = exp(cumsum(ens.lm.return)))