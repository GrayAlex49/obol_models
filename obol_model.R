library(tidyverse)
library(modelr)

sample.cutoff <- as.Date("2017-12-01")

### Load Data --------------------------------------------

data <- readRDS("./data/2018_0208_modelDataset.RDS")

eth.data <- data[,c("date", str_subset(names(data), "ETH"))] %>% 
  na.omit(.)

### Synthetic Variables ------------------------------------

data.roll <- data %>% 
  mutate_if(is.numeric, funs(zoo::rollmean(., k = 3, fill = NA, align = 'right'))) %>% 
  rename_if(is.numeric, funs(paste0(., '_roll3'))) 

data.direction <- data %>% 
  mutate_if(is.numeric, funs(c(NA, if_else(diff(.)>0, 1, 0)))) %>% 
  rename_if(is.numeric, funs(paste0(., '_dir')))

data.diff <- data %>% 
  mutate_if(is.numeric, funs(c(NA, diff(.)))) %>% 
  rename_if(is.numeric, funs(paste0(., '_diff')))

data.diff.roll <- data.diff %>% 
  mutate_if(is.numeric, funs(c(NA, diff(.)))) %>% 
  rename_if(is.numeric, funs(paste0(., '_roll3')))

data <- data %>% 
  left_join(data.roll) %>% 
  left_join(data.direction) %>% 
  left_join(data.diff) %>% 
  left_join(data.diff.roll)
  
train.data <- data %>% 
  filter(date < sample.cutoff)

rm(data.roll, data.diff, data.direction, data.diff.roll)

### Liniar Point Components -------------------------------

# I'm doing what I know how to do here but my thought is take the measures that 
# make sense to me for the currencies trading on coinbase (+ ripple) and throw them
# in simple liniar models and then do an ensemble.  Obviously a rolling average or
# something will be more robust but I figure this is a decent place to start.

# Simple one day lag close price of top currencies 
mod.point.lm.close <-lm(ETH_close ~
           lag(ETH_close) +
           lag(BTC_close) +
           lag(XRP_close) +
           lag(BCH_close) +
           lag(LTC_close),
         data=train.data, na.action="na.omit")

summary(mod.point.lm.close)

data <- data %>% 
  add_predictions(mod.point.lm.close, var = 'pred.point.lm.close')

cor(data$ETH_close, data$pred.point.lm.close, use = 'complete.obs')

# We'll want to include volume somehow 
mod.point.lm.vol <-lm(ETH_close ~
                 lag(ETH_volume) +
                 lag(BTC_volume) +
                 lag(XRP_volume) +
                 lag(BCH_volume) +
                 lag(LTC_volume),
               data=train.data, na.action="na.omit")

summary(mod.point.lm.vol)

data <- data %>% 
  add_predictions(mod.point.lm.vol, var = 'pred.point.lm.volume')

cor(data$ETH_close, data$pred.point.lm.volume, use = 'complete.obs')

# This gets at the volitility information 
mod.point.lm.spread <-lm(ETH_close ~
                lag(ETH_spread) +
                lag(BTC_spread) +
                lag(XRP_spread) +
                lag(BCH_spread) +
                lag(LTC_spread),
              data=train.data, na.action="na.omit")

summary(mod.point.lm.spread)

data <- data %>% 
  add_predictions(mod.point.lm.spread, var = 'pred.point.lm.spread')

cor(data$ETH_close, data$pred.point.lm.spread, use = 'complete.obs')

# Google trends
mod.point.lm.trends <-lm(ETH_close ~
                  lag(google_bitcoin) +
                  lag(google_coinbase) +
                  lag(google_ethereum) +
                  lag(google_litecoin) +
                  lag(google_cryptocurrency),
                data=train.data, na.action="na.omit")

summary(mod.point.lm.trends)

data <- data %>% 
  add_predictions(mod.point.lm.trends, var = 'pred.point.lm.trends')

cor(data$ETH_close, data$pred.point.lm.trends, use = 'complete.obs')


### Liniar Binomial Components ----------------------------------

# For right now, the most important metric is direction so we may as well
# model that directly

mod.binom.lm.close <-glm(ETH_close_dir ~
                 lag(ETH_close_dir) +
                 lag(BTC_close_dir) +
                 lag(XRP_close_dir) +
                 lag(BCH_close_dir) +
                 lag(LTC_close_dir),
               data=data, na.action="na.omit", family = "binomial")

summary(mod.binom.lm.close)

data <- data %>%
  add_predictions(mod.binom.lm.close, var="pred.binom.lm.close")

cor(data$ETH_close_dir, data$pred.binom.lm.close, use = 'complete.obs')

### Liniar Difference Components --------------------------------

# This is really what we are after, difference in price 

# Simple one day lag close price of top currencies 
mod.diff.lm.close <-lm(ETH_close_diff ~
                          lag(ETH_close_diff) +
                          lag(BTC_close_diff) +
                          lag(XRP_close_diff) +
                          lag(BCH_close_diff) +
                          lag(LTC_close_diff),
                        data=train.data, na.action="na.omit")

summary(mod.diff.lm.close)

data <- data %>% 
  add_predictions(mod.diff.lm.close, var = 'pred.diff.lm.close')

cor(data$ETH_close_diff, data$pred.diff.lm.close, use = 'complete.obs')

# We'll want to include volume somehow 
mod.diff.lm.vol <-lm(ETH_close_diff ~
                        lag(ETH_volume_diff) +
                        lag(BTC_volume_diff) +
                        lag(XRP_volume_diff) +
                        lag(BCH_volume_diff) +
                        lag(LTC_volume_diff),
                      data=train.data, na.action="na.omit")

summary(mod.diff.lm.vol)

data <- data %>% 
  add_predictions(mod.diff.lm.vol, var = 'pred.diff.lm.volume')

cor(data$ETH_close_diff, data$pred.diff.lm.volume, use = 'complete.obs')

# This gets at the volitility information 
mod.diff.lm.spread <-lm(ETH_close_diff ~
                           lag(ETH_spread_diff) +
                           lag(BTC_spread_diff) +
                           lag(XRP_spread_diff) +
                           lag(BCH_spread_diff) +
                           lag(LTC_spread_diff),
                         data=train.data, na.action="na.omit")

summary(mod.diff.lm.spread)

data <- data %>% 
  add_predictions(mod.diff.lm.spread, var = 'pred.diff.lm.spread')

cor(data$ETH_close_diff, data$pred.diff.lm.spread, use = 'complete.obs')

# Google trends
mod.diff.lm.trends <-lm(ETH_close_diff ~
                           lag(google_bitcoin) +
                           lag(google_coinbase) +
                           lag(google_ethereum) +
                           lag(google_litecoin) +
                           lag(google_cryptocurrency),
                         data=train.data, na.action="na.omit")

summary(mod.diff.lm.trends)

data <- data %>% 
  add_predictions(mod.diff.lm.trends, var = 'pred.diff.lm.trends')

cor(data$ETH_close_diff, data$pred.diff.lm.trends, use = 'complete.obs')

### Ensembles ---------------------------------------------------

# Ensemble of Liniar Point Models
ens.point.lm <- lm(ETH_close ~ 
                     pred.point.lm.close +
                     pred.point.lm.volume +
                     pred.point.lm.spread +
                     pred.point.lm.trends,
             data = data, na.action = 'na.omit'
)

summary(ens.point.lm)

data <- data %>%
  add_predictions(ens.point.lm, var="pred.ens.point.lm")

cor(data$ETH_close, data$pred.ens.point.lm, use = 'complete.obs')

ggplot(data)+
  geom_line(aes(date, pred.ens.point.lm), color = 'red') +
  geom_line(aes(date, ETH_close))

# Ensemble of Liniar Difference Models
ens.diff.lm <- lm(ETH_close_diff ~ 
                    pred.diff.lm.close +
                    pred.diff.lm.volume +
                    pred.diff.lm.spread +
                    pred.diff.lm.trends,
                   data = data, na.action = 'na.omit'
)

summary(ens.diff.lm)

data <- data %>%
  add_predictions(ens.diff.lm, var="pred.ens.diff.lm")

cor(data$ETH_close_diff, data$pred.ens.diff.lm, use = 'complete.obs')

ggplot(data)+
  geom_line(aes(date, pred.ens.diff.lm), color = 'red') +
  geom_line(aes(date, ETH_close_diff))


### Model Evaluation --------------------------------------------

model.predictions <- data %>% 
  select(date, ETH_close, ETH_close_diff, contains("pred."))

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

test <- data.frame()
for(i in str_subset(names(model.predictions), "point")){
  temp <- model.eval(model.predictions, ETH_close, get(i))
  test <- rbind(test, temp)
}




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