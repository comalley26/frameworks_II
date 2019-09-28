setwd("C:/Users/casey/OneDrive/Documents/Frameworks II/HTML Files and Datasets")

# Frameworks II Time Series HW Part I

library(tidyverse)
library(ggplot2)
library(quantmod)
library(xts)
library(zoo)
library(forecast)
library(fpp)
library(fpp2)
library(dplyr)
library(ggthemes)

goog <- readRDS('goog.RDS')

class(goog) #xts, zoo

goog # price was 221.0374 in June 2010

goog[37:48] %>% mean() # average price in 2010 was 260.9768

length(goog) # 142 months of data

cor(goog, lag(goog, 1), use='complete.obs') # 0.9923893

google <- ts(goog,start=c(2007,01),frequency=12)
train <- window(google,start=c(2007,01),end=c(2015,12))
test <- window(google,start=c(2016,01),end=c(2018,10))

length(train) # 108 months

ggAcf(train) # first month has highest bar

# Part II

avg_model <- meanf(train, h = 34)

avg_model$fitted # mean value for predictions is 355.776

accuracy(avg_model) # train RMSE is 144.5429

accuracy(avg_model, x = google) # test RMSE is 588.3496

naive_model <- naive(train, h = 34)

naive_model # point forecast is 758.88

accuracy(naive_model, x = google) # RMSE is 230.50860

# Part III

ets_model <- ets(train, model = 'AAA') # error, trend, and seasonality are all additive features

autoplot(forecast(ets_model))

summary(ets_model) # AICc is 1255.624

ggAcf(forecast(ets_model)) # Not white noise

forecast(ets_model, h = 34) # October 2018 is 1028.4942

accuracy(forecast(ets_model, h = 34), x = test) # RMSE is 102.20154

# Part IV

auto_arima_model <- auto.arima(train)

summary(auto_arima_model) # (0, 1, 0) with drift

checkresiduals(auto_arima_model) # p-value not significant (yes white noise)

forecast(auto_arima_model, h = 34) # Oct 2018 point forecast is 920.8568

accuracy(forecast(auto_arima_model, h = 34), google) # test RMSE is 143.69172

BoxCox.lambda(train) # 0.4748787

arima_model <- Arima(train,
                     order = c(1,1,1),
                     seasonal = c(3,1,0),
                     lambda=BoxCox.lambda(train))

arima_model$aicc # 343.1639

checkresiduals(arima_model) # p-value not significant (yes white noise)

forecast(arima_model, h = 34) # Oct 2018 point forecast is 1165.1694

accuracy(forecast(arima_model, h = 34), google) # test RMSE is 56.19179
