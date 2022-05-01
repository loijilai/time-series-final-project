setwd("D:/WEN/1102course/Time Series Analysis/project/time-series-final-project")
# Dataset:
# https://www.kaggle.com/datasets/parisrohan/faang-stocks-covid190101202004012022
# If there is problem showing the graph:
dev.off()
par("mar")
#par(mar=c(1,1,1,1))

library(tidyverse)
data <- read.csv("faang_stocks_pandemic_data.csv")
data <- as_tibble(data)
head(data)
# Extract the closing price of Apple and stores in variable "close"
close <- data %>% filter(Name == "Apple") %>% pull(Close)
length(close) # There are 568 observations

# Initialize a time series object
close.ts <- ts(close)
plot(close.ts)


# Test for stationarity
library(tseries)
adf.test(close.ts) # Not stationary

# Differencing one time
apple.ts <- diff(close.ts)
plot(apple.ts) # may be stationary ?
adf.test(apple.ts) 
# p-value 0.01 we may reject null hypothesis 
# that the time series is not stationary

# Choose parameters based on ACF and PACF
acfplot <- acf(apple.ts, plot = F)
pacfplot <- pacf(apple.ts, plot = F)
plot(acfplot) # The ACF of apple.ts cuts off after lag 0
plot(pacfplot) # The PACF of apple.ts tails off

#try both Rate of Return and log(Rate of Return)
close_ror <- diff(close)/close[-568]
#log_ror <- log(close_ror) #it will produce some NaNs if we use log()
#sum(is.na(log_ror))=267
close_ror.ts <- ts(close_ror)
plot(close_ror.ts)

adf.test(close_ror.ts) #p-value=0.01
acf_ror <- acf(close_ror.ts) # The ACF of close_ror.ts cuts off after lag 1?
pacf_ror <- pacf(close_ror.ts) # The PACF of close_ror.ts tails off
#it seems better if we turn the closing price to RoR

library(forecast)
auto.arima(close_ror.ts) #ARIMA(1,0,0)

#try some different data that we might use
#download from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
library("dplyr")
covid <- read_csv("time_series_covid19_confirmed_global.csv")
covid <- covid[,c(-1:-4)]
covid_total <- apply(covid, 2, sum)
covid.ts <- ts(covid_total)
plot(covid.ts)# Not stationary

#refer to result of 
auto.arima(covid.ts) #ARIMA(3,2,4) 

#differencing once
d_covid.ts <- diff(covid.ts)
plot(d_covid.ts) 
adf.test(d_covid.ts) #p-value = 0.0129
#differencing twice
d_covid.ts <- diff(d_covid.ts)
plot(d_covid.ts) #this graph seems better
adf.test(d_covid.ts) #p-value = 0.01
#d=1? d=2?

acf_covid <- acf(d_covid.ts,150) # The ACF of d_covid.ts tails off
pacf_covid <- pacf(d_covid.ts) # The PACF of d_covid.ts tails off?

#arima(covid.ts, order=c(3,2,3)) 
#i have some questions about selecting (p,d,q)
#and i'm not sure whether the data is a better choice