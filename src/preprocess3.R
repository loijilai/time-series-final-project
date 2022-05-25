setwd("/home/loijilai/CS-hub/myR/time-series/time-series-final-project")
#setwd("D:/WEN/1102course/Time Series Analysis/project/time-series-final-project")
# download from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
library("dplyr")
library("tidyverse")
library("astsa")
library("forecast")
library("tseries")

# Step 1. 讀取資料
covid <- read_csv("time_series_covid19_confirmed_global.csv")[,c(-1:-4)]
covid_total <- apply(covid, 2, sum)

covid600.ts <- ts(diff(covid_total)[1:600])
plot(covid600.ts)
# The variance is not constant, it increases over time
# sq_covid600.ts <- plot(sqrt(covid600.ts))


#if we check the small part of data 
#we can find that it might have a weekly regular pattern (lag = 7)
plot(covid600.ts[200:250], type = "b")
acf2(covid600.ts, max.lag = 150)

#only based on fisrt 600 data
#do seasonal differencing first
sd_covid600.ts <- diff(covid600.ts, 7)
plot(sd_covid600.ts)
adf.test(sd_covid600.ts) #p-value = 0.01, stationary
acf2(sd_covid600.ts)#it seems need to take first difference

#first-difference
sdd_covid600.ts <- diff(sd_covid600.ts)
plot(sdd_covid600.ts, type = "l")
acf2(sdd_covid600.ts)
# PACF spikes at lag = 13, 14, 15, 20, 27

fit1 <- sarima(covid600.ts, 0, 1, 1, 0, 1, 1, 7)

fit2 <- sarima(covid600.ts, 0, 1, 1, 0, 1, 2, 7)
# Nonseasonal part:
# acf cuts off after lag = 1, pacf tails off -> MA(1)
# Seasonal part:
# acf cuts off after lag = 2 (s = 7), pacf tails off -> SMA(2)

fit3 <- sarima(covid600.ts, 0, 1, 1, 1, 1, 2, 7)
# Seasonal part:
# acf cuts off after lag = 2 (s = 7), pacf cuts off after lag = 1 (s = 7)
# -> SARMA(1, 2)

fit4 <- sarima(covid600.ts, 0, 1, 1, 2, 1, 2, 7)
# Seasonal part:
# acf cuts off after lag = 2 (s = 7), pacf cuts off after lag = 2 (s = 7)
# -> SARMA(2, 2)

fit5 <- sarima(covid600.ts, 0, 1, 1, 3, 1, 2, 7)
# Seasonal part:
# acf cuts off after lag = 2 (s = 7), pacf cuts off after lag = 3 (s = 7)
# -> SARMA(3, 2)

fit6 <- sarima(covid600.ts, 0, 1, 1, 3, 1, 4, 7)
# -> SARMA(3, 4)

fit1$AIC #24.64652
fit2$AIC #24.64103
fit3$AIC #24.63804
fit4$AIC #24.64044
fit5$AIC #24.62478
fit6$AIC #24.6071(preferred)

