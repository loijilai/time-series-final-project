#setwd("/home/loijilai/CS-hub/myR/time-series/time-series-final-project")
setwd("D:/WEN/1102course/Time Series Analysis/project/time-series-final-project")
# download from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
library("dplyr")
library("tidyverse")
library("astsa")
library("forecast")
library("tseries")

# Step 1. 讀取資料
covid <- read_csv("time_series_covid19_confirmed_global.csv")[,c(-1:-4)]
covid_total <- apply(covid, 2, sum)

covid.ts <- ts(diff(covid_total)) # 建立每日確診數的時間數列
covid600.ts <- ts(diff(covid_total)[1:600])
plot(covid.ts)
plot(covid600.ts)
#if we check the small part of data 
#we can find that it might have a weekly regular pattern (lag = 7)
plot(covid.ts[200:250], type = "b")
acf(covid.ts,120)
pacf(covid.ts)

#based on  all data
#do seasonal differencing first
sd_covid.ts <- diff(covid.ts, 7)
plot(sd_covid.ts)
adf.test(sd_covid.ts) #p-value = 0.01, stationary
acf2(sd_covid.ts)#it seems need to take first difference

#first-difference
sdd_covid.ts <- diff(sd_covid.ts)
plot(sdd_covid.ts, type = "l")
acf2(sdd_covid.ts)

fit1 <- sarima(covid.ts, 0, 1, 1, 0, 1, 1, 7)
fit2 <- sarima(covid.ts, 0, 1, 3, 0, 1, 1, 7)
fit3 <- sarima(covid.ts, 0, 1, 4, 0, 1, 1, 7)
fit4 <- sarima(covid.ts, 2, 1, 0, 0, 1, 1, 7)
fit5 <- sarima(covid.ts, 2, 1, 4, 0, 1, 1, 7)
fit6 <- sarima(covid.ts, 2, 1, 4, 2, 1, 1, 7)
fit7 <- sarima(covid.ts, 2, 1, 3, 2, 1, 1, 7)
fit8 <- sarima(covid.ts, 2, 1, 1, 2, 1, 1, 7)

fit1$AIC #26.07277
fit2$AIC #26.06357
fit3$AIC #26.06554
fit4$AIC #26.06524
fit5$AIC #26.01717
fit6$AIC #25.9925
fit7$AIC #25.9892(v)
fit8$AIC #26.0168

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

fitt1 <- sarima(covid600.ts, 0, 1, 1, 0, 1, 1, 7)
fitt2 <- sarima(covid600.ts, 0, 1, 1, 0, 1, 2, 7)

fitt1$AIC #24.64652
fitt2$AIC #24.64103(v)

