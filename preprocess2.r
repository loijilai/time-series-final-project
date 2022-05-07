setwd("/home/loijilai/CS-hub/myR/time-series/time-series-final-project")
# setwd("D:/WEN/1102course/Time Series Analysis/project/time-series-final-project")
# download from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
library("dplyr")

# Step 1. 讀取資料
covid <- read_csv("time_series_covid19_confirmed_global.csv")[,c(-1:-4)]
covid_total <- apply(covid, 2, sum)

covid.ts <- ts(diff(covid_total)) # 建立每日確診數的時間數列
plot(covid.ts)# Not stationary

# Step 2. 預處理：將時間數列調整成Stationary
acf(covid.ts,150)
# 觀察到可能有季節性
pacf(covid.ts,150)

# differencing once
d_covid.ts <- diff(covid.ts)
plot(d_covid.ts) 
acf(d_covid.ts,150)
# 觀察到可能有lag=7的季節性

# 以lag=7做季節性差分
sd_covid.ts <- diff(d_covid.ts, 7)
plot(sd_covid.ts)
adf.test(sd_covid.ts) #p-value < 0.01
# 應該是達成了調整成stationary的目標


# Step 3. Model Selection: 決定ARMA的order

# 第一種：用所有資料做配適
auto.arima(sd_covid.ts, trace = T, stepwise = F, approximation = F) 
# Suggestion: ARIMA(2, 0, 3)

acf(sd_covid.ts,15)
# 好像可以說cuts off after lag = 4 -> MA(4) ?
pacf(sd_covid.ts,15)
# cuts off after lag = 2 -> AR(2) ?

fit1 <- arima(sd_covid.ts, order=c(2,0,4))
fit1$aic # 21359.18 

fit2 <- arima(sd_covid.ts, order=c(2,0,3))
fit2$aic # 21423.11

# fit1 優於 fit2 -> 選擇fit1

# 第二種：用前600筆資料做配適
sd_600_covid.ts <- ts(sd_covid.ts[1:600])
plot(sd_600_covid.ts)
acf(sd_600_covid.ts, 15) # Cuts off after lag = 1 or 8
pacf(sd_600_covid.ts, 15) # Cuts off after lag = 8

auto.arima(sd_600_covid.ts, trace = T, stepwise = F, approximation = F) 
# Suggestion: ARIMA(1, 0, 2)

# 根據acf和pacf的結果分別測試
fit3 <- arima(sd_600_covid.ts, order=c(8,0,1))
fit4 <- arima(sd_600_covid.ts, order=c(8,0,8))

fit3$aic # 14889.62
fit4$aic # 14776.86

# fit4 優於 fit3 -> 選擇fit4

# Step 4. Diagnostic and residual analysis
# 將step3選出的兩個模型進行診斷
library(TSA)
ts.plot(rstandard(fit1)) 
ts.plot(rstandard(fit4))
