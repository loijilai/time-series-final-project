setwd("/home/loijilai/CS-hub/myR/time-series/time-series-final-project")
#setwd("D:/WEN/1102course/Time Series Analysis/project/time-series-final-project")
# download from: https://data.taipei/dataset/detail?id=a0183350-7951-4f4b-993d-0e8ebae04b4c
library("tidyverse")
library("astsa")
library("forecast")
library("tseries")

metro <- read.csv("datasets/臺北市捷運客運概況按月別時間數列統計資料.csv")[,c(1,2)]
head(metro)
metro.ts <- ts(metro[1:252,2], start=c(1998, 1), frequency=12) # 264-12=252
plot(metro.ts)
# Observe that there is trend and non-constant variance

# Plot the time series
originalts <- metro.ts
plot(originalts)

# Detrend by differencing once
doriginalts <- diff(originalts)
plot(doriginalts)
plot(doriginalts[150:180], type = 'l') # Seasonality present

adf.test(doriginalts)
acf2(doriginalts, max.lag = 100)

sddoriginalts <- diff(doriginalts, 12)
plot(sddoriginalts)
acf2(sddoriginalts)

model1 <- sarima(metro.ts, 0, 1, 0, 0, 1, 1, 12)
model2 <- sarima(metro.ts, 0, 1, 0, 1, 1, 1, 12)
model3 <- sarima(metro.ts, 0, 1, 0, 2, 1, 1, 12)
model4 <- sarima(metro.ts, 1, 1, 1, 2, 1, 1, 12)

# Teacher's suggestion
model6 <- sarima(metro.ts, 3, 1, 0, 2, 1, 0, 12)
model7 <- sarima(metro.ts, 3, 1, 0, 2, 1, 1, 12)
model8 <- sarima(metro.ts, 3, 1, 1, 2, 1, 0, 12)

model1$AIC # 31.97018
model2$AIC # 31.9448
model3$AIC # 31.9265
model4$AIC # 31.8205 <-

model6$AIC # 31.84111
model7$AIC # 31.83588
model8$AIC # 31.83882

