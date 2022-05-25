setwd("/home/loijilai/CS-hub/myR/time-series/time-series-final-project")
#setwd("D:/WEN/1102course/Time Series Analysis/project/time-series-final-project")
# download from: https://data.taipei/dataset/detail?id=a0183350-7951-4f4b-993d-0e8ebae04b4c
library("tidyverse")
library("astsa")
library("forecast")
library("tseries")

metro <- read.csv("datasets/臺北市捷運客運概況按月別時間數列統計資料.csv")[,c(1,2)]
head(metro)
metro.ts <- ts(metro[1:264,2], start=c(1998, 1), frequency=12)
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

model1 <- sarima(sddoriginalts, 0, 1, 0, 0, 1, 1, 12)
model2 <- sarima(sddoriginalts, 0, 1, 0, 1, 1, 1, 12)
model3 <- sarima(sddoriginalts, 0, 1, 0, 2, 1, 1, 12)
model4 <- sarima(sddoriginalts, 1, 1, 1, 2, 1, 1, 12)
model5 <- sarima(sddoriginalts, 3, 1, 1, 2, 1, 1, 12)

model1$AIC # 33.57524
model2$AIC # 33.30868
model3$AIC # 33.09823
model4$AIC # 32.15482
model5$AIC # 32.13897 (<-)



# The log version

logts <- log(metro.ts) # non-constant variance
plot(logts)
dlogts <- diff(logts) 
plot(dlogts)
adf.test(dlogts)
acf2(dlogts, max.lag = 100) # Observe that there is seasonality
sddlogts <- diff(dlogts, 12)
plot(sddlogts)
acf2(sddlogts)

fit1 <- sarima(sddlogts, 0, 1, 0, 0, 1, 1, 12)
fit2 <- sarima(sddlogts, 0, 1, 0, 1, 1, 1, 12)
fit3 <- sarima(sddlogts, 0, 1, 0, 2, 1, 1, 12)
fit4 <- sarima(sddlogts, 3, 1, 3, 2, 1, 1, 12)

fit1$AIC # -0.9007193
fit2$AIC # -1.179481
fit3$AIC # -1.343357
fit4$AIC # -2.064412

