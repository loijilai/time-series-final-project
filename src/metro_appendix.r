setwd("/home/loijilai/CS-hub/myR/time-series/time-series-final-project")
#setwd("D:/WEN/1102course/Time Series Analysis/project/time-series-final-project")
# download from: https://data.taipei/dataset/detail?id=a0183350-7951-4f4b-993d-0e8ebae04b4c
library("tidyverse")
library("astsa")
library("forecast")
library("tseries")
library("MLmetrics")

# Part 1. Data Preprocessing
metro <- read_csv("datasets/臺北市捷運客運概況按月別時間數列統計資料.csv")[,c(1,2)]
head(metro)
metro.ts <- ts(metro[1:252,2], start=c(1998, 1), frequency=12) # 264-12=252
plot(metro.ts)
# Plot the time series
originalts <- metro.ts
plot(originalts)

# Seasonal differencing + ordinary differencing
doriginalts <- diff(originalts)
plot(doriginalts)
adf.test(doriginalts)#p-value = 0.01 < 0.05, reject H0: non-stationary
acf2(doriginalts, max.lag = 100)
sddoriginalts <- diff(doriginalts, 12)
plot(sddoriginalts)
acf2(sddoriginalts)

# Part 2. Model Fitting
model1 <- sarima(metro.ts, 0, 1, 0, 2, 1, 1, 12)
model2 <- sarima(metro.ts, 0, 0, 0, 1, 1, 0, 12)

model3 <- sarima(metro.ts, 0, 1, 1, 2, 1, 1, 12)
model4 <- sarima(metro.ts, 0, 1, 1, 2, 1, 0, 12)

model5 <- sarima(metro.ts, 1, 1, 1, 2, 1, 1, 12) # Previous best
model6 <- sarima(metro.ts, 1, 1, 1, 2, 1, 0, 12)

model1$AIC # 31.9265
model2$AIC # 31.92074
model3$AIC # 31.82654
model4$AIC # 31.83403
model5$AIC # 31.8205  <-
model6$AIC # 31.82421

model3 # Figure 17
model4 # Figure 18
model5 # Figure 19
model6 # Figure 20


# Part 3. Model prediction

# Model 3 Prediction
fcast1 <- predict(model3$fit, n.ahead=12)

fcast1$pred
fcast1.U <- fcast1$pred + 1.96*fcast1$se
fcast1.L <- fcast1$pred - 1.96*fcast1$se

min(fcast1.L)
max(fcast1.U)
metro[253:264, 2]
# par(mar=c(3,3,2,1))
plot(c(metro.ts[200:252], rep(NA,12)), ylim = c(54620801, 81326166), type = "l")
lines(length(metro.ts[200:252])+(1:12), fcast1$pred, col=2)
lines(length(metro.ts[200:252])+(1:12), fcast1.U, col=3, lty=2)
lines(length(metro.ts[200:252])+(1:12), fcast1.L, col=3, lty=2)
points(length(metro.ts[200:252])+(1:12), metro[253:264, 2], pch=16)


# Model 5 Prediction
fcast2 <- predict(model5$fit, n.ahead=12)

fcast2$pred
fcast2.U <- fcast2$pred + 1.96*fcast2$se
fcast2.L <- fcast2$pred - 1.96*fcast2$se

min(fcast2.L)
max(fcast2.U)
metro[253:264, 2]
# par(mar=c(3,3,2,1))
plot(c(metro.ts[200:252], rep(NA,12)), ylim = c(54620801, 81326166), type = "l")
lines(length(metro.ts[200:252])+(1:12), fcast2$pred, col=2)
lines(length(metro.ts[200:252])+(1:12), fcast2.U, col=3, lty=2)
lines(length(metro.ts[200:252])+(1:12), fcast2.L, col=3, lty=2)
points(length(metro.ts[200:252])+(1:12), metro[253:264, 2], pch=16)

# Branch W
#forecast
m3 <- sarima.for(metro.ts, n.ahead = 12, 0, 1, 1, 2, 1, 1, 12)
m4 <- sarima.for(metro.ts, n.ahead = 12, 0, 1, 1, 2, 1, 0, 12)
m5 <- sarima.for(metro.ts, n.ahead = 12, 1, 1, 1, 2, 1, 1, 12)
m6 <- sarima.for(metro.ts, n.ahead = 12, 1, 1, 1, 2, 1, 0, 12)
m.ts <- ts(metro[1:264,2], start=c(1998, 1), frequency=12)
lines(m.ts) #compare to actual values

a <- unlist(metro[253:264, 2], use.names=FALSE)
#MSE is too large
RMSE(a, m3$pred)#1679145
RMSE(a, m4$pred)#1828481
RMSE(a, m5$pred)#1695756
RMSE(a, m6$pred)#1830158
