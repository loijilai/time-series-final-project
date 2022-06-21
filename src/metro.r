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

# if only do seasonal differencing
sdoriginalts <- diff(originalts, 12)
plot(sdoriginalts)
adf.test(sdoriginalts) #p-value = 0.0165 < 0.05, reject H0: non-stationary
acf2(sdoriginalts)
#only doing seasonal differecing seems an acceptable choice

# ---------------------------------------------------------------------

# Part 2. Model Fitting

# 1. First perspective of the seasonal order: SARIMA(2, 1, 0)

# Adding ordinary MA
fitA1 <- sarima(metro.ts, 0, 0, 1, 2, 1, 0, 12)
# Bad Resiual ACF and Ljung-Box test -> reject
fitA2 <- sarima(metro.ts, 0, 0, 2, 2, 1, 0, 12)
# Not much improvement from fitA1 -> reject

# Adding ordinary AR
fitA3 <- sarima(metro.ts, 1, 0, 0, 2, 1, 0, 12) 
# Bad Ljung-Box test -> reject
fitA4 <- sarima(metro.ts, 2, 0, 0, 2, 1, 0, 12) 
# Good residual ACF and Ljung-Box test -> accept
fitA5 <- sarima(metro.ts, 3, 0, 0, 2, 1, 0, 12) 
# The improvement from fitA4 is not obvious, 
# maybe fitA4 is preferable, but I still preserve this one -> accept

# Adding both ordinary AR and MA
# Accept all models because I only consider residual ACF and Ljung-Box result here
fitA6 <- sarima(metro.ts, 1, 0, 1, 2, 1, 0, 12) 
fitA7 <- sarima(metro.ts, 2, 0, 1, 2, 1, 0, 12) 
fitA8 <- sarima(metro.ts, 1, 0, 2, 2, 1, 0, 12) 
fitA9 <- sarima(metro.ts, 2, 0, 2, 2, 1, 0, 12) 


# 2. Second perspective of the seasonal order: SARIMA(0, 1, 1)

# Adding ordinary MA
fitB1 <- sarima(metro.ts, 0, 0, 1, 0, 1, 1, 12) 
# Bad Residual ACF and Ljung-Box test -> reject
fitB2 <- sarima(metro.ts, 0, 0, 2, 0, 1, 1, 12) 
# Same problem as fitB1-> reject
fitB3 <- sarima(metro.ts, 0, 0, 5, 0, 1, 1, 12) 
# Slightly better Residual ACF but Ljung-Box test still fails -> reject

# Conclusion: only adding ordinary MA does not pass the residual analysis

# Adding ordinary AR
fitB4 <- sarima(metro.ts, 1, 0, 0, 0, 1, 1, 12)
# Good residual ACF but Ljung-Box test fails -> reject
fitB5 <- sarima(metro.ts, 2, 0, 0, 0, 1, 1, 12) 
# Good residual ACF and Ljung-Box -> accept
fitB6 <- sarima(metro.ts, 3, 0, 0, 0, 1, 1, 12)
# There is not much improvement from fitB5
# fitB5 may be a better choice because it is simpler 
# However, I do not have evidence based on only ACF residual and Ljung-Box test
# so I preserve this model -> accept

# Adding both ordinary AR and MA
# Accept all models because I only consider residual ACF and Ljung-Box result here
fitB7 <- sarima(metro.ts, 1, 0, 1, 0, 1, 1, 12)
fitB8 <- sarima(metro.ts, 2, 0, 1, 0, 1, 1, 12)
fitB9 <- sarima(metro.ts, 1, 0, 2, 0, 1, 1, 12)
# fitB10 can not be fitted(NaNs produced). I don't know why so I skip this model in the final paper
# fitB10 <- sarima(metro.ts, 2, 0, 2, 0, 1, 1, 12)


# ---------------------------------------------------------------------
# Part 3. Parameter Estimation

# Match names of fitted model in source code with final paper
# fitA4 = ModelA1
# fitA6 = ModelA2

# fitB5 = ModelB1
# fitB7 = ModelB2

# From perspective 1.

# ARIMA(2, 0, 0)*(2, 1, 0) 
fitA4 # Pass parameter estimation -> Accept

# ARIMA(3, 0, 0)*(2, 1, 0) 
fitA5 # Fail parameter estimation at AR3 -> Reject

# ARIMA(1, 0, 1)*(2, 1, 0) 
fitA6 # Pass parameter estimation -> Accept
# ARIMA(2, 0, 1)*(2, 1, 0) 
fitA7 # Fail parameter estimation -> Reject
# ARIMA(1, 0, 2)*(2, 1, 0) 
fitA8 # Fail parameter estimation -> Reject
# ARIMA(2, 0, 2)*(2, 1, 0) 
fitA9 # Fail parameter estimation -> Reject


# From perspective 2.

# ARIMA(2, 0, 0)*(0, 1, 1) 
fitB5 # Pass parameter estimation -> Accept

# ARIMA(3, 0, 0)*(0, 1, 1) 
fitB6 # Fail parameter estimation at AR3 -> Reject

# ARIMA(1, 0, 1)*(0, 1, 1) 
fitB7 # Pass parameter estimation -> Accept
# ARIMA(2, 0, 1)*(0, 1, 1) 
fitB8 # Fail parameter estimation -> Reject
# ARIMA(1, 0, 2)*(0, 1, 1) 
fitB9 # Fail parameter estimation -> Reject

# ---------------------------------------------------------------------

# Part 4. Model Selection using AIC

fitA4$AIC #31.84105
fitA6$AIC #31.8302 <-
fitB5$AIC #31.85006
fitB7$AIC #31.83999

# ---------------------------------------------------------------------

# Part 5. Prediction
#forecast
fcastA4 <- sarima.for(metro.ts, n.ahead = 12, 2, 0, 0, 2, 1, 0, 12)
fcastA6 <- sarima.for(metro.ts, n.ahead = 12, 1, 0, 1, 2, 1, 0, 12)
fcastB5 <- sarima.for(metro.ts, n.ahead = 12, 2, 0, 0, 0, 1, 1, 12)
fcastB7 <- sarima.for(metro.ts, n.ahead = 12, 1, 0, 1, 0, 1, 1, 12)
m.ts <- ts(metro[1:264,2], start=c(1998, 1), frequency=12)
lines(m.ts) #compare to actual values

test_data <- unlist(metro[253:264, 2], use.names=FALSE)
#MSE is too large
RMSE(test_data, fcastA4$pred) # 1738694
RMSE(test_data, fcastA6$pred) # 1699324
RMSE(test_data, fcastB5$pred) # 1881868
RMSE(test_data, fcastB7$pred) # 1820581
