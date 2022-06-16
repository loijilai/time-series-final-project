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

# 1. First perspective of the seasonal terms: (2, 1, 0)

# Adding ordinary MA terms
modelA1 <- sarima(metro.ts, 0, 0, 1, 2, 1, 0, 12)
# Bad Resiual ACF and Ljung-Box test -> reject
modelA2 <- sarima(metro.ts, 0, 0, 2, 2, 1, 0, 12)
# Not much improvement from modelA1 -> reject

# Adding ordinary AR terms
modelA3 <- sarima(metro.ts, 1, 0, 0, 2, 1, 0, 12) 
# Bad Ljung-Box test -> reject
modelA4 <- sarima(metro.ts, 2, 0, 0, 2, 1, 0, 12) 
# Good Residual ACF and Ljung-Box test -> accept
modelA5 <- sarima(metro.ts, 3, 0, 0, 2, 1, 0, 12) 
# The improvement from modelA4 is not obvious, 
# maybe modelA4 is preferable, but I still preserve this one -> accept

# Adding both ordinary AR and MA
modelA6 <- sarima(metro.ts, 2, 0, 1, 2, 1, 0, 12) 
# Slightly improve the residual analysis but not obvious -> accept


# 2. Second perspective of the seasonal terms: (0, 1, 1)

# Adding ordinary MA terms
modelB1 <- sarima(metro.ts, 0, 0, 1, 0, 1, 1, 12) 
# Bad Residual ACF and Ljung-Box test -> reject
modelB2 <- sarima(metro.ts, 0, 0, 2, 0, 1, 1, 12) 
# Same problem as model modelB1 -> reject
modelB3 <- sarima(metro.ts, 0, 0, 5, 0, 1, 1, 12) 
# Slightly better Residual ACF but Ljung-Box test still fails -> reject

# Conclusion: only adding ordinary MA does not pass the residual analysis

# Adding ordinary AR terms
modelB4 <- sarima(metro.ts, 1, 0, 0, 0, 1, 1, 12)
# Good residual ACF but Ljung-Box test fails -> reject
modelB5 <- sarima(metro.ts, 2, 0, 0, 0, 1, 1, 12) 
# Good residual ACF and Ljung-Box -> accept
modelB6 <- sarima(metro.ts, 3, 0, 0, 0, 1, 1, 12)
# There is not much improvement from model14
# So the order of ordinary AR may be too high in modelB6
# Conclusion: modelB5 may be a better choice because it is simpler -> accept

# Adding both ordinary AR and MA
modelB7 <- sarima(metro.ts, 2, 0, 1, 0, 1, 1, 12)
# Not much improvement from modelB5 -> accept
modelB8 <- sarima(metro.ts, 2, 0, 2, 0, 1, 1, 12)
# Not much improvement from modelB5 -> reject


# Conclusion in model fitting:
# From perspective 1.
modelA4$AIC # 31.84105
modelA5$AIC # 31.84204
modelA6$AIC # 31.83081  

# From perspective 2.
modelB5$AIC # 31.85006
modelB6$AIC # 31.85451
modelB7$AIC # 31.84526  

# ---------------------------------------------------------------------

# Part 3. Prediction
#forecast
fcastA4 <- sarima.for(metro.ts, n.ahead = 12, 2, 0, 0, 2, 1, 0, 12)
fcastA5 <- sarima.for(metro.ts, n.ahead = 12, 3, 0, 0, 2, 1, 0, 12)
fcastA6 <- sarima.for(metro.ts, n.ahead = 12, 2, 0, 1, 2, 1, 0, 12)
fcastB5 <- sarima.for(metro.ts, n.ahead = 12, 2, 0, 0, 0, 1, 1, 12)
fcastB6 <- sarima.for(metro.ts, n.ahead = 12, 3, 0, 0, 0, 1, 1, 12)
fcastB7 <- sarima.for(metro.ts, n.ahead = 12, 2, 0, 1, 0, 1, 1, 12)
m.ts <- ts(metro[1:264,2], start=c(1998, 1), frequency=12)
lines(m.ts) #compare to actual values

test_data <- unlist(metro[253:264, 2], use.names=FALSE)
#MSE is too large
RMSE(test_data, fcastA4$pred) # 1738694
RMSE(test_data, fcastA5$pred) # 1724152 <-
RMSE(test_data, fcastA6$pred) # 1691764
RMSE(test_data, fcastB5$pred) # 1881868
RMSE(test_data, fcastB6$pred) # 1862180 <-
RMSE(test_data, fcastB7$pred) # 1790835
