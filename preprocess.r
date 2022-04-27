setwd("/home/loijilai/CS-hub/myR/time-series/time-series-final-project")
# Dataset:
# https://www.kaggle.com/datasets/parisrohan/faang-stocks-covid190101202004012022
# If there is problem showing the graph:
dev.off()
par("mar")
par(mar=c(1,1,1,1))

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
