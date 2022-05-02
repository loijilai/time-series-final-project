setwd("/home/loijilai/CS-hub/myR/time-series/time-series-final-project")#try some different data that we might use
#download from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
library("dplyr")
covid <- read_csv("time_series_covid19_confirmed_global.csv")
covid <- covid[,c(-1:-4)]
covid_total <- apply(covid, 2, sum)
covid.ts <- ts(covid_total)
plot(covid.ts)# Not stationary

#refer to result of 
auto.arima(covid.ts) #ARIMA(3,2,4) 
auto.arima(covid.ts, trace = T, stepwise = F, approximation = F) 
# Another choice: ARIMA(2, 2, 3)

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
# 也許可以說是cuts off after 9 lags? -> AR

#arima(covid.ts, order=c(3,2,3)) 
#i have some questions about selecting (p,d,q)
#and i'm not sure whether the data is a better choice

fit1 <- arima(covid.ts, order=c(9,2,0), seasonal=list(order=c(0,1,1), period=12) )
fit1
