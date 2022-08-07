####           Time Series           ######
###########################################

####  Air Passangers Data  #####

rm(list = ls())
# install.packages("forecast")
library(forecast)
library(tseries)
data("AirPassengers")
t<- AirPassengers


##1(a)
par(mfrow=c(1,2))
plot(t)
plot(log(t))

##1(b) Display and interpret the time series plots of the first difference of the logged series.
logt<- log(t)
logts<- ts(logt, frequency = 12, start = c(1949,1))
tslog<- diff(logts, lag = 1, differences = 1)
par(mfrow = c(1, 1))
plot(tslog)


##1(c) Display and interpret the time series plot of the seasonal difference of 
#the first difference of the logged series.

nsdiffs(tslog)# number of seasonal difference needed
tslogsdiff<- diff(tslog, lag=12, differences = 1)     # seasonal diff jonno lag = 12
plot(tslogsdiff)


##1(d) Calculate and interpret the sample ACF of the seasonal difference of the first
# difference of the logged series.
acf(tslogsdiff)  #ACF theke MA part decide korbo

##1(e) Fit an appropriate ARIMA model to the logged series.
acf(tslogsdiff) # acf suggest ma(1) sma(1) or ma(2) sma(1)
pacf(tslogsdiff,lag.max = 30) # acf suggest ar(1) sar(1) or ar(2) sar(1)


#candidate models are
# SARIMA[(1,1,1)(1,1,1)]12
# SARIMA[(1,1,2)(1,1,1)]12
# SARIMA[(0,1,1)(1,1,1)]12
# SARIMA[(0,1,1)(0,1,1)]12
# SARIMA[(1,1,0)(1,1,0)]12
# SARIMA[(2,1,1)(1,1,1)]12
# SARIMA[(2,1,2)(1,1,1)]12
# SARIMA[(2,1,0)(1,1,0)]12
#......

f1<-arima(logts, order = c(0,1,1),seasonal = list(order=c(1,1,1), period=12))
f2<-arima(logts, order = c(1,1,1),seasonal = list(order=c(1,1,1), period=12))
f3<-arima(logts, order = c(2,1,1),seasonal = list(order=c(1,1,1), period=12))

f4<-arima(logts, order = c(0,1,1),seasonal = list(order=c(0,1,1), period=12))
f5<-arima(logts, order = c(1,1,1),seasonal = list(order=c(0,1,1), period=12))
f6<-arima(logts, order = c(2,1,1),seasonal = list(order=c(0,1,1), period=12))

f7<-arima(logts, order = c(0,1,2),seasonal = list(order=c(1,1,1), period=12))
f8<-arima(logts, order = c(1,1,2),seasonal = list(order=c(1,1,1), period=12))
f9<-arima(logts, order = c(2,1,2),seasonal = list(order=c(1,1,1), period=12))

f10<-arima(logts, order = c(0,1,0),seasonal = list(order=c(1,1,1), period=12))
f11<-arima(logts, order = c(1,1,0),seasonal = list(order=c(1,1,1), period=12))
f12<-arima(logts, order = c(2,1,0),seasonal = list(order=c(1,1,1), period=12))

f13<-arima(logts, order = c(0,1,0),seasonal = list(order=c(0,1,1), period=12))
f14<-arima(logts, order = c(1,1,0),seasonal = list(order=c(0,1,1), period=12))
f15<-arima(logts, order = c(2,1,0),seasonal = list(order=c(0,1,1), period=12))


f16<-arima(logts, order = c(0,1,2),seasonal = list(order=c(0,1,1), period=12))
f17<-arima(logts, order = c(1,1,2),seasonal = list(order=c(0,1,1), period=12))
f18<-arima(logts, order = c(2,1,2),seasonal = list(order=c(0,1,1), period=12))


aic<-c(f1$aic,f2$aic,f3$aic,f4$aic,f5$aic,f6$aic,
       f7$aic,f8$aic,f9$aic,f10$aic,f11$aic,f12$aic,
       f13$aic,f14$aic,f15$aic,f16$aic,f17$aic,f18$aic)

data.frame(model=1:18,aic)

# model 4 and model 6 are the best model

# Shortcut: 
auto.arima(logts)

# Parameter Estimate
f4$coef
f6$coef

##1(f) forecast for lead 2 years including forecast limits
plot(forecast(f4, h = 24))
forecast(f4, h=32)
















##################################################
######     Time series data simulation     #######
##################################################


rm(list = ls())
set.seed(123)
t <- 1000

# white noise:
wt <- ts(rnorm(t,0,1))

# initialize the first value:
# AR(1): Yt = phi*Y(t-1) + et
ar1 <- wt[1]
for(i in 2:t){
  ar1[i]<- ar1[i - 1]*0.8 + wt[i]
  
}

# MA(1): Yt = et - theta* et-1    ?akhane + niye korche!
ma1<- wt[1]
for(i in 2:t){
  ma1[i]<- wt[i-1]*0.8 + wt[i]
  
}

# ARMA(1,1): Yt =  phi*Y(t-1) + et + theta* e(t-1)
arma11<- wt[1]
for(i in 2:t){
  arma11[i]   <- arma11[i - 1] * 0.8 + wt[i - 1] * 0.8 + wt[i]
}


# ARMA(2,2): Yt =  phi1*Y(t-1) + phi2*Y(t-2)+ et + theta1* e(t-1) + + theta2* e(t-2)
arma22 <- wt[1:2]
for(i in 3:t){
  arma22[i]   <- arma22[i - 1] * 0.8 + arma22[i - 2]  * (-0.3) + 0.8 * wt[i-1] - 0.3 * wt[i-2] + wt[i]
}


# turn them into time series, and for the last two, "integrate" them via cumulative sum
arima111 <- cumsum(arma11) # d = 1
arima222 <- cumsum(cumsum(arma22)) # d = 2

ar1 <- ts(ar1)
ma1 <- ts(ma1)
arma11 <- ts(arma11)
arima111 <- ts(arima111)
arima222 <- ts(arima222)

set.seed(123)
ar1_sim<-arima.sim(model = list(order = c(1,0,0 ),ar=.8), n = 1000)
ma1_sim<-arima.sim(model = list(order = c(0,0,1 ),ma=.8), n = 1000)
arma11_sim<-arima.sim(model = list(order = c(1,0,1 ),ar=.8, ma=.8), n = 1000)
arima111_sim<-arima.sim(model = list(order = c(1,1,1 ),ar=.8, ma=.8), n = 1000)
arima222_sim<-arima.sim(model = list(order = c(2,2,2 ),ar=c(.8,-.3),ma=c(.8,-.3)), n = 1000)


auto.arima(ar1)
auto.arima(ma1)
auto.arima(arma11)
auto.arima(arima111)
auto.arima(arima222)


auto.arima(ar1_sim)
auto.arima(ma1_sim)
auto.arima(arma11_sim)
auto.arima(arima111_sim)
auto.arima(arima222_sim)

par(mfrow=c(2,2), bg="skyblue")
acf(ar1)
pacf(ar1)

acf(ar1_sim)
pacf(ar1_sim)

acf(ma1)
pacf(ma1)

acf(arma11)
pacf(arma11)
