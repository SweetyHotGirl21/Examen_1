remove(list = ls())

library(forecast)
library(ggplot2)
library(fma)
library(expsmooth)
library(fpp2)

DATA = read.csv(file = "Data.csv", header = TRUE, sep = ";")

GDP = ts(DATA[,2], frequency = 4, start = c(1978, 1))

#Training Data: Q1 1978 - Q4 2012
GDP.train = window(GDP, end = (2012 + 3/4))

#Descriptiv analysis of Data
autoplot(GDP.train)+geom_smooth(method = loess, formula = y ~ x)+
  ggtitle("Gross Domestic Product Norway")



autoplot(X)
gglagplot(X)
var(X)
ggAcf(X, lag.max = 20)
ggsubseriesplot(X)
aes(X)

fcast.choc.S   <- forecast(choc.STL$time.series[,"seasonal"], h = h1)
summary(fcast.choc.S)
fcast.choc.T   <- forecast(choc.STL$time.series[,"trend"], h = h1)
summary(fcast.choc.T)
fcast.choc.R   <- forecast(choc.STL$time.series[,"remainder"], h = h1)
summary(fcast.choc.R)


