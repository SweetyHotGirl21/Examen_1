remove(list = ls())

library(forecast)
library(ggplot2)
library(fma)
library(expsmooth)
library(fpp2)

DATA = read.csv(file = "Data.csv", header = TRUE, sep = ";")

GDP = ts(DATA[,2], frequency = 4, start = c(1978, 1))

#Training Data: Q1 1978 - Q4 2012
GDP.train = window(GDP, start = 1978, end = c(2012,4))

#Out-off sample data Q1 2013 - Q4 2019 (24)
GDP.out = window(GDP, start = (2013))
#length of forecast
h28 = 28

###Preliminary (exploratory) analysis

#outliers which need to be explained
autoplot(GDP.train/1000)+
  ggtitle("Gross Domestic Product Norway")+
  xlab("Qauter")+
  ylab("NOK in Thousands") 

#controlling Data for outliers
which.min(GDP.train)
#smalles observaton near to the beginning of serie
which.max(GDP.train)
#smalles observaton at the end of serie
boxplot(GDP.train/1000) #no outliers
#!!! boxplot in ggplot

#autoplot(GDP.train)+geom_smooth(method = loess, formula = y ~ x)+
#  ggtitle("Gross Domestic Product Norway")

#Seasonal plots
ggseasonplot(GDP.train/1000, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("NOK in Thousands") +
  ggtitle("Seasonal plot: GDP Norway")
ggseasonplot(GDP.train, polar = TRUE)+
  ylab("NOK in Thousands") +
  ggtitle("Ploar seasonal plot: GDP Norway")

#Seasonal subseries plots to emphasises seasonal patterns
ggsubseriesplot(GDP.train/1000)+
  ylab("NOK in Thousands") +
  ggtitle("Seasonal subseries plot: GDP Norway")

##Scatterplots to explore relationships between timeseries
#autoplot(GDP.train, facet TRUE)+
#  ggtitle("Seasonal subseries plot: GDP Norway")
#muss noch ausgearbeitet werden
##usefull plotting one series against an other
#qplot()

#Lag plots
gglagplot(GDP.train/1000)+
  ylab("NOK in Thousands")+ xlab("NOK in Thousands")+
  ggtitle("Lagged scatterplots for GDP Norway")
  
#Autocorrelation plots
ggAcf(GDP.train, lag.max = 40)+
  ggtitle("Autocorrelation function for GDP Norway")


#Forecasting methods to compare with
#mean
autoplot(GDP.train/1000) +
  autolayer(meanf(GDP.train/1000, h = h28), series = "Mean", PI = FALSE)+
  autolayer(naive(GDP.train/1000, h = h28), series = "Naive", PI = FALSE)+
  autolayer(snaive(GDP.train/1000, h =  h28), series = "Sesonal naive", PI = FALSE)+
  autolayer(rwf(GDP.train/1000, h = h28, drift = TRUE), series = "Random Walk", PI = FALSE)+
  autolayer(GDP.out/1000, series = "DATA")+
  ggtitle("Forcast for quarterly GDP Norway")+
  xlab("Quater")+ylab("NOK in Thousands")+
  guides(colour=guide_legend(title="Forecast"))


#snaive with include BoxCOx
#autoplot(snaive(GDP.train, lambda = BoxCox.lambda(GDP.train), h = h28)) + autolayer(GDP.out)
#autoplot(rwf(GDP.train, drift = TRUE, h = h28)) + autolayer(GDP.out)
