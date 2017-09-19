print("here We have manage to find data from 'http://robjhyndman.com/tsdldata/hurst/precip1.dat'")
print("Rain Forecasting in Englend Starts from year 1813 to 1912(100 years)")

setwd("C:\\Users\\ramir\\Desktop\\R project")

rain <- scan("rainForecasting.dat",skip=1)

rainseries <- ts(rain,start = c(1813))

png(filename = "rainseries.png")
plot.ts(rainseries)#plots the graph
dev.off()


rainseriesForecast <- HoltWinters(rainseries,beta = FALSE,gamma = FALSE)
print("This is our Forecasting about rain")
print(rainseriesForecast)


plotForecastErrors(rainseriesForecast2$residuals)

print(rainseriesForecast$fitted)

png(filename = "rainseriesForecast.png")
plot(rainseriesForecast)      
dev.off()

print(rainseriesForecast$SSE)#SSE stores sum of squared errors

library(forecast)

HoltWinters(rainseries,beta = FALSE,gamma = FALSE, l.start=23.56)
rainseriesForecast2 <- forecast:::forecast.HoltWinters(rainseriesForecast,h=8)
print(rainseriesForecast2)

png(filename = "rainseriesForecast2.png")
plot(rainseriesForecast2)
dev.off()
#obj = 

print(rainseriesForecast2$residuals)

png(filename = "corellogram.png")
Acf(rainseriesForecast2$residuals,lag.max = 20)
dev.off();

Box.test(rainseriesForecast2$residuals,lag = 20,type = "Ljung-Box")

png(filename = "corellogram2.png")
plot.ts(rainseriesForecast2$residuals)
dev.off()

plotForecastErrors <- function(forecasterrors)
{
  forecasterrors[1] = 0
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  
  mynorm <- rnorm(10000,mean = 0,sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  
  if(mymin2<mymin){
    mymin <- mymin2
  }
  if(mymax2>mymax){
    mymax <- mymax2
  }
  mybins <- seq(mymin,mymax,mybinsize)
  hist(forecasterrors, col="red",freq=FALSE,breaks = mybins)
  myhist <- hist(mynorm,plot = FALSE,breaks = mybins) 
  points(myhist$mids,myhist$density,type = 1,col = "blue",lwd = 2)
}

png(filename = "histogram.png")
plotForecastErrors(rainseriesForecast2$residuals)
dev.off()

