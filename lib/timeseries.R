require(forecast)
library(tseries)
library(dplyr)
require(stats)
library(plotly)
library(wday)
library("forecast")
library("highcharter")
library(data.table)
library(dplyr)
library(lubridate)


collisionData = data.frame(read.csv("C://Users//svish//Documents//Sem 2//Time Series//Fall2016-Proj2-grp14-master//data//collision_dataframe.csv", header = TRUE,stringsAsFactors = FALSE))
#setwd("G:/Columbia/study/3rd semester/5243/project2/data")
#collisionData=read.csv("collision_dataframe.csv")
collisionData$Date2 = as.Date(collisionData$DATE)
NewFile = collisionData
##//why set this to 1?
#NewFile$Dead=1
NewFile=mutate(NewFile,Dead=NUMBER.OF.PERSONS.INJURED +NUMBER.OF.PERSONS.KILLED +
                 NUMBER.OF.PEDESTRIANS.INJURED +
                 NUMBER.OF.PEDESTRIANS.KILLED +NUMBER.OF.CYCLIST.INJURED +
                 NUMBER.OF.CYCLIST.KILLED +
                 NUMBER.OF.MOTORIST.INJURED +NUMBER.OF.MOTORIST.KILLED)
FinalFile = data.frame(NewFile$Date2, NewFile$Dead)
p = aggregate(FinalFile$NewFile.Dead, by=list(Date=FinalFile$NewFile.Date2), FUN=sum)
Date_killed = as.data.frame(p)
Date_killed$dyname = wday(Date_killed$Date)

CHECK=function(model,lag=30){
  E=residuals(model)
  model
  par(mfrow=c(2,1))
  acf(E,lag.max = lag)
  pacf(E,lag.max = lag)
}

Chart = function(model1){
  airforecast <- forecast(model1, level = 95)
  #CHECK(model1)
  hchart(airforecast)
}

Date_killed$year=substr(Date_killed$Date,  1, 4)
Date_killed$month=substr(Date_killed$Date,  6, 7)
Date_killed$day=substr(Date_killed$Date,9,10)

TrialForMonth = aggregate(Date_killed$x,by = list(Date_killed$year, Date_killed$month), FUN = "mean")


Death_Month=as.data.frame(TrialForMonth$x)
ww =TrialForMonth[1:44,]
Death_Month = ww$x
m=Date_killed[1:1339,]
salesForDay = m$x

# #death toll for the first 1339 days
# plot(salesForDay,type="l")
# plot(Death_Month,type="l")
# 
modelMonth1=arima(Death_Month,order=c(1,1,1),
                  seasonal = list(order = c(0, 1, 0), period=12))
Chart(modelMonth1)
# modelMonth2=arima(Death_Month,order=c(0,1,1),
#                   seasonal = list(order = c(0, 1, 0), period=12))
# Chart(modelMonth2)
# modelMonth3=arima(Death_Month,order=c(1,1,0),
#                   seasonal = list(order = c(0, 1, 0), period=12))
# Chart(modelMonth3)

# modelDay1=arima(salesForDay,order=c(1,1,1),
#                 seasonal = list(order = c(0, 1, 0), period=365))
# Chart(modelDay1)
# modelDay2=arima(salesForDay,order=c(0,1,1),
#                 seasonal = list(order = c(0, 1, 0), period=365))
# Chart(modelDay2)
# modelDay3=arima(salesForDay,order=c(1,1,0),
#                 seasonal = list(order = c(0, 1, 0), period=365))
# Chart(modelDay3)


# FORECAST(salesForDay,modelDay1,h=500)
# CHECK(modelDay2)
# FORECAST(salesForDay,modelDay2,h=500)
# CHECK(modelDay3)
# FORECAST(salesForDay,modelDay3,h=500)

##Year #month #Week

##To plot yearly average
YearlyPlot = function(Data){
  p = Data
  p$newdaymonth = paste(p$month, p$day)
  n = aggregate(p$x, by = list(p$newdaymonth), FUN = "mean")
  plot_ly(data = n, x = n$Group.1, y = n$x, type = 'scatter', mode = "lines")
}

YearlyPlot(Date_killed)

##To plot monthly average
MonthlyPlot = function(Data){
  p = Data
  n = aggregate(p$x, by = list(p$day), FUN = "mean")
  plot_ly(data = n, x = n$Group.1, y = n$x, type = 'scatter', mode = "lines+markers")
}

MonthlyPlot(Date_killed)

###
DayOfWeekPlot = function(Data){
  p = Data
  n = aggregate(p$x, by = list(p$dyname), FUN = "mean")
  plot_ly(data = n, x = n$Group.1, y = n$x, type = 'scatter', mode = "lines+markers")
}
DayOfWeekPlot(Date_killed)
