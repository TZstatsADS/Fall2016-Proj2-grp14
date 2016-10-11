require(forecast)
library(tseries)
library(dplyr)
require(stats)
library(plotly)
library(wday)
library("forecast")
library("highcharter")


OurFile = data.frame(read.csv("C://Users//svish//Documents//Sem 2//Time Series//Fall2016-Proj2-grp14-master//data//collision_dateframe.csv", header = TRUE,stringsAsFactors = FALSE))
OurFile$Date2 = as.Date(OurFile$DATE)
NewFile = OurFile
NewFile$Dead = 1
NewFile$Dead = NewFile$NUMBER.OF.PERSONS.INJURED + NewFile$NUMBER.OF.PERSONS.KILLED + NewFile$NUMBER.OF.PEDESTRIANS.INJURED + NewFile$NUMBER.OF.PEDESTRIANS.KILLED + NewFile$NUMBER.OF.CYCLIST.INJURED +
  NewFile$NUMBER.OF.CYCLIST.KILLED + NewFile$NUMBER.OF.MOTORIST.INJURED + NewFile$NUMBER.OF.MOTORIST.KILLED 
FinalFile = data.frame(NewFile$Date2, NewFile$Dead)
p = aggregate(FinalFile$NewFile.Dead, by=list(Category=FinalFile$NewFile.Date2), FUN=sum)
q = as.data.frame(p)
q$dyname = wday(q$Category)

# FORECAST = function(Y,model,h,b=400){
#   prev=forecast(model,h)
#   n=length(Y)
#   T=1:n
#   Tfutur=(n+1):(n+h)
#   plot(T,Y,type="l",xlim=c(1,n+h),ylim=c(300-b,300+b))
#   polygon(c(Tfutur,rev(Tfutur)),c(prev$lower[,2],rev(prev$upper[,2])),col="orange",border=NA)
#   polygon(c(Tfutur,rev(Tfutur)),c(prev$lower[,1],rev(prev$upper[,1])),col="yellow",border=NA)
#   lines(prev$mean,col="blue")
#   lines(Tfutur,prev$lower[,2],col="red")
#   lines(Tfutur,prev$upper[,2],col="red")
# }


CHECK=function(model,lag=30){
  E=residuals(model)
  model
  par(mfrow=c(2,1))
  acf(E,lag.max = lag)
  pacf(E,lag.max = lag)
}

Chart = function(model){
  airforecast <- forecast(model1, level = 95)
  CHECK(model1)
  hchart(airforecast)
}

q$year=substr(q$Category,  1, 4)
q$month=substr(q$Category,  6, 7)
q$day=substr(q$Category,9,10)

TrialForMonth = aggregate(q$x,by = list(q$year, q$month), FUN = "mean")


salesForMonth=as.data.frame(TrialForMonth$x)
ww =TrialForMonth[1:44,]
salesForMonth = ww$x
m=q[1:1339,]
salesForDay = m$x

plot(salesForDay,type="l")
plot(salesForMonth,type="l")

modelMonth1=arima(salesForMonth,order=c(1,1,1),
                  seasonal = list(order = c(0, 1, 0), period=12))
Chart(modelMonth1)
modelMonth2=arima(salesForMonth,order=c(0,1,1),
                  seasonal = list(order = c(0, 1, 0), period=12))
Chart(modelMonth2)
modelMonth3=arima(salesForMonth,order=c(1,1,0),
                  seasonal = list(order = c(0, 1, 0), period=12))
Chart(modelMonth3)
# CHECK(modelMonth1)
# FORECAST(salesForMonth,modelMonth1,h=20)
# CHECK(modelMonth2)
# FORECAST(salesForMonth,modelMonth2,h=20)
# CHECK(modelMonth3)
# FORECAST(salesForMonth,modelMonth3,h=20)

modelDay1=arima(salesForDay,order=c(1,1,1),
                seasonal = list(order = c(0, 1, 0), period=365))
Chart(modelDay1)
modelDay2=arima(salesForDay,order=c(0,1,1),
                seasonal = list(order = c(0, 1, 0), period=365))
Chart(modelDay2)
modelDay3=arima(salesForDay,order=c(1,1,0),
                seasonal = list(order = c(0, 1, 0), period=365))
Chart(modelDay3)

airforecast <- forecast(modelDay1, level = 95)
CHECK(modelDay1)
hchart(airforecast)

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

YearlyPlot(q)

##To plot monthly average
MonthlyPlot = function(Data){
  p = Data
  n = aggregate(p$x, by = list(p$day), FUN = "mean")
  plot_ly(data = n, x = n$Group.1, y = n$x, type = 'scatter', mode = "lines+markers")
}

MonthlyPlot(q)

###
DayOfWeekPlot = function(Data){
  p = Data
  n = aggregate(p$x, by = list(p$dyname), FUN = "mean")
  plot_ly(data = n, x = n$Group.1, y = n$x, type = 'scatter', mode = "lines+markers")
  }
DayOfWeekPlot(q)

















