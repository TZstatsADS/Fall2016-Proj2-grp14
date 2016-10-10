require(forecast)
library(tseries)
library(dplyr)
require(stats)
#sales <- ts(c(99, 58, 52, 83, 94, 73, 97, 83, 86, 63, 77, 70, 87, 84, 60, 105, 
            #  87, 93, 110, 71, 158, 52, 33, 68, 82, 88, 84),frequency=2)
#model = auto.arima(sales)
#plot(forecast(model))
require(forecast)
library(tseries)
require(stats)
library(dplyr)


OurFile = data.frame(read.csv("C://Users//svish//Documents//Sem 2//Time Series//Fall2016-Proj2-grp14-master//data//collision_dateframe.csv", header = TRUE,stringsAsFactors = FALSE))
OurFile$Date2 = as.Date(OurFile$DATE)
NewFile = OurFile
NewFile$Dead = 1
#NewFile$Dead = NewFile$NUMBER.OF.PERSONS.INJURED + NewFile$NUMBER.OF.PERSONS.KILLED + NewFile$NUMBER.OF.PEDESTRIANS.INJURED + NewFile$NUMBER.OF.PEDESTRIANS.KILLED + NewFile$NUMBER.OF.CYCLIST.INJURED +
#  NewFile$NUMBER.OF.CYCLIST.KILLED + NewFile$NUMBER.OF.MOTORIST.INJURED + NewFile$NUMBER.OF.MOTORIST.KILLED 
FinalFile = data.frame(NewFile$Date2, NewFile$Dead)
p = aggregate(FinalFile$NewFile.Dead, by=list(Category=FinalFile$NewFile.Date2), FUN=sum)
q = as.data.frame(p)


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


q$year=substr(q$Category,  1, 4)
q$month=substr(q$Category,  6, 7)
q$day=substr(q$Category,9,10)

TrialForMonth = aggregate(q$x,by = list(q$year, q$month), FUN = "mean")
TrialForMonth = TrialForMonth[order(TrialForMonth$Group.1),]



salesForMonth=as.data.frame(TrialForMonth$x)
ww =TrialForMonth[1:44,]
salesForMonth = ww$x
salesForDay=q$x

plot(salesForDay,type="l")
plot(salesForMonth,type="l")

modelMonth1=arima(salesForMonth,order=c(1,1,1),
                  seasonal = list(order = c(0, 1, 0), period=12))
modelMonth2=arima(salesForMonth,order=c(0,1,1),
                  seasonal = list(order = c(0, 1, 0), period=12))
modelMonth3=arima(salesForMonth,order=c(1,1,0),
                  seasonal = list(order = c(0, 1, 0), period=12))

library("forecast")
library("highcharter")

airforecast <- forecast(modelMonth3, level = 95)
CHECK(modelMonth3)
hchart(airforecast)


CHECK(modelMonth1)
FORECAST(salesForMonth,modelMonth1,h=20)
CHECK(modelMonth2)
FORECAST(salesForMonth,modelMonth2,h=20)
CHECK(modelMonth3)
FORECAST(salesForMonth,modelMonth3,h=20)

modelDay1=arima(salesForDay,order=c(1,1,1),
                seasonal = list(order = c(0, 1, 0), period=365))
modelDay2=arima(salesForDay,order=c(0,1,1),
                seasonal = list(order = c(0, 1, 0), period=365))
modelDay3=arima(salesForDay,order=c(1,1,0),
                seasonal = list(order = c(0, 1, 0), period=365))




FORECAST(salesForDay,modelDay1,h=500)
CHECK(modelDay2)
FORECAST(salesForDay,modelDay2,h=500)
CHECK(modelDay3)
FORECAST(salesForDay,modelDay3,h=500)
