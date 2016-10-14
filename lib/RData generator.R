
library(RCurl)
library(RJSONIO)
library(dplyr)
library(shiny)
library(bitops)
library(leaflet)
library(plotly) ##//Yunyi
library(shinydashboard) ##//Yunyi
library(magrittr) ##//Yunyi
library(highcharter) ##//Yunyi
library(chron)##//Yunyi
library(timeDate)##//Yunyi
require(forecast)
library(tseries)
require(stats)
library(data.table)
library(lubridate)
library(RColorBrewer)



collisionData=read.csv("collision_dataframe.csv")
cluster_full=read.csv("cluster_full.csv")[,-1]  ##// Nicole
collisionData$Weather=as.character(collisionData$Weather)
collisionData$Weather[is.na(collisionData$Weather)]="Others(Sunny,Cloudy,etc.)"
collisionData$Weather=as.factor(collisionData$Weather)
collisionData$DATE= as.Date.factor(collisionData$DATE)
collisionData$YEAR=substr(collisionData$DATE,start=1,stop=4)
collisionData$INDEX=c(1:dim(collisionData)[1])
collisionData=subset(collisionData,DATE<as.Date("2016-06-30"))
collisionData$Date2 = as.Date(collisionData$DATE)
year_start=2012
year_end=2015
time_start=10
time_end=15

data=collisionData%>% 
  filter(VEHICLE.TYPE.CODE.1 !=""|VEHICLE.TYPE.CODE.1 !="UNKNOWN")%>%
  mutate(VEHICLE.TYPE=VEHICLE.TYPE.CODE.1) %>% 
  select(-VEHICLE.TYPE.CODE.1)
data$VEHICLE.TYPE=as.character(data$VEHICLE.TYPE)
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("SCOOTER","PEDICAB","FIRE TRUCK","AMBULANCE"))]="OTHER"
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("PICK-UP TRUCK"))]="TRUCK"
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("SMALL COM VEH(4 TIRES)"))]="4 TIRES"
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("LARGE COM VEH(6 OR MORE TIRES)"))]=">=6 TIRES"
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("SPORT UTILITY / STATION WAGON"))]="SPORT UTILITY"
data$VEHICLE.TYPE=as.factor(data$VEHICLE.TYPE)

collisionData=data

rm(data);gc()

YearlyPlot = function(Data){
  p = Data
  p$newdaymonth = paste(p$month)
  n = aggregate(p$x, by = list(p$newdaymonth), FUN = "mean")
  plot_ly(data = n, x = n$Group.1, y = n$x, type = 'scatter', mode = "lines")
}


MonthlyPlot = function(Data){
  p = Data
  n = aggregate(p$x, by = list(p$day), FUN = "mean")
  plot_ly(data = n, x = n$Group.1, y = n$x, type = 'scatter', mode = "lines+markers")
}

WeeklyPlot = function(Data){
  p = Data
  n = aggregate(p$x, by = list(p$dyname), FUN = "mean")
  plot_ly(data = n, x = n$Group.1, y = n$x, type = 'scatter', mode = "lines+markers")
}


cctvLoc=data.frame(CID=c(798,688,468,5,407,305),
                   LNG=c(-73.954841,-73.9547816,-73.9838152,-73.983551,-73.9601119,-73.986752),
                   LAT=c(40.7865992,40.8107875,40.7786139,40.7680809,40.7585756,40.7484496),
                   NAME=c("Park Ave @ 96 St","St Nicholas Ave @ 125 St","Amsterdam @ 72 St",
                          "Central Park S @ Columbus Cr","QBB NOR @ York Ave","5 Ave @ 34 St"))
cid=5

url0="http://207.251.86.238/cctv5.jpg?math=0.21909423107"
url <- function(address, return.call = "json", sensor = "false") {
  root <- "https://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}
geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  # print(u)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat=lat, lng=lng,type= location_type, addr=formatted_address))
  } else {
    return(c(NA,NA,NA,NA))
  }
}

monthSeq=c("January" = 1,  "Febrary" = 2,  "Match" = 3,
           "April"=4, "May"=5,"June"=6,"July"=7,
           "August"=8,"September"=9,"October"=10,"November"=11,"December"=12)

daySeq=c("Weekdays"=1,"Weekends and Holidays"=2)

weatherSeq=c("Fog"=1,"Fog-Rain"=2,"Fog-Rain-Snow"=3,"Fog-Snow"=4,"Rain"=5,"Rain-Snow"=6,"Snow"=7,"Others(Sunny,Cloudy,etc.)"=8)


spr=sample(c(1:700000),size=300,replace=F)


save.image(file="appdata.Rdata")
