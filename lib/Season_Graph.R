library(data.table)
library(chron)
library(timeDate)
collision.bike<-fread("1.csv",select=c("DATE","NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","CONTRIBUTING FACTOR VEHICLE 1"))
collision.auto= fread("2.csv",select=c("DATE","NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","CONTRIBUTING FACTOR VEHICLE 1"))
data<-rbind(collision.bike,collision.auto)
colnames(data)<-c("date","injured","killed","contributing_factor")
trans.date <- function(x){
  a=as.Date(as.character.Date(x),"%m/%d/%Y") 
  return(a)
}
data$date<-trans.date(data$date)
data$season <- quarters(data$date)
data$year<-years(data$date)
library(dplyr)
plotdata<-data %>% 
  group_by(year,season) %>% 
  summarise(
    accident=n(),
    killed = sum(killed)
  )
plotdata<-plotdata[1:(nrow(plotdata)-1),]
plotdata$time<-2011+as.numeric(plotdata$year)+as.numeric(as.factor(plotdata$season))/4
```

```{r}
library(magrittr)
library(highcharter)
highchart() %>% 
  hc_title(text = "Number of Accidents Grouped by Seasons") %>% 
  hc_xAxis(tickInterval=1,showFirstLabel=TRUE,title=list(text="Time"))%>%
  hc_yAxis(title=list(text="Number of Accidents"))%>%
  hc_add_series_scatter(plotdata$time,plotdata$accident,plotdata$killed,color=plotdata$season,lineWidth=2,name=" ",showInLegend=FALSE,label=paste(plotdata$year,plotdata$season)) %>%
  hc_tooltip(pointFormat = "Time: {point.label} <br> Number of Accidents: {point.y} <br> People Killed (Size of The Bubble): {point.z}")