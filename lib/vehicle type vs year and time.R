#ads-proj2
setwd("G:/Columbia/study/3rd semester/5243/project2/data")
library(data.table)
library(dplyr)
library(tidyr)
library(rjson)
library(data.table)
library(rbokeh)
library(plotly)

year_start=2012
year_end=2015

time_start=10
time_end=15

####start plotting
colli=fread("dataframe.txt",drop="V1")
data=colli %>% mutate(YEAR=substr(DATE,start=1,stop=4)) #%>% rename(VEHICLE.TYPE=VEHICLE.TYPE.CODE.2)
data=filter(data,!(VEHICLE.TYPE.CODE.1 %in% c("","UNKNOWN")))
#data=filter(data,!(VEHICLE.TYPE.CODE.1 %in% c("","UNKNOWN")),!(VEHICLE.TYPE.CODE.2 %in% c("","UNKNOWN")))
data=mutate(data,VEHICLE.TYPE=VEHICLE.TYPE.CODE.1) %>% 
     select(-VEHICLE.TYPE.CODE.1,-VEHICLE.TYPE.CODE.2)
# same=filter(data,VEHICLE.TYPE.CODE.1==VEHICLE.TYPE.CODE.2) %>% 
#   mutate(VEHICLE.TYPE=VEHICLE.TYPE.CODE.1)%>%
#   select(-VEHICLE.TYPE.CODE.1,-VEHICLE.TYPE.CODE.2)
# different=filter(data,VEHICLE.TYPE.CODE.1!=VEHICLE.TYPE.CODE.2)
# diff1=mutate(different,VEHICLE.TYPE=VEHICLE.TYPE.CODE.1)%>%
#   select(-VEHICLE.TYPE.CODE.1,-VEHICLE.TYPE.CODE.2)
# diff2=mutate(different,VEHICLE.TYPE=VEHICLE.TYPE.CODE.2)%>%
#   select(-VEHICLE.TYPE.CODE.1,-VEHICLE.TYPE.CODE.2)
# data=rbind(same,diff1,diff2)


###I simply delete the missing values in the vehicle type
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("SCOOTER","PEDICAB","FIRE TRUCK","AMBULANCE"))]="OTHER"
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("PICK-UP TRUCK"))]="TRUCK"
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("SMALL COM VEH(4 TIRES)"))]="4 TIRES"
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("LARGE COM VEH(6 OR MORE TIRES)"))]=">=6 TIRES"
data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("SPORT UTILITY / STATION WAGON"))]="SPORT UTILITY"

graphdata2<-data %>% 
  group_by(YEAR,VEHICLE.TYPE) %>% 
  summarise(
    count=log(n()),
    INTENSITY=mean(NUMBER.OF.PERSONS.KILLED)+10)%>%filter(YEAR<2016)

plot_ly(x=graphdata2$YEAR,y=graphdata2$count,type ='scatter',mode ='lines+markers',group = graphdata2$VEHICLE.TYPE,color = graphdata2$VEHICLE.TYPE)%>%layout(xaxis = list(title = "Year"), yaxis =list(title = "log number of average collisions"))


graphdata3<-data %>% 
  group_by(TIME,VEHICLE.TYPE) %>% 
  summarise(
    count=log(n()),
    INTENSITY=mean(NUMBER.OF.PERSONS.KILLED)+10)

plot_ly(x=graphdata3$YEAR,y=graphdata3$count,type ='scatter',mode ='lines+markers',group = graphdata3$VEHICLE.TYPE,color = graphdata3$VEHICLE.TYPE)%>%layout(xaxis = list(title = "Year"), yaxis =list(title = "log number of average collisions"))


# byYEAR=group_by(data,YEAR,VEHICLE.TYPE)
# risk_by_type=summarise(byYEAR, count=log(n()/5),INTENSITY=mean(NUMBER.OF.PERSONS.KILLED))
# #p1=figure(width=1000,legend_location = "top_left")%>% 
# # ly_lines(data=risk_by_type,x=YEAR,y=count,col=VEHICLE.TYPE) %>% ly_points(data=risk_by_type,x=YEAR,y=count,col=VEHICLE.TYPE)
# #p1
# 
# #f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
# plot_ly(data.frame(risk_by_type),x=~YEAR,y=~count,type ='scatter',mode ='lines+markers',group=~VEHICLE.TYPE,size=~INTENSITY) %>%
#   layout(xaxis = list(title = "Year"), yaxis =list(title = "log number of average collisions"))%>%
#   # add_trace(x = c(year_start,year_end), y =c(max(count)+1,max(count)+1), fill = "tozeroy")
# 
# byTIME=group_by(data,TIME,VEHICLE.TYPE)
# risk_by_time=byTIME %>% summarise(count=log(n()/5),INTENSITY=mean(NUMBER.OF.PERSONS.KILLED))
# risk_by_time$TIME=as.numeric(risk_by_time$TIME)
# # p2=figure(width=1000,legend_location = "top_left")%>% 
# #   #ly_lines(data=risk_by_time,x=as.numeric(TIME),y=count,col=VEHICLE.TYPE) %>% 
# #   ly_points(data=risk_by_time,x=TIME,y=count,col=VEHICLE.TYPE) %>% 
# #   x_axis(label="TIME") %>% y_axis("Number of Collisions")%>%
# #   theme_legend(label_text_font_size=0.01)
# # p2
# plot_ly(data.frame(risk_by_time),x=TIME,y=count,type ='scatter',mode ='markers',group=VEHICLE.TYPE,size=INTENSITY)%>%
#   layout(xaxis = list(title = "Time"), yaxis =list(title = "log number of average collisions"))%>%
#   add_trace(x = c(time_start,time_end), y =c(max(count)+1,max(count)+1), fill = "tozeroy")


