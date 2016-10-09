library(data.table)
collision.bike<-fread("1.csv",select=c("DATE","NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","CONTRIBUTING FACTOR VEHICLE 1"))
collision.auto= fread("2.csv",select=c("DATE","NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","CONTRIBUTING FACTOR VEHICLE 1"))
data<-rbind(collision.bike,collision.auto)
colnames(data)<-c("date","injured","killed","contributing_factor")
##table data
library(dplyr)
graphdata<-data %>% 
  group_by(contributing_factor) %>% 
  summarise(
    Accident=n(),
    Injured=sum(injured),
    Killed = sum(killed)
  )%>%
  filter(contributing_factor!="Unspecified")%>%
  filter(contributing_factor!="")%>%
  arrange(desc(Accident))
graphdata$cf<-paste(order(graphdata$Accident,decreasing=TRUE),graphdata$contributing_factor)
graphdata<-graphdata[1:9,]
library(plotly)
xaxis<-list(title="Contributing Factor",showticklabels=FALSE)
yaxis<-list(title="Number of Accident")
p <- plot_ly(
  x = 1:9,
  y = graphdata$Accident,
  label=graphdata$contributing_factor,
  name = " ",
  type = "bar",
  color=graphdata$Killed,
  text = paste("Contributing Factor: ", graphdata$contributing_factor,"<br>","People Killed (Color of The Bar):",graphdata$Killed),
  hoverinfo="text",
  colors=c("#fcbba1","#fc9272","#fb6a4a", "#ef3b2c","#cb181d","#99000d"),
  showlegend=FALSE
)%>%
  layout(xaxis=xaxis,yaxis=yaxis)
add_lines(p,y=graphdata$Injured, name=paste("Number of","<br>","People Injured"),line=list(shape="linear"),color=I("black"),showlegend=TRUE)