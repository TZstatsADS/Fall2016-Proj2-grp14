
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
collisionData$INDEX=c(1:dim(collisionData)[1])
collisionData=subset(collisionData,DATE<as.Date("2016-06-30"))
collisionData$Date2 = as.Date(collisionData$DATE)

YearlyPlot = function(Data){
  p = Data
  p$newdaymonth = paste(p$month, p$day)
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
#pal <- colorNumeric("Reds", domain = range(collisionData$People.Killed))




sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Summary by Area", tabName = "SummarybyArea", icon = icon("file-text-o")),
              menuItem("Area Cluster", tabName = "AreaCluster", icon=icon("table")),
              menuItem("About",  icon = icon("file-text-o"),
                       menuSubItem("Read Me", tabName = "ReadMe", icon = icon("angle-right")),
                       menuSubItem("About Team", tabName = "AboutTeam", icon = icon("angle-right")),
                       menuSubItem("To Be Developed", tabName = "TBD", icon = icon("angle-right"))
                      )
  ),
  hr()
)


body <- dashboardBody(
  tabItems(
           tabItem(tabName = "SummarybyArea",
                   fluidRow(column(8,leafletOutput("Map2", width="100%",height=300)),
                            column(4,h4(strong("Selected Area Summary")),##//Yunyi
                                   strong("Total Number of Accidents: "), textOutput("AccidentNum"), 
                                   strong("Number of People Injured:"), textOutput("Injured"), 
                                   strong("Number of People Killed:"), textOutput("Killed"),
                                   strong("Vehicle Type with The Most Accidents:"),textOutput("Vehicle")
                                  )),
                   hr(),
                   tabsetPanel(id="SummaryTabSet",
                              tabPanel("Map Setting",value="MapSetting",
                                       hr(),
                                       fluidRow(column(4,textInput("AddressInput","Find a Place:","Columbia University, NY"),
                                                       actionButton("Search",label = "Search"),
                                                       textOutput("AddressError"),
                                                       hr(),
                                                       sliderInput("TimeSlot",label="Hours included in Summary",min=0,max=24,value=c(0,24))),
                                                column(4,checkboxGroupInput("Month", "Months included in Risk by Hour:",monthSeq,selected = c(1:12))),
                                                column(4,checkboxGroupInput("Week", "Weekdays or else:",daySeq,selected = c(1:2)),
                                                       hr(),
                                                       checkboxGroupInput("Weather", "Weather included:",weatherSeq,selected = c(1:8)),
                                                       actionButton("ApplyChanges",label = "Apply Changes"),
                                                       hr(),
                                                       downloadButton("report", "Generate report"))  ##// YOU download button
  
                                               )
                                       ),
                              tabPanel("Accident Seasonality",value="Summary2",highchartOutput("Seasonality",width = "100%")), ##//Yunyi
                              tabPanel("Contributing Factor",value="Summary3",plotlyOutput("ContributingFactor",width = "100%")),##//Yunyi
                              tabPanel("Time Series Forecast",value="TimeSeries",highchartOutput("TSPlot",width = "100%")),
                              tabPanel("Time Analysis",value="TimeAnalysis",fluidRow(column(4,plotlyOutput("TimebyYear")),
                                                                                     column(4,plotlyOutput("TimebyMonth")),
                                                                                     column(4,plotlyOutput("TimebyWeek")))),
                              tabPanel("Vehicle Type by Year",value="ByCar",plotlyOutput("ByCar",width = "100%")),
                              tabPanel("Vehicle Type by Hour",value="ByCar2",plotlyOutput("ByCar2",width = "100%"))
                              
                              )
                  ),
           tabItem(tabName = "AreaCluster",
                   leafletOutput("ClusterMap",height=400,width = "100%"),
                   hr(),
                   ##//
                   fluidRow(column(2),
                            column(8,plotlyOutput("ClusterLines",height = 250))
                           )
                  ),
           tabItem(tabName = "ReadMe",
                   textOutput("ppplot3")
                  ),
           tabItem(tabName = "AboutTeam",
                   textOutput("ppplot4")
                  ),
           tabItem(tabName = "TBD",
                   textOutput("ppplot5")
                  )
          )
)

ui=dashboardPage(
  dashboardHeader(title = "Traffic Supervisor"),
  sidebar,
  body
)

#####--------server-------#####

server <- function(input, output,session) {
  

###Page2  Summary By Area
  #####---Map---#####
  spr=sample(c(1:700000),size=3000,replace=F)
  summary_map=leaflet(collisionData[spr,])%>%
    addProviderTiles("OpenStreetMap.BlackAndWhite")%>%
    addCircles(lat=~LATITUDE,lng = ~LONGITUDE,
               radius=~NUMBER.OF.PERSONS.INJURED+1,color="Red",
               layerId=~INDEX)%>%
    addMarkers(lat=cctvLoc$LAT,lng = cctvLoc$LNG,
               layerId=cctvLoc$CID,icon=icons(iconUrl=
                                 "http://icons.iconarchive.com/icons/martz90/circle/256/video-camera-icon.png",
               iconWidth = 20,iconHeight = 20))%>%
    setView(lng =-73.95,lat =  40.8, zoom = 12)
  output$Map2=renderLeaflet(summary_map)
  
  
  
  #---Search---
  observeEvent(input$Search,{
    Inputaddress=input$AddressInput
    response=geoCode(Inputaddress)
    if (!is.na(response[1])){
      NewLng=response[2]
      NewLat=response[1]
      output$AddressError=renderText(" ")
      leafletProxy("Map2") %>% 
        clearMarkers()%>%
        addMarkers(lng=NewLng,lat=NewLat)%>%
        setView(lng =NewLng,lat =NewLat, zoom = 12)
    } else{
      output$AddressError=renderText("Place Not Found")
    }})
  
  #---Apply Changes---
  New.Sum.Para=reactiveValues()
  New.Sum.Data1=reactiveValues()
  New.Sum.Data2=reactiveValues()
  New.Sum.Data1$ForSeasonality=collisionData
  New.Sum.Data1$ForContributingFactor=collisionData
  New.Sum.Data1$ForSummary=collisionData
  New.Sum.Data1$ForTS=collisionData
  New.Sum.Data2$ForSeasonality=collisionData
  New.Sum.Data2$ForContributingFactor=collisionData
  New.Sum.Data2$ForSummary=collisionData
  New.Sum.Data2$ForTS=collisionData
  
  
  
  observeEvent(input$ApplyChanges,{  ##// YOU
    output$Month=renderText(paste(levels(collisionData$Month), as.character(New.Sum.Para$New.Month)))
    New.Sum.Para$New.TimeSlot=input$TimeSlot
    New.Sum.Para$New.Month=input$Month
    New.Sum.Para$New.Weather=as.integer(input$Weather)
    New.Sum.Para$New.Week=c(TRUE,FALSE)[as.integer(input$Week)]
    New.Sum.Data1$ForSeasonality=subset(collisionData, 
                                        as.logical(Weekend)%in%New.Sum.Para$New.Week & 
                                          as.integer(Month) %in% as.integer(New.Sum.Para$New.Month) &
                                          Weather %in% names(weatherSeq)[New.Sum.Para$New.Weather])
    New.Sum.Data1$ForContributingFactor=subset(collisionData, 
                                               as.logical(Weekend)%in%New.Sum.Para$New.Week & 
                                                 as.integer(Month) %in% as.integer(New.Sum.Para$New.Month) &
                                                 Weather %in% names(weatherSeq)[New.Sum.Para$New.Weather])
    
    New.Sum.Data1$ForSummary=subset(collisionData, 
                                    as.logical(Weekend)%in%New.Sum.Para$New.Week & 
                                      as.integer(Month) %in% as.integer(New.Sum.Para$New.Month) &
                                      Weather %in% names(weatherSeq)[New.Sum.Para$New.Weather])
    
    New.Sum.Data1$ForTS=subset(collisionData, 
                                          Weather %in% names(weatherSeq)[New.Sum.Para$New.Weather])
    
    
    New.Sum.Para$New.Map2_Bounds=input$Map2_bounds
    latRng <- range(New.Sum.Para$New.Map2_Bounds$north, New.Sum.Para$New.Map2_Bounds$south)
    lngRng <- range(New.Sum.Para$New.Map2_Bounds$east, New.Sum.Para$New.Map2_Bounds$west)
    New.Sum.Data2$ForSeasonality=subset(New.Sum.Data1$ForSeasonality, 
                                        LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                          LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
    New.Sum.Data2$ForContributingFactor=subset(New.Sum.Data1$ForSeasonality, 
                                               LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                                 LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
    New.Sum.Data2$ForSummary=subset(New.Sum.Data1$ForSummary, 
                                    LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                      LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
    New.Sum.Data2$ForTS=subset(New.Sum.Data2$ForTS, 
                               LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                 LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
    
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Accident Report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = TRUE)
      bounds=input$Map2_bounds
      latRng <- mean(bounds$north, bounds$south)
      lngRng <- mean(bounds$east, bounds$west)
      DownloadMap=summary_map%>%setView(lng=lngRng,lat=latRng,zoom=input$Map2_zoom-2)
      # Set up parameters to pass to Rmd document
      params <- list(Map = DownloadMap, CtFactor=ContributingFactorDownload$p,
                     SeasonPlot=SeasonalityPlot$p)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #---View Changes---
  
  observeEvent(input$Map2_bounds,{  ##//  YOU
    #if (is.null(input$Map2_bounds))
    #  return(NULL)
    New.Sum.Para$New.Map2_Bounds=input$Map2_bounds
    latRng <- range(New.Sum.Para$New.Map2_Bounds$north, New.Sum.Para$New.Map2_Bounds$south)
    lngRng <- range(New.Sum.Para$New.Map2_Bounds$east, New.Sum.Para$New.Map2_Bounds$west)
    New.Sum.Data2$ForSeasonality=subset(New.Sum.Data1$ForSeasonality, 
                                        LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                        LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
    New.Sum.Data2$ForContributingFactor=subset(New.Sum.Data1$ForSeasonality, 
                                               LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                               LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
    New.Sum.Data2$ForTS=subset(New.Sum.Data2$ForTS, 
                               LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                 LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
    New.Sum.Data2$ForSummary=subset(New.Sum.Data1$ForSummary, 
                                    LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                      LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
    
  })
  
  
  
  #---Popup---
  
  
  showInfoPopup <- function(ind, lat, lng) {
    selectedPoint <- collisionData[ind,]
    content <- as.character(tagList(
      tags$h4("Date:", selectedPoint$DATE),
      tags$strong(HTML(sprintf("Injured: "))),sprintf("%s", selectedPoint$NUMBER.OF.PERSONS.INJURED), tags$br(),
      tags$strong(HTML(sprintf("Killed: "))),sprintf("%s", selectedPoint$NUMBER.OF.PERSONS.KILLED), tags$br(),
      tags$strong(HTML(sprintf("Weekend: "))),sprintf("%s", selectedPoint$Weekend), tags$br(),
      tags$strong(HTML(sprintf("Holiday: "))),sprintf("%s", selectedPoint$Holiday), tags$br(),
      tags$strong(HTML(sprintf("Contributing Factor: "))),sprintf("%s", selectedPoint$CONTRIBUTING.FACTOR.VEHICLE.1), tags$br(),
      tags$strong(HTML(sprintf("Vehicle Type: "))),sprintf("%s", selectedPoint$VEHICLE.TYPE.CODE.1), tags$br()
    ))
    leafletProxy("Map2") %>% addPopups(lng, lat, content, layerId = ind)
  }
  
  observe({
    leafletProxy("Map2") %>% clearPopups()
    event <- input$Map2_shape_click
    if (is.null(event))   
      return() else{
        isolate({
          showInfoPopup(event$id, event$lat, event$lng)
        })
      }    
  })  
  
  reactTime=reactiveTimer(1000)


  #---Summary---

  output$TimeSlot=renderText(New.Sum.Para$New.TimeSlot) ##//Yunyi
  output$Weather=renderText(New.Sum.Para$New.Weather)##//Yunyi
  output$AreaSummary=renderText("Setting Summary Here")
  output$SummaryText=renderText("Area Summary Here")
  
  output$AccidentNum=renderText(as.character(dim(New.Sum.Data2$ForSummary)[1]))
  output$Killed=renderText(as.character(sum(New.Sum.Data2$ForSummary[7])))
  output$Injured=renderText(as.character(sum(New.Sum.Data2$ForSummary[6])))
  output$Vehicle=renderText(as.character(as.data.frame(table(New.Sum.Data2$ForSummary[15]))[which(as.data.frame(table(New.Sum.Data2$ForSummary[15]))[,2]==max(as.data.frame(table(New.Sum.Data2$ForSummary[15]))[,2])),1]))
  
  #---CCTV---
  reactTime=reactiveTimer(1000)
  output$cctv=renderUI({
    event <- input$Map2_marker_click
    if (!is.null(event)) {
      cid=event$id
      url0=paste("http://207.251.86.238/cctv",cid,".jpg?math=0.21909423107",sep="")
    }
    reactTime()
    rdn=sample(1:9, 1)
    url=paste(url0,rdn,sep="")
    tags$body(h3(cctvLoc$NAME[cctvLoc$CID==cid], align="center"),
              div(img(src = url, height = 300, width = 500),align="middle"))
  })
  
  ContributingFactorDownload=reactiveValues()
  
  ##//Yunyi
  #---ContributingFactor---
  output$ContributingFactor=renderPlotly({
    graphdata<-New.Sum.Data2$ForContributingFactor %>% 
      group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>% 
      summarise(
        Accident=n(),
        Injured=sum(NUMBER.OF.PERSONS.INJURED),
        Killed = sum(NUMBER.OF.PERSONS.KILLED)
      )%>%
      filter(CONTRIBUTING.FACTOR.VEHICLE.1!="Unspecified")%>%
      filter(CONTRIBUTING.FACTOR.VEHICLE.1!="")%>%
      arrange(desc(Accident))
    graphdata$cf<-paste(order(graphdata$Accident,decreasing=TRUE),graphdata$CONTRIBUTING.FACTOR.VEHICLE.1)
    graphdata<-graphdata[1:9,]
    xaxis<-list(title="Contributing Factor",showticklabels=FALSE)
    yaxis<-list(title="Number of Accident")
    p <- plot_ly(
      x = 1:9,
      y = graphdata$Accident,
      label=graphdata$CONTRIBUTING.FACTOR.VEHICLE.1,
      name = " ",
      type = "bar",
      color=graphdata$Killed,
      text = paste("Contributing Factor: ", graphdata$CONTRIBUTING.FACTOR.VEHICLE.1,"<br>","People Killed (Color of The Bar):",graphdata$Killed),
      hoverinfo="text",
      colors=c("#fcbba1","#fc9272","#fb6a4a", "#ef3b2c","#cb181d","#99000d"),
      showlegend=FALSE
    )%>%
      layout(xaxis=xaxis,yaxis=yaxis)
    ContributingFactorDownload$p=add_lines(p,y=graphdata$Injured, name=paste("Number of","<br>","People Injured"),line=list(shape="linear"),color=I("black"),showlegend=TRUE) 
    ContributingFactorDownload$p
  })
  
  
  
  
  SeasonalityPlot=reactiveValues()
  #---Seasonality---
  output$Seasonality=renderHighchart({
    ##//Yunyi
    SeasonalData=New.Sum.Data2$ForSeasonality
    SeasonalData$season <- quarters(New.Sum.Data2$ForSeasonality$DATE)
    SeasonalData$year<-as.factor(substr(New.Sum.Data2$ForSeasonality$DATE,1,4))
    plotdata<-SeasonalData %>% 
      group_by(year,season) %>% 
      summarise(
        accident=n(),
        killed = sum(NUMBER.OF.PERSONS.KILLED+NUMBER.OF.PERSONS.INJURED)
      )

    #plotdata<-plotdata[1:(nrow(plotdata)-2),] ##//????????????????
    plotdata$time<-2011+as.numeric(plotdata$year)+as.numeric(as.factor(plotdata$season))/4
    SeasonalityPlot$p=highchart() %>% 
      hc_title(text = "Number of Accidents Grouped by Seasons") %>% 
      hc_xAxis(tickInterval=1,showFirstLabel=TRUE,title=list(text="Time"))%>%
      hc_yAxis(title=list(text="Number of Accidents"))%>%
      hc_add_series_scatter(plotdata$time,plotdata$accident,plotdata$killed,color=plotdata$season,lineWidth=2,name=" ",showInLegend=FALSE,label=paste(plotdata$year,plotdata$season)) %>%
      hc_tooltip(pointFormat = "Time: {point.label} <br> Number of Accidents: {point.y} <br> People Injured & Killed (Size of The Bubble): {point.z}")
    SeasonalityPlot$p
  })
  
  

  #---Time Series---#
  

  observe({ 
    NewFile = New.Sum.Data2$ForTS
    
    NewFile=mutate(NewFile,Dead=NUMBER.OF.PERSONS.INJURED +NUMBER.OF.PERSONS.KILLED +
                     NUMBER.OF.PEDESTRIANS.INJURED +
                     NUMBER.OF.PEDESTRIANS.KILLED +NUMBER.OF.CYCLIST.INJURED +
                     NUMBER.OF.CYCLIST.KILLED +
                     NUMBER.OF.MOTORIST.INJURED +NUMBER.OF.MOTORIST.KILLED)
    FinalFile = data.frame(NewFile$Date2, NewFile$Dead)
    if (length(FinalFile$NewFile.Dead)!=0){
    p = aggregate(FinalFile$NewFile.Dead, by=list(Date=FinalFile$NewFile.Date2), FUN=sum)
    Date_killed = as.data.frame(p)
    Date_killed$dyname = wday(Date_killed$Date)
    Date_killed$year=substr(Date_killed$Date,  1, 4)
    Date_killed$month=substr(Date_killed$Date,  6, 7)
    Date_killed$day=substr(Date_killed$Date,9,10)
    if (length(Date_killed$x)!=0) {
    TrialForMonth = aggregate(Date_killed$x,by = list(Date_killed$year, Date_killed$month), FUN = "sum")
    
    Death_Month=as.data.frame(TrialForMonth$x)
    ww =TrialForMonth[1:44,]
    Death_Month = ww$x
    m=Date_killed[1:1339,]
    salesForDay = m$x
    
    modelMonth1=arima(Death_Month,order=c(1,1,1),
                      seasonal = list(order = c(0, 1, 0), period=12))
    
    airforecast <- forecast(modelMonth1, level = 95)
    
    output$TSPlot=renderHighchart({ hchart(airforecast)})
    output$TimebyYear=renderPlotly(YearlyPlot(Date_killed))
    output$TimebyMonth=renderPlotly(MonthlyPlot(Date_killed))
    output$TimebyWeek=renderPlotly(WeeklyPlot(Date_killed))
    }}
    })

  #---By car---
 observe({try({
  year_start=2012
  year_end=2015
  time_start=10
  time_end=15
  data=New.Sum.Data2$ForSummary%>% mutate(YEAR=substr(DATE,start=1,stop=4)) #%>% rename(VEHICLE.TYPE=VEHICLE.TYPE.CODE.2)
  data=filter(data,!(VEHICLE.TYPE.CODE.1 %in% c("","UNKNOWN")))
  data=mutate(data,VEHICLE.TYPE=VEHICLE.TYPE.CODE.1) %>% 
    select(-VEHICLE.TYPE.CODE.1)
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
  graphdata3<-data %>% 
    group_by(TIME,VEHICLE.TYPE) %>% 
    summarise(
      count=log(n()),
      INTENSITY=mean(NUMBER.OF.PERSONS.KILLED)+10)
  
  output$ByCar=renderPlotly(
    plot_ly(x=graphdata2$YEAR,y=graphdata2$count,type ='scatter',mode ='lines+markers',group = graphdata2$VEHICLE.TYPE,color = graphdata2$VEHICLE.TYPE)%>%
      layout(xaxis = list(title = "Year"), yaxis =list(title = "log number of average collisions"))
  )
  
  output$ByCar2=renderPlotly(
    plot_ly(x=graphdata3$YEAR,y=graphdata3$count,type ='scatter',mode ='lines+markers',group = graphdata3$VEHICLE.TYPE,color = graphdata3$VEHICLE.TYPE)%>%
      layout(xaxis = list(title = "Hour"), yaxis =list(title = "log number of average collisions"))
    
  )
  })})
###Page3  CCTV


###Page 4 Cluster
  cols1=brewer.pal(4,"RdBu")
  cols2=c(cols1[4],cols1[2],cols1[3],cols1[1])
  
  factpal <- colorFactor(cols2, cluster_full$type)
  cluster_map <- leaflet(cluster_full) %>% 
    addProviderTiles("OpenStreetMap.BlackAndWhite") %>%  # Add default OpenStreetMap map tiles
    addRectangles(lng1=~lo.min, lat1=~la.min, lng2=~lo.max, lat2=~la.max, stroke=F, 
                  weight = 1, fillColor = ~factpal(type), fillOpacity = 0.4, popup = NULL, options = pathOptions())# %>%
  output$ClusterMap=renderLeaflet(cluster_map)
  c1= apply(cluster_full[cluster_full[,29]==1,],2,mean)
  c2= apply(cluster_full[cluster_full[,29]==2,],2,mean)
  c3= apply(cluster_full[cluster_full[,29]==3,],2,mean)
  c4= apply(cluster_full[cluster_full[,29]==4,],2,mean)
  
  plot_cluster= as.data.frame(cbind(c1,c2,c3,c4)[c(-1:-4,-29),])
  plot_cluster=round(plot_cluster, digits= 2)
  xaxis<-list(title="Time")
  yaxis<-list(title="Number of Collision")
  
  clusterlines <- plot_ly(data= plot_cluster, x=1:24, y=~c4,type="scatter", mode= "lines", line=list(color=cols1[1]), name="Type 1") %>%
    layout(xaxis=xaxis,yaxis=yaxis,title="Averafe ollision numbers for different pattens") %>%
    add_trace(p,y=~c2,mode= "lines",line=list(color=cols1[2]),name="Type 2") %>%
    add_trace(p,y=~c3,mode= "lines",line=list(color=cols1[3]),name="Type 3") %>%
    add_trace(p,y=~c1,mode= "lines",line=list(color=cols1[4]),name="Type 4")
  output$ClusterLines=renderPlotly(clusterlines)
  
  
  
  
###Page 5 Read Me
  output$ppplot3=renderText("This is an unstable version, if it goes wrong, just don't click that tab next tiem run it.
                            In the map, there are only 5000 points drawn, but in fact there are 740000 data entries in our analysis plots")
  
###Page 6 About Team
  output$ppplot4=renderText("To be written")

###Page 7 To Be Developed
  output$ppplot5=renderText("To be written")

}

shinyApp(ui = ui, server = server)
