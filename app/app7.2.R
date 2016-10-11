
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

collisionData=read.csv("collision_dataframe.csv")
cluster_full=read.csv("cluster_full.csv")[,-1]  ##// Nicole
collisionData$Weather=as.character(collisionData$Weather)
collisionData$Weather[is.na(collisionData$Weather)]="Others(Sunny,Cloudy,etc.)"
collisionData$Weather=as.factor(collisionData$Weather)

##//Yunyi ##transform date

collisionData$DATE= as.Date.factor(collisionData$DATE)
collisionData$INDEX=c(1:dim(collisionData)[1])
collisionData=subset(collisionData,DATE<as.Date("2016-06-30"))

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
                            column(4,h3("Map Settings:"),
                                     h4("Months:"), textOutput("Month"), ##//Yunyi
                                     h4("Working Day?"), textOutput("WorkingDay"),
                                     h4("Time Slot:"),textOutput("TimeSlot"),
                                     h4("Weather:"), textOutput("Weather") ##//Yunyi
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
                                                       textOutput("test2"))
                                               )
                                       ),
                              tabPanel("Accident Seasonality",value="Summary2",highchartOutput("Seasonality",width = "100%")), ##//Yunyi
                              tabPanel("Contributing Factor",value="Summary3",plotlyOutput("ContributingFactor",width = "100%")),##//Yunyi
                              tabPanel("Summary 4",value="Summary4",plotOutput("ppplot7",width = "100%")),
                              tabPanel("Summary 5",value="Summary5",plotOutput("ppplot8",width = "100%")),
                              tabPanel("View CCTV",value="ViewCCTV",uiOutput("cctv"))
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
                   plotOutput("ppplot3",width = "100%")
                  ),
           tabItem(tabName = "AboutTeam",
                   plotOutput("ppplot4",width = "100%")
                  ),
           tabItem(tabName = "TBD",
                   plotOutput("ppplot5",width = "100%")
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
  summary_map=leaflet(collisionData[1:500,])%>%
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
  New.Sum.Data2$ForSeasonality=collisionData
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
    
    
    New.Sum.Para$New.Map2_Bounds=input$Map2_bounds
    latRng <- range(New.Sum.Para$New.Map2_Bounds$north, New.Sum.Para$New.Map2_Bounds$south)
    lngRng <- range(New.Sum.Para$New.Map2_Bounds$east, New.Sum.Para$New.Map2_Bounds$west)
    New.Sum.Data2$ForSeasonality=subset(New.Sum.Data1$ForSeasonality, 
                                        LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                        LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
    New.Sum.Data2$ForContributingFactor=subset(New.Sum.Data1$ForSeasonality, 
                                               LATITUDE  >= latRng[1] & LATITUDE  <= latRng[2] &
                                               LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2] )
  })
  
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
    add_lines(p,y=graphdata$Injured, name=paste("Number of","<br>","People Injured"),line=list(shape="linear"),color=I("black"),showlegend=TRUE) 
  
  })
  
  #---Seasonality---
  output$Seasonality=renderHighchart({
    ##//Yunyi
    SeasonalData=New.Sum.Data2$ForSeasonality
    SeasonalData$season <- quarters(New.Sum.Data2$ForSeasonality$DATE)
    SeasonalData$year<-years(New.Sum.Data2$ForSeasonality$DATE)
    plotdata<-SeasonalData %>% 
      group_by(year,season) %>% 
      summarise(
        accident=n(),
        killed = sum(NUMBER.OF.PERSONS.KILLED+NUMBER.OF.PERSONS.INJURED)
      )

    #plotdata<-plotdata[1:(nrow(plotdata)-2),] ##//????????????????
    plotdata$time<-2011+as.numeric(plotdata$year)+as.numeric(as.factor(plotdata$season))/4
    highchart() %>% 
      hc_title(text = "Number of Accidents Grouped by Seasons") %>% 
      hc_xAxis(tickInterval=1,showFirstLabel=TRUE,title=list(text="Time"))%>%
      hc_yAxis(title=list(text="Number of Accidents"))%>%
      hc_add_series_scatter(plotdata$time,plotdata$accident,plotdata$killed,color=plotdata$season,lineWidth=2,name=" ",showInLegend=FALSE,label=paste(plotdata$year,plotdata$season)) %>%
      hc_tooltip(pointFormat = "Time: {point.label} <br> Number of Accidents: {point.y} <br> People Injured & Killed (Size of The Bubble): {point.z}")
  })
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
  output$ppplot3=renderPlot(hist(rnorm(100)))
  
###Page 6 About Team
  output$ppplot4=renderPlot(hist(rnorm(100)))

###Page 7 To Be Developed
  output$ppplot5=renderPlot(hist(rnorm(100)))

}

shinyApp(ui = ui, server = server)
