
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

toyPoint=data.frame(Lng=runif(1000,min=-74.1,max=-73.5),
                    Lat=runif(1000,min=40.4,max=41.0),
                    Radius=runif(1000,min=1,max=5),
                    Color=runif(1000),
                    Index=c(1:1000)
                   )

#setwd("C:/Users/Administrator/Desktop/proj2/try2")
r<-runif(50000,1,745607)
collisionData=read.csv("dataframe.csv")[r,]

##//Yunyi ##transform date
trans.date <- function(x){
  a=as.Date.factor(x) 
  return(a)
}
collisionData$DATE= trans.date(collisionData$DATE)


TruePoint=data.frame(Lng=collisionData$LONGITUDE,
                     Lat=collisionData$LATITUDE,
                     Radius=as.numeric(collisionData$NUMBER.OF.PERSONS.INJURED),
                     Color=as.numeric(collisionData$NUMBER.OF.PERSONS.KILLED),
                     Index=c(1:dim(collisionData)[1])
                    )

#TruePoint=toyPoint

cctvLoc=data.frame(CID=c(798,688,468,5,407,305),
                   LNG=c(-73.954841,-73.9547816,-73.9838152,-73.983551,-73.9601119,-73.986752),
                   LAT=c(40.7865992,40.8107875,40.7786139,40.7680809,40.7585756,40.7484496),
                   NAME=c("Park Ave @ 96 St","St Nicholas Ave @ 125 St","Amsterdam @ 72 St",
                          "Central Park S @ Columbus Cr","QBB NOR @ York Ave","5 Ave @ 34 St"))
url0="http://207.251.86.238/cctv5.jpg?math=0.21909423107"
cid=5

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

weatherSeq=c("Suny"=1,"Rain"=2,"Snow"=3)
#pal <- colorNumeric("Reds", domain = range(collisionData$People.Killed))


sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Map Summary",  icon = icon("file-text-o"),
                       menuSubItem("Map Setting", tabName = "MapSetting", icon = icon("angle-right"),selected=TRUE),
                       menuSubItem("Summary by Area", tabName = "SummaryByArea", icon = icon("angle-right"))
                      ),
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
           tabItem(tabName = "MapSetting",
                   leafletOutput("Map", width="100%",height=300),
                   hr(),
                   fluidRow(column(4,textInput("AddressInput","Find a Place:","Columbia University, NY"),
                                               actionButton("Search",label = "Search"),
                                   textOutput("AddressError"),
                                   hr(),
                                   sliderInput("TimeSlot",label="Hours included in Summary",min=0,max=24,value=c(0,24))),
                            column(4,checkboxGroupInput("Month", "Months included in Risk by Hour:",monthSeq)),
                            column(4,checkboxGroupInput("Week", "Weekdays or else:",daySeq),
                                   hr(),
                                   checkboxGroupInput("Weather", "Weather included:",weatherSeq),
                                   actionButton("ApplyChanges",label = "Apply Changes"),
                                   textOutput("test2"))
                     
                           )
                  ),
           tabItem(tabName = "SummaryByArea",
                   fluidRow(column(8,leafletOutput("Map2", width="100%",height=300)),
                            column(4,h3("Map Settings:"),
                                     h4("Months:"), textOutput("Month"), ##//Yunyi
                                     h4("Working Day?"), textOutput("WorkingDay"),
                                     h4("Time Slot:"),textOutput("TimeSlot"),
                                    h4("Weather:"), textOutput("Weather") ##//Yunyi
                                   )),
                   hr(),
                   tabsetPanel(id="SummaryTabSet",
                              tabPanel("Summary 1",value="Summary1",textOutput("SummaryText")),
                              tabPanel("Summary 2",value="Summary2",highchartOutput("Seasonality",width = "100%")), ##//Yunyi
                              tabPanel("Summary 3",value="Summary3",plotlyOutput("ContributingFactor",width = "100%")),##//Yunyi
                              tabPanel("Summary 4",value="Summary4",plotOutput("ppplot7",width = "100%")),
                              tabPanel("Summary 5",value="Summary5",plotOutput("ppplot8",width = "100%")),
                              tabPanel("View CCTV",value="ViewCCTV",uiOutput("cctv"))
                              ),
                   textOutput("test1")
                  ),
           tabItem(tabName = "AreaCluster",
                   plotOutput("ppplot2",width = "100%")
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
###Page1 Map Setting
  #---Map---
  risk_map=leaflet(TruePoint)%>%
    addProviderTiles("OpenStreetMap.BlackAndWhite")%>%
    addCircles(lat=~Lat,lng = ~Lng,
               radius=~Radius,color="Red",
               layerId=~Index)%>%
    setView(lng =-73.8,lat =  40.7, zoom = 10)
  
  output$Map=renderLeaflet(risk_map)
  
  #---Search---
  observeEvent(input$Search,{
    Inputaddress=input$AddressInput
    response=geoCode(Inputaddress)
    if (!is.na(response[1])){
      NewLng=response[2]
      NewLat=response[1]
      output$AddressError=renderText(" ")
      leafletProxy("Map") %>% 
        clearMarkers()%>%
        addMarkers(lng=NewLng,lat=NewLat)%>%
        setView(lng =NewLng,lat =NewLat, zoom = 12)
    } else{
      output$AddressError=renderText("Place Not Found")
    }})
  #---Apply Changes---
  New.Sum.Para=reactiveValues()
  observeEvent(input$ApplyChanges,{  ##//
               New.Sum.Para$New.Map2_Bound=input$Map_bounds
               New.Sum.Para$New.Map2_Zoom=input$Map_zoom
               New.Sum.Para$New.TimeSlot=input$TimeSlot
               New.Sum.Para$New.Month=input$Month
               New.Sum.Para$New.Weather=input$Weather
               New.Sum.Para$New.Week=input$Week
               NewLat <- mean(c(New.Sum.Para$New.Map2_Bound$north, New.Sum.Para$New.Map2_Bound$south))
               NewLng <- mean(c(New.Sum.Para$New.Map2_Bound$east , New.Sum.Para$New.Map2_Bound$west ))
               leafletProxy("Map2") %>% 
                 setView(lng =NewLng,lat =NewLat, zoom = New.Sum.Para$New.Map2_Zoom)
              })
  
###Page2  Summary By Area
  #---Map---
  summary_map=leaflet(TruePoint)%>%
    addProviderTiles("OpenStreetMap.BlackAndWhite")%>%
    addCircles(lat=~Lat,lng = ~Lng,
               radius=~Radius,color="Red",
               layerId=~Index)%>%
    addMarkers(lat=cctvLoc$LAT,lng = cctvLoc$LNG,
               layerId=cctvLoc$CID,icon=icons(iconUrl=
                                 "http://icons.iconarchive.com/icons/martz90/circle/256/video-camera-icon.png",
               iconWidth = 20,iconHeight = 20))%>%
    setView(lng =-73.95,lat =  40.8, zoom = 12)
  output$Map2=renderLeaflet(summary_map)
  
  showInfoPopup <- function(ind, lat, lng) {
    selectedPoint <- TruePoint[ind,]
    content <- as.character("Some Information Here")
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
  PointInBounds <- reactive({
    if (is.null(input$Map2_bounds))
      return(NULL)
    bounds <- input$Map2_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    subset(TruePoint,
           Lat >= latRng[1] & Lat <= latRng[2] &
           Lng >= lngRng[1] & Lng <= lngRng[2])
  })
  output$TimeSlot=renderText(New.Sum.Para$New.TimeSlot) ##//Yunyi
  output$Month=renderText(New.Sum.Para$New.Month)##//Yunyi
  output$Weather=renderText(New.Sum.Para$New.Weather)##//Yunyi
  output$AreaSummary=renderText("Setting Summary Here")
  output$SummaryText=renderText("Area Summary Here")
  output$ppplot1=renderPlot({   
    if (is.null(PointInBounds())) return(NULL) else
    {hist(PointInBounds()$Radius)}})
  output$ppplot6=renderPlot(hist(rnorm(100)))
  output$ppplot7=renderPlot(hist(rnorm(100)))
  output$ppplot8=renderPlot(hist(rnorm(100)))
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
  output$ContributingFactor=renderPlotly({
    graphdata<-collisionData %>% 
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
  output$Seasonality=renderHighchart({
    collisionData$season <- quarters(collisionData$DATE)
    collisionData$year<-years(collisionData$DATE)
    plotdata<-collisionData %>% 
      group_by(year,season) %>% 
      summarise(
        accident=n(),
        killed = sum(NUMBER.OF.PERSONS.KILLED+NUMBER.OF.PERSONS.INJURED)
      )
    plotdata<-plotdata[1:(nrow(plotdata)-2),]
    plotdata$time<-2011+as.numeric(plotdata$year)+as.numeric(as.factor(plotdata$season))/4
    highchart() %>% 
      hc_title(text = "Number of Accidents Grouped by Seasons") %>% 
      hc_xAxis(tickInterval=1,showFirstLabel=TRUE,title=list(text="Time"))%>%
      hc_yAxis(title=list(text="Number of Accidents"))%>%
      hc_add_series_scatter(plotdata$time,plotdata$accident,plotdata$killed,color=plotdata$season,lineWidth=2,name=" ",showInLegend=FALSE,label=paste(plotdata$year,plotdata$season)) %>%
      hc_tooltip(pointFormat = "Time: {point.label} <br> Number of Accidents: {point.y} <br> People Injured & Killed (Size of The Bubble): {point.z}")
    
    
  })
###Page3  CCTV

  #---Map---


  
  #---CCTV img---


###Page 4 Cluster
  output$ppplot2=renderPlot({   
    if (is.null(PointInBounds())) return(NULL) else
    {hist(PointInBounds()$Color)}})
  
###Page 5 Read Me
  output$ppplot3=renderPlot(hist(rnorm(100)))
  
###Page 6 About Team
  output$ppplot4=renderPlot(hist(rnorm(100)))

###Page 7 To Be Developed
  output$ppplot5=renderPlot(hist(rnorm(100)))

  
}


shinyApp(ui = ui, server = server)
