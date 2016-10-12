library(RCurl)
library(RJSONIO)
library(dplyr)
library(shiny)
library(bitops)
library(leaflet)
library(data.table)
library(shinydashboard)
##//Yiwei
library(plotly)

# toyPoint=data.frame(Lng=runif(1000,min=-74.1,max=-73.5),
#                     Lat=runif(1000,min=40.4,max=41.0),
#                     Radius=runif(1000,min=1,max=5),
#                     Color=runif(1000),
#                     Index=c(1:1000)
# )

setwd("G:/Columbia/study/3rd semester/5243/project2/data")
collisionData=fread("dataframe.txt")
collisionData=sample_n(collisionData,size=nrow(collisionData)/100)

TruePoint=data.frame(Lng=collisionData$LONGITUDE,
                     Lat=collisionData$LATITUDE,
                     Radius=as.numeric(collisionData$NUMBER.OF.PERSONS.INJURED),
                     Color=as.numeric(collisionData$NUMBER.OF.PERSONS.KILLED),
                     Index=c(1:dim(collisionData)[1])
)

# TruePoint=toyPoint

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

weatherSeq=c("Sunny"=1,"Rain"=2,"Snow"=3)
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
                            h4("Months Selected:"), textOutput("MonthSelected"),
                            h4("Working Day?"), textOutput("WorkingDay"),
                            h4("Weather Selected:"), textOutput("WeatherSelect"),
                            h4("Time Slot:"),textOutput("TimeSlot")
                     )),
            hr(),
            tabsetPanel(id="SummaryTabSet",
                        tabPanel("Summary 1",value="Summary1",textOutput("SummaryText")),
                        tabPanel("Summary 2",value="Summary2",plotOutput("ppplot1",width = "100%")),
                        tabPanel("Summary 3",value="Summary3",plotOutput("ppplot6",width = "100%")),
                        tabPanel("Summary 4",value="Summary4",plotOutput("ppplot7",width = "100%")),
                        ##//Yiwei
                        tabPanel("Summary 5",value="Summary5",plotlyOutput("ppplot8",width = "100%")),
                        tabPanel("Summary 6",value="Summary6",plotlyOutput("ppplot9",width = "100%")),
                        
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
            plotOutput("ppplot5",width = "100%"))
    
    ##//

  )
)

ui=dashboardPage(
  dashboardHeader(title = "Traffic Supervisor"),
  sidebar,
  body
)



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
  observeEvent(input$ApplyChanges,{
    New.Map2_Bound=input$Map_bounds
    New.Map2_Zoom=input$Map_zoom
    New.TimeSlot=input$TimeSlot
    New.Month=input$Month
    New.Weather=input$Weather
    New.Week=input$Week
    NewLat <- mean(c(New.Map2_Bound$north, New.Map2_Bound$south))
    NewLng <- mean(c(New.Map2_Bound$east , New.Map2_Bound$west ))
    leafletProxy("Map2") %>% 
      setView(lng =NewLng,lat =NewLat, zoom = New.Map2_Zoom)
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
  
  ##//Yiwei Page 8 Vehicle type against Year
  year_start=2012
  year_end=2015
  time_start=10
  time_end=15
  data=collisionData%>% mutate(YEAR=substr(DATE,start=1,stop=4)) #%>% rename(VEHICLE.TYPE=VEHICLE.TYPE.CODE.2)
  data=filter(data,!(VEHICLE.TYPE.CODE.1 %in% c("","UNKNOWN")))
  data=mutate(data,VEHICLE.TYPE=VEHICLE.TYPE.CODE.1) %>% 
  select(-VEHICLE.TYPE.CODE.1,-VEHICLE.TYPE.CODE.2)
  data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("SCOOTER","PEDICAB","FIRE TRUCK","AMBULANCE"))]="OTHER"
  data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("PICK-UP TRUCK"))]="TRUCK"
  data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("SMALL COM VEH(4 TIRES)"))]="4 TIRES"
  data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("LARGE COM VEH(6 OR MORE TIRES)"))]=">=6 TIRES"
  data$VEHICLE.TYPE[which(data$VEHICLE.TYPE %in% c("SPORT UTILITY / STATION WAGON"))]="SPORT UTILITY"
  # byYEAR=group_by(data,YEAR,VEHICLE.TYPE)
  # risk_by_type=summarise(byYEAR, count=log(n()/5),INTENSITY=mean(NUMBER.OF.PERSONS.KILLED))
  # byTIME=group_by(data,TIME,VEHICLE.TYPE)
  # risk_by_time=byTIME %>% summarise(count=log(n()/5),INTENSITY=mean(NUMBER.OF.PERSONS.KILLED))
  # risk_by_time$TIME=as.numeric(risk_by_time$TIME)
  # 
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
  
  output$ppplot8=renderPlotly(
    plot_ly(x=graphdata2$YEAR,y=graphdata2$count,type ='scatter',mode ='lines+markers',group = graphdata2$VEHICLE.TYPE,color = graphdata2$VEHICLE.TYPE)%>%layout(xaxis = list(title = "Year"), yaxis =list(title = "log number of average collisions"))
  )
  
  output$ppplot9=renderPlotly(
    plot_ly(x=graphdata3$YEAR,y=graphdata3$count,type ='scatter',mode ='lines+markers',group = graphdata3$VEHICLE.TYPE,color = graphdata3$VEHICLE.TYPE)%>%layout(xaxis = list(title = "Year"), yaxis =list(title = "log number of average collisions"))
    
    )
  
}


shinyApp(ui = ui, server = server)
