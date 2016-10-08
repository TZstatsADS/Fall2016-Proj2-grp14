library(shinydashboard)
library(shinyBS)
library(RCurl)
library(RJSONIO)
library(plyr)
library(leaflet)

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




sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Map Summary",  icon = icon("file-text-o"),
                       menuSubItem("Map Setting", tabName = "MapSetting", icon = icon("angle-right"),selected=TRUE),
                       menuSubItem("Summary by Area", tabName = "SummaryByArea", icon = icon("angle-right")),
                       menuSubItem("View CCTV", tabName = "ViewCCTV", icon = icon("angle-right"))
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
                   leafletOutput("Map", width="100%",height=350),
                   hr(),
                   fluidRow(column(4,textInput("AddressInput","Find a Place:","Columbia University, NY"),
                                   submitButton("Search"),
                                   textOutput("AddressError")),
                            column(4),
                            column(4)
                     
                           )
                  ),
           tabItem(tabName = "SummaryByArea",
                   leafletOutput("Map2", width="100%",height=350),
                   hr(),
                   plotOutput("ppplot1",width = "100%")
                  ),
           tabItem(tabName = "ViewCCTV",
                   leafletOutput("Map3", width="100%",height=350),
                   hr(),
                   uiOutput("cctv")
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




server <- function(input, output) {
  pal <- colorNumeric("Reds", domain = c(0,1))
  toyPoint=data.frame(Lng=runif(1000,min=-74.1,max=-73.5),
                      Lat=runif(1000,min=40.4,max=41.0),
                      Radius=runif(1000,min=1,max=5),
                      Color=runif(1000),
                      Index=c(1:1000)
  ) 
  risk_map=leaflet(toyPoint)%>%
    addProviderTiles("OpenStreetMap.BlackAndWhite")%>%
    addCircles(lat=~Lat,lng = ~Lng,
               radius=~Radius,color=~pal(Color),
               layerId=~Index)%>%
    setView(lng =-73.8,lat =  40.7, zoom = 10)
  
  cctv_map=leaflet(cctvLoc)%>%
    addProviderTiles("OpenStreetMap.BlackAndWhite")%>%
    addMarkers(lat=~LAT,lng = ~LNG,
               layerId=~CID,icon=icons(iconUrl=
                                         "http://icons.iconarchive.com/icons/martz90/circle/256/video-camera-icon.png",
                                       iconWidth = 20,iconHeight = 20))%>%
    setView(lng =-73.95,lat =  40.8, zoom = 12)
  
  
  
  
  PointInBounds <- reactive({
    if (is.null(input$Map_bounds))
      return(NULL)
    bounds <- input$Map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    subset(toyPoint,
           Lat >= latRng[1] & Lat <= latRng[2] &
             Lng >= lngRng[1] & Lng <= lngRng[2])
  })
  showInfoPopup <- function(ind, lat, lng) {
    selectedPoint <- toyPoint[ind,]
    content <- as.character("Some Information Here")
    leafletProxy("Map2") %>% addPopups(lng, lat, content, layerId = ind)
  }
  observe({
    address <- input$AddressInput
    response=geoCode(address)
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
    }
  })
  
  
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
  output$Map=renderLeaflet(risk_map)
  output$Map3=renderLeaflet(cctv_map)
  output$cctv=renderUI({
    event <- input$Map3_marker_click
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
  output$Map2=renderLeaflet(risk_map)
  output$ppplot1=renderPlot({   
    if (is.null(PointInBounds())) return(NULL) else
    {hist(PointInBounds()$Radius)}})
  output$ppplot2=renderPlot({   
    if (is.null(PointInBounds())) return(NULL) else
    {hist(PointInBounds()$Color)}})
  output$ppplot3=renderPlot(hist(rnorm(100)))
  output$ppplot4=renderPlot(hist(rnorm(100)))
  output$ppplot5=renderPlot(hist(rnorm(100)))
  output$ppplot6=renderPlot(hist(rnorm(100)))
  output$ppplot7=renderPlot(hist(rnorm(100)))
  output$ppplot8=renderPlot(hist(rnorm(100)))
  
  #  output$text1=renderText({
  #    if (is.null(input$Map_bounds))
  #      return("No data") else{
  #    bounds <- input$Map_bounds
  #    latRng <- range(bounds$north, bounds$south)
  #    lngRng <- range(bounds$east, bounds$west)
  #    st=paste(latRng,latRng,sep=" ")
  #    return(st)}
  #  })
}


shinyApp(ui = ui, server = server)
