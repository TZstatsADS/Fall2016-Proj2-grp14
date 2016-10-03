#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/gallery/superzip-example.html

library(shiny)
library(leaflet)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Traffic Accident Analysis",id="nav",position = "fixed-top",fluid=T,
             tabPanel("Map", 
                      div(class="outer",
                          tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                            #includeScript("gomap.js")  
                            #Wait for further development
                                    ),
                          leafletOutput("Map", width="65%",height=750),
                          absolutePanel(id = "Dashboard1", class = "dashboardPanel", fixed = F,
                                        draggable = F, top = 0, left = "auto", right = 0, bottom = "auto",
                                        width = "30%", height = "auto",
                                        h3("Dashboard"),
                                        checkboxInput("included", "Add this layer",F)
                                       ),
                          absolutePanel(id = "Dashboard2", class = "dashboardPanel", fixed = F,
                                        draggable = F, top = 240, left = "auto", right = 0, bottom = "auto",
                                        width = "30%", height = "auto",
                                        h3("Dashboard"),
                                        checkboxInput("included", "Add this layer",F)
                                       ),
                          absolutePanel(id = "Dashboard3", class = "dashboardPanel", fixed = F,
                                        draggable = F, top = 480, left = "auto", right = 0, bottom = "auto",
                                        width = "30%", height = "auto",
                                        h3("Dashboard"),
                                        checkboxInput("included", "Add this layer",F)
                                       ),
                          fluidRow(
                                   column(6, plotOutput("ppplot",width = "100%")),
                                   column(6, plotOutput("ppplot2",width = "100%"))
                            ),
                          fluidRow(
                                   column(6, plotOutput("ppplot3",width = "100%")),
                                   column(6, plotOutput("ppplot4",width = "100%"))
                                  ),
                          textOutput("text1") # an output place for debug
                          )
                      ),
             tabPanel("Summary"),
             tabPanel("Table")
             )
)

# Define server logic required to draw a histogram
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
    leafletProxy("Map") %>% addPopups(lng, lat, content, layerId = ind)
  }
  observe({
    leafletProxy("Map") %>% clearPopups()
    event <- input$Map_shape_click
    output$text1=renderText(as.character(event))
    if (is.null(event))   
      return() else{
        isolate({
         showInfoPopup(event$id, event$lat, event$lng)
        })
      }    
  })  
  output$Map=renderLeaflet(risk_map)
  output$ppplot=renderPlot({   
    if (is.null(PointInBounds())) return(NULL) else
     {hist(PointInBounds()$Radius)}})
  output$ppplot2=renderPlot({   
    if (is.null(PointInBounds())) return(NULL) else
     {hist(PointInBounds()$Color)}})
  output$ppplot3=renderPlot(hist(rnorm(100)))
  output$ppplot4=renderPlot(hist(rnorm(100)))
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

# Run the application 
shinyApp(ui = ui, server = server)

