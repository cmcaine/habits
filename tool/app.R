library(shiny)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(sf)

variables = c('# Journeys', '% Journeys', 'Health impact (total)', 'Health impact (pollution)')
filters = c('mode', 'IMD', 'age', 'gender')

plot = plotlyOutput("plot", height="100%")
map = leafletOutput("map", height="100%")
inputWidgets = inputPanel(
  selectInput('variable', 'variable', variables),
  selectInput('filter', 'filter', filters),
  selectInput('split', 'split', filters)
)

# Get aggregated lines
# Each segment will have a list of trips IDs that start, intersecting, or end in the segment
# Table of trips relates trip ID to aggregate statistics
#  load("../data/roads_sf.Rdata")
load("../data/trips.export.RData")
trips.export = st_as_sf(trips.export)
trips.export$startDT = as.POSIXct(trips.export$startDT, format="%a %Y-%m-%d %H:%M:%S")
trips.export$MET[is.na(trips.export$MET)] = 0

# Get pollution raster for overlay
load("../data/pollution_brick.RData")

# extract raster

updateOutputs = function(input, output, regions) {

  inner = function(variable, filter, split, regions, trips) {
    # Filter trips by filter and regions
    # Aggregate variable for each line for the hotroads
    # Split trips by split and plot
  }

  inner(input$variable, input$filter, input$split, regions, trips)
}

factpal = colorFactor(topo.colors(5), trips.export$modality)

# Define UI for application that draws a histogram
ui <- fluidPage(

   titlePanel("HABITS Decision Support Tool"),

   fluidRow(
     column(2, inputWidgets),
     column(10,
            tabsetPanel(
              tabPanel("map", map),
              tabPanel("plot", plot)
            )
     )
   ),

   # Fix the size of various ancestor elements so we can use 100% as a size
   # without messing everything up.
   theme = "custom.css"
)

# Alternative entirely custom version
# ui <- htmlTemplate(filename = "www/split-pane-template.html", map = map, plot = plot)

# Draw the map and plot and react to changes in inputs, map draw events, etc.
server <- function(input, output) {

   output$map <- renderLeaflet({
     leaflet(options = leafletOptions(preferCanvas = T)) %>%
       addProviderTiles(provider = "CartoDB.Positron") %>%
       setView(-1.61, 54.97, zoom = 13) %>%
       addDrawToolbar(targetGroup = 'draw', circleOptions = drawCircleOptions(), editOptions = editToolbarOptions()) %>%
       addLayersControl(overlayGroups = c("Pollution map")) %>%
       addRasterImage(pollution_brick[[1]], group = "Pollution map", opacity = .4) # %>%
       # addPolygons(data = roads, smoothFactor = 20)
   })

   #output$plot <- renderPlot({plot(rnorm(100))})
   output$plot <- renderPlotly({plot_ly(data.frame(1:100, rnorm(100)), x = ~X1.100, y = ~rnorm.100.)})

   observe({
     # If required, be more efficient and have different observers for different kinds of event
     # (e.g. don't filter the trips unless the filter or region have changed)
       # Act iff a region has just been drawn
       # req(input$map_draw_stop)

     # For each region in 'draw':
       # Convert region to sf
       # Add to region list

     # updateOutputs(input, output, regions)

     proxy = leafletProxy("map") %>%
       hideGroup(c("Pollution map"))

     # Draw trips that
     req(input$map_click)
     latlong<- reactiveVal(value=input$map_click)
     print(latlong)
     buffer<- st_buffer(sf::st_point(c(latlong()$lng, latlong()$lat)), dist=0.00022, nQuadSegs=2)
     inter<- st_intersects(buffer, trips.export)

     lines.to.plot<- trips.export[inter[[1]],]

     # This isn't an acceptable substitute. Don't understand why not.
     # lines.to.plot<- st_intersection(trips.export, buf)

     leafletProxy("map", data=lines.to.plot) %>%
       clearShapes() %>%
       addPolylines(color=factpal(lines.to.plot$modality))
   })
}

# Run the application
shinyApp(ui = ui, server = server)