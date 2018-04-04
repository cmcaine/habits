library(shiny)
library(leaflet)
library(leaflet.extras)
# library(plotly)
library(sf)
# library(tidyquant)
library(ggplot2)
library(dplyr)

### Load and prepare data ###

# Get aggregated lines
# Each segment will have a list of trips IDs that start, intersecting, or end in the segment
# Table of trips relates trip ID to aggregate statistics
#  load("../data/roads_sf.Rdata")
load("../data/trips.export.RData")
trips.export = st_as_sf(trips.export)
trips.export$startDT = as.Date(trips.export$startDT, format="%a %Y-%m-%d %H:%M:%S")
# Assign each trip to Thursday of the week its in.
trips.export$startWeek = as.Date(strptime(strftime(trips.export$startDT, "%Y %W 4"), format = "%Y %W %u"))
trips.export$MET[is.na(trips.export$MET)] = 0.1
# Exclude rare modes
modes = c("Bike", "Bus", "Car", "Foot", "Train")
trips.export = trips.export[trips.export$modality %in% modes,]

# Get pollution raster for overlay
load("../data/pollution_brick.RData")

# Get LSOAs for chloropleth
load("../data/combined_lsoas.RData")


### Update logic ###

updateOutputs = function(input, output, regions) {

  inner = function(variable, filter, split, regions, trips) {
    # Filter trips by filter and regions
    # Aggregate variable for each line for the hotroads
    # Split trips by split and plot
  }

  inner(input$variable, input$filter, input$split, regions, trips)
}

factpal = colorFactor(rainbow(5), trips.export$modality)

updatePlot = function(input, output, trips) {
  # If we weren't given enough data, use the full set.
  if (nrow(trips) < 1) {
    trips = trips.export
  }

  if (nrow(trips) == nrow(trips.export)) {
    title = "(All data)"
  } else {
    title = "(Region)"
  }

  if (input$variable == 'MET-h') {
    title = paste("Change in MET-h per mode over time", title)
    plot = (ggplot(trips, aes(x = factor(startWeek), y = MET, color = modality))
           + scale_y_log10()
           + xlab("Start week")
           + ylab("MET-h")
           + ggtitle(title))

    if (nrow(subset(trips, MET > 0.1)) > 500) {
      plot = plot + geom_violin(data = subset(trips, MET > 0.1))
    } else {
      plot = plot + geom_jitter(height = 0)
    }

    plot = plot + geom_smooth(method = 'lm', aes(group = 1, color = "All modes"))

    if (nrow(trips) > 500) {
      plot = plot + geom_smooth(method = 'lm', aes(group = modality))
    }
  } else if (input$variable == '# Journeys') {
    title = paste("Number of Journeys by mode per week", title)
    freq = count(data.frame(week = trips$startWeek, modality = trips$modality), week, modality)
    plot = (ggplot(freq, aes(x = week, y = n, fill = modality))
      + geom_bar(stat = 'identity')
      + ggtitle(title))
  } else if (input$variable == '% Journeys') {
    title = paste("Number of Journeys by mode per week", title)
    freq = count(data.frame(week = trips$startWeek, modality = trips$modality), week, modality)
    plot = (ggplot(freq, aes(x = week, y = n, fill = modality))
      + geom_bar(position = 'fill', stat = 'identity')
      + ggtitle(title))
  }
  output$plot = renderPlot(plot)
}



### Define UI ###

variables = c('# Journeys', '% Journeys', 'MET-h', 'Health impact (total)', 'Health impact (pollution)')
filters = c('region')
splits = c('modality', 'IMD', 'age', 'gender', 'region')

inputWidgets = inputPanel(
  selectInput('variable', 'variable', variables),
  selectInput('filter', 'filter', filters),
  selectInput('split', 'split', splits),
  conditionalPanel('document.querySelector(\'[aria-expanded="true"][data-value="before and after"][data-toggle="tab"]\') !== null',
                   dateInput('before_after_date', 'intervention date'))
)

plot = plotOutput("plot", height="100%")
map = leafletOutput("map", height="100%")
before_after_plot = plotOutput("before_after_plot", height="100%")

ui <- fluidPage(

  # titlePanel("HABITS Decision Support Tool"),

   fluidRow(
     column(2, inputWidgets),
     column(10,
            tabsetPanel(
              tabPanel("map", map),
              tabPanel("plot", plot),
              tabPanel("before and after", before_after_plot)
            )
     )
   ),

   # Fix the size of various ancestor elements so we can use 100% as a size
   # without messing everything up.
   theme = "custom.css"
)

# Alternative entirely custom version
# ui <- htmlTemplate(filename = "www/split-pane-template.html", map = map, plot = plot)


### Server-side logic (setup and observers) ###

# Draw the map and plot and react to changes in inputs, map draw events, etc.
server <- function(input, output) {

  addLSOAs <- function(map, data=combined_lsoas, ...) {
    addPolygons(map, data = data, group = "LSOAs", layerId = ~LSOA11CD, weight = 2, ...)
  }

   output$map <- renderLeaflet({
     map = leaflet(options = leafletOptions(preferCanvas = T)) %>%
       addProviderTiles(provider = "CartoDB.Positron") %>%
       setView(-1.61, 54.97, zoom = 13) %>%
       addDrawToolbar(targetGroup = 'draw', circleOptions = drawCircleOptions(), editOptions = editToolbarOptions()) %>%
       addLayersControl(overlayGroups = c("Pollution map", "LSOAs")) %>%
       addRasterImage(pollution_brick[[1]], group = "Pollution map", opacity = .4) %>%
       addLSOAs() %>%
       hideGroup(c("Pollution map", "LSOAs"))
       # addPolygons(data = roads, smoothFactor = 20)
   })

   filtered = trips.export

   # updatePlot(data.frame(variable = c('# Journeys')), output, filtered)

   # Use an empty df of the right shape initially.
   regions = subset(combined_lsoas, F)

   # Update regions on click
   observeEvent(input$map_shape_click, {
     # Uncolour currently selected regions
     leafletProxy("map") %>%
       addLSOAs(regions)

     # Update regions var.
     regions <<- subset(combined_lsoas, id == input$map_shape_click$id)

     # Highlight selected regions.
     leafletProxy("map") %>%
       addLSOAs(regions, fillColor = 'red')

     # Update everything else
     # update(input, output, regions)
   })

   # observe({
   #   # If required, be more efficient and have different observers for different kinds of event
   #   # (e.g. don't filter the trips unless the filter or region have changed)
   #     # Act iff a region has just been drawn
   #     # req(input$map_draw_stop)
   #
   #   # For each region in 'draw':
   #     # Convert region to sf
   #     # Add to region list
   #
   #
   #   # proxy = leafletProxy("map") %>%
   #   #   hideGroup(c("Pollution map", "LSOAs"))
   #
   #   # updateOutputs(input, output, regions)
   #   #updatePlot(input, output, filtered)
   #
   #   # Draw trips that
   #   if (isTruthy(input$map_click)) {
   #     latlong<- reactiveVal(value=input$map_click)
   #     print(latlong)
   #     buffer<- st_buffer(sf::st_point(c(latlong()$lng, latlong()$lat)), dist=0.00022, nQuadSegs=2)
   #     inter<- st_intersects(buffer, trips.export)
   #
   #     filtered <<- trips.export[inter[[1]],]
   #     # filtered = filtered[filtered$modality %in% c("Bike", "Foot"),]
   #
   #     # This isn't an acceptable substitute. Don't understand why not.
   #     # filtered<- st_intersection(trips.export, buf)
   #
   #     if (nrow(filtered) > 0) {
   #       leafletProxy("map", data=filtered) %>%
   #         clearGroup('routes') %>%
   #         addPolylines(color=factpal(filtered$modality), label=filtered$modality, group = 'routes')
   #     }
   #   }
   #
   #   updatePlot(input, output, filtered)
   # })
}

# Run the application
shinyApp(ui = ui, server = server)