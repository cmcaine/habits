# Complete shiny app for the HABITS DST.
#
# You might need to do setwd("tool") before you run it.

library(shiny)
library(leaflet)
# Install my fork
# devtools::install_github("cmcaine/leaflet.extras")
library(leaflet.extras)
library(sf)
library(ggplot2)
library(dplyr)
library(jsonlite)

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
st_crs(trips.export)<-4326


# Get pollution raster for overlay
load("../data/pollution_brick.RData")

# Get LSOAs for chloropleth
load("../data/combined_lsoas.RData")
combined_lsoas = subset(combined_lsoas, select = c('id', 'LSOA11NM', 'geometry')) %>% rename(label = LSOA11NM)
combined_lsoas$group = factor("LSOAs")
st_crs(combined_lsoas)<-4326

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

update <- function(input, output, regions) {
  # Update map and both plots. Bit expensive, could be a reactive expression
  print("Updating")
  # Filter trips.export to match input$filter
  trips = filterTrips(input$filter, regions)
  output$plot = updatePlot(input$variable, trips, quote(factor(startWeek)), "Start week")
  BA_xaxis = substitute(startDT < intervention_date, list(intervention_date = input$intervention_date))
  output$before_after_plot = updatePlot(input$variable, trips, BA_xaxis, "Before intervention")
  updateMap("map", input$variable, trips)
}

# NB: this can be a reactive value to avoid recomputation
filterTrips <- function(filter, regions) {
  # Return a subset of trips.export
  if (filter == "region") {
    if (nrow(regions) > 0) {
      inter = st_intersects(regions, trips.export)[[1]]
      if (length(inter) != 0) {
        trips.export[inter,]
      } else {
        subset(trips.export, F)
      }
    } else {
      subset(trips.export, F)
    }
  } else {
    print("Unknown filter!")
  }
}

plotMeta = data.frame(
  variable = c('MET-h', '# Journeys', '% Journeys'),
  title = c(
    "Change in MET-h per mode over time",
    "Number of Journeys by mode per week",
    "Proportion of Journeys by mode per week"
  ),
  row.names = 'variable'
)

updatePlot <- function(variable, trips, xaxis, xlabel) {
  # Prepare a ggplot according to variable and xaxis
  if (nrow(trips) < 1) {
    trips = trips.export
  }

  if (nrow(trips) == nrow(trips.export)) {
    title = "(All data)"
  } else {
    title = "(Region)"
  }

  plot = switch(as.character(variable),
                'MET-h' = {
                  activeTrips = subset(trips, MET > 0.1)
                  plot = (ggplot(trips, aes_(x = xaxis, y = ~MET, color = ~modality))
                           + scale_y_log10()
                           + geom_smooth(method = 'lm', aes(group = 1, color = "All modes")))

                  if (nrow(trips) > 500) {
                    plot = (plot + geom_violin(data = activeTrips)
                            + geom_smooth(data = activeTrips, method = 'lm', aes(group = modality)))
                  } else {
                    plot = plot + geom_jitter(data = activeTrips, height = 0)
                  }

                  plot
                },
                '# Journeys' = (ggplot(trips, aes_(x = xaxis, fill = ~modality))
                                + geom_histogram(stat = "count")
                                ),
                '% Journeys' = (ggplot(trips, aes_(x = xaxis, fill = ~modality))
                                + geom_histogram(stat = "count", position = "fill"))
  )

  plot = plot + xlab(xlabel) + ylab(variable) + ggtitle(paste(plotMeta[variable, 'title'], title))

  # Render it to outPlot
  renderPlot(plot)
}

updateMap <- function(mapref, variable, trips) {
  # Redraw routes, coloured by $variable
  if (nrow(trips) > 0) {
    leafletProxy(mapref, data=trips) %>%
      clearGroup('routes') %>%
      addPolylines(color=factpal(trips$modality), label=trips$modality, popup = trips$modality, group = 'routes')
  }
}




### Define UI ###

variables = c('# Journeys', '% Journeys', 'MET-h') #, 'Health impact (total)', 'Health impact (pollution)')
filters = c('region')
splits = c('modality', 'IMD', 'age', 'gender', 'region')

inputWidgets = inputPanel(
  selectInput('variable', 'variable', variables),
  selectInput('filter', 'filter', filters),
  selectInput('split', 'split', splits),
  conditionalPanel('document.querySelector(\'[aria-expanded="true"][data-value="before and after"][data-toggle="tab"]\') !== null',
                   dateInput('intervention_date', 'intervention date', value = "2017-08-01"))
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
    addSelectableRegions(map, data = data, label = ~htmltools::htmlEscape(label), ...)
  }

  addSelectableRegions <- function(map, data, ...) {
    # The labels are fussy
    #   Warning: Error in sum: invalid 'type' (list) of argument
    addPolygons(map, data = data, weight = 2, layerId = ~id, group = ~group, ...) #, label = htmltools::htmlEscape(data$label), ...)
  }

   output$map <- renderLeaflet({
     map = leaflet(options = leafletOptions(preferCanvas = T)) %>%
       addProviderTiles(provider = "CartoDB.Positron") %>%
       setView(-1.61, 54.97, zoom = 13) %>%
       addDrawToolbar(targetGroup = 'draw', circleOptions = drawCircleOptions(), editOptions = editToolbarOptions()) %>%
       addLayersControl(overlayGroups = c("Pollution map", "LSOAs")) %>%
       addRasterImage(pollution_brick[[1]], group = "Pollution map", opacity = .4) %>%
       addLSOAs(combined_lsoas) %>%
       hideGroup(c("Pollution map", "LSOAs"))
       # addPolygons(data = roads, smoothFactor = 20)
   })

   filtered = trips.export

   # updatePlot(data.frame(variable = c('# Journeys')), output, filtered)

   # Use an empty df of the right shape initially.
   regions = subset(combined_lsoas, F)
   drawn_shapes = regions

   # Update drawn_shapes
   observeEvent(input$map_draw_all_features, {
     if (length(input$map_draw_all_features$features) > 0) {
       drawn_shapes <<- read_sf(toJSON(input$map_draw_all_features, force=T, auto_unbox = T)) %>%
         rename(id = X_leaflet_id) %>% cbind(list(., group="draw", label=.$id))
     }
   })

   # observeEvent(input$map_draw_new_feature, {
   #   shape = read_sf(toJSON(input$map_draw_new_feature, force=T, auto_unbox = T)) %>%
   #       rename(id = X_leaflet_id) %>% cbind(list(., group="draw", label=.$id))
   #   leafletProxy("map") %>%
   #     removeShape(shape$id) %>%
   #     addSelectableRegions(shape)
   # })

   # Update regions on click
   observeEvent(input$map_shape_click, {

     print(input$map_shape_click)

     if (nrow(regions) > 0) {
       # Uncolour currently selected regions
       leafletProxy("map") %>%
         addLSOAs(regions)
     }

     # Update regions var.
     regions <<- switch(input$map_shape_click$group,
       "LSOAs" = subset(combined_lsoas, id == input$map_shape_click$id),
       "draw" = subset(drawn_shapes, id == input$map_shape_click$id),
     )

     # Highlight selected regions.
     leafletProxy("map") %>%
       addLSOAs(regions, fillColor = 'red')

     # Update everything else
     update(input, output, regions)
   })

   observe({update(input, output, regions)})

   # update(data.frame(variable = c('# Journeys'), filter = c('region')), output, regions)

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
