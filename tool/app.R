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

# Crop a Simple Features Data Frame to the extent of a raster
crop.sf <- function(sfdf, rasta) {
  # Get extent and crs
  ext.sp <- as(extent(rasta), "SpatialPolygons")
  crs(ext.sp) <- crs(rasta)

  # crop
  st_intersection(sfdf, st_as_sf(ext.sp))
}

# Get pollution raster for overlay
load("../data/pollution_brick.RData")

# Get aggregated lines

# Each segment will have a list of trips IDs that start, intersect, or end in the segment
# Table of trips relates trip ID to aggregate statistics
# load("../data/roads_sf.Rdata")

# Didn't do this in the end

# Get trips

load("../data/trips.export.RData")
# trips.export = crop(trips.export, pollution_brick)
trips.export = st_as_sf(trips.export)
# trips.export = crop.sf(trips.export, pollution_brick)
trips.export$startDT = as.Date(trips.export$startDT, format = "%a %Y-%m-%d %H:%M:%S")
# Assign each trip to Thursday of the week its in.
trips.export$startWeek = as.Date(strptime(strftime(trips.export$startDT, "%Y %W 4"), format = "%Y %W %u"))
trips.export$MET[is.na(trips.export$MET)] = 0.1
# Exclude rare modes
modes = c("Bike", "Bus", "Car", "Foot", "Train")
trips.export = trips.export[trips.export$modality %in% modes, ]
st_crs(trips.export) <- 4326

# Get LSOAs for chloropleth
load("../data/combined_lsoas.RData")
combined_lsoas = subset(combined_lsoas, select = c('id', 'LSOA11NM', 'geometry')) %>% rename(label = LSOA11NM)
combined_lsoas$group = factor("LSOAs")
st_crs(combined_lsoas) <- 4326

# Colours and theming
cbbPalette <-
  c("#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7")
defraPalette <- c("#01237D", "#57FF00", "#FB0400")
theme_update(text = element_text(size = 20))

### Update logic ###

updateOutputs = function(input, output, regions) {
  inner = function(variable, filter, split, regions, trips) {
    # Filter trips by filter and regions
    # Aggregate variable for each line for the hotroads
    # Split trips by split and plot
  }

  inner(input$variable, input$filter, input$split, regions, trips)
}

update <- function(input, output, regions) {
  # Update map and both plots. Bit expensive, could be a reactive expression
  print("Updating")
  # Filter trips.export to match input$filter
  trips = filterTrips(input$filter, regions)
  output$plot = updatePlot(input$variable, trips, quote(factor(startWeek)), "Start week")
  BA_xaxis = substitute(startDT < intervention_date,
    list(intervention_date = input$intervention_date))
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
        trips.export[inter, ]
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

  plot = switch(
    as.character(variable),
    'MET-h' = {
      activeTrips = subset(trips, MET > 0.1)
      plot = (ggplot(trips, aes_(
        x = xaxis,
        y = ~ MET,
        color = ~ modality
      ))
        + scale_y_log10())

      if (nrow(trips) > 500) {
        plot = (
          plot + geom_violin(data = activeTrips)
          + geom_smooth(
            data = activeTrips,
            method = 'lm',
            aes(group = modality)
          )
        )
      } else {
        plot = plot + geom_jitter(data = activeTrips, height = 0)
      }

      plot + scale_linetype_manual(values = cbbPalette) + geom_smooth(method = 'lm', aes(group = 1, color = "All modes"))
    },
    '# Journeys' = (
      ggplot(trips, aes_(x = xaxis, fill = ~ modality))
      + geom_histogram(stat = "count")
      + scale_fill_manual(values = cbbPalette)
    ),
    '% Journeys' = (
      ggplot(trips, aes_(x = xaxis, fill = ~ modality))
      + geom_histogram(stat = "count", position = "fill")
      + scale_fill_manual(values = cbbPalette)
    )
  )

  plot = plot + xlab(xlabel) + ylab(variable) + ggtitle(paste(plotMeta[variable, 'title'], title))

  # Render it to outPlot
  renderPlot(plot)
}

# Partial application of $
cg <- function (name) {
  function (df) {
    df[[name]]
  }
}

mapMeta = list(
  "MET-h" = list(
    col = function(trips) {
      log(trips$MET)
    },
    pal = colorNumeric(palette = "RdBu", domain = log(trips.export$MET))
  ),
  "# Journeys" = list(
    col = cg("modality"),
    pal = colorFactor(cbbPalette, trips.export$modality)
  ),
  "% Journeys" = list(
    col = cg("modality"),
    pal = colorFactor(cbbPalette, trips.export$modality)
  )
)

addTrips <- function (map, trips, variable) {
  meta <- mapMeta[[variable]]
  col <- meta$col(trips)
  addPolylines(
    map,
    group = "routes",
    data = trips,
    color = meta$pal(col),
    label = col
  ) %>%
    addLegend(
      "bottomright",
      pal = meta$pal,
      values = col,
      layerId = "routesLegend"
    )
}

updateMap <- function(mapref, variable, trips) {
  # Redraw routes, coloured by $variable
  if (nrow(trips) > 0) {
    map <- leafletProxy(mapref, data = trips) %>% clearGroup('routes')
    addTrips(map, trips, variable)
  }
}




### Define UI ###

variables = c('# Journeys', '% Journeys', 'MET-h') #, 'Health impact (total)', 'Health impact (pollution)')
filters = c('region')
splits = c('modality', 'IMD', 'age', 'gender', 'region')

inputWidgets = inputPanel(
  selectInput('variable', 'variable', variables),
  selectInput('filter', 'filter', filters),
  # Show split panel unless active tab is the map. Map is the first active tab, hence the more elaborate function
  conditionalPanel(
    '(() => {
      tab = document.querySelector(\'[aria-expanded="true"][data-toggle="tab"]\')
      if (tab == null) {
        return false
      } else {
        return tab.getAttribute("data-value") !== map
      }
    })()',
    selectInput('split', 'split', splits)
  ),
  conditionalPanel(
    'document.querySelector(\'[aria-expanded="true"][data-value="before and after"][data-toggle="tab"]\') !== null',
    dateInput('intervention_date', 'intervention date', value = "2017-08-01")
  )
)

plot = plotOutput("plot", height = "100%")
map = leafletOutput("map", height = "100%")
before_after_plot = plotOutput("before_after_plot", height = "100%")

ui <- fluidPage(# titlePanel("HABITS Decision Support Tool"),

  fluidRow(column(2, inputWidgets),
    column(
      10,
      tabsetPanel(
        tabPanel("map", map),
        tabPanel("plot", plot),
        tabPanel("before and after", before_after_plot)
      )
    )),

  # Fix the size of various ancestor elements so we can use 100% as a size
  # without messing everything up.
  theme = "custom.css")

# Alternative entirely custom version
# ui <- htmlTemplate(filename = "www/split-pane-template.html", map = map, plot = plot)


### Server-side logic (setup and observers) ###

# Draw the map and plot and react to changes in inputs, map draw events, etc.
server <- function(input, output) {
  addLSOAs <- function(map, data = combined_lsoas, ...) {
    addSelectableRegions(map,
      data = data,
      label = ~ htmltools::htmlEscape(label),
      ...)
  }

  addSelectableRegions <- function(map, data, ...) {
    # The labels are fussy
    #   Warning: Error in sum: invalid 'type' (list) of argument
    addPolygons(
      map,
      data = data,
      weight = 2,
      layerId = ~ id,
      group = ~ group,
      ...
    )
  }

  addPolutionRaster <- function(map, rasta, group, ...) {
    minmax <- c(cellStats(rasta, min), cellStats(rasta, max))
    palette = colorNumeric(palette = defraPalette, domain = c(0, 70))
    map %>%
      addRasterImage(rasta,
        group = "Pollution map",
        opacity = .4,
        col = palette) %>%
      addLegend("bottomright",
        pal = palette,
        values = minmax,
        group = "Pollution map")
  }

  output$map <- renderLeaflet({
    map = leaflet(options = leafletOptions(preferCanvas = T)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-1.61, 54.97, zoom = 13) %>%
      addDrawToolbar(
        targetGroup = 'draw',
        circleOptions = drawCircleOptions(),
        editOptions = editToolbarOptions()
      ) %>%
      addLayersControl(overlayGroups = c("Pollution map", "LSOAs")) %>%
      addPolutionRaster(pollution_brick[[1]]) %>%
      addLSOAs(combined_lsoas) %>%
      hideGroup(c("Pollution map", "LSOAs"))
    # addPolygons(data = roads, smoothFactor = 20)
  })

  ### Reactive/dynamic stuff ###

  # Persistent variables

  filtered = trips.export
  # Use an empty df of the right shape initially.
  regions = subset(combined_lsoas, F)
  drawn_shapes = regions

  # Observers

  # Update drawn_shapes
  observeEvent(input$map_draw_all_features, {
    if (length(input$map_draw_all_features$features) > 0) {
      drawn_shapes <<-
        read_sf(toJSON(
          input$map_draw_all_features,
          force = T,
          auto_unbox = T
        )) %>%
        rename(id = X_leaflet_id) %>% cbind(list(., group = "draw", label =
            .$id))
    }
  })

  # Attempt to fix double geometry problem with drawn features
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
    regions <<- switch(
      input$map_shape_click$group,
      "LSOAs" = subset(combined_lsoas, id == input$map_shape_click$id),
      "draw" = subset(drawn_shapes, id == input$map_shape_click$id),
    )

    # Highlight selected regions.
    leafletProxy("map") %>%
      addLSOAs(regions, fillColor = 'red')

    # Update everything else
    update(input, output, regions)
  })

  # Update everything on events
  observe({
    update(input, output, regions)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
