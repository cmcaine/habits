library(shiny)
library(leaflet)
library(leaflet.extras)
library(plotly)

variables = c('# Journeys', '% Journeys', 'Health impact (total)', 'Health impact (pollution)')
filters = c('mode', 'IMD', 'age', 'gender')

plot = plotlyOutput("plot", height="100%")
map = leafletOutput("map", height="100%")
inputWidgets = inputPanel(
  selectInput('variable', 'variable', variables),
  selectInput('filter', 'filter', filters),
  selectInput('split', 'split', filters)
)

updateOutputs = function(input, output, regions) {

  inner = function(variable, filter, split, regions, trips) {
    # Filter trips by filter and regions
    # Aggregate variable for each line for the hotroads
    # Split trips by split and plot
  }

  inner(input$variable, input$filter, input$split, regions, trips)
}


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
     leaflet() %>%
       addProviderTiles(provider = "CartoDB.Positron") %>%
       setView(lng = -2.24, 53.48, zoom = 13) %>%
       addDrawToolbar(targetGroup = 'draw', circleOptions = drawCircleOptions(), editOptions = editToolbarOptions())
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
   })
}

# Run the application
shinyApp(ui = ui, server = server)