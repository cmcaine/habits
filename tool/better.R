# This whole chain is more efficiently expressed as a bunch of reactive expressions

update <- function(input, output, regions) {
	#
	print("Updating")
	# Filter trips.export to match input$filter
	trips = filterTrips(input$filter, regions)
	updatePlot(output$plot, input$variable, trips, ~(factor(startWeek)), "Start week")
	updatePlot(output$before_after_plot, input$variable, trips, substitute(~(startDate < input$intervention_date)), "Before intervention")
	updateMap("map", input$variable, trips)
}

updatePlot <- function(outPlot, variable, trips, xaxis, xlabel) {
	# Prepare a ggplot according to variable and xaxis
	  if (nrow(trips) < 1) {
	    trips = trips.export
	  }

	  if (nrow(trips) == nrow(trips.export)) {
	    title = "(All data)"
	  } else {
	    title = "(Region)"
	  }

	plot = switch(input$variable,
	       'MET-h' = (ggplot(trips, aes_(x = xaxis, y = ~MET, color = ~modality))
				 + scale_y_log10()
				 + geom_violin(data = subset(trips, MET > 0.1))
				 ),
		'# Journeys' = (ggplot(trips, aes_(x = xaxis, fill = ~modality))
				+ geom_histogram(stat = "count")),
		'% Journeys' = (ggplot(trips, aes_(x = xaxis, fill = ~modality))
				+ geom_histogram(stat = "count", position = "fill"))
		)

	plot = plot + xlab(xlabel) + ylab(input$variable) + ggtitle(paste(plotMeta[variable, 'title'], title))

	# Render it to outPlot
	outPlot = renderPlot(plot)
}

plotMeta = data.frame(
	variable = c('MET-h', '# Journeys', '% Journeys'),
	title = c(
		  "Change in MET-h per mode over time",
		  "Number of Journeys by mode per week",
		  "Proportion of Journeys by mode per week"
		  ),
	plot = c(
		 function(xaxis) {
	row.names = 'variable'
	)


# NB: this can be a reactive value to avoid recomputation
filterTrips <- function(input$filter, regions) {
	# Return a subset of trips.export
	if (input$filter == "region") {
		st_intersection(trips.export, regions)
	} else {
		print("Unknown filter!")
	}
}

updateMap <- function(mapref, variable, trips) {
	# Redraw routes, coloured by $variable
	# Highlight selected regions TODO
	leafletProxy(mapref, data=trips) %>%
		clearGroups('routes') %>%
		addPolylines(color=factpal(routes$modality), label=routes$modality, group = 'routes')
}

observeEvent(input$map_click, {
		     # Update regions
		     latlng <- reactiveVal(value=input$map_click)()
		     pt <- sf::st_point(c(latlng$lng, latlng$lat))
		     regions <<- st_intersection(combined_lsoas, pt)
		     update(input, output, regions)
})

observeEvent(input$variable, {
		     update(input, output, regions)
})

observe({update(input, output, regions)})
