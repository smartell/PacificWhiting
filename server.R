#server.R
library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)
library(rMaps)
library(ggplot2)
library(dplyr)
library(rjson)

shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      addProviderTiles("Esri.OceanBaseMap") %>%
      # setView(lng = -93.85, lat = mean(A$Latitude), zoom = 4)
      fitBounds(bb.lon[1],bb.lat[1],bb.lon[2],bb.lat[2])
  })

  observeEvent(input$dateRange,{
  	val <- input$dateRange

    updateSliderInput(session,"sliderDay",
                      value=c(val[1],val[1]+7),
                      min=val[1],max=val[2])
  })

  # A reactive expression that returns the set of hauls that are
  # in bounds right now
  haulsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(hake.df[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    # subset(hake.df,
    #   Latitude >= latRng[1] & Latitude <= latRng[2] &
    #     Longitude >= lngRng[1] & Longitude <= lngRng[2])

    dates <- seq(input$sliderDay[1],input$sliderDay[2],by=1)
    
    hake.df %>% filter(as.Date(HaulDate) %in% dates) %>%
    	filter(Latitude >= latRng[1] & Latitude <= latRng[2] &
        Longitude >= lngRng[1] & Longitude <= lngRng[2] )
  })



  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy  <- input$size
   	hib <- haulsInBounds()

   	if (!dim(hib)[1]) return(NULL)

   	# Determine area of each circle.
   	tmp <- hib[[sizeBy]]
    radius <- sqrt(tmp / max(tmp)) * 10000
    
    #	Determine color of each circle.
    colorData <- hib[[colorBy]]
    pal <- colorNumeric("Spectral", colorData, na.color = "#808080", alpha = FALSE)
    #print(clr)
    
    # Cant' seem to get clearShapes to work.
    leafletProxy("map", data = hib) %>%
      clearShapes() %>%
      addCircles(~Longitude, ~Latitude, radius = ~radius,
                 stroke = FALSE, fillColor = pal(colorData)
                 ) 

  #   if (colorBy == "superzip") {
  #     # Color and palette are treated specially in the "superzip" case, because
  #     # the values are categorical instead of continuous.
  #     colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
  #     pal <- colorFactor("Spectral", colorData)
  #   } else {
  #     colorData <- zipdata[[colorBy]]
  #     pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
  #   }

  #   if (sizeBy == "superzip") {
  #     # Radius is treated specially in the "superzip" case.
  #     radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
  #   } else {
  #     radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
  #   }

  #   leafletProxy("map", data = zipdata) %>%
  #     clearShapes() %>%
  #     addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
  #       stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
  #     addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
  #       layerId="colorLegend")
  
  })


  output$plotCatch <- renderPlot({
  	icol <- match(input$color,names(hake.df))
  	
  	# Filter Data
  	heat_data <- hake.df %>% 
  		dplyr::filter(as.Date(HaulDate) >= input$sliderDay[1] & 
  		              as.Date(HaulDate) <= input$sliderDay[2]) %>%
  		dplyr::select(HaulDate,catch = icol, ManagementProgram) %>%
  		dplyr::group_by(HaulDate,ManagementProgram) %>% 
  		dplyr::summarise(Catch=sum(catch))
  		# print(heat_data)


  	ggplot(heat_data,aes(as.Date(HaulDate),Catch,fill=ManagementProgram)) + 
  				geom_bar(stat="identity",show.legend=FALSE) + guides(fill=FALSE) +
  				labs(x="Date",y="Catch (mt)") + 
  				xlim(c(input$sliderDay[1],input$sliderDay[2]))
  })

  output$plotBycatchRate <- renderPlot({
  	icol <- match(input$color,names(hake.df))
  	jcol <- match(input$size,names(hake.df))
  	
  	# Filter Data
  	heat_data <- hake.df %>% 
  		dplyr::filter(as.Date(HaulDate) >= input$sliderDay[1] & 
  		              as.Date(HaulDate) <= input$sliderDay[2]) %>%
  		dplyr::select(HaulDate,catch = WhitingWt, bycatch = jcol, ManagementProgram) %>%
  		dplyr::group_by(HaulDate,ManagementProgram) %>% 
  		dplyr::summarise(Rate=sum(bycatch/catch*1000))
  		# print(heat_data)


  	ggplot(heat_data,aes(as.Date(HaulDate),Rate,color=ManagementProgram)) + 
  				# geom_bar(stat="identity",fill="red") + 
  				geom_line(show.legend=FALSE,size=4) +
  				labs(x="Date",y="Bycatch Rate (kg/mt)") + 
  				xlim(c(input$sliderDay[1],input$sliderDay[2]))
  })

  output$plotHaulCount <- renderPlot({
  	heat_data <- hake.df %>% 
  		dplyr::filter(as.Date(HaulDate) >= input$sliderDay[1] & 
  		              as.Date(HaulDate) <= input$sliderDay[2]) %>%
  		              # ManagementProgram %in% .COOPS) %>%
  		dplyr::group_by(latGroup,ManagementProgram) %>%
  		dplyr::summarise(count=n())


  	 ggplot(heat_data,aes(latGroup,count,fill=ManagementProgram)) + 
  	 geom_bar(stat="identity",position="dodge") + coord_flip() + 
  	 labs(x="Latitude",y="No. of Hauls",fill="Program") + 
  	 guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  	 theme(legend.position="top",
  	       legend.text = element_text(size=rel(0.7)),
  	       legend.title= element_text(size=rel(0.7)))
  })












  output$baseMap <- renderMap({
    baseMap <- Leaflet$new() 
    baseMap$setView(c(45.02055,-124.22209) ,7) 
    baseMap$tileLayer(provider="Esri.OceanBasemap")
    baseMap 
  })

  output$heatMap <-renderUI({

  	# print(hake.df[[input$color]])
  	icol <- match(input$color,names(hake.df))
  	# DATES <<- input$sliderDay
  	heat_data <- hake.df %>% 
  		dplyr::filter(as.Date(HaulDate) >= input$sliderDay[1] & 
  		              as.Date(HaulDate) <= input$sliderDay[2]) %>%
  		dplyr::select(lat = Latitude,
  		              lon = Longitude,
  		              density = icol) %>%
  		transform(density=density/mean(density))

  		# print(heat_data)
  		# Convert data to JSON for plotting in heatmaps
  		j <- toJSONArray2(as.data.frame(heat_data),json=FALSE,names=FALSE)
			# dmax <- max(heat_data$density)
  		# dataJSON <- rjson::toJSON(toJSONArray2(as.data.frame(heat_data),json=FALSE,names=FALSE))
  		# print(j)
  		# Run some java script to add heat layer.
  		tags$body(tags$script(HTML(sprintf("
      var addressPoints = %s
      if (typeof heat === typeof undefined) {
                  heat = L.heatLayer(addressPoints, {
                    maxZoom: 9, 
                    radius: 10, 
                    blur: 15})
                  heat.addTo(map)
                } else {
                  heat.setOptions({maxZoom: 9, radius: 10, blur: 10})
                  heat.setLatLngs(addressPoints)
                }
       </script>",rjson::toJSON(j)
    ))))

  })

})