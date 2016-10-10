#ui.R
library(shiny)
library(leaflet)
library(rMaps)
library(rCharts)
library(rjson)

vars <- c(
  "Whiting" = "WhitingWt",
  "Pacific Ocean Perch" = "PacificOceanPerchWt",
  "Widow Rockfish"  = "WidowRockfishWt",
  "Dark Blotched Rockfish" = "DarkBlotchedRockfishWt",
  "Chinook Salmon No." = "ChinookSalmonNo"
  )


alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
    style="margin-left:auto;margin-right:auto;"
  )
}


shinyUI(navbarPage("Whiting", id="nav",







	tabPanel("Heat map",
	  div(class="outer",
		  tags$head(
		    # Include our custom CSS
		    # includeCSS("styles.css"),
		    includeCSS("www/custom.css")
		    # includeScript("gomap.js")
		  ),

			showOutput("baseMap", "leaflet"),
	    tags$style('.leaflet {height: 100%; width: 100%;}'),
	    tags$head(tags$script(src="http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js")),
	    uiOutput('heatMap'),


	    # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 550, height = "auto",

        h3("Pacific Whiting Fishery"),
        # h3(textOutput("caption")),

        # Date Range Input
        alignCenter(
        dateRangeInput("dateRange","Date Range",
                       start="2016-05-15",
                       min=min(as.Date(hake.df$HaulDate)),
                       max=max(as.Date(hake.df$HaulDate)),
                       startview="year"
        )),
        # Slider for selecting dates
        alignCenter(
        sliderInput("sliderDay","Day",
                    min=as.Date("2016-05-15"),
                    max=max(as.Date(hake.df$HaulDate)),
                    value=as.Date(c("2016-05-15","2016-05-22")),
                    step=1, width=380,
                    animate=animationOptions(interval=1800,loop=TRUE)
        )),

        plotOutput("plotHaulCount",height=250),

        fluidRow(
        column(6,
          selectInput("color", "Catch (heat map)",width=200, vars, selected = vars[1])
        ),
        column(6,
          selectInput("size", "Bycatch rate",width=200, vars, selected = vars[2])
        )),



        # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
        #   # Only prompt for threshold when coloring or sizing by superzip
        #   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        # ),


        plotOutput("plotCatch", height = 200),
        plotOutput("plotBycatchRate", height = 200)
      )



	    # absolutePanel(id = "heatmapcontrols", class = "panel panel-default",
	    # 	fixed = TRUE, draggable = TRUE,
	    # 	top = 80, left = "auto", right = 80, bottom = "auto",
	    # 	width = 400 , height = "auto",

	    # 	h4("Heat map controls")
	    # )
		)
	) # end of Heat map tabPanel



	# tabPanel("Interactive map",
 #    div(class="outer",


 #      leafletOutput("map", width="100%", height="100%"),

 #      # # Shiny versions prior to 0.11 should use class="modal" instead.
 #      # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
 #      #   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
 #      #   width = 330, height = "auto",

 #      #   h2("Haul explorer"),

 #      #   # Date Range Input
 #      #   dateRangeInput("dateRange","Date Range",
 #      #                  start="2016-05-15",
 #      #                  min=min(as.Date(hake.df$HaulDate)),
 #      #                  max=max(as.Date(hake.df$HaulDate)),
 #      #                  startview="year"),
 #      #   # Slider for selecting dates
 #      #   animationOptions(interval = 1, loop = FALSE, 
 #      #                    playButton = NULL, pauseButton = NULL),
 #      #   sliderInput("sliderDay","Day",
 #      #               min=as.Date("2016-05-15"),
 #      #               max=max(as.Date(hake.df$HaulDate)),
 #      #               value=as.Date(c("2016-05-15","2016-05-22")),
 #      #               step=1,
 #      #               animate=TRUE),



 #      #   selectInput("color", "Color", vars, selected = vars[1]),
 #      #   selectInput("size", "Size", vars, selected = vars[2]),
 #      #   conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
 #      #     # Only prompt for threshold when coloring or sizing by superzip
 #      #     numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
 #      #   ),

 #      #   plotOutput("histCentile", height = 200),
 #      #   plotOutput("scatterCollegeIncome", height = 250)
 #      # ),

 #      tags$div(id="cite",
 #        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
 #      )
 #    )
 #  )  # end of Interactive map tabPanel



))
# ---------------------------------------------------------------------------- #
#	END OF UI
# ---------------------------------------------------------------------------- #

















