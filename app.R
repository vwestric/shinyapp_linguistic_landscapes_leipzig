#lingustic landscapes of leipzig
# Leaflet mapping exercise in R
# ESU DH 2018 Humanities Data and Mapping Environments
# adapted from https://trendct.org/2015/06/26/tutorial-how-to-put-dots-on-a-leaflet-map-with-r/

# https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
# https://github.com/geocomPP/sdv
# https://github.com/Robinlovelace/Creating-maps-in-R
# https://rstudio.github.io/leaflet/showhide.html

#R Shiny tutorial:
#https://rstudio.github.io/leaflet/

#Loading in the necessary libraries
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(shiny)
library(RColorBrewer)
library(sp)
library(htmltools)
library(shinyBS)

#Data
#Shapefiles for the Leipzig districts (leipzig_stadtteile) and sub districts (leipzig_ortsteile) overlays
leipzig_stadtteile <- readOGR('leipzig_shapefile_stadtteile/sbz.shp')
leipzig_ortsteile <- readOGR('leipzig_shapefile_ortsteile/ot.shp')

#Transforming the shapefiles into the right CRS
leipzig_stadtteile <- spTransform(leipzig_stadtteile, CRS("+init=epsg:4326"))
leipzig_ortsteile <- spTransform(leipzig_ortsteile, CRS("+init=epsg:4326"))

#The data from the mapping workshop
mydata <- read.csv("LLB17.csv", stringsAsFactors=FALSE)

#Creating a list of all unique occurrences of language and language combinations
language_choices <- data.frame(unique(mydata$name))
colnames(language_choices) <- c("name")
#Adding string 'Input' to language_choices list as default option
language_choices <- rbind(language_choices,data.frame(name = "Input"))


#Creating a list of choices for switching between districts and sub districts
choices <- c("City districts", "Sub districts")
overlay_choices <- data.frame(choices)


#user interface(ui)
#bootstrapPage serves here as what body would be in html, roughly
ui <- bootstrapPage(
  #defining the style of the page
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  #Loading in the map first, everything else will show up in front of it
  leafletOutput("map", width="100%", height = "100%"),
  #absolutePanel in the top right corner, contains interface options and the button to revoke the info box
  absolutePanel(top = 10, right = 10,
                #Choosing a language or language combination to display
                selectInput("text", label = h3("Choose a language"), 
                            choices = language_choices, 
                            selected = "Input"),
                #Choosing a overlay, districts or sub districts, to display (Default is city districts)
                selectInput("overlay", label = h3("Choose an overlay"), 
                            choices = overlay_choices, 
                            selected = "City districts"),
                #Button to revoke the info box when clicked
                actionButton("info", "Info")
                
  ),
  #absolutePanel on the bottom left to display the 'title' (See inside server under output$title)
  absolutePanel(bottom = 10, left = 10,
                #actual text output of 'title'
                textOutput('title'),
                #style of the 'title' text
                tags$head(tags$style('#title{font-size: 30px;
                                     font-style: bold
                                     }')
                )
                
                ),
  #absolutePanel to display the info box
  absolutePanel(
    #bsModal allows to only display the box when the button 'info' is pressed
    #Careful: Both the button and the htmlOutput are called info, but are not the same thing
    bsModal("modalExample", h3("About 'Linguistic landscapes of Leipzig'"), "info", size = "large",
            htmlOutput('info'))
  )
  )

#server
server <- function(input, output, session) {
  
  #Output text of the 'title' variable
  output$title <- renderText('Linguistic landscapes of Leipzig, ESU 2018')
  
  #Output text of the 'info' variable
  output$info <- renderText("This project is a result of the <a href='http://www.culingtec.uni-leipzig.de/ESU_C_T/'>European Summer University Leipzig</a> 2017 and 2018.
                             The data was collected by students of the course 'Humanities Data and Mapping Enviroments.' It aims to show the occurrences of non-german languages in Leipzig.
                             This language may present itself on stickers, billboards, or any other kind of writing in the public sphere. <br><br>

                             The overlays show the distribution of citizens with immigration background in Leipzig, 
                             on district ('Stadtteil') and sub district ('Ortsteil') level.") #lapply(htmltools::HTML)
  
  #This is the function to filter the points based on the user interface choices
  filteredPoints <- reactive({
    #If input$text equals 'Input'
    if (input$text == 'Input') {
      #Display complete dataset (Default)
      data <- mydata
    }
    #Else if input$text doesn't equal 'Input'
    else if (input$text != 'Input') {
      #Subset the to be displayed data by only showing the rows of the data frame which contain the string
      #from input$text in their respective name column (mydata$name), for example:
      #If input$text is set to 'English', display all rows, which in their name column also feature the string 'English'
      data <- mydata[ grep(input$text, mydata$name),]
    }
  })
  
  #Function to choose betwenn the overlays
  filteredPoly <- reactive({
    #If input$overlay equals 'City districts'
    if (input$overlay == 'City districts') {
      #Display district overlay (Default)
      data <- leipzig_stadtteile
    }
    #If input$overlay equals 'Sub districts'
    else if (input$overlay == 'Sub districts') {
      #Display sub district overlay
      data <- leipzig_ortsteile
    }
  })
  
  
  #Creating the actual map to be displayed
  output$map <- renderLeaflet({
    
    #Bins for the choropleth map
    bins <- c(0, 5, 10, 15, 20, 25, 30, 40, Inf)
    #Colour palette for the choropleth map
    pal <- colorBin("YlOrRd", domain = filteredPoly()$MIGRANTEN, bins = bins)
    
    #Creating the labels to be displayed in the marker popups in HTML
    labels <- sprintf(
      #Text between <strong></strong> is displayed as bold
      #Variable %s = content of the name column, variable %g content of the migranten column (in the overlays)
      "<strong>%s,%g</strong>",
      filteredPoly()$NAME, filteredPoly()$MIGRANTEN
    ) %>% lapply(htmltools::HTML)
    
    #The leaflet map
    #'%>%' is called a magrittr operator. It is necessary after every element of the map, except the last one, in this case, addLegend.
    #Short version: The magrittr operator allows you to 'chain' all the listed commands to the original 'map' output. 
    #For the long version, google it.
    leaflet(leipzig_stadtteile) %>%
      #Default leaflet tiles, = OpenStreetMap
      addTiles() %>%
      #Center the map, in this case on Leipzig
      setView (12.3748542,51.3346351, zoom=12) %>%
      #AddLayersControl to switch overlays and markers on and off from the display
      addLayersControl(overlayGroups = c("Marker", "Citizens with immigration background, percentage"), position = "bottomright") %>%
      #Add markers from the filteredPoints() function, clusterOptions creates the displayed cluster
      addAwesomeMarkers(data=filteredPoints(),~lon, ~lat, popup=filteredPoints()$name, clusterOptions = markerClusterOptions()) %>%
      #Add polygons from the filteredPoly() function and turn them into choropleths
      addPolygons(data=filteredPoly(),
      #Fill with colours from the colour palette            
      fillColor = ~pal(MIGRANTEN),
      #Display options
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      #Name in the LayersControl
      group = "Citizens with immigration background",
      #Labels to be displayed by hovering over/clicking on the polygons
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
      #Adding a legend on the bottomright to explain the choropleth cholors
      addLegend(pal = pal, values = filteredPoly()$MIGRANTEN, opacity = 0.7, title = NULL,
              position = "bottomright")
    
  })
  
  
  
}

#Invoking the shiny application
shinyApp(ui = ui, server = server)