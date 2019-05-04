library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

gap_ss <- gs_url('https://docs.google.com/spreadsheets/d/1mrKG5vcOZWGlsiIxc_S_D_ZyWWuPlNXfyo4hFfFJm_g/edit#gid=697789776')
airbnb1 <- gs_read_csv(gap_ss)

sixcity <- aggregate(cbind(longitude,latitude)~city,data=airbnb1,FUN=mean)

shinyServer(function(input, output, session) {
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng=sixcity[sixcity$city==input$city,2],lat=sixcity[sixcity$city==input$city,3],zoom=10) %>% addTiles() %>% 
            addCircles(lng=airbnb1$longitude,lat=airbnb1$latitude,weight=airbnb1[[input$size]]/20,color=substr(rainbow(nlevels(as.factor(airbnb1[[input$color]])))[as.numeric(as.factor(airbnb1[[input$color]]))],1,7),fill=TRUE)
    })
    
   
    
    
    # This observer is responsible for maintaining the circles and legend,
    # according to the variables the user has chosen to map to color and size.
    # observe({
    #     colorBy <- input$color
    #     sizeBy <- input$size
    #     
    #     if (colorBy == "superzip") {
    #         # Color and palette are treated specially in the "superzip" case, because
    #         # the values are categorical instead of continuous.
    #         colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
    #         pal <- colorFactor("viridis", colorData)
    #     } else {
    #         colorData <- zipdata[[colorBy]]
    #         pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    #     }
    #     
    #     if (sizeBy == "superzip") {
    #         # Radius is treated specially in the "superzip" case.
    #         radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    #     } else {
    #         radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    #     }
    #     
    #     leafletProxy("map", data = zipdata) %>%
    #         clearShapes() %>%
    #         addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
    #                    stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    #         addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
    #                   layerId="colorLegend")
    # })
    # 
    # # Show a popup at the given location
    # showZipcodePopup <- function(zipcode, lat, lng) {
    #     selectedZip <- allzips[allzips$zipcode == zipcode,]
    #     content <- as.character(tagList(
    #         tags$h4("Score:", as.integer(selectedZip$centile)),
    #         tags$strong(HTML(sprintf("%s, %s %s",
    #                                  selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
    #         ))), tags$br(),
    #         sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
    #         sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
    #         sprintf("Adult population: %s", selectedZip$adultpop)
    #     ))
    #     leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
    # }
    # 
    # # When map is clicked, show a popup with city info
    # observe({
    #     leafletProxy("map") %>% clearPopups()
    #     event <- input$map_shape_click
    #     if (is.null(event))
    #         return()
    #     
    #     isolate({
    #         showZipcodePopup(event$id, event$lat, event$lng)
    #     })
    # })
    # 
    # 
    # ## Data Explorer ###########################################
    # 
    # observe({
    #     cities <- if (is.null(input$states)) character(0) else {
    #         filter(cleantable, State %in% input$states) %>%
    #             `$`('City') %>%
    #             unique() %>%
    #             sort()
    #     }
    #     stillSelected <- isolate(input$cities[input$cities %in% cities])
    #     updateSelectInput(session, "cities", choices = cities,
    #                       selected = stillSelected)
    # })
    # 
    # observe({
    #     zipcodes <- if (is.null(input$states)) character(0) else {
    #         cleantable %>%
    #             filter(State %in% input$states,
    #                    is.null(input$cities) | City %in% input$cities) %>%
    #             `$`('Zipcode') %>%
    #             unique() %>%
    #             sort()
    #     }
    #     stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    #     updateSelectInput(session, "zipcodes", choices = zipcodes,
    #                       selected = stillSelected)
    # })
    # 
    # observe({
    #     if (is.null(input$goto))
    #         return()
    #     isolate({
    #         map <- leafletProxy("map")
    #         map %>% clearPopups()
    #         dist <- 0.5
    #         zip <- input$goto$zip
    #         lat <- input$goto$lat
    #         lng <- input$goto$lng
    #         showZipcodePopup(zip, lat, lng)
    #         map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    #     })
    # })
    # 
    # output$ziptable <- DT::renderDataTable({
    #     df <- cleantable %>%
    #         filter(
    #             Score >= input$minScore,
    #             Score <= input$maxScore,
    #             is.null(input$states) | State %in% input$states,
    #             is.null(input$cities) | City %in% input$cities,
    #             is.null(input$zipcodes) | Zipcode %in% input$zipcodes
    #         ) %>%
    #         mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #     action <- DT::dataTableAjax(session, df)
    #     
    #     DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    # })
})