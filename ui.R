library(leaflet)
library(googlesheets)

# Choices for drop-downs

shinyUI(navbarPage("Airbnb", id="airbnb",
           
           tabPanel("Interactive map",
                    div(class="outer",

                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      selectInput("city", "City", c('Boston'='Boston',
                                                                     'Chicago'='Chicago',
                                                                     'Washington'='DC',
                                                                     'Los Angeles'='LA',
                                                                     'New York'='NYC',
                                                                     'San Francisco'='SF')),
                                      selectInput("color", "Color", c('property_type'='property_type',
                                                                      'room_type'='room_type',
                                                                      'bed_type'='bed_type',
                                                                      'cancellation_policy'='cancellation_policy',
                                                                      'cleaning_fee'='cleaning_fee',
                                                                      'neighbourhood'='neighbourhood')),
                                      selectInput("size", "Size", c('log_price'='log_price',
                                                                    'number_of_reviews'='number_of_reviews',
                                                                    'review_scores_rating'='review_scores_rating'))
                            )
                        )
                    )
))