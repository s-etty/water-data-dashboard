#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# GENERAL PACKAGES
library(dplyr)
library(purrr)
library(janitor)
library(glue)

# SPATIAL PACKAGES
library(sf)
library(mapview)
library(leaflet)

library(tigris)
library(units)
library(dataRetrieval)
library(tmap)
library(tmaptools)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$usMap <- renderLeaflet({
      us_states <- states() |>
        st_transform(4326)
      
      watershed_boundary <- st_read("data/WBD_National_GDB.gdb")
      
      leaflet() %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 3) |>
        addTiles() %>%
        addPolygons(data = us_states,
                    color = "steelblue",
                    fillColor = "grey",
                    fillOpacity = 0.3,
                    weight = 1)

    })

}
