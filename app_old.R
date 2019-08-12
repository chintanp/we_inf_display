#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# All the includes
library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(htmltools)
library(Hmisc)
library(RColorBrewer)
library(shinythemes)
library(readr)
library(leaflet.extras)
library(curl)
library(mapdeck)

# afdc_api_key <- Sys.getenv("AFDC_API_KEY")
# Read the EVSE information through the AFDC API
afdc_url  <-
  paste0(
    "https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?fuel_type=ELEC&state=WA&ev_charging_level=dc_fast&status=E&access=public&api_key=",
    Sys.getenv('AFDC_API_KEY')
  )
evse_dcfc <- read_csv(afdc_url)

# Convert the connector type to code for easy parsing in GAMA
# CHADEMO only - 1
# J1772COMBO only - 2
# CHADEMO and J1772COMBO - 3
# TESLA - 4
# Ignore J1772 as it is level-2
for (i in 1:nrow(evse_dcfc)) {
  conns <- evse_dcfc$`EV Connector Types`[i]
  if (grepl("CHADEMO", conns)) {
    if (grepl("J1772COMBO", conns)) {
      evse_dcfc$EV_Connector_Code[i] <- 3
    } else {
      evse_dcfc$EV_Connector_Code[i] <- 1
    }
  } else if (grepl("J1772COMBO", conns)) {
    evse_dcfc$EV_Connector_Code[i] <- 2
  } else if (grepl("TESLA", conns)) {
    evse_dcfc$EV_Connector_Code[i] <- 4
  }
}

all_chargers_combo <-
  evse_dcfc[evse_dcfc$EV_Connector_Code == 2 |
              evse_dcfc$EV_Connector_Code == 3, ]

all_chargers_chademo <-
  evse_dcfc[evse_dcfc$EV_Connector_Code == 1 |
              evse_dcfc$EV_Connector_Code == 3, ]

shape_trip_feasibility_combo <-
  readOGR(dsn = "data/combo", layer = "shape_combo")
shape_trip_feasibility_chademo <-
  readOGR(dsn = "data/chademo", layer = "shape_chademo")

# shape_feasibility <-
#     readOGR(dsn = "data/feasibility_overlay", layer = "road_network_weighted2019-03-29-10-13-38")
# Transform to the correct CRS
shape_trip_feasibility_combo_ll <-
  spTransform(shape_trip_feasibility_combo,
              CRS("+proj=longlat +datum=WGS84 +no_defs"))

shape_trip_feasibility_combo_prj <-
  spTransform(shape_trip_feasibility_combo,
              CRS("+proj=utm +zone=10 +datum=WGS84"))

shape_trip_feasibility_chademo_ll <-
  spTransform(shape_trip_feasibility_chademo,
              CRS("+proj=longlat +datum=WGS84 +no_defs"))

shape_trip_feasibility_chademo_prj <-
  spTransform(shape_trip_feasibility_chademo,
              CRS("+proj=utm +zone=10 +datum=WGS84"))

base_layers <- c("Combo", "CHAdeMO")
base_tile_layers <- c("MapBox Light", "OSM (default)")

combo_icons <-
  icons(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/7/7d/Symbol_electric_vehicle_charging_stations.jpg",
    iconWidth = 10,
    iconHeight = 10,
    iconAnchorX = 0,
    iconAnchorY = 0
  )

# Define UI for application that draws a histogram
ui <-  navbarPage(
    theme = shinytheme("flatly"),
    "WSDOT EVSE Dashboard",
    id = "nav",
    
    tabPanel(
        "Interactive map",
        div(
            class = "outer",
            
            tags$head(# Include our custom CSS
                includeCSS("styles.css")),
            
            # If not using custom CSS, set height of leafletOutput to a number instead of percent
            leafletOutput("map", height = "100%"),
            
            tags$div(
                id = "cite",
                'Data compiled for ',
                tags$em('WSDOT EVSE Project'),
                ' by Chintan Pathak and Don MacKenzie'
            )
        )
    ),
    
    tabPanel("Explanation",
             div(
                 h2("Description of Various Overlays")
             ))
)


server <- function(input, output, session) {

    # Create the map
    output$map <- renderLeaflet({
        leaflet() %>%
            # Base groups
            addTiles(group = base_tile_layers[2]) %>%
            addProviderTiles(
                "MapBox",
                options = providerTileOptions(
                    id = "mapbox.light",
                    noWrap = FALSE,
                    accessToken = Sys.getenv("MAPBOX_ACCESS_TOKEN")
                )
            )  %>%
            # Overlay groups
        addPolylines(
          data = shape_trip_feasibility_combo_ll,
          weight = shape_trip_feasibility_combo_ll$trip_count / 20000,
          color = "#750db5",
          group = base_layers[1],
          opacity = 1,
          label = paste0(
            "trip_count : ",
            shape_trip_feasibility_combo_ll$trip_count
          ),
          popup = paste0(
            "trip_count : ",
            shape_trip_feasibility_combo_ll$trip_count
          )
        ) %>%
        addPolylines(
          data = shape_trip_feasibility_chademo_ll,
          weight = shape_trip_feasibility_chademo_ll$trip_count / 20000,
          color = "#0d4db5",
          group = base_layers[2],
          opacity = 1,
          label = paste0(
            "trip_count : ",
            shape_trip_feasibility_chademo_ll$trip_count
          ),
          popup = paste0(
            "trip_count : ",
            shape_trip_feasibility_chademo_ll$trip_count
          )
        ) %>%
        addMarkers(
          lng = all_chargers_combo$Longitude ,
          lat = all_chargers_combo$Latitude,
          popup = paste0("ID : ", all_chargers_combo$ID),
          label = paste0("ID : ", all_chargers_combo$ID),
          icon = combo_icons,
          group = base_layers[1]
        )  %>%
        addMarkers(
          lng = all_chargers_chademo$Longitude ,
          lat = all_chargers_chademo$Latitude,
          popup = paste0("ID : ", all_chargers_chademo$ID),
          label = paste0("ID : ", all_chargers_chademo$ID),
          icon = combo_icons,
          group = base_layers[2]
        ) %>%
        addLayersControl(
          baseGroups = base_layers,
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addResetMapButton() %>%
        addSearchOSM() 
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
