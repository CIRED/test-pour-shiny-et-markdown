# Load packages ----
{library(shiny)
library(maps)
library(mapproj)
library(stats)
library(sf)
library(readxl)
library(stringr)
library(mapview) 
library(ggmap)
library(grid)
library(gridExtra)
library(plm)
#library(OpenStreetMap)
library(rgdal)
library("ggspatial")
library("ggrepel")
#library(raster)
library(tidyverse)
library(mapview) 
library(grid)
library(gridExtra)
library(openxlsx)
library(corrplot)
library(dplyr)
library(raster)
library(kableExtra)
library(RColorBrewer)
library(leaflet)}

# Load data ----
Retention_Eau_Bassin_Versant<-st_read("./data/Retention_Eau_Bassin_Versant/Retention_m3_WS.shp")

difference_entre_all_scenarios <- Retention_Eau_Bassin_Versant %>% 
    pivot_longer(-c(1:11,28,35),names_to = "scenario",values_to = "value")  %>% 
    mutate(diff_with_2017=(value-Retentio16)/Retentio16*100) %>%  #je crois que 2017 c'est Retentio16 ?
    st_as_sf(., sf_column_name = "geometry") 

difference_entre_all_scenarios_wgs84 <- st_transform(difference_entre_all_scenarios, 4326)

# Create a continuous palette function
pal <- colorNumeric(
   palette='RdBu',
    domain = max(abs(difference_entre_all_scenarios_wgs84$diff_with_2017),na.rm = TRUE) * c(-1, 1),)

# labels <- sprintf(
#   "<strong>%s</strong><br/>%g %%",
#   difference_entre_all_scenarios_wgs84$nom_riv, difference_entre_all_scenarios_wgs84$diff_with_2017
# ) %>% lapply(htmltools::HTML)

labels <- sprintf(
  "%g %%",
  difference_entre_all_scenarios_wgs84$diff_with_2017
) %>% lapply(htmltools::HTML)

# Source helper functions -----
source("helpers.R")


# User interface ----
ui <- fluidPage(
    titlePanel("Service de recharge des nappes"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Choix du scénario"),
            
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = unique(difference_entre_all_scenarios$scenario)),
        
        ),
        
        #mainPanel(plotOutput("map"))
        mainPanel(leafletOutput("mymap"))
    )
)

# Server logic ----
server <- function(input, output) {
    # output$map <- renderPlot({map_idefese(input$var,difference_entre_all_scenarios)})
    
    output$mymap <- renderLeaflet({
        leaflet(difference_entre_all_scenarios_wgs84) %>%
        addPolygons(fill=FALSE) %>% 
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) 
    })
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
      
      leafletProxy("mymap", 
                   data = difference_entre_all_scenarios_wgs84 
                   %>% filter(scenario==input$var)) %>%
        clearShapes() %>%
        addPolygons(smoothFactor = 0.2,
          fillColor = ~pal(diff_with_2017),
          color = "#666",
          weight = 0.4,
          dashArray = "3",
          fillOpacity = 0.5,
          highlight = highlightOptions(
            weight = 1,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
    })
    observe({
      leafletProxy("mymap", 
                   data = difference_entre_all_scenarios_wgs84 %>% 
                     filter(scenario==input$var)) %>%
        clearControls() %>% 
      addLegend("bottomright", pal = pal, values = ~diff_with_2017,
                title = "Variation compared to 2017",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1)
    })
}

# Run app ----
shinyApp(ui, server)