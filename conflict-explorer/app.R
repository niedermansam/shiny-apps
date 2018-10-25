#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)

conflict <- read_csv("conflicts.csv") %>% as.tibble()

conflict$label <- sprintf("<p style='text-align:center'>%s %s %s %s %s %s %s</p>",
                          paste0(format(as.Date(conflict$date_start), "%b %d %Y"),"<br/>"),
                          paste0("<strong>",conflict$dyad_name,"</strong><br/>"),
                          paste0("<strong>Total Fatalities: ", conflict$deaths_total, " Dead</strong><br/><br/>"),
                          paste0(conflict$side_a,": ", conflict$deaths_a, "<br/>"),
                          paste0(conflict$side_b,": ", conflict$deaths_b, "<br/>"),
                          paste0("Civilians: ", conflict$deaths_civilians, "<br/>"),
                          paste0("Unknown: ", conflict$deaths_unknown,"<br/><br/>")) %>%
  lapply(htmltools::HTML)

actors <- c(conflict$side_a %>% as.factor() %>% levels(),
            conflict$side_b %>% as.factor() %>% levels()) %>% unique()

country <- conflict$country %>% as.factor() %>% levels() %>% unique()

region <- conflict$region %>% as.factor() %>% levels() %>% unique()

# CREATE USER INTERFACE #############################################

# header board (the blue bar at the top)
header <- dashboardHeader(title = "Conflict Explorer")

# Side bar board (Where the filters are)
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    # Create Sidebar Tabs
    id = 'menu_tabs', 
    menuItem('Interactive Map', tabName = 'map') 
  ),
  
  
  sliderInput(inputId = "year", 
              label = "Conflict Year:", 
              min = 1989, max = 2017, 
              value = 2017, step = 1,
              sep = ""),
  
  numericInput(inputId = "deaths", 
              label = "Minimum Total Fatalities:", 
              min = 0, max = 300559, 
              value = 0, step = 1),
  
  selectInput(inputId = "actor", label = "Filter by Actor:", choices = c("No Filter" = ".", actors)),
  
  selectInput(inputId = "country", label = "Filter by Country:", choices = c("No Filter" = ".", country)),
  
  selectInput(inputId = "region", label = "Filter by Region:", choices = c("No Filter" = ".", region)),
  
  # Labels on Click or Hover?
  radioButtons("labels","Popups:",
               list("On Click" = "popup","On Hover" = "label"), inline = T
  ),
  
  # Select Base Map
  radioButtons("base_map", "Base Map:",
               c("ESRI World Street Map" = "Esri.WorldStreetMap",
                 "Stamen Terrain" = "Stamen.Terrain",
                 "Open Street Map" = "OpenStreetMap",
                 "Open Topo Map" = "OpenTopoMap"),inline = T)
  
)

# Body Board Object (what is displayed in panel)
body <- dashboardBody(
  
  # Create Multiple pages based on tab selected
  tabItems(
    # Create Body for "Interactive Map" Tab
    tabItem(
      tabName = 'map', # from output$map in shiny server
      fluidRow(column(12,leafletOutput('map', height=1000)))
      )
    )
  )


# Put it all together
ui <- dashboardPage(
  title = 'Conflict Explorer', # This goes in the browser tab
  header, # Blue Header Bar
  sidebar, # Tabs/Filters
  body # Display Content
)


# START SERVER #############################################


server <- function(input, output, session) { 
  
  
  
  # Create Leaflet Object #####
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 0, lat = 20, zoom = 2)
  })
  
  
  
  
  # Find Objects in View ####
  inBounds <- reactive({
    
    conflict_year <- input$year
    
    min_deaths <- input$deaths
    
    actor.sel <- input$actor
    region.sel <- input$region
    country.sel <- input$country
    
    subset(conflict, 
             year == conflict_year & deaths_total >= min_deaths & 
             str_detect(dyad_name, actor.sel) & str_detect(region, region.sel) &
             str_detect(country, country.sel))
  })
  
  
  
  
  # Populate Leaflet Object ####
  observe({
    
    sites <- inBounds() # select conflict in bounds
    label_type <- input$labels # select label type (hover or click)
    base_map <- input$base_map # select base map
    
    
    
    # Create labels ####
    # make map with popups on click
    if(label_type == "popup"){
      leafletProxy("map") %>% 
        clearMarkers() %>% clearMarkerClusters() %>%
        addProviderTiles(base_map) %>%
        addMarkers(lng = sites$longitude,
                   lat = sites$latitude,
                   popup = sites$label,
                   clusterOptions = markerClusterOptions())
    }
    
    # make map with labels on hover
    if(label_type == "label"){
      leafletProxy("map") %>% 
        clearMarkers() %>% clearMarkerClusters() %>%
        clearTiles() %>%
        addProviderTiles(base_map) %>%
        addMarkers(lng = sites$longitude,
                   lat = sites$latitude,
                   label = sites$label,
                   clusterOptions = markerClusterOptions())
    }
  })
  
  
  }


#  RUN APP
shinyApp(ui,server)

