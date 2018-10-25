#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(ggplot2)
library(RCurl)
library(tidyverse)
library(leaflet)
library(shinydashboard)

# header board
header <- dashboardHeader(
  title = "Intentional Community Explorer"
  # task list for status of data processing
  , dropdownMenuOutput('task_menu'))

# Side bar boardy
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Communities', tabName = 'map') 
  ),
  selectInput("com_type","Community Type:",list("No Filter" = ".", 
                                                "Ecovillage",
                                                "Cohousing" = "Cohousing|Cohouseholding",
                                                "Coliving",
                                                "Commune",
                                                "Community Housing",
                                                "Student Housing",
                                                "Spiritual Or Religious Community",
                                                "Traditional Or Indigenous Community",
                                                "Transition Town",
                                                "Other")
              ),
  selectInput("food","Dietary Practices:",list("No Filter" = ".",
                                               "Vegetarian",
                                               "Vegan",
                                               "Local",
                                               "Organic",
                                               "Paleo",
                                               "Raw",
                                               "Opportunivore",
                                               "Omnivorous",
                                               "Kosher",
                                               "Halal",
                                               "GMO Free",
                                               "Gluten-Free")
              ),
  selectInput("status","Status:",list("No Filter" = ".","Established",
                                                        "Forming",
                                                        "Disbanded",
                                                        "Re-forming")
              ),
  selectInput("map_type","Base Map:",list("Stamen Terrain" = "Stamen.Terrain",
                                                               "Open Street Map" = "OpenStreetMap",
                                                               "Hike Bike"="HikeBike",
                                                               "Open Topo Map" = "OpenTopoMap",
                                                               "Stamen Watercolor" = "Stamen.Watercolor")
              ),
  radioButtons("labels","Popups:",
               list("On Click" = "popup","On Hover" = "label"), inline = T
               ),
  sliderInput(inputId = "mem", 
              label = "Minimum Size (members):", 
              min = 0, max = 100, value = 5, step = 1),
  
  sliderInput(inputId = "res", 
              label = "Minimum Size (residences):", 
              min = 0, max = 70, value = 1, step = 1),
  
  p(style = "text-align:center", a(href="https://www.ic.org/directory/listings/","Data from the Fellowship for Intentional Communities")
    )
  )



# Body board
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'map', leafletOutput("map", width = "100%", height = 800)
    )
  )
)

# Shiny UI
ui <- dashboardPage(
  title = 'Intentional Community map',
  header,
  sidebar,
  body
)

server <- function(input, output, session) {
  
  full <- read.csv("http://142.93.183.164:3838/data/IC_MapReady.csv") %>% as.tibble()
  
  full$label <- sprintf("<strong><a href= %s target=\"_blank\">%s</a></strong><br/> %s %s %s %s %s %s",
                        full$source, full$org, 
                        ifelse(str_detect(full$type, "No Info"),"", paste0("<strong>Type(s): </strong>",full$type,"<br/>")), 
                        ifelse(str_detect(full$area, "No Info"),"", paste0("<strong>Area: </strong>",full$area,"<br/>")),
                        ifelse(is.na(full$members),"", paste0("<strong>Adult Members: </strong>",full$members,"<br/>")),
                        ifelse(is.na(full$residences),"", paste0("<strong>Residences: </strong>",full$residences,"<br/>")),
                        ifelse(str_detect(full$movein, "No Info"),"", paste0("<strong>Start Year: </strong>",full$movein,"<br/>")),
                        ifelse(str_detect(full$meals_practices, "No Info"),"", paste0("<strong>Dietary Practices: </strong>",full$meals_practices,"<br/>"))) %>% 
    lapply(htmltools::HTML)
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -50, lat = 50, zoom = 2)
  })
  
  
  observe({
    
    min_res <- input$res
    min_mem <- input$mem
    base_map <- input$base_map
    food_grep <- paste0("(?i)",input$food)
    type_grep <- paste0("(?i)",input$com_type)
    status_grep <- paste0("(?i)",input$status)
    base_map <- input$map_type
    label_type <- as.character(input$labels)
    
    sites <- full %>% 
      filter(members >= min_mem & residences >= min_res & str_detect(meals_practices,food_grep) & str_detect(type,type_grep) & str_detect(as.character(status), status_grep))
    
    if(label_type == "popup"){
    leafletProxy("map") %>% clearMarkers() %>% 
      addProviderTiles(base_map) %>%
      addMarkers(lng = sites$lon,
                 lat = sites$lat,
                 popup = sites$label)
    }
    
    if(label_type == "label"){
      leafletProxy("map") %>% clearMarkers() %>% 
        addProviderTiles(base_map) %>%
        addMarkers(lng = sites$lon,
                   lat = sites$lat,
                   label = sites$label)
    }
    
  })
  
}


shinyApp(ui, server)