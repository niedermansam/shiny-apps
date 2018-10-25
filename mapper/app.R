#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Required packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(stringr)
library(htmlwidgets)
library(leaflet)

# Create User Interface ######################################################
ui <- dashboardPage(
  # Create Header for dashboard
  dashboardHeader(title = "Map Generator"),

  # Create Sidebar ###########################################################
  dashboardSidebar(

    # Allow for users to input a file
    fileInput("file1",
              "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),

    selectInput("demo", "or select demo data:",
                choices = c("","Glacier Visits"=2, "Australian Restaurant Sales" = 3)),

    # Create Download Button

     tags$label("Download your Map:", id="download"),
    downloadButton("export"),

    tags$hr()

    ),


  # Create Dashboard Body Content ###############################################################
  dashboardBody(
    tags$head(

      # Link to style sheet
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "styles.css")
    ),


    # Create tabs for main panel ##########################################################
    fluidPage(
      leafletOutput(outputId = "map")
    )
  )
)


server <- function(input, output, session) {

  widget <- reactive({

    req(input[['file1']][['datapath']])

    df <- read.csv(input[['file1']][['datapath']])

    user_map <- leaflet(df) %>%
      addTiles() %>%
      addMarkers()
    return(user_map)

  })


  map <- renderLeaflet({

    req(input[['file1']][['datapath']])

    df <- read.csv(input[['file1']][['datapath']])

    user_map <- leaflet(df) %>%
      addTiles() %>%
      addMarkers()
    return(user_map)

  })

  output$map <- reactive({

    map()

    })

  output$export <- downloadHandler(
    filename = function(){"avg-map.html"},
    content = function(file){


      saveWidget(widget(),file = file)
    }
  )


}



# Run the application
shinyApp(ui = ui, server = server)
