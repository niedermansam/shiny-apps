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
require(readr)
require(openxlsx)

# Create User Interface ######################################################
ui <- dashboardPage(
  # Create Header for dashboard
  dashboardHeader(title = "Map Generator"),

  # Create Sidebar ###########################################################
  dashboardSidebar(

  # Allow for users to input a file
    fileInput("file1",
            "Choose a File to Summarize:",
            accept = c(
              ".csv", ".xls",".xlsx",".tsv")),

    selectInput("type","File Type:", choices = c("Excel Document (.xlsx)" = "excel","Comma Separated Values (.csv)" = "csv","Tab Separated Values (.tsv)" = "tsv")),



    selectInput("demo", "or select demo data:",
                choices = c("choose demo" = '1',"Ski Resorts"='3', "Intentional Communities" = '2')),



    # Select Field to forecast
#    selectInput("select", "Select input",
#                label = c("Please load a CSV")),

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
      leafletOutput(outputId = "map",height=700)
    )
  )
)


server <- function(input, output, session) {



  data <- reactive({

    if(!is.null(input$file1)){

      df <- read.csv(input$file1$datapath)
      return(df)
    }

     if(input$demo =="1") {

       df <- NA
       return(df)

     } else if(input$demo %>% as.character() == "2" & !isTruthy(input$file1)){

      df <- read.csv("intentional-communities.csv")

      return(df)

    } else if (input$demo %>% as.character() == "3" & !isTruthy(input$file1)) {

      df <- read.csv('ski-resorts.csv')
      return(df)
    } else if (isTruthy(input$file1)){
    # check the user has entered a file
    req(input$file1)

    # Get the data
    if(input$type == "csv"){
    tryCatch({
      # Read user-provided csv
        df <- read.csv(input$file1$datapath) },

      # return a safeError if a parsing error occurs
        error = function(e) {
        stop(safeError(e))
        })
    } else if(input$type == "excel"){
      tryCatch({
        # Read user-provided csv
        df <- read.xlsx(input$file1$datapath) },

        # return a safeError if a parsing error occurs
        error = function(e) {
          stop(safeError(e))
        })
    } else if(input$type == "tsv"){
      tryCatch({
        # Read user-provided csv
        df <- read_delim(input$file1$datapath, "\t") },

        # return a safeError if a parsing error occurs
        error = function(e) {
          stop(safeError(e))
        })
    }

    }


    return(df)

  })


  widget <- reactive({

    df <- data()

    user_map <- leaflet(df) %>%
      addTiles()  %>%
      clearMarkers() %>% addMarkers(popup=df$name)
    return(user_map)

  })


  map <- renderLeaflet({

    user_map <- leaflet() %>%
      addTiles()
    return(user_map)

  })

  observe({
    df <- data()

    if(!is.na(df)){

    leafletProxy("map", data=df) %>%
      clearMarkers() %>% addMarkers(popup=df$name)
    }

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
