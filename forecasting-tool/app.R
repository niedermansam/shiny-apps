#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Required packages
require(shiny)
require(shinydashboard)
require(shinycssloaders)
require(fpp2)
require(dplyr)
require(ggplot2)
require(stringr)
require(stargazer)
require(openxlsx)

source('disclaimer.R')

# Create User Interface ######################################################
ui <- dashboardPage(
  # Create Header for dashboard
  dashboardHeader(title = "Forecasting App"),

  # Create Sidebar ###########################################################
  dashboardSidebar(


    # Allow for users to input a file
      fileInput("file1",
              "Choose CSV File",
              accept = c( ".csv", ".xls",".xlsx",".tsv")),

    radioButtons("type","File Type:", choices = c("csv","tsv", "excel"), inline = T),


    selectInput("demo", "or select demo data:",
                choices = c("","Glacier Visits"=2, "Australian Restaurant Sales" = 3)),

    # Create a horizontal line element
    tags$hr(),


    # Select Field to forecast
    selectInput("select", "Select input",
              label = c("Please load a CSV")),

    # User Chooses Frequency
    selectizeInput("freq",
                   "Data Frequency:",
                   choices = c("Yearly" = 1,
                               "Quartlerly" = 4,
                               "Monthly" = 12,
                               "Daily" = 365),
                   selected = 12),

    # User Chooses Start Date
    dateInput("date","Start Date:",
              value="2009-01-01",
              startview = "decade"),




#    # Create Download Button
#    conditionalPanel(condition = "input.tab != 'Data'",
#                     tags$label("Download Current Plot:", id="download"),
#                     downloadButton("export")),

    tags$hr(),


    # Create Info/Disclaimer Content for Sidebar #########################################
    tags$p(disclaimer)),


  # Create Dashboard Body Content ###############################################################
  dashboardBody(
    tags$head(

      # Link to style sheet
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "styles.css")
      ),


       h1("ARIMA Time Series Forecast", align = 'center'),

      # Create tabs for main panel ##########################################################
      tabsetPanel(

        # Basic plot and summary stats
        tabPanel("Plot",plotOutput("contents") %>% withSpinner(),
                 htmlOutput('summary') %>% withSpinner(), align = "center"),

        # JQuery Data Table
        tabPanel("Data",dataTableOutput('table') %>% withSpinner(), align = 'center',padding='10px'),

        # ARIMA Forecast
        tabPanel("Forecast", plotOutput('arima_forecast')),

        # STL Decomposition
        tabPanel("Decomposition", plotOutput('decomp') %>% withSpinner()),

        id = "tab"

        )
    )
)


server <- function(input, output, session) {


  glacier <- read.csv('ecns513-data04-glacier_month.csv')

  aus <- data.frame(sales = fpp2::auscafe *1000000000, year = trunc(time(fpp2::auscafe)),
                    month = month.abb[cycle(fpp2::auscafe)])

  # Get User Input Data #############################
  data <- reactive({

      if(input$demo == "2" & !isTruthy(input$file1)){
        df <- glacier
        # Update the "Fields" selector based on user data
        updateSelectInput(session, "select",
                          # Remove date fields from forecasting options
                          choices = names(df)[!str_detect(names(df),"(?i)year|month|day|date")],
                          label = c('Choose value to Forecast:'))

        return(df)
      } else if (input$demo == "3" & !isTruthy(input$file1)) {
        df <- aus
        # Update the "Fields" selector based on user data
        updateSelectInput(session, "select",
                          # Remove date fields from forecasting options
                          choices = names(df)[!str_detect(names(df),"(?i)year|month|day|date")],
                          label = c('Choose value to Forecast:'))
        return(df)
      }

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

      # Update the "Fields" selector based on user data
      updateSelectInput(session, "select",
                      # Remove date fields from forecasting options
                      choices = names(df)[!str_detect(names(df),"(?i)year|month|day|date")],
                      label = c('Choose value to Forecast:'))

          return(df)

    })



  # Create interactive data table #############################################
  output$table <- renderDataTable({
    data()
  })

  # Create SUmmary Statistics
  output$summary <- renderText({
    names = data() %>% names() %>% str_replace_all("^|$","\t \t")
    data() %>% stargazer(type = "html", digits = 0, digits.extra = 2, median =T)
  })



  # Basic Time Series Plot ##########################################
  output$contents <- renderPlot({

    # Get data frame
    df <- data()

    # Get frequency from UI
    freq <- input$freq %>% as.integer()

    # Get start year from UI
    start.year <- input$date %>% str_extract("^....") %>% as.integer()

    # Output a Time Series plot
    df[input$select %>% as.character()] %>%
      ts(frequency = freq, start = start.year) %>% autoplot()

  })

  # ARIMA forecast  ###############################################
  output$arima_forecast <- renderPlot({

    # Make a modal for when the forecast is being calculated
    showModal(modalDialog("Calculating your forecast... This may take a moment", easyClose = T))

    # Get data
    df <- data()

    # Get user inputs
    freq <- input$freq %>% as.integer()
    start.year <- input$date %>% str_extract("^....") %>% as.integer()

    # Create ARIMA plot
    plot <- df[input$select] %>%
      ts(frequency = freq, start = start.year) %>%
      auto.arima() %>%
      forecast() %>%
      autoplot()

    # Remove Modal (I think you got that...)
    removeModal()

    # Output the plot
    plot
  })


arima <- renderPlot({

    # Get data
    df <- data()

    # Get user inputs
    freq <- input$freq %>% as.integer()
    start.year <- input$date %>% str_extract("^....") %>% as.integer()

    # Create ARIMA plot
    plot <- df[input$select] %>%
      ts(frequency = freq, start = start.year) %>%
      auto.arima() %>%
      forecast() %>%
      autoplot()

    # Output the plot
    plot
  })


  # STL Seasonal Decomposition ####################################
  output$decomp <- renderPlot({

    # Get data
    df <- data()
    df <- df[[input$select]]

    # Get user inputs
    freq <- input$freq %>% as.integer()
    start.year <- input$date %>% str_extract("^....") %>% as.integer()

    # Make and return STL decomposition plot
    ts(df[1:length(df)], frequency = freq, start = start.year) %>%
      stl(t.window=7, s.window="periodic") %>%
      autoplot()
  })

  # Download ARIMA Plot
  output$export = downloadHandler(
    filename = function() {paste0(input$tab,".png")},
    content = function(file) {
      ggsave(file, plot = arima(), width=11, height=8.5, device = "png")

    }
  )

}



# Run the application
shinyApp(ui = ui, server = server)
