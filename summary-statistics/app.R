#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(rlang)

## Only run examples in interactive R sessions

  
  ui <- dashboardPage(
    dashboardHeader(title = "SummarizeR"),
    
    dashboardSidebar(
        fileInput("file", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        
        selectizeInput("demo", "or select demo data:",
                       choices = c("choose demo" = '1',"Ski Resorts"='3', "Intentional Communities" = '2')),
        
                  tags$hr(),
        
        selectInput("group", "Grouping Variable(s):",
                       choices = c("Please choose/load data"), multiple = T),
        
        selectizeInput("var", "Variable to Summarize:",
                       choices = c("Please choose/load data")),
        
        tags$label("Download Summary Statistics:", id="down"),
        downloadButton('download')

        ),
        
      dashboardBody(
        
        tags$head(
          
          # Link to style sheet
          tags$link(rel = "stylesheet", 
                    type = "text/css", 
                    href = "styles.css")
        ),
        tabsetPanel(
          
          # Basic plot and summary stats
          tabPanel("Summary Statistics",dataTableOutput("contents") %>% withSpinner(), align = "center"),
          
          # JQuery Data Table
          tabPanel("Raw Data",dataTableOutput('table') %>% withSpinner(), align = 'center',padding='10px')
          
          )
      )
    
  )
  
  server <- function(input, output, session) {
    
    data <- reactive({
      
      update_vars <- function(){
        updateSelectInput(session, 
                          "var",
                          choices = names(df),
                          label = c('Variable to Summarize:'))
        
        updateSelectInput(session, 
                          "group",
                          choices = c("none",names(df)),
                          label = c('Grouping Variable(s):'))
      }
      
      
      file1 <- input$file
      
      if(is.null(file1) && input$demo == "1"){
        return(NULL)
      } else if (is.null(file1) && input$demo == "3"){
        df <- read.csv('ski-resorts.csv')
        # Update the "Fields" selector based on user data
        update_vars()
        return(df)
        
      } else if (is.null(file1) && input$demo == "2"){
        df <- read.csv('intentional-communities.csv')
        # Update the "Fields" selector based on user data
        update_vars()
        return(df)
        
      }
      
      
      df <- read.csv(file1$datapath)
      update_vars()
      df
    })
    
    output$table <- renderDataTable({
      data()
    })
    
    
    get_summ <- function() {
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      df <- data()
      if(is.null(df))
        return(NULL)
      
      variable <- input$var
      group <- input$group
      
      
      if(is.null(group) || group %>% str_flatten(" ") %>% str_detect('none')){
        
        summ <- df %>% summarize(
          N = n(),
          min = min(!! sym(variable) %>% as.double(), na.rm = T),
          Q1 = quantile(!! sym(variable) %>% as.double(), na.rm = T, probs=0.25),
          mean = mean(!! sym(variable) %>% as.double(), na.rm = T),
          median = median(!! sym(variable) %>% as.double(), na.rm = T),
          Q3 = quantile(!! sym(variable) %>% as.double(), na.rm = T, probs=0.75), 
          max = max(!! sym(variable) %>% as.double(), na.rm = T),
          NAs = sum(is.na(!! sym(variable)))
          
        )
        
        
        
        return(summ)
      }
      
      df %>% group_by(.dots = group) %>%
        
        summarize(
          
          N = n(),
          Min = min(!! sym(variable) %>% as.double(), na.rm = T),
          Q1 = quantile(!! sym(variable) %>% as.double(), na.rm = T, probs=0.25),
          Mean = mean(!! sym(variable) %>% as.double(), na.rm = T),
          Median = median(!! sym(variable) %>% as.double(), na.rm = T),
          Q3 = quantile(!! sym(variable) %>% as.double(), na.rm = T, probs=0.75), 
          Max = max(!! sym(variable) %>% as.double(), na.rm = T),
          NAs = sum(is.na(!! sym(variable)))
          
        )
    }
    
    output$contents <- renderDataTable({

    get_summ()
      
    })
    
    
   output$download <- downloadHandler(
      'avg-summary.csv',
      content = function(file) {
        write.csv(get_summ(), file)
      }
    )
    
  }
  
  shinyApp(ui, server)

