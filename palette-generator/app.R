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
library(DT)
library(webshot)

if(!require(marketR)){
    devtools::install_github("niedermansam/marketR")
}

library(marketR)


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(


    includeCSS("styles.css"),

    # Application title
    titlePanel("Color Palette Generator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload Image:"),
            textInput( inputId = "path",
                       label="Image URL:",
                       value = ""),
            actionButton("generate", "Generate Palette", width="100%"),
            radioButtons("data_from", "", c("Upload", "URL"), inline=T),
            shiny::hr(),

            textInput( inputId = "exclude",
                       label = "Exclude Colors:",
                       value = "#000000 #FFFFFF"),
            checkboxInput("click_exclude", "Exclude Selected Rows", value=T),

            numericInput("max", "Max Colors:", 10),
            HTML("&copy; Sam Supplee-Niederman 2019")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("output_table")
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output_palette <<- NULL

    generatePalette <- function(inputId){

        img_formats <- "\\.(png|gif|jpeg|rbg|rgba|tiff|svg)$"

        if(inputId == "generate"){
            if(!str_detect(input$path, "http(s)+://|ftp(s)+://")){
                user_path <- paste0("http://",input$path)
            } else {
                user_path <- input$path
            }
            data <- user_path
        } else if(inputId == "file"){
            data <- input$file$datapath
        }

        col_name = "Color"


        tryCatch(
        output_palette <- color_palette(data, max = 100) %>%
            tibble::enframe(name=NULL, value=col_name),
        error = function(e){
            print(data)
            return(NULL)
        }
        )
        output_palette
    }


    renderPalette <- function(palette, col=1){

        if(is.null(palette)) {return(NULL)}

        exclude <- input$exclude %>%
            stringr::str_split("(, *| +| *,)")

        exclude <- exclude[[1]] %>% str_subset("^#") %>% paste0(collapse = "|")

        output_palette <- palette %>% filter(!str_detect(Color, exclude)) %>%
            head(n=input$max)

        dt <- DT::datatable(output_palette,
                            autoHideNavigation = TRUE,
                            rownames = F,
                            filter = 'none',
                            extensions = c('Buttons', 'Responsive', 'Scroller'),
                            selection = "single",
                            options = list(
                                deferRender = TRUE,
                                scrollY = 450,
                                scroller = TRUE,
                                dom = 'Brt',
                                buttons = c('copy', 'csv', 'excel')
                            )) %>%
            formatStyle(names(palette),
                        backgroundColor = styleEqual(output_palette, output_palette),
                        color="white")

        output$output_table <- renderDataTable({
            dt
        })
    }

    observeEvent( input$generate, {

        if(is.null(input$file) & input$path == "") { return(NULL) }

        if(input$data_from == "Upload"){
            output_palette <<- generatePalette('file')
        } else if(input$data_from  == "URL"){
            output_palette <<- generatePalette("generate")
        }

        renderPalette(output_palette)
    })

    observeEvent(input$file, {
        updateRadioButtons(session, "data_from", selected="Upload")
        output_palette <<- generatePalette('file')
        renderPalette(output_palette)
    })

    observeEvent(input$path, {
        updateRadioButtons(session, "data_from", selected="URL")

    })


    observeEvent({
        input$max
        input$exclude
        }, {
        renderPalette(output_palette)
    })

    observeEvent(
        {
            input$output_table_cell_clicked
            input$click_exclude
            },
        {
            if(input$click_exclude){
              click = input$output_table_cell_clicked$value
              newExclude <-  paste(input$exclude, click)

              updateTextInput(session, "exclude", value=newExclude)
            }
        })

    output$path <- renderText(input$path)
}

# Run the application
shinyApp(ui = ui, server = server)
