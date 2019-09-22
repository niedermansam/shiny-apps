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

if(!require(marketR)){
    devtools::install_github("niedermansam/marketR")
}

library(marketR)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Color Palette Generator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            actionButton("generate", "Generate Palette", width="100%"),
            hr(),
            fileInput("file", "Upload an image:"),
            helpText("Or include a URL to an image below. URLs must include either http(s):// or ftp(s)://."),
            textInput( inputId = "path",
                       label="",
                       value = ""),
            shiny::hr(),
            textInput( inputId = "exclude",
                       label = "Exclude Colors:",
                       value = "#000000 #FFFFFF"),
            helpText("Remove colors by clicking on them in the table."),
            numericInput("max", "Max Colors:", 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("output_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output_palette <<- NULL

    generatePalette <- function(inputId){

        if(inputId == "generate"){
            data <- input$path
        } else if(inputId == "file"){
            data <- input$file$datapath
        }

        col_name = "Color"

        output_palette <- color_palette(data, max = 100) %>%
            tibble::enframe(name=NULL, value=col_name)

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
                            options = list(
                                deferRender = TRUE,
                                scrollY = 400,
                                scroller = TRUE,
                                dom = 'Bfrtip',
                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                            )) %>%
            formatStyle(names(palette),
                        backgroundColor = styleEqual(output_palette, output_palette),
                        color="white")

        output$output_table <- renderDataTable({
            dt
        })
    }

    observeEvent( input$generate, {

        if(!is.null(input$file)){
            output_palette <<- generatePalette('file')
        } else if(input$path == ""){
            print('no url or file uploaded')
        } else {
            output_palette <<- generatePalette("generate")
        }

        renderPalette(output_palette)
    })

    observeEvent(input$file, {
        output_palette <<- generatePalette('file')
        renderPalette(output_palette)
    })

    observeEvent({
        input$max
        input$exclude
        }, {
        renderPalette(output_palette)
    })

    observeEvent(
        input$output_table_cell_clicked,
        {
            click = input$output_table_cell_clicked$value
            newExclude <-  paste(input$exclude, click)

            updateTextInput(session, "exclude", value=newExclude)
            input$output_table_row_last_clicked
        })

    output$path <- renderText(input$path)
}

# Run the application
shinyApp(ui = ui, server = server)
