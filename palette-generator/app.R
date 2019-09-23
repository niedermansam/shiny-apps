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
library(shinycssloaders)

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
            checkboxInput("click_exclude", "Exclude on Click", value=T),

            numericInput("max", "Max Colors:", 10),
            HTML("&copy; Sam Supplee-Niederman 2019")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tags$h3(textOutput("message")),
            dataTableOutput("output_table")
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Options and Intilization #######
    inputImage <- list()
    output_palette <- NULL
    dt_options <- list(
        deferRender = TRUE,
        scrollY = 400,
        scroller = TRUE,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
    )
    current_palette <- tibble(Colors="", .rows=0)

    output$message <- renderText("Upload a image or input a URL to begin!")

    outputOptions(output, "message", suspendWhenHidden = FALSE)

    # Generate DataTable and Proxy #########
    output$output_table <- renderDataTable({

        output_palette <- c("Upload an image or enter a URL to begin!")
        message = tibble(Colors=output_palette)

        tibble(Colors="Upload an image or enter a URL to begin!") %>%
            DT::datatable(autoHideNavigation = TRUE,
                          rownames = F,
                          filter = 'none',
                          extensions = c('Buttons', 'Responsive', 'Scroller'),
                          selection = "single",
                          options = dt_options ) %>%
            formatStyle(names(message),
                        backgroundColor = styleEqual(output_palette, output_palette),
                        color="white")

        })

    dt_proxy <- dataTableProxy("output_table")

    # Get Data from URL #########
    url_data <- reactive({

            img_formats <- "\\.(png|gif|jp(e){0,1}g|rbg|rgba|tiff|svg)$"

            if(!str_detect(input$path, "http(s)+://|ftp(s)+://")){
                    user_path <- paste0("http://",input$path)
                } else {
                    user_path <- input$path
                }
            data <- user_path

             if(!str_detect(data, img_formats)){
                 screen_shot <- paste0(tempfile(),".png")
                 tryCatch(webshot(data, file=screen_shot, cliprect = "viewport"),
                          error = function(e){
                              print(e)

                              })
                 data <-screen_shot
             }

            tryCatch(
            output_palette <- color_palette(data, max = 100) %>%
                tibble::enframe(name=NULL, value="Colors"),
            error = function(e){
                print(e)
                return(NULL)
            })

            output_palette

            })

    # Get Data from File Upload ########
    upload_data <- reactive({


        data <- input$file$datapath

        tryCatch(
            output_palette <- color_palette(data, max = 100) %>%
                tibble::enframe(name=NULL, value="Colors"),
            error = function(e){
                print(e)
                return(NULL)
            })

        output_palette
    })

    exclude <- reactive({
            exclude <- input$exclude %>%
                stringr::str_split("(, *| +| *,)")

            exclude <- exclude[[1]] %>%
                str_subset("^#") %>%
                paste0(collapse = "|")

            if(exclude == "") return("Do not filter")

            exclude
    })

    # Generate Palette if user uploads file #######
    observeEvent(input$file, {

        output$message <- renderText("Loading...")


        updateRadioButtons(session, "data_from", selected="Upload")
        current_palette <<- upload_data()

        data <- current_palette %>%
            filter(!str_detect(Colors, exclude())) %>%
            head(n=input$max)



        output$message <- NULL
        dt_proxy %>% replaceData(data, resetPaging = FALSE, rownames = FALSE)
    })

    # Generate Palette if User Clicks Button ##########
    observeEvent(input$generate, {


        output$message <- renderText("Loading...")


        if(input$data_from == "URL"){
            if(input$path == "") {
                output$message <- renderText('Please input a URL under "Image URL:"')
                return(NULL)
                }
            current_palette <<- url_data()
        } else if (input$data_from == "Upload"){
            if(is.null(input$file)) {
                output$message <-  renderText("Please upload an Image")
                return(NULL)
                }
            current_palette <<- upload_data()
        }

        data <- current_palette %>%
            filter(!str_detect(Colors, exclude())) %>%
            head(n=input$max)

        output$message <- NULL

        replaceData(dt_proxy, data, resetPaging = FALSE, rownames = FALSE)
    })

    observeEvent(input$path, {
        updateRadioButtons(session, "data_from", selected="URL")
    })

    observeEvent({
        input$max
        input$exclude
        }, {
        data <- current_palette %>%
            filter(!str_detect(Colors, exclude())) %>%
            head(n=input$max)

        replaceData(dt_proxy, data, resetPaging = FALSE, rownames = FALSE)
    })

    observeEvent({
        input$output_table_cell_clicked
        input$click_exclude },
        { if(input$click_exclude){
            click = input$output_table_cell_clicked$value
            newExclude <-  paste(input$exclude, click) %>% str_replace_all(" {2,}", "")
            updateTextInput(session, "exclude", value=newExclude)
            }
        })
}

# Run the application
shinyApp(ui = ui, server = server)
