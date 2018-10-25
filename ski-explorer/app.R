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
library(ggplot2)
library(tidyverse)
library(leaflet)


# CREATE USER INTERFACE #############################################

# header board (the blue bar at the top)
header <- dashboardHeader(title = "Ski Resort Explorer")

# Side bar board (Where the filters are)
sidebar <- dashboardSidebar(

  sidebarMenu(
    # Create Sidebar Tabs
    id = 'menu_tabs'
    , menuItem('Interactive Map', tabName = 'map')
    , menuItem('Filtered Table', tabName = 'table')
  ),
  # Create Sidebar Filter Inputs

  # Max Price Slider
  sliderInput(inputId = "price",
              label = "Maximum Ticket Price:",
              min = 0, max = max(resorts$ticket, na.rm = T), value = max(resorts$ticket, na.rm = T), step = 1),

   # Min Vertical Ft. Slider
  sliderInput(inputId = "vert",
              label = "Minimum Vertical Rise:",
              min = 0, max = 4000, value = 0, step = 100),

  # Number of Lifts Slider
  numericInput("lifts","Minimum Lifts:",0,0,40),

   # State Select Filter
  selectInput("state","State/Province:",
              c("No Filter", resorts$state %>% as.character() %>% unique())
  ),

  # Labels on Click or Hover?
  radioButtons("labels","Popups:",
               list("On Click" = "popup","On Hover" = "label"), inline = T
  ),

  # Select Base Map
  radioButtons("base_map", "Base Map:",
               c("Stamen Terrain" = "Stamen.Terrain",
                 "Open Street Map" = "OpenStreetMap",
                 "Open Topo Map" = "OpenTopoMap"),inline = T)

)

# Body Board Object (what is displayed in panel)
body <- dashboardBody(

  # Create Multiple pages based on tab selected
  tabItems(

    # Create Body for "Interactive Maps" Tab
    tabItem(
      tabName = 'map', # from output$map in shiny server
      fluidRow(column(12,leafletOutput('map', height=500))), #Print Map
      fluidRow(column(6, plotOutput("histPrice", width = "auto")), # Print Price Historam
               column(6, plotOutput("scatterPriceVert", width = "auto")))), #Print Scatter Plot

    # Create body for "Table" Tab
     tabItem(
       tabName = "table",
       column(12, dataTableOutput('table')))
  ))


# Put it all together
ui <- dashboardPage(
  title = 'Ski Resort Explorer', # This goes in the browser tab
  header, # Blue Header Bar
  sidebar, # Tabs/Filters
  body # Display Content
)


# START SERVER #############################################


server <- function(input, output, session) {

  # Read in data
  resorts <- read.csv("skiResorts_geocoded3.csv", stringsAsFactors = F) %>% as.tibble()

  # Render the base map (does not have markers)
  output$map <- renderLeaflet({
  leaflet() %>% addTiles() %>%
    setView(lng = -100, lat = 50, zoom = 4)
})



# A reactive expression that returns the set of resorts that are
# in bounds right now
resortInBounds <- reactive({
  if (is.null(input$map_bounds))
    return(resorts[FALSE,])
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  max_price <- input$price
  min_vert <- input$vert
  min_lifts <- input$lifts
  state_select <- input$state
  state_select <- ifelse(state_select == "No Filter",".",state_select)
  state_select <- paste(state_select,sep="|") %>% str_remove("\\|$")

  subset(resorts,
         lat >= latRng[1] & lat <= latRng[2] &
           lon >= lngRng[1] & lon <= lngRng[2] &
           ticket <= max_price & vertical >= min_vert &
           lifts >= min_lifts & str_detect(state, state_select))
})

## Make Ticket Prices Histogram
priceBreaks <- hist(plot = FALSE, resorts$ticket, breaks = 20)$breaks

output$histPrice <- renderPlot({

  # If no resorts are in view, don't plot
  if (nrow(resortInBounds()) == 0)
    return(NULL)

  # Plot histogram of resorts in view
  print(ggplot(resortInBounds(), aes(x=ticket)) +
          geom_histogram(fill = "lightblue", color="black", bins = 30) +
          labs(title = "Histogram of Ticket Prices",
               subtitle = "Resorts in View",
               x = "Ticket Price") +
          theme(plot.title = element_text(hjust = .5,size=16),
                plot.subtitle = element_text(hjust = .5,size=14),
                plot.background = element_rect(color = "black"),
                axis.title = element_text(size=14),
                plot.margin = margin(10,20,10,10)))
})

# Scatter Plot
output$scatterPriceVert <- renderPlot({

  # If no resorts are in view, don't plot
  if (nrow(resortInBounds()) == 0)
    return(NULL)

  # Create scatter plot of resorts in view
  print(ggplot(resortInBounds(),
               aes(y = acres, x = vertical,color = ticket)) +
          geom_point(size=2) +
          theme_grey() +
          scale_x_continuous(trans='log2',
                             breaks = c(100,200,400,800,1600,3200,6400,12800),
                             minor_breaks = NULL) +
          scale_y_continuous(trans='log2',
                             breaks = c(3,6,12,25,50,100,200,400,800,1600,3200,6400),
                             minor_breaks = NULL) +
          labs(title = "Ski Resort Acres vs. Vertical Feet",
               subtitle = "Colored by Ticket Price",
               y = "Skiable Acres",
               x =" Vertical Rise") +
          theme(plot.title = element_text(hjust = .5,size=16),
                plot.subtitle = element_text(hjust = .5,size=14),
                plot.background = element_rect(color = "black"),
                axis.title = element_text(size=14),
                plot.margin = margin(10,1,10,10)) +
          labs(color = "Price"))
})


# Re-render map with markers and info
observe({

  sites <- resortInBounds() # select resorts in bounds
  label_type <- input$labels # select label type (hover or click)
  base_map <- input$base_map # select base map

  # create labels
  sites$label <- sprintf("<div style='text-align:center'><a href='%s' target='_blank'><strong>%s</strong></a><br/> %s %s %s %s</div>",
                           sites$url,sites$name,
                           ifelse(!is.na(sites$vertical), paste0("<br/>Vertical Rise: <strong>",sites$vertical," ft.</strong>"),""),
                           ifelse(!is.na(sites$acres), paste0("<br/>Skiable Acres: <strong>",sites$acres," acres</strong>"),""),
                           ifelse(!is.na(sites$lifts), paste0("<br/>Number of Lifts: <strong>",sites$lifts," lifts</strong>"),""),
                           ifelse(!is.na(sites$ticket), paste0("<br/><br/><strong>Ticket Price: $",sites$ticket,"</strong>"),"")) %>% lapply(htmltools::HTML)

  # make map with popups on click
  if(label_type == "popup"){
    leafletProxy("map") %>% clearMarkers() %>%
      addProviderTiles(base_map) %>%
      addMarkers(lng = sites$lon,
                 lat = sites$lat,
                 popup = sites$label)
    }

  # make map with labels on hover
  if(label_type == "label"){
    leafletProxy("map") %>% clearMarkers() %>%
      addProviderTiles(base_map) %>%
      addMarkers(lng = sites$lon,
                 lat = sites$lat,
                 label = sites$label)
    }
  })


# Create Filterable Table
observe({

  # get sites in bounds
  sites.table <- resortInBounds()

  # Select important variables
  sites.table <- sites.table %>% dplyr::select(name,state,ticket,vertical,acres, url)

  # Render reactive table
  output$table <- renderDataTable(sites.table[order(sites.table$ticket),])


})

}


#  RUN APP
shinyApp(ui,server)
