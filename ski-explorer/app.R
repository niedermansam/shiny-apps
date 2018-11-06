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
library(httr)
library(jsonlite)
library(shinycssloaders)

resorts <- read.csv("skiResorts_geocoded3.csv", stringsAsFactors = F) %>% as.tibble()

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

  # Select Base Map
  selectInput("base_map", "Base Map:",
               c("Stamen Terrain" = "Stamen.Terrain",
                 "Open Street Map" = "OpenStreetMap",
                 "Open Topo Map" = "OpenTopoMap")),

  # Labels on Click or Hover?
  radioButtons("labels","Popups:",
               list("On Click" = "popup","On Hover" = "label"), inline = T
  )

)

# Body Board Object (what is displayed in panel)
body <- dashboardBody(

  # Create Multiple pages based on tab selected
  tabItems(

    # Create Body for "Interactive Maps" Tab
    tabItem(
      tabName = 'map', # from output$map in shiny server
      fluidRow(
        
        column(8,
               leafletOutput('map', height=500), #Print Map
               htmlOutput("Click_text", align = 'center'),
               htmlOutput("snow_report"),
              conditionalPanel('input.map_marker_click != null' ,tableOutput('forecast') %>% withSpinner())), #Print Forecast
        
        
        column(4, 
               plotOutput("histPrice", width = "auto"), # Print Price Historam
               plotOutput("scatterPriceVert", width = "auto")) #Print Scatter Plot
        )), 

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
                 popup = sites$label,
                 layerId = paste0(sites$name, ", ", sites$state))
    }

  # make map with labels on hover
  if(label_type == "label"){
    leafletProxy("map") %>% clearMarkers() %>%
      addProviderTiles(base_map) %>%
      addMarkers(lng = sites$lon,
                 lat = sites$lat,
                 label = sites$label,
                 layerId = sites$name)
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

getWeather <- function(lat,lon) {
  # 44,-113/forecast
  url <- paste0('https://api.weather.gov/points/',lat,',',lon,'/forecast')
  unicode <- GET(url)
  forecast <- unicode$content %>% rawToChar() %>% fromJSON()
  forecast <- forecast$properties$periods %>% 
    as.tibble()%>% 
    mutate(wind = paste(windSpeed, windDirection)) %>% 
    select(name,detailedForecast) 
  
  forecast$Snow_Low <- forecast$detailedForecast %>% 
    str_extract("(?i)snow accumulation.*\\.") %>% 
    str_replace_all("one", "1") %>% str_extract("of \\d+|around \\d") %>% str_extract("\\d+") %>% as.integer()
  
  forecast$Snow_High <- forecast$detailedForecast %>% 
    str_extract("(?i)snow accumulation.*\\.") %>% 
    str_replace_all("one", "1") %>% 
    str_extract("to \\d+|around \\d") %>% 
    str_extract("\\d+") %>% as.integer()
  return(forecast)
}


observeEvent(input$map_marker_click, {
  
  output$forecast <- NULL
  
  click<-input$map_marker_click
  if(is.null(click))
    return()
  text2<-paste("<h4>Forecast for", click$id, "</h4>", "Coordinates: (", click$lat,", ", click$lng, ")")
  output$Click_text<- renderText({
    text2
  })
  
  forecast <- getWeather(click$lat, click$lng)
  
  
  snow <- forecast %>% summarize(
    low24 = sum(Snow_Low[1:2], na.rm=T),
    high24 = sum(Snow_High[1:2], na.rm=T),
    
    low48 = sum(Snow_Low[1:4], na.rm=T),
    high48 = sum(Snow_High[1:4], na.rm=T),
    
    low72 = sum(Snow_Low[1:6], na.rm=T),
    high72 = sum(Snow_High[1:6], na.rm=T),
    
    low = sum(Snow_Low, na.rm=T),
    high = sum(Snow_High, na.rm=T)
  )
  
  output$snow_report <- renderText({
    sprintf("<br/><ul><li><strong>%d-%d inches</strong> of snow in the next 24 hours</li><li><strong>%d-%d inches</strong> of snow in the next 48 hours</li><li><strong>%d-%d inches</strong> of snow in the next 72 hours<li> and <strong>%d-%d inches</strong> in the forecast period</li></li></ul>",
          snow$low24,
          snow$high24,
          snow$low48,
          snow$high48,
          snow$low72,
          snow$high72,
          snow$low,
          snow$high
          ) %>% str_replace_all("0-0","0")
  })
  
    
  output$forecast <- renderTable({
    forecast %>% select(name,detailedForecast)
  }, colnames = F)
  
  
})


}


#  RUN APP
shinyApp(ui,server)
