library(shiny)
library(httr)
library(leaflet)
library(leaflet.extras)
library(lubridate)

iss_location <- function(){
  bar <- NA
  
  foo <- GET("http://api.open-notify.org/iss-now.json")
  foo <- jsonlite::fromJSON(content(foo, "text", encoding = "utf8"))
  
  suppressWarnings(
  bar$time <- as.POSIXct(foo$timestamp, origin = "1970-1-1", tz = Sys.timezone())
  )
  
  bar$latitude <- foo$iss_position$latitude
  bar$longitude <- foo$iss_position$longitude
  bar
}




UI <- bootstrapPage(
  title = "ISS Tracker",
  headerPanel("ISS Tracker"),
  leafletOutput("map", height = 1000)
  )


server <- function(input,output,session){
  
  mps <- 4.76
  kps <- 7.67
  
  iss <- iss_location()
  
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$Stamen.Terrain) %>% 
      setView(lng = iss$longitude, lat = iss$latitude, zoom = 4)
    })
  
  observe({
    
    iss <- iss_location()
    
    leafletProxy("map")  %>% 
      addCircles(lng = as.double(iss$longitude), lat = as.double(iss$latitude))
    
      invalidateLater(300)
      
  })
  
  
  
}

shinyApp(ui = UI, server = server)
