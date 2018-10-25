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
library(lubridate)
library(rtweet)

#############################################
# CREATE USER INTERFACE

# header board (the blue bar at the top)
header <- dashboardHeader(title = "Trump Tweets")

# Side bar board (Where the filters are)
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    # Create Sidebar Tabs
    id = 'menu_tabs'
    , menuItem('Calendar', tabName = 'calendar')
  ),
  
  dateRangeInput("filter_dates","Choose a Time Period",
                 start = "2017-1-21", end = Sys.Date(), 
                 min = "2009-05-04", max = Sys.Date(), startview = "decade"),
  
  textInput("search","Search Tweets:","No Filter"),
  
  radioButtons("retweets","Include Retweets?", c("Yes","No"), inline = T)
)

# Body Board Object (what is displayed in panel)
body <- dashboardBody(
  
  # Create Multiple pages based on tab selected
  tabItems(
    
    # Create Body for "Interactive Maps" Tab
    tabItem("calendar",
            
            fluidRow(column(12, plotOutput("calendar", width = "auto"))),
            fluidRow(column(12, plotOutput("time", width = "auto", height = 125))),
            div(p(" ")),
            fluidRow(h4(style = "text-align:center",uiOutput("count"))),
            fluidRow(column(12, dataTableOutput("table")))
            )
    )
  )


# Put it all together
ui <- dashboardPage(
  title = 'Trump Twitter Search', # This goes in the browser tab
  header, # Blue Header Bar
  sidebar, # Tabs/Filters
  body # Display Content
)


#############################################
# START SERVER

server <- function(input, output, session) { 
  
  tweets <- read_csv("TrumpTweets.csv")

  tweets <- arrange(tweets, created_at)
  
  tweets$created_at <- tweets$created_at %>% ymd_hms() 
  
  tweets$date <- tweets$created_at %>% date()
  
  tweets$hour <- tweets$created_at %>% hour()
  
  
  days <- tibble(date = seq(as.Date(tweets$date[1]), as.Date(tweets$date[nrow(tweets)]), by = "days"))
  
  days$day <- days$date %>% day()
  
  days$monthweek <- factor(ceiling(days$day / 7), levels = c(5,4,3,2,1))
  
  days$weekdayf <- days$date %>% weekdays() %>% str_trunc(3, ellipsis = "") %>% 
    factor(levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
  
  days$year <- days$date %>% year()
  
  days$monthf <- days$date %>% months() %>% str_trunc(3, ellipsis = "") %>% factor(
    levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  
search.tweets <- reactive({
  
  phrase <- input$search
  phrase <- ifelse(phrase == "No Filter",".",phrase)
  phrase <- paste0("(?i)",phrase)
  
  retweets <- ifelse(input$retweets == "Yes",1,0)
  
  start_date <- input$filter_dates[1]
  end_date <- input$filter_dates[2]
  
  tweets %>% filter(str_detect(text, phrase) & date >= start_date & date <= end_date & is_retweet <= retweets & is_quote <= retweets)
  
})  


daily_tweets <- reactive({
  
  start_date <- input$filter_dates[1]
  end_date <- input$filter_dates[2]

  tweets.daily <- search.tweets() %>% 
    group_by(date) %>% 
    summarize(tweets = n()) 
  
  left_join(days,tweets.daily, by = "date") %>%
    subset(date >= start_date & date <= end_date)
  
})
 
hour_tweets <- reactive({
  
  search.tweets() %>% 
    group_by(hour) %>% 
    summarize(tweets = n())
  
})

output$calendar <- renderPlot({
  
  print(ggplot(daily_tweets(), aes(weekdayf,monthweek, fill = tweets)) + 
    geom_tile(colour = "white") + 
    facet_grid(year~monthf) + 
    scale_fill_gradientn(colors = c("green","red","black"))  +
      theme(axis.text.x = element_text(angle=90, 
                                       vjust = .25, 
                                       face = 'bold'), 
            plot.title = element_text(hjust = .5, face = "bold"),
            plot.background = element_rect(color = "black"),
            plot.margin = margin(20,30,0,15))  +
      labs(y = "Week of the Month",
           x = "Day of the Week")
  )
  
output$time <- renderPlot({
  
  hour_all <- hour_tweets()
  hour_all$pct <- (hour_all$tweets / sum(hour_all$tweets))*100
  
  print(ggplot(hour_all, aes(hours(hour),1, fill = pct)) + 
          geom_tile(colour = "white")) + 
    scale_fill_gradientn(colors = c("green","red","black")) +
    scale_x_time() + scale_y_continuous(breaks = NULL) +
    labs(x = "Hour",
         y = "") +
    theme(plot.background = element_rect(color = "black"),
          plot.margin = margin(30,0,10,0))
  
})  
  
observe({

  final.tweets <- search.tweets() %>% plain_tweets()
  
  output$table <- renderDataTable(final.tweets %>% arrange(desc(created_at)) %>%
                                    select(created_at,text,retweet_count,favorite_count))
  
  output$count <- renderText(paste0("Showing ",nrow(final.tweets)," Tweets"))
  
})


})
  
  
}


#  RUN APP
shinyApp(ui,server)

