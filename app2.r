#install.packages("shinydashboard")
#install.packages(c("maps", "mapdata"))
#install.packages("dplyr")
#install.packages("moderndive")
#install.packages('skimr')
#install.packages('tidyverse')

library(shiny)
library(shinydashboard)
library(dplyr)
library(moderndive)
library(skimr)
library(tidyverse)
top_zip <- house_prices %>%
  count(zipcode, sort = TRUE) %>%
  slice(1:10) %>%
  pull(zipcode)
#Sidebar =title
data(house_prices)
ui <- dashboardPage(
  dashboardHeader(title = "Row layout"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon=icon("dashboard")),
      menuItem("Sale Dashboard", tabName = "my", icon = icon("th"))
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              # Boxes need to be put in a row(or column)
              fluidRow(
                box(
                  title = "Select year built:",
                  width = 3, background = "green", solidHeader = TRUE, status = "primary",
                  selectInput("yrbuilt", label="", year <- sort(unique(house_prices$yr_built)))
                  #radioButtons("yr_built",label="", year <- unique(house_prices$yr_built))
                  # sliderInput("slider", "Year Built:", min(year), max(year), 1)
                ),
                box(
                  title = "Annual Mean price by number of bedrooms",
                  width = 5, solidHeader = TRUE, status = "info",
                  plotOutput("plot1", height = 250)
                ),
                
                box(
                  title = "Relationship between price and number of bedrooms", width = 4, 
                  solidHeader = TRUE, status = "info",
                  plotOutput("plot2", height = 250))
              ),
              
              fluidRow(
                box(
                  title = "Seattle Map", width = 6, background = "light-blue", solidHeader = TRUE, status = "primary",
                  plotOutput("plot3", height = 450)
                ),
                
                box(
                  title = "Title 1", width = 4, background = "maroon", solidHeader = TRUE, status = "warning",
                  "Box content"
                )
              )
              
              
      ),
      #second tab content
      tabItem(tabName = "my",
              h2("Sale Price Dashboard"),
              fluidRow(
                box(
                  title = "Select year built:",
                  width = 3, background = "green", solidHeader = TRUE, status = "primary",
                  selectInput("yrbuilt", label="", year <- sort(unique(house_prices$yr_built)))
                  #radioButtons("yr_built",label="", year <- unique(house_prices$yr_built))
                  # sliderInput("slider", "Year Built:", min(year), max(year), 1)
                ),
                box(
                  title = "Layout of plot4",
                  width = 5, solidHeader = TRUE, status = "info",
                  plotOutput("plot4", height = 250)
                ),
                box(
                  title = "Layout of plot5",
                  width = 5, solidHeader = TRUE, status = "info",
                  plotOutput("plot5", height = 250)
                )
              )                                                                                                            
      )
    )
    
  )
)

server <- function(input, output) { 
  output$plot1 <- renderPlot({
    
    house_prices %>% 
      filter(yr_built == input$yrbuilt) %>% 
      group_by(bedrooms)%>%
      summarise(AvgPrice = mean(price)) %>%
      #      ggplot(aes(x=bedrooms, y=mean(price)))+geom_point()
      ggplot(aes(x=bedrooms, y=AvgPrice, fill=factor(bedrooms)))+geom_col()
  })
  output$plot2 <- renderPlot({
    
    house_prices %>% 
      filter(yr_built == input$yrbuilt) %>% 
      ggplot(aes(x=bedrooms, y=price))+geom_point()
  })
  
  output$plot3 <- renderPlot({
    library(maps)
    library(mapdata)
    states <- map_data("state")
    washington <- states %>%
      filter(region %in% c("washington"))
    usa <- map_data("usa")
    
    gg1 <- ggplot() + 
      geom_polygon(data = washington, aes(x = long, y = lat, group = group), fill = NA, color = "red") + 
      coord_quickmap(xlim = c(-123, -121.0),  ylim = c(47, 48))
    labs <- house_prices %>% 
      filter(yr_built == input$yrbuilt)
    
    labs <- labs %>% select(long, lat)  
    #tibble(long=)  
    #labs <- tibble(house_prices$lat, house_prices$long)
    gg1 + 
      geom_point(data = labs, aes(x = long, y = lat), shape = 21, color = "black", fill = "yellow", size = 4) # +
    #geom_text(data = labs, aes(x = long, y = lat), hjust = 0, nudge_x = 1)
    
    
  })
  output$plot4 <- renderPlot({
    
    house_prices %>% 
      filter(yr_built == input$yrbuilt) %>% 
      ggplot(aes(x = grade)) +
      geom_bar() +
      labs(x = "grade", title = "grade level")
  })
  output$plot5 <- renderPlot({
   
    house_prices %>% 
      filter(yr_built == input$yrbuilt) %>% 
      ggplot(aes(x = zipcode)) + 
        geom_bar() +
        labs(title="Price by Top 10 Zipcode",
             x = "Zipcode",
        )
  })
}

shinyApp(ui, server)
