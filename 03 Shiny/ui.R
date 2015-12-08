#ui.R 

library(shiny)
require(shinydashboard)
require(leaflet)

# Define UI for application that plots random distributions 
dashboardPage(
  # Application title
  dashboardHeader(title = "2014-2015 NBA Basketball Data", titleWidth = 350),
  dashboardSidebar(width = 350,
                   sidebarMenu(
                     menuItem("Points By Team", tabName = "pointsbyteam", icon = icon("bar-chart")),
                     menuItem("Points By Position", tabName = "pointsbypos", icon = icon("bar-chart")),
                     menuItem("Assists By Position", tabName = "assistsbypos", icon = icon("line-chart")),
                     menuItem("Rebounds By Position", tabName = "rebsbypos", icon = icon("line-chart")),
                     menuItem("Steals By Position", tabName = "stealsbypos", icon = icon("line-chart")),
                     menuItem("Blocks By Position", tabName = "blocksbypos", icon = icon("line-chart")),
                     menuItem("Points By Position in Southwest Division", tabName = "crosstab", icon = icon("table"))
                   )
  ),
  dashboardBody(
    tabItems(
      #first tab content
      tabItem(tabName = "pointsbyteam",
              h4("Points By Team"),
              plotOutput("pointsbyteam")
        
      ),
      
      tabItem(tabName = "pointsbypos",
              h4("Points By Position"),
              plotOutput("pointsbypos")
      ),
      
      tabItem(tabName = "assistsbypos",
              h4("Assists By Position"),
              plotOutput("assistsbypos")
      ),
      
      tabItem(tabName = "rebsbypos",
              h4("Rebounds By Position"),
              plotOutput("rebsbypos")
      ),
      
      tabItem(tabName = "stealsbypos",
              h4("Steals By Position"),
              plotOutput("stealsbypos")
      ),
      
      tabItem(tabName = "blocksbypos",
              h4("Blocks By Position"),
              plotOutput("blocksbypos")
      ),
      
      tabItem(tabName = "crosstab",
              h4("Points By Position"),
              
              radioButtons("Division", 
                           "Choose a Divison:",
                           choices = list("Atlantic" = 1, "Southwest" = 2,
                                          "Southeast" = 3, "Pacific" = 4), selected = 1),
              
              sliderInput("KPI1", 
                          "KPI Low Max value:", 
                          min = 0,
                          max = 14, 
                          value = 7),
              sliderInput("KPI2", 
                          "KPI Medium Max value:", 
                          min = 5,
                          max = 30, 
                          value = 20),
              plotOutput("crosstab")
              
      )
      
    )  
  
  
  )
  
  
)