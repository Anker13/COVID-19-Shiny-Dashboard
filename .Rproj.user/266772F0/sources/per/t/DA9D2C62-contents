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
library(utils)
library(leaflet)
library(dplyr)
library(rgdal)
library(shinydashboard)
library(tigris)

title <- tags$a(href='https://www.hs-kl.de/', target="_blank", style = "color: rgb(255,255,255); text-align: bottom",
                tags$img(src= "https://upload.wikimedia.org/wikipedia/commons/5/5e/Logo_of_Hochschule_Kaiserslautern.png",height= '40', width= '76.8',style ="vertical-align: top"),
                'CoVid-19')

# Define UI for application that draws a histogram
ui <- dashboardPage(
    title ="Hochschule Kaiserslautern Covid-19 Dashboard",
    skin = "blue",
    # Application title
    dashboardHeader(title=title),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Statistic", tabName = "statistic", icon = icon("list")),
        
        menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                 badgeLabel = "new", badgeColor = "green")
      )
    ),
    dashboardBody(
      tabItem(tabName = "dasboard",
        
        fluidRow(column(width = 12),
                 align = "center",
                 sliderInput("Times",
                             "Time Series",
                             min = min(data_from_github$Date),
                             max = max(data_from_github$Date),
                             value = min(data_from_github$Date),
                             timeFormat = "%d %b %Y")),
        
        fluidRow(column(width = 12),
                 align ="center",
                 leafletOutput(outputId = "mymap",width = 800))
      ),
      tabItem(tabName = "Statistic",
              
              plotOutput("plot")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$mymap <- renderLeaflet({
        tmp_data <- filter(data_from_github, data_from_github$Date == input$Times, data_from_github$Confirmed>0,data_from_github$Lat > 0 && data_from_github$Long > 0)
        qpal <- colorBin(palette = c("#FFD82E","#E8A115","#FF8E24","#E84D15","#FF1408","#761408","#5A1408"),domain = tmp_data$logarithmic, n=12)
        label <- paste("<b>",
                       tmp_data$Country,
                       "<b/><br>Confirmed: ",
                       tmp_data$Confirmed,
                       "<br>Deaths: ",
                       tmp_data$Deaths,
                       "<br>Recovered: ",
                       tmp_data$Recovered,
                       sep = "")
        leaflet("mymap",data = tmp_data)%>%
            addTiles(urlTemplate = "https://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png")%>%
            clearShapes()%>%
            fitBounds(~min(Long),~min(Lat),~max(Long),~max(Lat))%>%
            addCircles(data = tmp_data,
                       lng = ~Long,
                       lat = ~Lat,
                       weight = 2,
                       opacity = 1,
                       radius = tmp_data$Confirmed,
                       color = ~qpal(tmp_data$logarithmic),
                       popup = label)%>%
            addLegend("bottomright",pal = qpal,values = tmp_data$Confirmed,title = "Log Scale",opacity = 0.2)
    })
  
    observe({
        tmp_data <- filter(data_from_github, data_from_github$Date == input$Times, data_from_github$Confirmed>0, data_from_github$Lat > 0 && data_from_github$Long > 0)
        qpal <- colorBin(palette = c("#FFD82E","#E8A115","#FF8E24","#E84D15","#FF1408","#761408","#5A1408"),domain = tmp_data$logarithmic, n=12)
        label <- paste("<b>",
                       tmp_data$Country,
                       "<b/><br>Confirmed: ",
                       tmp_data$Confirmed,
                       "<br>Deaths: ",
                       tmp_data$Deaths,
                       "<br>Recovered: ",
                       tmp_data$Recovered,
                       sep = "")
        leafletProxy("mymap",data = tmp_data)%>%
            clearShapes()%>%
            addCircles(data = tmp_data,
                       lng = ~Long,
                       lat = ~Lat,
                       weight = 2,
                       opacity = 1,
                       radius = tmp_data$Confirmed,
                       color = ~qpal(tmp_data$logarithmic),
                       popup = label)%>%
            fitBounds(~min(Long),~min(Lat),~max(Long),~max(Lat))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
