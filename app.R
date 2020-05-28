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
library(plotly)
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
        menuItem("Dashboard", tabName = "Map", icon = icon("dashboard")),
        menuItem(timeslider<-sliderInput("Times",
                              "Time Series",
                              min = min(data_from_github$Date),
                              max = max(data_from_github$Date),
                              value = min(data_from_github$Date),
                              timeFormat = "%d %b %Y")),
        menuItem(country<-selectInput("country", "Choose a country:",
                             list(`Country` = unique(data_from_github$Country)
                                  )
                             
        )),
        menuItem("Statistic", tabName = "statistic", icon = icon("list")),
        
        menuItem("Glossar", icon = icon("th"), tabName = "glossar",
                 badgeLabel = "new", badgeColor = "green")
      )
    ),
    dashboardBody(
      tabItems(
      tabItem(tabName = "Map",
        
        fluidRow(box(title = "World Map",
                     width = 8,
                     height = '80vh',
                     leafletOutput(outputId = "mymap",height = '80vh')),
                 align ="center"
                 )
      ),
      tabItem(tabName = "statistic",
              splitLayout(plotlyOutput("plot1", height = '40vh'),
                          tabBox(title = "Epidemiological measures",
                              width = 10,
                              height = '40vh',
                              tabPanel("Prevelance",  plotlyOutput("prevelance")),
                              tabPanel("All Case Mortality",plotlyOutput("allcasemort")),
                              tabPanel("Case Fatality Rate",plotlyOutput("casefatalityrate"))
                              ))
          
          
      ),
      tabItem(tabName = "glossar",
              
              textOutput("selected_country"),
              h2("Widgets tab content")
            
      )
      
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
        
        
      #  output$plot1<- renderPlot({
       #   plot( data_from_github$Date, data_from_github$Confirmed )
        #})
        
        countrydata <-filter(data_from_github, data_from_github$Country == input$country, data_from_github$Date <= input$Times)
       
      #Confirmed 
        output$plot1<- renderPlotly({
            ggplotly(
              ggplot(data=countrydata)+
                geom_line(aes(x = Date, y = Confirmed, color="Red"))+
                geom_point(aes(x = Date, y = Confirmed, color="Red"))+
                geom_line(aes(x = Date, y = Recovered, color="chartreuse"))+
                geom_point(aes(x = Date, y = Recovered, color="chartreuse"))+
                geom_line(aes(x = Date, y = Deaths, color="Black"))+
                geom_point(aes(x = Date, y = Deaths, color="Black"))+
                scale_color_identity(name ="Cases",
                                     breaks =c("Red", "chartreuse", "Black"),
                                     labels =c("Confirmed","Recovered","Deaths"),
                                     guide = "legend")+
                labs(title = paste("Covid-19 Cases in ", input$country, "(Population:",countrydata$Population,")",sep = ""), subtitle = format(input$Times, "%x"), y = "People", x="Date")+
                theme(legend.position = "top")
            )
        })
        output$prevelance <- renderPlotly({
          ggplotly(
            ggplot(data=countrydata,aes(x= Date, y = prevelance_100k))+
              geom_line()+
              geom_point()+
              labs(title = "Prevelance", x="Date", y="Prevelance (per 100.000 People)")
          )
        })
        output$allcasemort <- renderPlotly({
          ggplotly(
            ggplot(data=countrydata,aes(x = Date, y = all_case_mortality_100k, color="deepskyblue"))+
              geom_line()+
              geom_point()+
              labs(title ="All Case Mortality", x = "Date", y = "All Case Mortality (per 100.000 people)")
          )
        })
        output$casefatalityrate <- renderPlotly({
          ggplotly(
            ggplot(data=countrydata,aes(x = Date, y = case_fatality_rate, color="deepksyblue"))+
              geom_line()+
              geom_point()+
              labs(title = "Case Fatality Rate", x = "Date", y= "Case Fatality Rate")
          )
        })
        output$selected_country <- renderText({ 
          paste("You have selected", input$country)
        })
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
