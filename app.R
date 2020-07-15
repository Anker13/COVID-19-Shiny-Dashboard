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
library(forecast)
library(lubridate)
title <- tags$a(href='https://www.hs-kl.de/', target="_blank", style = "color: rgb(255,255,255); text-align: bottom",
                tags$img(src= "https://upload.wikimedia.org/wikipedia/commons/5/5e/Logo_of_Hochschule_Kaiserslautern.png",height= '40', width= '76.8',style ="vertical-align: top"),
                'CoVid-19')

values <- rbind(c('Salaries', 'Office', 'Merchandise', 'Legal', '<b>TOTAL<br>EXPENSES</b>'), c("Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad"))

fig <- plot_ly(
  type = 'table',
  height = 800,
  columnorder = c(1,2),
  columnwidth = c(10,90),
  header = list(
    values = c('<b>Keyword</b>', '<b>DESCRIPTION</b>'),
    line = list(color = '#506784'),
    fill = list(color = '#119DFF'),
    align = c('left','center'),
    font = list(color = 'white', size = 12),
    height = 40
  ),
  cells = list(
    values = rbind(
      t(as.matrix(unname(glossar)))
    ),
    line = list(color = '#506784'),
    fill = list(color = c('#25FEFD', 'white')),
    align = c('left', 'left'),
    font = list(color = c('#506784'), size = 12),
    height = 30
  ))


# Define UI for application that draws a histogram
ui <- dashboardPage(
    title ="Hochschule Kaiserslautern Covid-19 Dashboard",
    skin = "blue",
    # Application title
    dashboardHeader(title=title),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "Map", icon = icon("dashboard")),
        menuItem(icon=icon("calendar"),timeslider<-sliderInput("Times",
                              "Time Series",
                              min = min(data_from_github$Date),
                              max = max(data_from_github$Date),
                              value = min(data_from_github$Date),
                              timeFormat = "%d %b %Y")),
        menuItem(icon=icon("globe"),country<-selectInput("country", "Choose a country:",
                             list(`Country` = unique(data_from_github$Country)
                                  )
                             
        )),
        htmlOutput("States"),
        menuItem("Statistic", tabName = "statistic", icon = icon("chart-line")),
        menuItem("Forecasts", tabName = "forecasts", icon = icon("chart-line")),
        
        menuItem("Glossar", icon = icon("list"), tabName = "glossar",
                  badgeColor = "green"),
        menuItem("Sources", icon = icon("book"), tabName = "sources",
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("Imprint", icon = icon("info"), tabName = "imprint",
                 badgeLabel = "new", badgeColor = "green")
      )
    ),
    dashboardBody(
      tabItems(
      tabItem(tabName = "Map",
        
        fluidRow(box(title = "World Map",
                     width = 80,
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
      tabItem(tabName="forecasts",
              tabBox(title="Forecasts",
                     width= 10,
                     height='80vh',
                     tabPanel("Forecast Confirmed Cases",plotlyOutput("forecast_confirmed", height = '60vh')),
                     tabPanel("Forecast Death Cases", plotlyOutput("forecast_deaths", height = '60vh')),
                     tabPanel("Forecast Recovered Cases", plotlyOutput("forecast_recovered",height = '60vh')))
      ),
      tabItem(tabName = "glossar",
              
              #textOutput("selected_country"),
              HTML('<center><h2>Glossar</h2></center>'),
             # tableOutput('glossartable')
            fig
      ),
      tabItem(tabName = "sources",
              
              HTML('<center><h2>Sources</h2></center>'),
              tags$br(),
              
              HTML('<center><h2>Data from </h2></center>'),
              HTML('<center><h3><a href="https://github.com/datasets/covid-19">Covid 19 Data Github Repository</a></h3></center>'),
              
              tags$br(),
              
              HTML('<center><h2>Glossarinformation from </h2></center>'),
              HTML('<center><h3><a href="https://github.com/datasets/covid-19">Covid 19 Data Github Repository</a></h3></center>'),
      ),
      tabItem(tabName = "imprint",
              HTML('<center><img src="https://upload.wikimedia.org/wikipedia/commons/5/5e/Logo_of_Hochschule_Kaiserslautern.png" height= "100" width= "200"></center>'),
              
             HTML('<center><h2>Project Covid-19-Dashboard</h2></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Jens Cedric Schug </h4></center>'),
             HTML('<center><h4>Fachbereich Informatik und Mikrosystemtechnik </h4></center>'),
             HTML('<center><h4>Hochschule Kaiserslautern </h4></center>'),
             HTML('<center><h4>Zweibruecken, Deutschland </h4></center>'),
             HTML('<center><h4><a href="mailto:jesc0030@stud.hs-kl.de">jesc0030@stud.hs-kl.de</a></h4></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Julian Bernhart </h4></center>'),
             HTML('<center><h4>Fachbereich Informatik und Mikrosystemtechnik </h4></center>'),
             HTML('<center><h4>Hochschule Kaiserslautern </h4></center>'),
             HTML('<center><h4>Zweibruecken, Deutschland </h4></center>'),
             HTML('<center><h4><a href="mailto:jube0010@stud.hs-kl.de">jube0010@stud.hs-kl.de</a></h4></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Marco Miles Noll </h4></center>'),
             HTML('<center><h4>Fachbereich Informatik und Mikrosystemtechnik </h4></center>'),
             HTML('<center><h4>Hochschule Kaiserslautern </h4></center>'),
             HTML('<center><h4>Zweibruecken, Deutschland </h4></center>'),
             HTML('<center><h4><a href="mailto:mano0010@stud.hs-kl.de">mano0010@stud.hs-kl.de</a></h4></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Supervisor </h4></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Prof. Dr. Manfred Brill </h4></center>'),
             HTML('<center><h4>Fachbereich Informatik und Mikrosystemtechnik </h4></center>'),
             HTML('<center><h4>Hochschule Kaiserslautern </h4></center>'),
             HTML('<center><h4>Zweibruecken, Deutschland </h4></center>'),
             HTML('<center><h4><a href="https://www.hs-kl.de/hochschule/profil/personenverzeichnis/detailanzeige-personen/person/manfred-brill">About Manfred Brill</a></h4></center>'),
             HTML('<center><h4><a href="mailto:manfred.brill@hs-kl.de">manfred.brill@hs-kl.de</a></h4></center>'),
             
             
             
             
             
             
             
              
      )
      
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$States <- renderUI({
    if(length(unique(Splitted_Global_DF[[input$country]]$Province))>1){
      menuItem(province <- selectInput("state",
                                       "States:",
                                       list(`State`= unique(Splitted_Global_DF[[input$country]]$Province))))  
    }
  })
    
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
        countrydata<-filter(Splitted_Global_DF[[input$country]],Splitted_Global_DF[[input$country]]$Date <= input$Times)
        if(!is_null(input$state)){
          if(input$state %in% Splitted_Global_DF[[input$country]]$Province){
            countrydata <-filter(Splitted_Global_DF[[input$country]],Splitted_Global_DF[[input$country]]$Province == input$state, Splitted_Global_DF[[input$country]]$Date <= input$Times)
          }
        }
        countrynotimedata <- filter(Splitted_Global_DF[[input$country]])
        if(!is_null(input$state)){
          if(input$state %in% Splitted_Global_DF[[input$country]]$Province){
            countrynotimedata <- filter(Splitted_Global_DF[[input$country]],Splitted_Global_DF[[input$country]]$Province == input$state)
          }
        }
      #create time series objects for forecasting operations
        countrynotimedata_TS_Confirmed <- structure(list(date = countrynotimedata$Date, confirmed = round(countrynotimedata$Confirmed, digits = 0)))
        countrynotimedata_TS_Deaths <- structure(list(date = countrynotimedata$Date, deaths = round(countrynotimedata$Deaths, digits = 0)))
        countrynotimedata_TS_Recovered <- structure(list(date = countrynotimedata$Date, recovered = round(countrynotimedata$Recovered, digits=0)))
      #fitting TS Objects
        countrynotimedata_FIT_Confirmed <- auto.arima(countrynotimedata_TS_Confirmed$confirmed)
        countrynotimedata_FIT_Deaths <- auto.arima(countrynotimedata_TS_Deaths$deaths)
        countrynotimedata_FIT_Recovered <- auto.arima(countrynotimedata_TS_Recovered$recovered)
      #conditions for forecasting
        forecast_length <- 14
      #forecasting using package forecast. With Method "ARIMA"
        countrynotimedata_FORECAST_Confirmed <- forecast(countrynotimedata_FIT_Confirmed, h = forecast_length)
        countrynotimedata_FORECAST_Deaths <- forecast(countrynotimedata_FIT_Deaths, h = forecast_length)
        countrynotimedata_FORECAST_Recovered <- forecast(countrynotimedata_FIT_Recovered, h = forecast_length)
      #generate sequence of dates for later plotly graphy
        countrynotimedata_FORECAST_Confirmed.date <- seq(as.POSIXct(countrynotimedata_TS_Confirmed$date[length(countrynotimedata_TS_Confirmed$date)]),by=countrynotimedata_TS_Confirmed$date[length(countrynotimedata_TS_Confirmed$date)]-countrynotimedata_TS_Confirmed$date[length(countrynotimedata_TS_Confirmed$date)-1], len = forecast_length)
        countrynotimedata_FORECAST_Deaths.date <- seq(as.POSIXct(countrynotimedata_TS_Deaths$date[length(countrynotimedata_TS_Deaths$date)]),by=countrynotimedata_TS_Deaths$date[length(countrynotimedata_TS_Deaths$date)]-countrynotimedata_TS_Deaths$date[length(countrynotimedata_TS_Deaths$date)-1], len = forecast_length)
        countrynotimedata_FORECAST_Recovered.date <- seq(as.POSIXct(countrynotimedata_TS_Recovered$date[length(countrynotimedata_TS_Recovered$date)]),by=countrynotimedata_TS_Recovered$date[length(countrynotimedata_TS_Recovered$date)]-countrynotimedata_TS_Recovered$date[length(countrynotimedata_TS_Recovered$date)-1], len = forecast_length)
      #formatting generated sequence of dates
        countrynotimedata_FORECAST_Confirmed.date <- format(countrynotimedata_FORECAST_Confirmed.date, "%Y-%m-%d")
        countrynotimedata_FORECAST_Deaths.date <- format(countrynotimedata_FORECAST_Deaths.date, "%Y-%m-%d")
        countrynotimedata_FORECAST_Recovered.date <- format(countrynotimedata_FORECAST_Recovered.date, "%Y-%m-%d")
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
        #Prevelance plotly graph
        output$prevelance <- renderPlotly({
          ggplotly(
            ggplot(data=countrydata,aes(x= Date, y = prevelance_100k))+
              geom_line()+
              geom_point()+
              labs(title = "Prevelance", x="Date", y="Prevelance (per 100.000 People)")
          )
        })
        # all case mortality plotly graph
        output$allcasemort <- renderPlotly({
          ggplotly(
            ggplot(data=countrydata,aes(x = Date, y = all_case_mortality_100k))+
              geom_line()+
              geom_point()+
              labs(title ="All Case Mortality", x = "Date", y = "All Case Mortality (per 100.000 people)")
          )
        })
        # case fatality rate plotly graph
        output$casefatalityrate <- renderPlotly({
          ggplotly(
            ggplot(data=countrydata,aes(x = Date, y = case_fatality_rate))+
              geom_line()+
              geom_point()+
              labs(title = "Case Fatality Rate", x = "Date", y= "Case Fatality Rate (%)")
          )
        })
        # forecast for Confirmed Cases plotly graph
        output$forecast_confirmed <- renderPlotly({
          plot_ly(data = as.data.frame(countrynotimedata_TS_Confirmed))%>%
            add_lines(x = ~countrynotimedata_TS_Confirmed$date, y = ~countrynotimedata_TS_Confirmed$confirmed, color=I("red"), name="Confirmed Cases")%>%
            add_ribbons(x = ~countrynotimedata_FORECAST_Confirmed.date, ymin = ~round(countrynotimedata_FORECAST_Confirmed$lower[,2],digits=0), ymax = ~round(countrynotimedata_FORECAST_Confirmed$upper[,2], digits = 0), color=I("gray95"), name ="95% confidence")%>%
            add_ribbons(x = ~countrynotimedata_FORECAST_Confirmed.date, ymin = ~round(countrynotimedata_FORECAST_Confirmed$lower[,1],digits=0), ymax = ~round(countrynotimedata_FORECAST_Confirmed$upper[,1], digits = 0), color=I("gray80"), name ="80% confidence")%>%
            add_lines(x = ~countrynotimedata_FORECAST_Confirmed.date, y = ~round(countrynotimedata_FORECAST_Confirmed$mean, digits = 0), color=I("blue"), name="prediction")%>%
            layout(title="Forecast Confirmed Cases")
        })
        # forecast for death cases plotly graph
        output$forecast_deaths <- renderPlotly({
          plot_ly(data = as.data.frame(countrynotimedata_TS_Deaths))%>%
            add_lines(x = ~countrynotimedata_TS_Deaths$date, y = ~countrynotimedata_TS_Deaths$deaths, color=I("black"), name="Death Cases")%>%
            add_ribbons(x = ~countrynotimedata_FORECAST_Deaths.date, ymin = ~round(countrynotimedata_FORECAST_Deaths$lower[,2], digits = 0), ymax = ~round(countrynotimedata_FORECAST_Deaths$upper[,2],digits=0), color=I("gray95"), name ="95% confidence")%>%
            add_ribbons(x = ~countrynotimedata_FORECAST_Deaths.date, ymin = ~round(countrynotimedata_FORECAST_Deaths$lower[,1], digits = 0), ymax = ~round(countrynotimedata_FORECAST_Deaths$upper[,1], digits=0), color=I("gray80"), name ="80% confidence")%>%
            add_lines(x = ~countrynotimedata_FORECAST_Deaths.date, y = ~round(countrynotimedata_FORECAST_Deaths$mean, digits=0), color=I("blue"), name="prediction")%>%
            layout(title="Forecast Death Cases")
        })
        
        #forecast for resurrected cases plotly graph
        output$forecast_recovered <- renderPlotly({
          plot_ly(data = as.data.frame(countrynotimedata_TS_Recovered))%>%
            add_lines(x = ~countrynotimedata_TS_Recovered$date, y = ~round(countrynotimedata_TS_Recovered$recovered, digits = 0), color=I("green"), name="Recovered Cases")%>%
            add_ribbons(x = ~countrynotimedata_FORECAST_Recovered.date, ymin = ~round(countrynotimedata_FORECAST_Recovered$lower[,2],digits = 0), ymax = ~round(countrynotimedata_FORECAST_Recovered$upper[,2], digits = 0), color=I("gray95"), name ="95% confidence")%>%
            add_ribbons(x = ~countrynotimedata_FORECAST_Recovered.date, ymin = ~round(countrynotimedata_FORECAST_Recovered$lower[,1],digits = 0), ymax = ~round(countrynotimedata_FORECAST_Recovered$upper[,1], digits = 0), color=I("gray80"), name ="80% confidence")%>%
            add_lines(x = ~countrynotimedata_FORECAST_Recovered.date, y = ~round(countrynotimedata_FORECAST_Recovered$mean, digits = 0), color=I("blue"), name="prediction")%>%
            layout(title="Forecast Recovered Cases")
        })
        
        output$selected_country <- renderText({ 
          paste("You have selected", input$country)
        })
        
    })
  
    
  
    
    
 
    
  #  output$glossartable<-renderTable(glossar)
}

# Run the application 
shinyApp(ui = ui, server = server)

