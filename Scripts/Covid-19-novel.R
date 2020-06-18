library(tidyverse)
library(utils)
library(dplyr)
library(stringi)
library(forecast)
library(plotly)
library(ggplot2)
library(TTR)
library(time)
library(lubridate)
library(xts)
#Static PATH to csv files from ohter github Repository
PATH <- paste(getwd(),"/Sources/covid-19/data",sep="")

#Read the data from the github Repository 
path_to_data <- paste(PATH,"/time-series-19-covid-combined.csv",sep = "")
data_from_github <- read.csv(path_to_data,sep = ",")
data_from_github$Date <- as.Date(data_from_github$Date)
data_from_github$Country.Region <- as.character(data_from_github$Country.Region)
#data_from_github$Country.Region <- str_replace(data_from_github$Country.Region, "US", "United States")
#data_from_github$Country.Region <- str_replace(data_from_github$Country.Region, "Congo (Brazzaville)","Democratic Republic of the Congo")
#data_from_github$Country.Region <- str_replace(data_from_github$Country.Region, "Congo (Kinshasa)","Democratic Republic of the Congo")
#data_from_github$Country.Region <- str_replace(data_from_github$Country.Region, "Korea, South", "Republic of Korea")
#data_from_github$Country.Region <- str_replace(data_from_github$Country.Region, "Laos","Lao PDR")
#data_from_github$Country.Region <- str_replace(data_from_github$Country.Region, "Russia","Russian Federation")
colnames(data_from_github)[2]<-"Country"
colnames(data_from_github)[3]<-"Province"
data_from_github <- select(data_from_github,-c(Lat,Long))
#data_from_github <- from_github %>% group_by(Date,Country) %>% summarise(Lat=max(Lat),Long=max(Long),Confirmed=sum(Confirmed),Recovered=sum(Recovered),Deaths=sum(Deaths))
data_from_github$Confirmed <- as.numeric(data_from_github$Confirmed)
data_from_github$Recovered <- as.numeric(data_from_github$Recovered)
data_from_github$Deaths <- as.numeric(data_from_github$Deaths)
data_from_github$Province <- as.character(data_from_github$Province)
#handling NA´s for later calculations
data_from_github$Recovered[is.na(data_from_github$Recovered)]<- 0
data_from_github$Deaths[is.na(data_from_github$Deaths)]<- 0
data_from_github$Confirmed[is.na(data_from_github$Confirmed)]<- 0
#get population data from reference.csv file
path_to_population_data <- paste(PATH,"/reference.csv",sep= "")
population_data <- read.csv(path_to_population_data,sep = ",")
population_data$Population[is.na(population_data$Population)] <- 0
#prepare for inner_join with @data_from_github 
colnames(population_data)[8]<-"Country"
colnames(population_data)[7]<- "Province"
population_data$Country <- as.character(population_data$Country)
population_data$Province <- as.character(population_data$Province)
population_data$Population <- as.numeric(population_data$Population)
#adding logarithmic scaling to the dataframe for better color grading on the leaflet map of the shiny app. every logarithmic scaling with -inf as value was replaced with an -1. Also format digits after comma.
data_from_github <- mutate(data_from_github,logarithmic = log(Confirmed))
data_from_github$logarithmic[is.infinite(data_from_github$logarithmic)]<- -1
data_from_github$logarithmic <- format(round(data_from_github$logarithmic, 2), nsmall=2)
data_from_github$logarithmic <- as.numeric(data_from_github$logarithmic)
#inner join with @data_from_github and @population data frame
data_from_github <- left_join(data_from_github,population_data, by=c("Country","Province"))
colnames(data_from_github)[15]<-"Long"
#adding prevelance analysis. specific: Infected per 100k people. Also format digits after comma
data_from_github <- mutate(data_from_github,prevelance_100k = (Confirmed/Population)*100000)
data_from_github$prevelance_100k <- format(round(data_from_github$prevelance_100k, 2),nsmall = 2)
data_from_github$prevelance_100k <- as.numeric(data_from_github$prevelance_100k)
#adding All-cause mortality. Also format digits after comma.
data_from_github <- mutate(data_from_github,all_case_mortality_100k = (Deaths/Population)*100000)
data_from_github$all_case_mortality_100k <- format(round(data_from_github$all_case_mortality_100k, 2),nsmall = 2)
data_from_github$all_case_mortality_100k <- as.numeric(data_from_github$all_case_mortality_100k)
#adding cause-fatality-ratio. Also format digits after comma.
data_from_github <- mutate(data_from_github,case_fatality_rate = (Deaths/Confirmed)*100)
data_from_github$case_fatality_rate[is.nan(data_from_github$case_fatality_rate)]<- 0
data_from_github$case_fatality_rate <- format(round(data_from_github$case_fatality_rate, 2),nsmall = 2)
data_from_github$case_fatality_rate <- as.numeric(data_from_github$case_fatality_rate)

data_from_github$Population <- format(data_from_github$Population,big.mark = ".",decimal.mark = ",")
#Handling NA´s and Infites caused by missing population data
data_from_github$prevelance_100k[is.na(data_from_github$prevelance_100k)]<- 0
data_from_github$prevelance_100k[is.infinite(data_from_github$prevelance_100k)]<- 0

data_from_github$all_case_mortality_100k[is.na(data_from_github$all_case_mortality_100k)]<- 0
data_from_github$all_case_mortality_100k[is.infinite(data_from_github$all_case_mortality_100k)]<- 0

data_from_github$case_fatality_rate[is.na(data_from_github$case_fatality_rate)]<- 0
data_from_github$case_fatality_rate[is.infinite(data_from_github$case_fatality_rate)]<- 0

data_from_github$Lat[is.na(data_from_github$Lat)]<- 0
data_from_github$Long[is.na(data_from_github$Long)]<- 0

Splitted_Global_DF <- split(data_from_github, data_from_github$Country)

# playground for Time Series
Germany <- Splitted_Global_DF[["Germany"]]
# fit a model for Time Series references:(https://www.statmethods.net/advstats/timeseries.html)
Germany_XTS <- structure(list(date=Germany$Date, Confirmed = Germany$Confirmed))
Germany_FIT <- auto.arima(Germany_XTS$Confirmed)

forecast_length <- 14

Germany_FORECAST <- forecast(model = Germany_FIT, h = forecast_length)

Germany_FORECAST.date <- seq(as.POSIXct(Germany_XTS$date[length(Germany_XTS$date)]),by=Germany_XTS$date[length(Germany_XTS$date)]-Germany_XTS$date[length(Germany_XTS$date)-1], len = forecast_length)
Germany_FORECAST.date <- format(Germany_FORECAST.date, "%Y-%m-%d")

plot(Germany_FORECAST)

p <- plot_ly(data = as.data.frame(Germany_XTS))%>%
  add_lines(x = ~Germany_XTS$date, y = ~Germany_XTS$Confirmed, color=I("green"), name="Confirmed Cases")%>%
  add_ribbons(x = ~Germany_FORECAST.date, ymin = ~Germany_FORECAST$lower[,2], ymax = ~Germany_FORECAST$upper[,2], color=I("gray95"), name ="95% confidence")%>%
  add_ribbons(x = ~Germany_FORECAST.date, ymin = ~Germany_FORECAST$lower[,1], ymax = ~Germany_FORECAST$upper[,1], color=I("gray80"), name ="80% confidence")%>%
  add_lines(x = ~Germany_FORECAST.date, y = ~Germany_FORECAST$mean, color=I("blue"), name="prediction")%>%
  layout(title="Forecast")
p



