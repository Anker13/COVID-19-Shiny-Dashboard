library(tidyverse)
library(utils)
library(leaflet)
library(dplyr)
library(tigris)

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

#get population data from reference.csv file
path_to_population_data <- paste(PATH,"/reference.csv",sep= "")
population_data <- read.csv(path_to_population_data,sep = ",")
population_data$Population[is.na(population_data$Population)] <- 0
#group by expression to get total amount of population in country. Problem: There was NA´s in the population col of the dataset, they were set to 0.
population <- population_data %>% group_by(Country_Region)%>% summarise(Population = sum(as.numeric(Population)))
#prepare for inner_join with @data_from_github 
colnames(population)[1]<-"Country"
population$Country <- as.character(population$Country)
#inner join with @data_from_github and @population data frame
data_from_github <- inner_join(data_from_github,population, by="Country")
#adding logarithmic scaling to the dataframe for better color grading on the leaflet map of the shiny app. every logarithmic scaling with -inf as value was replaced with an -1. Also format digits after comma.
data_from_github <- mutate(data_from_github, logarithmic = log(data_from_github$Confirmed))
data_from_github$logarithmic[is.infinite(data_from_github$logarithmic)]<- -1
data_from_github$logarithmic <- format(round(data_from_github$logarithmic, 2), nsmall=2)
data_from_github$logarithmic <- as.numeric(data_from_github$logarithmic)
#adding prevelance analysis. specific: Infected per 100k people. Also format digits after comma
data_from_github <- mutate(data_from_github, prevelance_100k = (data_from_github$Confirmed/data_from_github$Population)*100000)
data_from_github$prevelance_100k <- format(round(data_from_github$prevelance_100k, 2),nsmall = 2)
data_from_github$prevelance_100k <- as.numeric(data_from_github$prevelance_100k)
#adding All-cause mortality. Also format digits after comma.
data_from_github <- mutate(data_from_github, all_case_mortality_100k = (data_from_github$Deaths/data_from_github$Population)*100000)
data_from_github$all_case_mortality_100k <- format(round(data_from_github$all_case_mortality_100k, 2),nsmall = 2)
data_from_github$all_case_mortality_100k <- as.numeric(data_from_github$all_case_mortality_100k)
#adding cause-fatality-ratio. Also format digits after comma.
data_from_github <- mutate(data_from_github, case_fatality_rate = (data_from_github$Deaths/data_from_github$Confirmed)*100)
data_from_github$case_fatality_rate[is.nan(data_from_github$case_fatality_rate)]<- 0
data_from_github$case_fatality_rate <- format(round(data_from_github$case_fatality_rate, 2),nsmall = 2)
data_from_github$case_fatality_rate <- as.numeric(data_from_github$case_fatality_rate)

data_from_github$Recovered[is.na(data_from_github$Recovered)]<- -1
data_from_github$Population <- format(data_from_github$Population,big.mark = ".",decimal.mark = ",")
#Handling NA´s and Infites caused by missing population data
data_from_github$prevelance_100k[is.na(data_from_github$prevelance_100k)]<- 0
data_from_github$prevelance_100k[is.infinite(data_from_github$prevelance_100k)]<- 0

data_from_github$all_case_mortality_100k[is.na(data_from_github$all_case_mortality_100k)]<- 0
data_from_github$all_case_mortality_100k[is.infinite(data_from_github$all_case_mortality_100k)]<- 0

data_from_github$case_fatality_rate[is.na(data_from_github$case_fatality_rate)]<- 0
data_from_github$case_fatality_rate[is.infinite(data_from_github$case_fatality_rate)]<- 0
