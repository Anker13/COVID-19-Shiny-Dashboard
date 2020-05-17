library(tidyverse)
library(utils)
library(leaflet)
library(dplyr)
library(rgdal)
library(spData)
library(sf)
library(tigris)
library(maps)
#Static PATH to csv files from ohter github Repository
PATH <- paste(getwd(),"/Sources/NovelCOVID-19/data",sep="")
PATHTOJSON <- paste(getwd(),"/Sources/json",sep = "")

#Make git pull with an .bat file in the Scripts Folder (should be local and not on remote github repository)
d <- paste(getwd(),"/Scripts/GitUpdate.bat",sep = "")
shell.exec(d)
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

data_from_github <- mutate(data_from_github, logarithmic = log(data_from_github$Confirmed))
data_from_github$Recovered[is.na(data_from_github$Recovered)]<- 0
data_from_github$logarithmic[is.infinite(data_from_github$logarithmic)]<- -1
