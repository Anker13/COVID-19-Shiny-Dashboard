# COVID-19-Shiny-Dashboard

1. [Introduction](#indroduction)
1. [Components](#components)
    1. [app.R](#app)
    1. [libraries.R](#libraries)
    1. [prepareData.R](#data)
    1. [setWorkingDirectory.R](#wd)
    1. [startShinyServer.R](#server)
    1. [Run.bat](#run)
1. [Dashboard](#dashboard)
    1. [World Map](#world_map)
    1. [Statistics](#statistics)
    1. [Forecast](#forecast)
    1. [Glossar](#glossar)
1. [Sources](#sources)
1. [Imprint](#imprints)

<a name="introduction"></a>
## Indroduction
A project from the summer semester 2020 for the University of Applied Sciences of Kaiserslautern at the Zweibrücken site. This project provides a dashboard which visualizes the data for the actual global Covid-19-situation. The data is provided by the John Hopkins University.

<a name="components"></a>
## Components

<a name="app"></a>
### app.R
The main file of the Shiny application that builds the dashboard. This script contains the code for user interface and server.

<a name="libraries"></a>
### libraries.R


<a name="data"></a>
### prepareData.R
This script contains the necessary steps to get the data in an appropriate form for plotting. Therefor data frames are created and their data types adjusted, in order to plot them. These data frames are then stored in a .RData file for further use.

<a name="wd"></a>
### setWorkingDirectory.R
Uses the path information provided by the Run.bat script, in order to set the working directory to the location this script remains.

<a name="server"></a>
### startShinyServer.R
Runs app.R by using the .RData file.

<a name="run"></a>
### Run.bat
The batch file that acts as a pipeline for creating the dashboard. This provides the information about their storage location and that of Rscript.exe via corresponding variables. Further this script downloads the data from a repository and uses the R scripts in order to start a server with the resulting dashboard.

<a name="dashboard"></a>
## Dashboard

<a name="world_map"></a>
### World Map
A bubble chart that shows on a world map the absolute number of reported cases of infection since the pandemic startet. Here, any point in time during the pandemic can be set, in order to display the corresponding global situation.

<a name="statistics"></a>
### Statistics
Histroy diagrams for important parameters of a pandemic.

<a name="forecast"></a>
### Forecast
Provides a possible forecast to predict the coming course.

<a name="glossar"></a>
### Glossar
Contains explanations of the most important epidemiological terms.

<a name="sources"></a>
## Sources

<a name="imprints"></a>
## Imprints
