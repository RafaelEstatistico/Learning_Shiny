library(shiny)
library(shinydashboard)
library(leaflet)                 # Cartograms
library(DT)                      # Data Tables
library(dplyr)                   # Data manipulation
library(rgdal)                   # Provides bindings to the 'Geospatial' 
library(shinythemes)             # Custumize Shiny 
library(tidyr)                   # For Spread

# carrega os dados pre-processados
load("dados/Dados_Shiny.RData")
