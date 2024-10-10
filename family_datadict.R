# Import libraries 
library(shiny)
library(shinyWidgets)
library(shinyTree)
library(shinydashboard)
library(shinydashboardPlus)
library(openxlsx)
library(DT)
library(stringr)
library(tidyverse)
library(conflicted)
library(plotly)
library(dplyr)
library(patchwork)
library(scales)
library(RColorBrewer)
library(formattable)


setwd('C:/Users/María/Documents/FAMILY_shinny_app_final/')


source("family_datadict_ui.R")
source("family_datadict_server.R")

addResourcePath(prefix = "www", directoryPath ='C:/Users/María/Documents/FAMILY_shinny_app_final/www/')
shinyApp(ui = ui, server = server) 


