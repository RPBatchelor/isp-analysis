

# Load packages


library(tidyverse)
library(ggpattern)
library(janitor)
library(readxl)
# library(openxlsx)
library(lubridate)
library(scales)
library(forcats)
library(plotly)
library(glue)
library(patchwork)
library(rvest)

# library(RSelenium)
# library(wdman)

# library(httr2)
# library(GWalkR)
library(chromote)

# library(shiny)

# 
# # Get the directory where app.R is located
# app_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# # Or if not using RStudio:
# # app_dir <- getSrcDirectory(function(){})[1]
# 
# # Set working directory to app folder
# setwd(app_dir)




# Value to test if values are equal
epsilon <- 1E-5



if(!dir.exists("raw-data/generator_information/")){
  dir.create("raw-data/generator_information/")
}









