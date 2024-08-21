

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(bslib)
library(thematic)





# -----1. Source useful functions and data ------------------------------------

# Load all the RDA data files
data_files <- list.files("data", full.names = TRUE, pattern = "\\.rda", all.files = TRUE)
for (d in data_files){
  load(d)
}

# Load all R files
r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")
for (f in r_files){
  source(f)
}




