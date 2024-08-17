

library(tidyverse)
library(shiny)
library(shinydashboard)
library(bslib)
library(thematic)



# -----1. Source useful functions and data ------------------------------------

files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (file in files) { source(file) }

# Load the necessary input data
load("data/isp_generator_capacity.rda")
