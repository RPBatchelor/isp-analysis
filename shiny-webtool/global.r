

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(bslib)
library(thematic)

library(scales)
library(glue)





#-----1. Source useful functions and data ------------------------------------

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


#-----2. Defaults ------------------------------------------------------------

# Set global theme for ggplot2 with larger font sizes
custom_theme <- theme_minimal(base_family = "Arial") +
  theme(
    axis.text = element_text(size = 14),       # Increase font size for axis text
    axis.title = element_text(size = 16),      # Increase font size for axis titles
    plot.title = element_text(size = 18),      # Increase font size for plot titles
    legend.text = element_text(size = 14),     # Increase font size for legend text
    legend.title = element_text(size = 16)     # Increase font size for legend title
  )

# Apply the custom theme globally
theme_set(custom_theme)

