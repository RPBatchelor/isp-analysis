

#===== Load required packages =================================================

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


# devtools::install_github("RPBatchelor/opennemr")
library(opennemr)

# oe_check_user()


# Value to test if values are equal
epsilon <- 1E-5



# Network timezones
# NEM = Australian Eastern Standard Time (UTC+10), no DST
# WEM = Australian Western Standard Time (UTC+8), no DST
nem_tz <- "Australia/Queensland"
wem_tz <- "Australia/Perth"



if(!dir.exists("raw-data/generator_information/")){
  dir.create("raw-data/generator_information/")
}





