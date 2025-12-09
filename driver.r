#-------------------------------------------------------------------------------
# 
# ISP analysis
# 
# Ryan Batchelor
# August 2024
#
#-------------------------------------------------------------------------------

# This script drives the overall project. 
# You should be able to run it from a clean clone.


#----- 1. LOAD SETUP SCRIPTS ---------------------------------------------------
source("setup.r")
source("R/run-all-r-scripts.r")
run_all_r_scripts("R", cleanup = FALSE)


#----- 2. PROCESSING -----------------------------------------------------------
run_all_r_scripts("processing", cleanup = FALSE)


#----- 3. ANALYSIS -------------------------------------------------------------



#----- 4. SHINY WEBTOOL PREP ---------------------------------------------------
run_all_r_scripts("prep", cleanup = FALSE)


#----- 5. Prep shiny tool

rsconnect::writeManifest(appDir = "shiny-webtool")




# data_files <- list.files("shiny-webtool/data", full.names = TRUE,
#                          pattern = "\\.rda$")
# 
# for (d in data_files){
#   load(d)
# }
