#-------------------------------------------------------------------------------
# 
# ISP analysis
# 
# Ryan Batchelor
# Date
#
#-------------------------------------------------------------------------------

# This script drives the overall project. 
# You should be able to run it from a clean clone.


#----- 1. LOAD SETUP SCRIPS ----------------------------------------------------
source("setup.r")
source("R/run-all-r-scripts.r")
run_all_r_scripts("R", cleanup = FALSE)


#----- 2. PROCESSING -----------------------------------------------------------
