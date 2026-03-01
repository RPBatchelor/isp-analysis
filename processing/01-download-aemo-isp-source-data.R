################################################################################
#                                                                              
# Downloads the ISP generation and storage outlook files from AEMO
# ETL the data into tidy data frames for analysis
#
# RBatchelor
# August 2024
#
################################################################################

# Data files are downloaded as a zip file from AEMO website as the
# 'generation and storage outlook' files
# These files will be used in subsequent scripts to extract all of the ISP output data

# 01. Generator capacity (MW) 
# 02. Generation capacity (GWh) 
# 03. Storage capacity (MW) 
# 04. Storage output (GWh)
# 05. Retirements (MW)


# ----- 00. Script drivers -----------------------------------------------------


#----- 1. Download the generation and storage data files ------------------------

# 2026
# url <- "https://www.aemo.com.au/-/media/files/major-publications/isp/draft-2026/draft-2026-isp-generation-and-storage-outlook.zip?rev=3ca832a1fdb94b96bce3dd23bd12391f&sc_lang=en"
# download_if_fresh(url, destfile = "raw-data/2026_draft/gen_storage.zip")
# unzip("raw-data/2026_draft/gen_storage.zip",
      # exdir = "raw-data/2026_draft/")

# 2024
url <- "https://aemo.com.au/-/media/files/major-publications/isp/2024/supporting-materials/2024-isp-generation-and-storage-outlook.zip?la=en"
download_if_fresh(url, destfile = "raw-data/2024_final/gen_storage.zip")
unzip("raw-data/2024_final/gen_storage.zip",
      exdir = "raw-data/2024_final/")

# 2022 Final
url <- "https://aemo.com.au/-/media/files/major-publications/isp/2022/2022-documents/generation-outlook.zip?la=en"
download_if_fresh(url, destfile = "raw-data/2022_final/gen_storage.zip")
unzip("raw-data/2022_final/gen_storage.zip",
      exdir = "raw-data/2022_final/")

# 2020 Final
url <- "https://aemo.com.au/-/media/files/major-publications/isp/2020/final-2020-isp-generation-outlook.zip?la=en"
download_if_fresh(url, destfile = "raw-data/2020_final/gen_storage.zip")
unzip("raw-data/2020_final/gen_storage.zip",
      exdir = "raw-data/2020_final/")

# 2018 Final
url <- "https://aemo.com.au/-/media/files/electricity/nem/planning_and_forecasting/isp/2018/2018-generation-and-transmission-outlooks.zip?la=en&hash=B291A4CF746051F2394112408CCC330F"
download_if_fresh(url, destfile = "raw-data/2018_final/gen_storage.zip")
unzip("raw-data/2018_final/gen_storage.zip",
      exdir = "raw-data/2018_final/")




