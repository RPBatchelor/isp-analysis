


url <- "https://aemo.com.au/-/media/files/major-publications/isp/2024/supporting-materials/2024-isp-generation-and-storage-outlook.zip?la=en"

download_if_fresh(url, destfile = "raw-data/2024_final/gen_storage.zip")
unzip("raw-data/2024_final/gen_storage.zip",
      exdir = "raw-data/2024_final/")
