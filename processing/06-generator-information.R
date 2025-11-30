



# -----1. Download generator information data from AEMO -----------------------

# TODO Automate scraping the AEMO website and downloading the list of files (excel)
# and then download each one and extract data

# Download page
aemo_page <- "https://www.aemo.com.au/energy-systems/electricity/national-electricity-market-nem/nem-forecasting-and-planning/forecasting-and-planning-data/generation-information"

# 
# html <- read_html(aemo_page)
# 
# 
# file_list <- html |> 
#   html_nodes("a") |>
#   html_attr("href") |> 
#   as_tibble() |> 
#   rename(html_data = value) |> 
#   mutate(excel_file = str_detect(html_data, ".xlsx")) |> 
#   filter(excel_file == T) |> 
#   mutate(download_link = paste0("www.aemo.com.au", html_data)) |> 
#   mutate(file_name = str_extract(download_link, "[^/]+\\.xlsx"))
# 
# 
# 
# download_location <- "raw-data/generator_information/"
# 
# 
# download_list <- file_list |> 
#   select(download_link, file_name) |> 
#   mutate(file_path = paste0(download_location, file_name))
# 
# 
# 
# download_function <- function(download_link, file_path){
#   download.file(url = download_link, destfile = file_path)
# }
# 
# 
# map2(download_list$download_link, download_list$file_path, 
#      download_function, .progress = T)



# Current file = October 2025

# file_url <- "https://www.aemo.com.au/-/media/files/electricity/nem/planning_and_forecasting/generation_information/2025/nem-generation-information-oct-2025.xlsx?rev=38f56f5fa2aa4ceb93272ba188e85852&sc_lang=en"
# 
# download_location <- "raw-data/generator_information/"
# 
# download_function <- function(download_link, file_path){
#     download.file(url = download_link, destfile = file_path)
#   }
# 
# 
# download_function(file_url, download_location)






# ----- 2. Extract data -------------------------------------------------------






# October 2023
# 
# raw <- read_xlsx(path = "raw-data/NEM Generation Information Oct 2023.xlsx",
#                  sheet = "ExistingGeneration&NewDevs",
#                  range = "A2:Y1433") |> 
#   clean_names() |> 
#   select(-c(closure_date,
#             survey_id,
#             aemo_kci_id,
#             survey_last_requested,
#             survey_version_date_time)) |> 
#   rename(capacity_np_mw = nameplate_capacity_mw,
#          capacity_lower_np_mw = lower_nameplate_capacity_mw,
#          capacity_upper_np_mw = upper_nameplate_capacity_mw,
#          capacity_lower_agg_np_mw = aggregated_lower_nameplate_capacity_mw,
#          capacity_upper_agg_np_mw = aggregated_upper_nameplate_capacity_mw) |> 
#   mutate(release_version = "October 2023",
#          across(starts_with("capacity"), as.numeric))
# 
# 
# 
# 
# 
# 
# 
# region_name <- "VIC1"
# 
# test <- raw |> 
#   filter(region == region_name) |>
#   group_by(fuel_bucket_summary, status_bucket_summary) |> 
#   summarise(capacity  = sum(capacity_upper_agg_np_mw)) |> 
#   ungroup()
# 
#   
# p <- test |> 
#   ggplot() +
#   geom_bar(aes(x = fuel_bucket_summary,
#               y = capacity,
#               fill = status_bucket_summary),
#           position = "stack",
#           stat = "identity")
# 
# p





