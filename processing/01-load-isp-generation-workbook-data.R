################################################################################
#                                                                              
# Downloads the ISP generation and storage outlook files from AEMO
# ETL the data into tidy data frames for analysis
#
# RBatchelor
# August 2024
#
################################################################################


#----- 1. Download the generation and storage data files ------------------------

# Data files are downloaded as a zip file from AEMO website as the
# 'generation and storage outlook' files

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



#----- 2. Extract the generation and storage info -----------------------------

# This version works for 2024 and 2022 files
read_isp_capacity_generation <- function(path = path,
                                         sheet = sheet,
                                         range = range,
                                         scenario = scenario,
                                         output_unit = output_unit,
                                         source_data = source_data){
  
  raw <- readxl::read_xlsx(path = path,
                           sheet = sheet,
                           range = range) |> 
    clean_names() |> 
    drop_na(cdp) |> 
    pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "value") |> 
    mutate(year = str_remove(year, "x"),
           value = round(as.numeric(value), digits = 1),
           unit = output_unit,
           scenario = scenario, 
           source = source_data)
  
  return(raw)
  
}

# Separate version required for 2020 - no CDP in the sheet
read_isp_capacity_generation_2020 <- function(path = path,
                                              sheet = sheet,
                                              range = range,
                                              scenario = scenario,
                                              output_unit = output_unit,
                                              source_data = source_data,
                                              cdp = cdp){
  
  raw <- readxl::read_xlsx(path = path,
                           sheet = sheet,
                           range = range) |> 
    clean_names() |> 
    pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "value") |> 
    mutate(year = str_remove(year, "x"),
           value = round(as.numeric(value), digits = 1),
           unit = output_unit,
           scenario = scenario, 
           source = source_data, 
           cdp = cdp)
  
  return(raw)
  
}

# Separate version for 2018 - fill down the region names for merge cells
read_isp_capacity_generation_2018 <- function(path = path,
                                              sheet = sheet,
                                              range = range,
                                              scenario = scenario,
                                              output_unit = output_unit,
                                              source_data = source_data){
  
  raw <- readxl::read_xlsx(path = path,
                           sheet = sheet,
                           range = range) |> 
    clean_names() |>
    fill(region, .direction = "down") |> 
    pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "value") |> 
    mutate(year = str_remove(year, "x"),
           value = round(as.numeric(value), digits = 1),
           unit = output_unit,
           scenario = scenario, 
           source = source_data)
  
  return(raw)
  
}



# ----- 2a. GENERATOR CAPACITY (MW) ----------------------------------------------------


# 2024 Final
list_files <- list.files("raw-data/2024_final/Core/", pattern = ".xlsx")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2024_final/Core/", .x),
                                                          sheet = "Capacity",
                                                          range = "A3:AG4604",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = "MW",
                                                          source_data = "2024_final"))

combined_2024_final <- bind_rows(all_data) |> 
  mutate(year = str_replace(year, "_", "-"),
         scenario = str_trim(str_remove_all(scenario, "-"))) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 6,7)))) |> 
  mutate(scenario = str_to_lower(scenario)) |> 
  group_by(cdp, region, technology, year, unit, scenario, source, year_ending) |> 
  summarise(value = sum(value)) |> 
  ungroup()


# 2024 Draft
# list_files <- list.files("raw-data/2024_draft/")
# 
# all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2024_draft/", .x),
#                                                           sheet = "Capacity",
#                                                           range = "A3:AF1579",
#                                                           scenario = str_extract(.x,
#                                                                                  pattern = "-\\D*"),
#                                                           output_unit = "MW",
#                                                           source_data = "2024_draft"))
# 
# combined_2024_draft <- bind_rows(all_data) |> 
#   mutate(scenario = str_remove_all(scenario, "-"),
#          scenario = str_trim(str_replace(scenario, ".xlsx", ""))) |> 
#   mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 3,4)))) |> 
#   mutate(scenario = str_to_lower(scenario))



# 2022 Final
list_files <- list.files("raw-data/2022_final/Final ISP Results/Scenarios/", pattern = ".xlsx")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2022_final/Final ISP Results/Scenarios/", .x),
                                                          sheet = "Capacity",
                                                          range = "A3:AF782",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = "MW",
                                                          source_data = "2022_final"))

combined_2022_final <- bind_rows(all_data) |> 
  select(-existing_and_committed) |> 
  mutate(scenario = str_trim(str_remove_all(scenario, "-"))) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 3,4)))) |> 
  mutate(scenario = str_to_lower(scenario))



# 2022 Draft
# list_files <- list.files("raw-data/2022_final/Draft ISP Results/Scenarios/")
# 
# all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2022_final/Draft ISP Results/Scenarios/", .x),
#                                                           sheet = "Capacity",
#                                                           range = "A3:AF1094",
#                                                           scenario = str_extract(.x,
#                                                                                  pattern = "-\\D*-"),
#                                                           output_unit = "MW",
#                                                           source_data = "2022_draft"))
# 
# combined_2022_draft <- bind_rows(all_data) |> 
#   select(-existing_and_committed) |> 
#   mutate(scenario = str_trim(str_remove_all(scenario, "-"))) |> 
#   mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 3,4)))) |> 
#   mutate(scenario = str_to_lower(scenario))




# 2020 Final

# 2020 scenarios are in separate folders (not extracted within the one folder like 2024/2022)
scenarios <- c("Central",
               "Fast Change",
               "High DER",
               "Slow Change",
               "Step Change")

loop_2020 <- function(scenario_name){
  
  folder <- paste0("raw-data/2020_final/", scenario_name, "/")
  files <- list.files(folder, pattern = ".xlsx") |> 
    as_tibble() |> 
    mutate(scenario = scenario_name)
  return(files)
  
}


list_files <- map(scenarios, loop_2020) 
list_files <- bind_rows(list_files) |> 
  mutate(file_path = paste0("raw-data/2020_final/", scenario, "/", value),
         cdp = str_extract(file_path, "\\(\\S{1,3}\\)")) |> 
  select(file_path, scenario, cdp)
  # pull(file_path)


all_data <- pmap(list(list_files$file_path,
                      list_files$scenario,
                      list_files$cdp),
                 function(file_path, scenario, cdp){
                   read_isp_capacity_generation_2020(path = file_path, 
                                                     sheet = "Capacity_2",
                                                     range = "A3:X91",
                                                     scenario = scenario,
                                                     cdp = cdp,
                                                     output_unit = "MW",
                                                     source_data = "2020_final")})


# Get the counterfactuals - just need 1 sheet from each of the scenarios
list_files <- map(scenarios, loop_2020) 
list_files <- bind_rows(list_files) |> 
  filter(str_detect(value, "DP1")) |> 
  mutate(file_path = paste0("raw-data/2020_final/", scenario, "/", value),
         cdp = "counterfactual") |> 
  select(file_path, scenario, cdp)


counterfactual <- pmap(list(list_files$file_path,
                            list_files$scenario,
                            list_files$cdp),
                       function(file_path, scenario, cdp){
                         read_isp_capacity_generation_2020(path = file_path, 
                                                           sheet = "Capacity_1",
                                                           range = "A3:X91",
                                                           scenario = scenario,
                                                           cdp = cdp,
                                                           output_unit = "MW",
                                                           source_data = "2020_final")})


combined_2020_final <- bind_rows(all_data, counterfactual) |> 
  select(-existing_committed_in_2021) |> 
  filter(!region %in% c("Total", "Total including DSP", "NEM", "Region")) |> 
  drop_na(region) |> 
  mutate(cdp = str_remove_all(cdp, "\\("),
         cdp = str_remove_all(cdp, "\\)"),
         cdp = str_trim(cdp)) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 3,4)))) |> 
  mutate(scenario = str_to_lower(scenario))





# 2018 Final

# 2018 has some other transmission files we want to remove
list_files <- list.files("raw-data/2018_final/2018 ISP Generation and Transmission Outlooks/", 
                         pattern = ".xlsx") |> 
  as_tibble() |> 
  filter(!str_detect(value, "Transmission")) |> 
  pull(value)


all_data <- map(list_files, ~read_isp_capacity_generation_2018(path = paste0("raw-data/2018_final/2018 ISP Generation and Transmission Outlooks/", .x),
                                                               sheet = "NEMInstalledCapacity Data",
                                                               range = "A18:X73",
                                                               scenario = str_extract(.x,
                                                                                      pattern = "-\\s.*"),
                                                               output_unit = "MW",
                                                               source_data = "2018_final"))

combined_2018_final <- bind_rows(all_data) |> 
  mutate(cdp = "default") |> 
  mutate(scenario = str_remove_all(scenario, "-"),
         scenario = str_replace(scenario, ".xlsx", ""),
         scenario = str_trim(scenario)) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 6,7)))) |> 
  mutate(scenario = str_to_lower(scenario))



# Combine all data
isp_generator_capacity <- bind_rows(combined_2018_final,
                                    combined_2020_final,
                                    combined_2022_final,
                                    combined_2024_final) |>  
  mutate(technology = str_to_lower(technology),
         scenario = str_to_lower(scenario),
         cdp = str_to_lower(cdp)) |>  
  mutate(technology = case_when(technology == "coordinated der storage" ~ "coordinated cer storage",
                              technology == "utility solar" ~ "utility-scale solar",
                              technology == "utility storage" ~"utility-scale storage",
                              technology == "distributed storage" ~ "passive cer storage",
                              technology == "behind the meter storage" ~ "passive cer storage",
                              technology == "ccgt" ~ "mid-merit gas",
                              technology == "dispatchable storage" ~ "utility-scale storage",
                              technology == "peaking gas + liquids" ~ "peaking gas+liquids",
                              technology == "solar" ~ "utility-scale solar",
                              technology == "rooftop pv" ~ "distributed pv",
                              .default = technology)) |> 
  mutate(year = as.numeric(year(year_ending)))






save(isp_generator_capacity, file = "shiny-webtool/data/isp_generator_capacity.rda", compress = "xz")
write_csv(isp_generator_capacity,
          file = "data/isp_generator_capacity.csv")

rm(combined_2018_final, 
   combined_2020_final, 
   combined_2022_final, 
   combined_2024_final, 
   all_data)
rm(list_files, scenarios, url)






# -----2b. Generator output (MWh) ----------------------------------------------

# 2024 Final
list_files <- list.files("raw-data/2024_final/Core/")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2024_final/Core/", .x),
                                                          sheet = "Generation",
                                                          range = "A3:AF5540",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = "GWh",
                                                          source_data = "ISP 2024 - Final"))

combined_2024_final <- bind_rows(all_data) |> 
  mutate(year = str_replace(year, "_", "-"),
         scenario = str_trim(str_remove_all(scenario, "-"))) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 6,7)))) |> 
  mutate(scenario = str_to_lower(scenario)) |> 
  group_by(cdp, region, technology, year, unit, scenario, source, year_ending) |> 
  summarise(value = sum(value)) |> 
  ungroup()



# 2024 Draft
list_files <- list.files("raw-data/2024_draft/")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2024_draft/", .x),
                                                          sheet = "Generation",
                                                          range = "A3:AE1864",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*"),
                                                          output_unit = "GWh",
                                                          source_data = "ISP 2024 - Draft"))

combined_2024_draft <- bind_rows(all_data) |> 
  mutate(scenario = str_remove_all(scenario, "-"),
         scenario = str_trim(str_replace(scenario, ".xlsx", ""))) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 3,4)))) |> 
  mutate(scenario = str_to_lower(scenario))



# 2022 Final
list_files <- list.files("raw-data/2022_final/Final ISP Results/Scenarios/")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2022_final/Final ISP Results/Scenarios/", .x),
                                                          sheet = "Generation",
                                                          range = "A3:AE932",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = "GWh",
                                                          source_data = "ISP 2022 - Final"))

combined_2022_final <- bind_rows(all_data) |> 
  mutate(scenario = str_trim(str_remove_all(scenario, "-"))) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 3,4)))) |> 
  mutate(scenario = str_to_lower(scenario))



# Combine all data
isp_generation_output <- bind_rows(combined_2022_final,
                                   combined_2024_draft,
                                   combined_2024_final) |>  
  mutate(technology = str_to_lower(technology)) |>  
  mutate(technology = case_when(technology == "coordinated der storage" ~ "coordinated cer storage",
                                technology == "utility solar" ~ "utility-scale solar",
                                technology == "utility storage" ~"utility-scale storage",
                                technology == "distributed storage" ~ "passive cer storage",
                                .default = technology)) |> 
  mutate(year = as.numeric(year(year_ending)))



save(isp_generation_output, file = "shiny-webtool/data/isp_generation_output.rda", compress = "xz")

rm(combined_2022_final, combined_2024_draft, combined_2024_final, all_data)
rm(list_files)





# -----2c. Storage capacity (MW)) ----------------------------------------------





# 
# 
# # Read in storage capacity
# step_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Step Change.xlsx",
#                                                  sheet = "Storage Capacity",
#                                                  range = "A3:AE610",
#                                                  scenario = "step_change",
#                                                  output_unit = "MW")
# 
# progressive_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Progressive Change.xlsx",
#                                                         sheet = "Storage Capacity",
#                                                         range = "A3:AE610",
#                                                         scenario = "progressive_change",
#                                                         output_unit = "MW")
# 
# export_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Green Energy Exports.xlsx",
#                                             sheet = "Storage Capacity",
#                                             range = "A3:AE610",
#                                             scenario = "green_energy_exports",
#                                             output_unit = "MW")
# 
# 
# isp_storage_capacity <- bind_rows(step_change_data, 
#                                   progressive_change_data,
#                                   export_data)
# 
# 
# 
# 
# # Read in storage output
# step_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Step Change.xlsx",
#                                                  sheet = "Storage Energy",
#                                                  range = "A3:AE610",
#                                                  scenario = "step_change",
#                                                  output_unit = "GWh")
# 
# progressive_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Progressive Change.xlsx",
#                                                         sheet = "Storage Energy",
#                                                         range = "A3:AE610",
#                                                         scenario = "progressive_change",
#                                                         output_unit = "GWh")
# 
# export_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Green Energy Exports.xlsx",
#                                             sheet = "Storage Energy",
#                                             range = "A3:AE610",
#                                             scenario = "green_energy_exports",
#                                             output_unit = "GWh")
# 
# 
# isp_storage_output <- bind_rows(step_change_data, 
#                                 progressive_change_data,
#                                 export_data)
# 
# 
# 
# 
# # Clean up
# 
# rm(export_data, progressive_change_data, step_change_data)






