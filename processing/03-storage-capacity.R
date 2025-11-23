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
# 03. Storage capacity (MW) (***This file***)
# 04. Storage output (GWh)
# 05. Retirements (MW)

# ----- 00. Script drivers -----------------------------------------------------

script_units <- "MW"




# ----- Storage capacity (MW) ----------------------------------------------

# 2024 Final
list_files <- list.files("raw-data/2024_final/Core/", pattern = ".xlsx")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2024_final/Core/", .x),
                                                          sheet = "Storage Capacity",
                                                          range = "A3:AF2082",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = script_units,
                                                          source_data = "2024_final"))

combined_2024_final <- bind_rows(all_data) |> 
  mutate(year = str_replace(year, "_", "-"),
         scenario = str_trim(str_remove_all(scenario, "-"))) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 6,7)))) |> 
  mutate(scenario = str_to_lower(scenario)) |> 
  group_by(cdp, region, storage_category, year, unit, scenario, source, year_ending) |> 
  summarise(value = sum(value)) |> 
  ungroup()




# 2022 Final
list_files <- list.files("raw-data/2022_final/Final ISP Results/Scenarios/", pattern = ".xlsx")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2022_final/Final ISP Results/Scenarios/", .x),
                                                          sheet = "Storage Capacity",
                                                          range = "A3:AE312",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = script_units,
                                                          source_data = "2022_final"))

combined_2022_final <- bind_rows(all_data) |> 
  mutate(scenario = str_trim(str_remove_all(scenario, "-"))) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 3,4)))) |> 
  mutate(scenario = str_to_lower(scenario))



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
                                                     sheet = "StorageCapacity_2",
                                                     range = "A3:W48",
                                                     scenario = scenario,
                                                     cdp = cdp,
                                                     output_unit = script_units,
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
                                                           sheet = "StorageCapacity_1",
                                                           range = "A3:W48",
                                                           scenario = scenario,
                                                           cdp = cdp,
                                                           output_unit = script_units,
                                                           source_data = "2020_final")})

combined_2020_final <- bind_rows(all_data, counterfactual) |>  
  filter(!region %in% c("Total", "Total including DSP", "Total excluding storage",
                        "NEM", "Region")) |> 
  filter(!str_detect(region, "Pump Load")) |> 
  drop_na(region) |> 
  mutate(cdp = str_remove_all(cdp, "\\("),
         cdp = str_remove_all(cdp, "\\)"),
         cdp = str_trim(cdp)) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 3,4)))) |> 
  mutate(scenario = str_to_lower(scenario)) |> 
  filter(region != "Total Storage Capacity") |> 
  rename("storage_category" = "technology")



# 2018 Final

# 2018 does not have dedicated storage capacity info like 2020+ years do

# 2018 has some other transmission files we want to remove
# list_files <- list.files("raw-data/2018_final/2018 ISP Generation and Transmission Outlooks/", 
  #                        pattern = ".xlsx") |> 
  # as_tibble() |> 
  # filter(!str_detect(value, "Transmission")) |> 
  # pull(value)


# all_data <- map(list_files, ~read_isp_capacity_generation_2018(path = paste0("raw-data/2018_final/2018 ISP Generation and Transmission Outlooks/", .x),
#                                                                sheet = "NEMEnergyGenerated Data",
#                                                                range = "A20:X85",
#                                                                scenario = str_extract(.x,
#                                                                                       pattern = "-\\s.*"),
#                                                                output_unit = "GWh",
#                                                                source_data = "2018_final"))
# 
# combined_2018_final <- bind_rows(all_data) |> 
#   mutate(cdp = "default") |> 
#   mutate(scenario = str_remove_all(scenario, "-"),
#          scenario = str_replace(scenario, ".xlsx", ""),
#          scenario = str_trim(scenario)) |> 
#   mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 6,7)))) |> 
#   mutate(scenario = str_to_lower(scenario))





# Combine the data files together and make final tidy up
isp_storage_capacity <- bind_rows(combined_2020_final,
                                  combined_2022_final,
                                  combined_2024_final) |>  
  mutate(storage_category = str_to_lower(storage_category),
         scenario = str_to_lower(scenario),
         cdp = str_to_lower(cdp)) |>  
  # Modify technology from previous ISP versions to match the full list based on ISP2024
  # TBC if this is OK - may need to create new column for 'modified tech' and keep original
  mutate(storage_category = case_when(storage_category == "coordinated der storage" ~ "coordinated cer storage",
                                .default = storage_category)) |> 
  mutate(year = as.numeric(year(year_ending))) |> 
  mutate(odp = case_when(
    source == "2018_final" & cdp == "default" ~ TRUE,
    source == "2020_final" & cdp == "dp1" ~ TRUE,
    source == "2022_final" & cdp == "cdp8" ~ TRUE,
    source == "2024_final" & cdp == "cdp14" ~ TRUE,
    .default = FALSE
  ))


save(isp_storage_capacity, file = "shiny-webtool/data/isp_storage_capacity.rda", compress = "xz")
write_csv(isp_storage_capacity,
          file = "data/isp_storage_capacity.csv")

rm(#combined_2018_final, 
   combined_2020_final, 
   combined_2022_final, 
   combined_2024_final, 
   all_data, 
   counterfactual)
rm(list_files, scenarios, loop_2020)
















