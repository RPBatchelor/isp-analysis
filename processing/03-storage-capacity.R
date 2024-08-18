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


# ----- Storage capacity (MW) ----------------------------------------------

# 2024 Final
list_files <- list.files("raw-data/2024_final/Core/", pattern = ".xlsx")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2024_final/Core/", .x),
                                                          sheet = "Storage Capacity",
                                                          range = "A3:AF2082",
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


# a <- combined_2024_final |> 
#   filter(region == "NSW",
#          cdp == "CDP14",
#          scenario == "progressive change")


# 2022 Final
list_files <- list.files("raw-data/2022_final/Final ISP Results/Scenarios/", pattern = ".xlsx")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2022_final/Final ISP Results/Scenarios/", .x),
                                                          sheet = "Generation",
                                                          range = "A3:AE932",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = "GWh",
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
                                                     sheet = "Generation_2",
                                                     range = "A3:W123",
                                                     scenario = scenario,
                                                     cdp = cdp,
                                                     output_unit = "GWh",
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
                                                           sheet = "Generation_1",
                                                           range = "A3:W123",
                                                           scenario = scenario,
                                                           cdp = cdp,
                                                           output_unit = "GWh",
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
  mutate(scenario = str_to_lower(scenario))



# 2018 Final

# 2018 has some other transmission files we want to remove
list_files <- list.files("raw-data/2018_final/2018 ISP Generation and Transmission Outlooks/", 
                         pattern = ".xlsx") |> 
  as_tibble() |> 
  filter(!str_detect(value, "Transmission")) |> 
  pull(value)


all_data <- map(list_files, ~read_isp_capacity_generation_2018(path = paste0("raw-data/2018_final/2018 ISP Generation and Transmission Outlooks/", .x),
                                                               sheet = "NEMEnergyGenerated Data",
                                                               range = "A20:X85",
                                                               scenario = str_extract(.x,
                                                                                      pattern = "-\\s.*"),
                                                               output_unit = "GWh",
                                                               source_data = "2018_final"))

combined_2018_final <- bind_rows(all_data) |> 
  mutate(cdp = "default") |> 
  mutate(scenario = str_remove_all(scenario, "-"),
         scenario = str_replace(scenario, ".xlsx", ""),
         scenario = str_trim(scenario)) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 6,7)))) |> 
  mutate(scenario = str_to_lower(scenario))





# Combine the data files together and make final tidy up
isp_generation_output <- bind_rows(combined_2018_final,
                                   combined_2020_final,
                                   combined_2022_final,
                                   combined_2024_final) |>  
  mutate(technology = str_to_lower(technology),
         scenario = str_to_lower(scenario),
         cdp = str_to_lower(cdp)) |>  
  # Modify technology from previous ISP versions to match the full list based on ISP2024
  # TBC if this is OK - may need to create new column for 'modified tech' and keep original
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


save(isp_generation_output, file = "shiny-webtool/data/isp_generation_output.rda", compress = "xz")
write_csv(isp_generation_output,
          file = "data/isp_generation_output.csv")

rm(combined_2018_final, 
   combined_2020_final, 
   combined_2022_final, 
   combined_2024_final, 
   all_data, 
   counterfactual)
rm(list_files, scenarios, loop_2020)

























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

