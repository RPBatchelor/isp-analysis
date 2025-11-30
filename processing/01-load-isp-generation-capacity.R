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

# 01. Generator capacity (MW) (***This file***)
# 02. Generation capacity (GWh) 
# 03. Storage capacity (MW) 
# 04. Storage output (GWh)
# 05. Retirements (MW)


# ----- 00. Script drivers -----------------------------------------------------

script_units <- "MW"



#----- 1. Download the generation and storage data files ------------------------

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



# ----- GENERATOR CAPACITY (MW) ------------------------------------------------


# 2024 Final
list_files <- list.files("raw-data/2024_final/Core/", pattern = ".xlsx")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2024_final/Core/", .x),
                                                          sheet = "Capacity",
                                                          range = "A3:AG4604",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = script_units,
                                                          source_data = "2024_final"))

combined_2024_final <- bind_rows(all_data) |> 
  mutate(year = str_replace(year, "_", "-"),
         scenario = str_trim(str_remove_all(scenario, "-"))) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 6,7)))) |> 
  mutate(scenario = str_to_lower(scenario)) |> 
  group_by(cdp, region, technology, year, unit, scenario, source, year_ending) |> 
  summarise(value = sum(value)) |> 
  ungroup()


# 2022 Final
list_files <- list.files("raw-data/2022_final/Final ISP Results/Scenarios/", pattern = ".xlsx")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2022_final/Final ISP Results/Scenarios/", .x),
                                                          sheet = "Capacity",
                                                          range = "A3:AF782",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = script_units,
                                                          source_data = "2022_final"))

combined_2022_final <- bind_rows(all_data) |> 
  select(-existing_and_committed) |> 
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
                                                     sheet = "Capacity_2",
                                                     range = "A3:X91",
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
                                                           sheet = "Capacity_1",
                                                           range = "A3:X91",
                                                           scenario = scenario,
                                                           cdp = cdp,
                                                           output_unit = script_units,
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
                                                               output_unit = script_units,
                                                               source_data = "2018_final"))

combined_2018_final <- bind_rows(all_data) |> 
  mutate(cdp = "default") |> 
  mutate(scenario = str_remove_all(scenario, "-"),
         scenario = str_replace(scenario, ".xlsx", ""),
         scenario = str_trim(scenario)) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 6,7)))) |> 
  mutate(scenario = str_to_lower(scenario))





# Combine the data files together and make final tidy up
isp_generator_capacity <- bind_rows(combined_2018_final,
                                    combined_2020_final,
                                    combined_2022_final,
                                    combined_2024_final) |>  
  mutate(technology = str_to_lower(technology),
         scenario = str_to_lower(scenario),
         cdp = str_to_lower(cdp)) |>  
  # Modify technology from previous ISP versions to match the full list based on ISP2024
  # TBC if this is OK - may need to create new column for 'modified tech' and keep original
  # TODO Confirm OK to use modified tech, or create new column for modified tech
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
  mutate(year = as.numeric(year(year_ending))) |> 
  mutate(odp = case_when(
    source == "2018_final" & cdp == "default" ~ TRUE,
    source == "2020_final" & cdp == "dp1" ~ TRUE,
    source == "2022_final" & cdp == "cdp8" ~ TRUE,
    source == "2024_final" & cdp == "cdp14" ~ TRUE,
    .default = FALSE
  ))


save(isp_generator_capacity, file = "shiny-webtool/data/isp_generator_capacity.rda", compress = "xz")
write_csv(isp_generator_capacity,
          file = "data/isp_generator_capacity.csv")

rm(combined_2018_final, 
   combined_2020_final, 
   combined_2022_final, 
   combined_2024_final, 
   all_data, 
   counterfactual)
rm(list_files, scenarios, url, loop_2020)




