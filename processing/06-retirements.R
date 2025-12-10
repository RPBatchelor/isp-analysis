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
# 04. Storage output (GWh) (***This file***)
# 05. Retirements (MW)

# ----- 00. Script drivers -----------------------------------------------------

script_units <- "MW"




# ----- Retirements (MW) ----------------------------------------------

# 2024 Final
list_files <- list.files("raw-data/2024_final/Core/", pattern = ".xlsx")

all_data <- map(list_files, ~read_isp_capacity_generation(path = paste0("raw-data/2024_final/Core/", .x),
                                                          sheet = "Retirements",
                                                          range = "A3:AE470",
                                                          scenario = str_extract(.x,
                                                                                 pattern = "-\\D*-"),
                                                          output_unit = script_units,
                                                          source_data = "2024_final"))

combined_2024_final <- bind_rows(all_data) |> 
  mutate(year = str_replace(year, "_", "-"),
         scenario = str_trim(str_remove_all(scenario, "-"))) |> 
  mutate(year_ending = dmy(paste0("30-Jun-", substr(year, 6,7)))) |> 
  mutate(scenario = str_to_lower(scenario)) |> 
  group_by(cdp, region, station, year, unit, scenario, source, year_ending) |> 
  summarise(value = sum(value)) |> 
  ungroup()



# None of the previous year files outline retirements

# Combine the data files together and make final tidy up
isp_retirements <- bind_rows(combined_2024_final) |>  
  mutate(scenario = str_to_lower(scenario),
         cdp = str_to_lower(cdp)) |>  
  mutate(year = as.numeric(year(year_ending))) |> 
  mutate(odp = case_when(
    source == "2018_final" & cdp == "default" ~ TRUE,
    source == "2020_final" & cdp == "dp1" ~ TRUE,
    source == "2022_final" & cdp == "cdp8" ~ TRUE,
    source == "2024_final" & cdp == "cdp14" ~ TRUE,
    .default = FALSE
  ))


save(isp_retirements, file = "shiny-webtool/data/isp_retirements.rda", compress = "xz")
write_csv(isp_retirements,
          file = "data/isp_retirements.csv")

rm(combined_2024_final, 
  all_data)


