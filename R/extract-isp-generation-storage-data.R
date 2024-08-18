



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
           source = source_data, 
           cdp = cdp)
  
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
