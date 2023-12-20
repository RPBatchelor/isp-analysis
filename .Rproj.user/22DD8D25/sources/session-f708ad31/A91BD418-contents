





#----- Generator capacity and output -----

read_isp_capacity_generation <- function(path = path,
                                         sheet = sheet,
                                         range = range,
                                         scenario = scenario,
                                         output_unit = output_unit){
  
  raw <- readxl::read_xlsx(path = path,
                           sheet = sheet,
                           range = range) |> 
    clean_names() |> 
    drop_na(cdp) |> 
    pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "value") |> 
    mutate(year = as.numeric(str_remove(year, "x")),
           value = round(as.numeric(value), digits = 1),
           unit = output_unit,
           scenario = scenario)
  
  return(raw)
  
}


# Read in generator capacity
step_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Step Change.xlsx",
                                                 sheet = "Capacity",
                                                 range = "A3:AF1579",
                                                 scenario = "step_change",
                                                 output_unit = "MW")

progressive_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Progressive Change.xlsx",
                                                        sheet = "Capacity",
                                                        range = "A3:AF1579",
                                                        scenario = "progressive_change",
                                                        output_unit = "MW")

export_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Green Energy Exports.xlsx",
                                            sheet = "Capacity",
                                            range = "A3:AF1579",
                                            scenario = "green_energy_exports",
                                            output_unit = "MW")


isp_generator_capacity <- bind_rows(step_change_data, 
                                    progressive_change_data,
                                    export_data)




# Read in generator output
step_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Step Change.xlsx",
                                                 sheet = "Generation",
                                                 range = "A3:AE1864",
                                                 scenario = "step_change",
                                                 output_unit = "GWh")

progressive_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Progressive Change.xlsx",
                                                        sheet = "Generation",
                                                        range = "A3:AE1864",
                                                        scenario = "progressive_change",
                                                        output_unit = "GWh")

export_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Green Energy Exports.xlsx",
                                            sheet = "Generation",
                                            range = "A3:AE1864",
                                            scenario = "green_energy_exports",
                                            output_unit = "GWh")


isp_generation_output <- bind_rows(step_change_data, 
                                    progressive_change_data,
                                    export_data)




# Read in storage capacity
step_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Step Change.xlsx",
                                                 sheet = "Storage Capacity",
                                                 range = "A3:AE610",
                                                 scenario = "step_change",
                                                 output_unit = "MW")

progressive_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Progressive Change.xlsx",
                                                        sheet = "Storage Capacity",
                                                        range = "A3:AE610",
                                                        scenario = "progressive_change",
                                                        output_unit = "MW")

export_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Green Energy Exports.xlsx",
                                            sheet = "Storage Capacity",
                                            range = "A3:AE610",
                                            scenario = "green_energy_exports",
                                            output_unit = "MW")


isp_storage_capacity <- bind_rows(step_change_data, 
                                  progressive_change_data,
                                  export_data)




# Read in storage output
step_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Step Change.xlsx",
                                                 sheet = "Storage Energy",
                                                 range = "A3:AE610",
                                                 scenario = "step_change",
                                                 output_unit = "GWh")

progressive_change_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Progressive Change.xlsx",
                                                        sheet = "Storage Energy",
                                                        range = "A3:AE610",
                                                        scenario = "progressive_change",
                                                        output_unit = "GWh")

export_data <- read_isp_capacity_generation(path = "raw-data/2024 Draft ISP results workbook - Green Energy Exports.xlsx",
                                            sheet = "Storage Energy",
                                            range = "A3:AE610",
                                            scenario = "green_energy_exports",
                                            output_unit = "GWh")


isp_storage_output <- bind_rows(step_change_data, 
                                progressive_change_data,
                                export_data)




# Clean up

rm(export_data, progressive_change_data, step_change_data, raw)






