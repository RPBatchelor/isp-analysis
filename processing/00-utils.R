


# Technology factors

tech_list <- c("black coal",
               "brown coal",
               "mid-merit gas",
               "mid-merit gas with ccs",
               "flexible gas",
               "flexible gas with ccs",
               "other renewable fuels",
               "hydrogen turbine",
               "hydro",
               "biomass",
               "utility-scale storage",
               "dsp",
               "coordinated cer storage",
               "passive cer storage",
               "solar thermal",
               "offshore wind",
               "wind",
               "utility-scale solar",
               "distributed pv",
               "peaking gas+liquids",
               "utility storage load",
               "coordinated cer storage load",
               "passive cer storage load",
               "rooftop and other small-scale solar")


colour_list <- c("#373A36",  # Black coal
                 "#94795D",  # Brown coal
                 "#008578",  # Mid-merit gas
                 "#00594F",  # Mid-merit gas with CCS
                 "#40C1AC",  # Flexible gas
                 "#0E6659",  # Flexible gas with CCS
                 "#B38210",  # other renewable fuels
                 "#FF6666",  # Hydrogen turbine
                 "#A4DBE8",  # Hydro
                 "#B38210",  # Biomass
                 "#77C5D5",  # Utility scale storage
                 "#606EB2",  # DSP - Demand Side Participation
                 "#A3519B",  # Coordinated CER storage
                 "#DD9CDE",  # Passive CER storage
                 "#FCB71C",  # Solar thermal
                 "#238DAA",  # Offshore wind
                 "#A1D884",  # Wind
                 "#FDD26E",  # Utility scale Solar
                 "#F8E08E",  # Distributed PV
                 "#3FB9C4",  # Peaking gas+liquids
                 "#77C5D5",  # utility storage load
                 "#A3519B",  # coordinated cer storage load
                 "#DD9CDE",  # passive cer storage load
                 "#F8E08E")  # Rooftop and other small-scale solar   


dispatch_list <- c(TRUE,  # Black coal
                   TRUE,  # Brown coal
                   TRUE,  # Mid merit gas
                   TRUE,  # Mid merit gas with CCS
                   TRUE,  # Flexible gas
                   TRUE,  # Flexible gas with CCS
                   TRUE,  # Other RE fuels
                   TRUE,  # Hydrogen turbine
                   TRUE,  # Hydro
                   TRUE,  # Biomass
                   TRUE,  # Utility scale storage
                   TRUE,  # DSP - Demand side participation
                   TRUE,  # Coordinated CER storage
                   FALSE, # Passive CER storage
                   FALSE, # Solar thermal
                   FALSE, # Offshore wind
                   FALSE, # Wind
                   FALSE, # Utility scale solar
                   FALSE, # Distributed PV
                   TRUE,  # Peaking gas+liquids
                   TRUE,  # Utility storage load
                   TRUE,  # Coordinated CER storage load
                   FALSE, # Passive cer storage load
                   FALSE) # Rooftop and other small-scale solar


tech_type_list <- c("coal",      # Black coal
                    "coal",      # Brown coal
                    "gas",       # Mid merit gas
                    "gas",       # Mid merit gas with CCS
                    "gas",       # Flexible gas
                    "gas",       # Flexible gas with CCS
                    "renewable", # OTher RE fuesl
                    "gas",       # Hydrogen turbine
                    "renewable", # Hydro
                    "renewable", # Biomass
                    "storage",   # Utility scale storage
                    "dsp",       # DSP - Demand side participation
                    "storage",   # Coordinated CER storage
                    "storage",   # Passive CER storage
                    "renewable", # Solar thermal
                    "renewable", # Offshore wind
                    "renewable", # Wind
                    "renewable", # Utilitiy scale solar
                    "renewable", # Distributed PV
                    "gas",       # Peaking gas+liquids
                    "storage",   # Utility storage load
                    "storage",   # Coordinated CER storage load
                    "storage",   # Passive CER storage load
                    "renewable") # Rooftop and other small-scale solar



util_table <- bind_cols(tech_list,
                        colour_list,
                        dispatch_list,
                        tech_type_list) |> 
  rename("technology" = "...1",
         "colour_label" = "...2",
         "dispatchable" = "...3",
         "tech_type_cgr" = "...4")





isp_list <- c("2018_final",
              "2020_final",
              "2022_final",
              "2024_final",
              "2026_draft")

odp_list <- list <- c("default",
                 "dp1",
                 "cdp8",
                 "cdp14",
                 "cdp4")

scenario_list <- c("neutral scenario",
              "central",
              "step change",
              "step change",
              "step change")


odp_table <- bind_cols(isp_list,
                       odp_list,
                       scenario_list) |>
  rename("isp_source" = "...1",
         "odp" = "...2",
         "scenario" = "...3")


# =============================================================================
# ISP Processing Configuration
# =============================================================================

# 2020 ISP scenarios (used for folder-based processing)
isp_2020_scenarios <- c("Central",
                        "Fast Change",
                        "High DER",
                        "Slow Change",
                        "Step Change")


# Technology name standardization mapping
# Maps old/variant technology names to standardized names
technology_name_map <- c(
  "coordinated der storage" = "coordinated cer storage",
  "utility solar" = "utility-scale solar",
  "utility storage" = "utility-scale storage",
  "distributed storage" = "passive cer storage",
  "behind the meter storage" = "passive cer storage",
  "ccgt" = "mid-merit gas",
  "dispatchable storage" = "utility-scale storage",
  "peaking gas + liquids" = "peaking gas+liquids",
  "solar" = "utility-scale solar",
  "rooftop pv" = "distributed pv",
  "rooftop and other small-scale solar" = "distributed pv"
)


# Storage category name standardization mapping
storage_name_map <- c(
  "coordinated der storage" = "coordinated cer storage"
)


# Year substring positions for different ISP versions
# Used for extracting year from column names
isp_year_config <- tibble(
  isp_source = c("2018_final", "2020_final", "2022_final", "2024_final", "2026_draft"),
  year_start = c(6, 3, 3, 6, 6),
  year_end = c(7, 4, 4, 7, 7)
)


# ISP data file paths configuration
isp_paths_config <- tibble(
  isp_source = c("2018_final", "2020_final", "2022_final", "2024_final", "2026_draft"),
  base_path = c("raw-data/2018_final/2018 ISP Generation and Transmission Outlooks/",
                "raw-data/2020_final/",  # Special case - uses scenario subfolders
                "raw-data/2022_final/Final ISP Results/Scenarios/",
                "raw-data/2024_final/Core/",
                "raw-data/2026_draft/Cores/")
)


# ISP data type configurations
# Defines sheet names and cell ranges for each data type and ISP version
isp_data_config <- tribble(
  ~data_type,           ~isp_source,    ~sheet_name,         ~cell_range,      ~unit,        ~value_column,
  # Generation Capacity
  "generation_capacity", "2026_draft",   "Capacity",          "A3:AC5305",      "MW",         "technology",
  "generation_capacity", "2024_final",   "Capacity",          "A3:AG4604",      "MW",         "technology",
  "generation_capacity", "2022_final",   "Capacity",          "A3:AF782",       "MW",         "technology",
  "generation_capacity", "2020_final",   "Capacity_2",        "A3:X91",         "MW",         "technology",
  "generation_capacity", "2020_final_cf","Capacity_1",        "A3:X91",         "MW",         "technology",  # Counterfactual
  "generation_capacity", "2018_final",   "NEMInstalledCapacity Data", "A18:X73", "MW",      "technology",

  # Generation Output
  "generation_output",   "2026_draft",   "Generation",        "A3:AB6386",      "GWh",        "technology",
  "generation_output",   "2024_final",   "Generation",        "A3:AF5540",      "GWh",        "technology",
  "generation_output",   "2022_final",   "Generation",        "A3:AE932",       "GWh",        "technology",
  "generation_output",   "2020_final",   "Generation_2",      "A3:W123",        "GWh",        "technology",
  "generation_output",   "2020_final_cf","Generation_1",      "A3:W123",        "GWh",        "technology",  # Counterfactual
  "generation_output",   "2018_final",   "NEMEnergyGenerated Data", "A20:X85",  "GWh",        "technology",

  # Storage Capacity
  "storage_capacity",    "2026_draft",   "Storage Capacity",  "A3:AB2354",      "MW",         "storage_category",
  "storage_capacity",    "2024_final",   "Storage Capacity",  "A3:AF2082",      "MW",         "storage_category",
  "storage_capacity",    "2022_final",   "Storage Capacity",  "A3:AE312",       "MW",         "storage_category",
  "storage_capacity",    "2020_final",   "StorageCapacity_2", "A3:W48",         "MW",         "storage_category",
  "storage_capacity",    "2020_final_cf","StorageCapacity_1", "A3:W48",         "MW",         "storage_category",

  # Storage Output
  "storage_output",      "2026_draft",   "Storage Energy",    "A3:AB2342",      "GWh",        "storage_category",
  "storage_output",      "2024_final",   "Storage Energy",    "A3:AF2082",      "GWh",        "storage_category",
  "storage_output",      "2022_final",   "Storage Energy",    "A3:AE312",       "GWh",        "storage_category",
  "storage_output",      "2020_final",   "StorageGeneration_2", "A3:W48",       "GWh",        "storage_category",
  "storage_output",      "2020_final_cf","StorageGeneration_2", "A3:W48",       "GWh",        "storage_category",

  # Retirements
  "retirements",         "2026_draft",   "Retirements",       "A3:AA434",       "MW",         "station",
  "retirements",         "2024_final",   "Retirements",       "A3:AE470",       "MW",         "station",

  # Emissions
  "emissions",           "2026_draft",   "Emissions",         "A3:AA146",       "Mt CO2-e",   "region",
  "emissions",           "2024_final",   "Emissions",          "A3:AE158",       "Mt CO2-e",   "region",
  "emissions",           "2022_final",   "Emissions",          "A3:AE13",        "Mt CO2-e",   "region",
  "emissions",           "2020_final",   "Emissions_2",        "A3:W4",          "Mt CO2-e",   "region",
  "emissions",           "2020_final_cf","Emissions_1",        "A3:W123",        "Mt CO2-e",   "region"
)







# Storage table


storage_list <- c("shallow storage",
                  "medium storage",
                  "deep storage",
                  "behind the meter storage",
                  "snowy 2.0",
                  "coordinated cer storage",
                  "distributed storage",
                  "passive cer storage",
                  "borumba")


# For now - keep distributed and behind the meter the same as passive cer storage

colour_list <- c("#A4DBE8",  # shallow storage
                 "#77C5D5",  # medium storage
                 "#6BADBA",  # deep storage
                 "#DD9CDF",  # behind the meter storage
                 "#777DA7",  # snowy 2.0"
                 "#A3519B",  # coordinated cer storage
                 "#DD9CDF",  # distributed storage
                 "#DD9CDF",  # passive cer storage
                 "#8FA2D4")  # borumba  




dispatch_list <- c(TRUE,   # shallow storage
                   TRUE,   # medium storage
                   TRUE,   # deep storage
                   FALSE,  # behind the meter storage
                   TRUE,   # snowy 2.0"
                   TRUE,   # coordinated cer storage
                   FALSE,  # distributed storage
                   FALSE,  # passive cer storage
                   TRUE)   # borumba  



tech_type_list <- c("generic storage",
                  "generic storage",
                  "generic storage",
                  "bess",
                  "pumped hydro",
                  "bess",
                  "bess",
                  "bess",
                  "pumped hydro")


storage_util_table <- bind_cols(storage_list,
                        colour_list,
                        dispatch_list,
                        tech_type_list) |> 
  rename("storage_category" = "...1",
         "colour_label" = "...2",
         "dispatchable" = "...3",
         "tech_type_storage" = "...4")




###############################################################################
# Save lookup tables for Shiny app
###############################################################################

save(util_table, file = "shiny-webtool/data/util_table.rda")
save(odp_table, file = "shiny-webtool/data/odp_table.rda")
save(storage_util_table, file = "shiny-webtool/data/storage_util_table.rda")


###############################################################################
# Cleanup - remove temporary variables
# Keep configuration objects for use in processing scripts:
#   - isp_2020_scenarios
#   - technology_name_map
#   - storage_name_map
#   - isp_year_config
#   - isp_paths_config
#   - isp_data_config
#   - util_table
#   - odp_table
#   - storage_util_table
###############################################################################

rm(tech_list, tech_type_list, dispatch_list, colour_list)
rm(isp_list, odp_list, scenario_list)
rm(storage_list)

