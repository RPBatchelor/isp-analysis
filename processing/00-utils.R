


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
               "peaking gas+liquids")


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
                 "#3FB9C4")  # Peaking gas+liquids    


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
                   TRUE)  # Peaking gas+liquids 


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
                    "gas")       # Peaking gas+liquids



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
              "2024_final")

odp_list <- list <- c("default",
                 "dp1",
                 "cdp8",
                 "cdp14")

scenario_list <- c("neutral scenario",
              "central",
              "step change",
              "step change")


odp_table <- bind_cols(isp_list,
                       odp_list,
                       scenario_list) |> 
  rename("isp_source" = "...1",
         "odp" = "...2",
         "scenario" = "...3")




rm(tech_list, tech_type_list, dispatch_list, colour_list)
rm(isp_list, odp_list, scenario_list)

