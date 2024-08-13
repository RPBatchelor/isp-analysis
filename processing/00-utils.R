


# Technology factors

tech_list <- c("black coal",
               "brown coal",
               "mid-merit gas",
               "mid-merit gas with ccs",
               "flexible gas",
               "flexible gas with ccs",
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
               "other renewable fuels")


colour_list <- c("#373A36",  # Black coal
                 "#94795D",  # Brown coal
                 "#008578",  # Mid-merit gas
                 "#00594F",  # Mid-merit gas with CCS
                 "#40C1AC",  # Flexible gas
                 "#0E6659",  # Flexible gas with CCS
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
                 "#5E8A48")   # Other renewable fuels 


dispatch_list <- c(TRUE,  # Black coal
                   TRUE,  # Brown coal
                   TRUE,  # Mid merit gas
                   TRUE,  # Mid merit gas with CCS
                   TRUE,  # Flexible gas
                   TRUE,  # Flexible gas with CCS
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
                   FALSE, # Utilitiy scale solar
                   FALSE, # Distributed PV
                   TRUE,  # Peaking gas+liquids
                   FALSE) # Other renewable fules



util_table <- bind_cols(tech_list,
                        colour_list,
                        dispatch_list) |> 
  rename("technology" = "...1",
         "colour_label" = "...2",
         "dispatchable" = "...3")



