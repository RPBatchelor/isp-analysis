

base_year <- 2010

start_date <- as.POSIXct(paste0(base_year - 1, "-07-01 00:00:00"), tz = nem_tz)
end_date   <- as.POSIXct("2025-06-30 23:59:59", tz = nem_tz)



# Download power data
nem_power_historical <- oe_get_network_data(network_code = "NEM", 
                                interval = "1M", 
                                metrics = "power", 
                                date_start = start_date,
                                date_end = end_date, 
                                primary_grouping = "network_region",
                                secondary_grouping = "fueltech",
                                split_confirm = FALSE)


# Download energy data
nem_energy_historical <- oe_get_network_data(network_code = "NEM", 
                                             interval = "1M", 
                                             metrics = "energy", 
                                             date_start = start_date,
                                             date_end = end_date, 
                                             primary_grouping = "network_region",
                                             secondary_grouping = "fueltech",
                                             split_confirm = FALSE)


# Download emissions data
nem_emissions_historical <- oe_get_network_data(network_code = "NEM", 
                                                interval = "1M", 
                                                metrics = "emissions", 
                                                date_start = start_date,
                                                date_end = end_date, 
                                                primary_grouping = "network_region",
                                                secondary_grouping = "fueltech",
                                                split_confirm = FALSE)


# Download demand (power) data
nem_demand_power_historical <- oe_get_network_market_data(network_code = "NEM", 
                                                interval = "1M", 
                                                metrics = "demand", 
                                                date_start = start_date,
                                                date_end = end_date, 
                                                primary_grouping = "network_region",
                                                split_confirm = FALSE)


# Download demand (energy) data
nem_demand_energy_historical <- oe_get_network_market_data(network_code = "NEM", 
                                                          interval = "1M", 
                                                          metrics = "demand_energy", 
                                                          date_start = start_date,
                                                          date_end = end_date, 
                                                          primary_grouping = "network_region",
                                                          split_confirm = FALSE)


# Download price
nem_price_historical <- oe_get_network_market_data(network_code = "NEM", 
                                                           interval = "1M", 
                                                           metrics = "price", 
                                                           date_start = start_date,
                                                           date_end = end_date, 
                                                           primary_grouping = "network_region",
                                                           split_confirm = FALSE)



 # ====== Compile


nem_historical_network_data <- bind_rows(nem_power_historical,
                                         nem_energy_historical,
                                         nem_emissions_historical) |> 
  rename(period_start = datetime)

save_isp_data(nem_historical_network_data, "nem_historical_network_data")



nem_historical_market_data <- bind_rows(nem_demand_power_historical,
                                        nem_demand_energy_historical,
                                        nem_price_historical) |> 
  rename(period_start = datetime)

save_isp_data(nem_historical_market_data, "nem_historical_market_data")



# ====== Cleanup

rm(nem_demand_energy_historical,
   nem_demand_power_historical,
   nem_emissions_historical,
   nem_energy_historical,
   nem_power_historical,
   base_year,
   start_date,
   end_date)






