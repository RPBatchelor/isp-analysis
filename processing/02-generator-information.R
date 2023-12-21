




# October 2023

raw <- read_xlsx(path = "raw-data/NEM Generation Information Oct 2023.xlsx",
                 sheet = "ExistingGeneration&NewDevs",
                 range = "A2:Y1433") |> 
  clean_names() |> 
  select(-c(closure_date,
            survey_id,
            aemo_kci_id,
            survey_last_requested,
            survey_version_date_time)) |> 
  rename(capacity_np_mw = nameplate_capacity_mw,
         capacity_lower_np_mw = lower_nameplate_capacity_mw,
         capacity_upper_np_mw = upper_nameplate_capacity_mw,
         capacity_lower_agg_np_mw = aggregated_lower_nameplate_capacity_mw,
         capacity_upper_agg_np_mw = aggregated_upper_nameplate_capacity_mw) |> 
  mutate(release_version = "October 2023",
         across(starts_with("capacity"), as.numeric))







region_name <- "VIC1"

test <- raw |> 
  filter(region == region_name) |>
  group_by(fuel_bucket_summary, status_bucket_summary) |> 
  summarise(capacity  = sum(capacity_upper_agg_np_mw)) |> 
  ungroup()

  
p <- test |> 
  ggplot() +
  geom_bar(aes(x = fuel_bucket_summary,
              y = capacity,
              fill = status_bucket_summary),
          position = "stack",
          stat = "identity")

p





