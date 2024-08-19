


source_to_compare <- c("2024_final",
                       "2022_final")

scenario_to_compare <- c("step change",
                         "step change")

cdp_to_compare <- c("cdp14",
                    "cdp8")



data <- isp_generation_output |> 
  filter(region == "NSW") |> 
  filter((source == source_to_compare[1] & 
           scenario == scenario_to_compare[1] & 
           cdp == cdp_to_compare[1]) | 
           (source == source_to_compare[2] & 
              scenario == scenario_to_compare[2] & 
              cdp == cdp_to_compare[2])) |> 
  select(source, technology, year, year_ending, value) |> 
  pivot_wider(names_from = source, values_from = value) |> 
  mutate(delta = .data[[source_to_compare[1]]] - .data[[source_to_compare[2]]]) |> 
  left_join(util_table, by = c("technology" = "technology"))









# Step 1: Filter data for the relevant scenarios and CDP
filtered_df <- df %>%
  filter(scenario == "Scenario A", cdp == "cdp14")

# Step 2: Pivot wider to create separate columns for each source's values
wide_df <- filtered_df %>%
  pivot_wider(names_from = source, values_from = value)

# Step 3: Calculate the delta between the sources
delta_df <- wide_df %>%
  mutate(delta = `Source 1` - `Source 2`)

# Step 4: Select relevant columns to display the result
delta_df <- delta_df %>%
  select(year, technology, delta)

# Display the result
print(delta_df)