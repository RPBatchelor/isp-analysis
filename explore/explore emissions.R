



data <- isp_emissions |> 
  filter(source == "2024_final",
         scenario == "step change",
         odp == TRUE) #|> 
  # group_by(year_ending) |> 
  # summarise(value = sum(value)) |> 
  # ungroup()


p <- data |> 
  ggplot(aes(x = year_ending, y = value)) + 
  geom_area(aes(fill = region))

p
