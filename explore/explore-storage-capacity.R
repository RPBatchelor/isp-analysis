





cdp_scenario <- "cdp14"
region_name <- "VIC"
isp_scenario <- "step change"
isp_source <- "2024_final"


data <- isp_storage_capacity |> 
  filter(source == isp_source,
         scenario == isp_scenario,
         cdp == cdp_scenario)

if(region_name != "NEM"){
  data <- data |> 
    filter(region == region_name)
}


data <- data |> 
  group_by(storage_category, year, year_ending) |> 
  summarise(value = sum(value)) |> 
  ungroup() |> 
  left_join(storage_util_table, by = c("storage_category" = "storage_category")) |> 
  mutate(storage_category = factor(storage_category, levels = storage_util_table$storage_category))



p <- data |> 
  ggplot() +
  geom_bar(aes(x = year,
               y = value,
               fill = reorder(storage_category, -as.numeric(storage_category))),
           position = "stack",
           stat = "identity",
           show.legend = TRUE) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  scale_fill_manual(values = setNames(storage_util_table$colour_label, storage_util_table$storage_category)) + 
  scale_x_continuous(breaks = unique(isp_storage_capacity$year),
                     labels = unique(isp_storage_capacity$year)) +
  labs(fill = "storage category",
       title = glue("{region_name} storage capacity"),
       subtitle = glue("{isp_scenario} scenario"),
       caption = glue("Source: {isp_source}"),
       x = "Year",
       y = "Capacity (GW)") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

p







data <- isp_storage_capacity |> 
  filter(source == isp_source,
         scenario == isp_scenario,
         cdp == cdp_scenario)

if(region_name != "NEM"){
  data <- data |> 
    filter(region == region_name)
}



region_name <- "NEM"


data <- isp_storage_capacity |>
  filter(source == isp_source,
         scenario == isp_scenario,
         cdp == cdp_scenario)

if(region_name != "NEM"){
  data <- data |> 
    filter(region == region_name)
}


d <- data |> 
  left_join(storage_util_table, by = c("storage_category" = "storage_category")) |>
  mutate(storage_category = factor(storage_category, levels = storage_util_table$storage_category)) |> 
  group_by(storage_category, year) |> 
  summarise(value = sum(value)) |> 
  arrange(year) |> 
  mutate(net_capacity_added = value - lag(value)) |> 
  ungroup()



p <- d |> 
  ggplot() +
  geom_bar(aes(x = year, 
               y = net_capacity_added, 
               fill = reorder(storage_category, -as.numeric(storage_category))),
           position = "stack",
           stat = "identity",
           show.legend = TRUE) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = label_number(scale = 1)) +
  scale_fill_manual(values = setNames(storage_util_table$colour_label, storage_util_table$storage_category)) + 
  scale_x_continuous(breaks = unique(d$year),
                     labels = unique(d$year)) +
  labs(fill = "Storage category",
       # title = glue("Generator capacity net change per year"),
       subtitle = glue("{cdp_scenario} scenario"),
       caption = glue("Source: {isp_source}"),
       x = "Year (financial year ending 30-jun-YYYY)",
       y = "Capacity change (MW)") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))

p











