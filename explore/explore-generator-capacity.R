



p1 <- chart_generator_capacity(cdp_scenario = "CDP11 (ODP)",
                               region_name = "VIC",
                               isp_scenario = "step_change")

p1


p2 <- chart_generator_capacity(cdp_scenario = "CDP11 (ODP)",
                              region_name = "NEM",
                              isp_scenario = "step_change")

p2



p3 <- p1 + p2
p3




# Facet all states
cdp_scenario <-  "CDP11 (ODP)"
isp_scenario <-  "step_change"


p4 <- isp_generator_capacity |> 
  filter(cdp == cdp_scenario,
         scenario == isp_scenario) |> 
  left_join(util_table, by = c("technology" = "technology")) |> 
  mutate(technology = factor(technology, levels = tech_list)) |> 
  ggplot() + 
  geom_bar(aes(x = year, 
               y = value, 
               fill = reorder(technology, -as.numeric(technology))),
           position = "stack",
           stat = "identity",
           show.legend = TRUE) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) + 
  labs(fill = "Technology",
       title = glue("NEM generator capacity"),
       subtitle = glue("{isp_scenario} scenario"),
       caption = "Source: Draft ISP 2024",
       x = "Year",
       y = "Capacity (GW)") +
  facet_wrap(~region)

p4





# Coal capacity

p5 <- isp_generator_capacity |> 
  filter(cdp == cdp_scenario,
         scenario == isp_scenario) |> 
  filter(technology %in% c("Black Coal",
                           "Brown Coal")) |> 
  ggplot() +
  geom_bar(aes(x = year,
               y = value,
               fill = region),
           position = "stack",
           stat = "identity") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = label_number(scale = 1e-3))

p5
