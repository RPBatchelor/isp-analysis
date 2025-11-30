

# View 2024 final data

cdps <- isp_generator_capacity |> 
  distinct(source, cdp)

p1 <- chart_generator_capacity(isp_source = "2024_final",
                               isp_scenario = "step change",
                               cdp_scenario = "cdp14",
                               region_name = "NEM",
                               dispatchable = T)
p1

ggplotly(p1)



gwalkr(isp_generator_capacity)




p1 <- chart_generator_capacity(isp_source = "2024_final",
                               isp_scenario = "step change",
                               cdp_scenario = "cdp14",
                               region_name = "NEM",
                               technology_type = c("wind", "black coal", "offshore wind"),
                               dispatchable = F)
p1

ggplotly(p1)





data <- isp_generator_capacity |> 
  filter(source %in% c("2024_final", "2022_final", "2020_final")) |> 
  mutate(year_scenario = str_extract(source, "\\d{4}"),
         year_scenario = paste0(year_scenario, "_", scenario)) |> 
  filter(year_scenario %in% c("2024_step change", "2022_step change", 
                              "2020_step change", "2020_central", "2020_slow change", "2020_fast change")) |> 
  left_join(odp_table, by = c("source" = "isp_source")) |> 
  filter(cdp == odp,
         year <= 2030) |>
  left_join(util_table, by = c("technology" = "technology")) |>
  filter(tech_type_cgr %in% c("renewable", "storage")) |> 
  group_by(year, year_scenario) |> 
  summarise(value = sum(value)) |>
  ungroup() 



p2 <- data |> 
  ggplot()+ 
  geom_line(aes(x = year, y = value / 1000, colour = year_scenario),
            size = 1.2) + 
  labs(x = "year",
       y = "capacity (mw)")

p2





# View of 2024 generation output

p3 <- chart_generation_output(isp_source = "2024_final",
                              isp_scenario = "step change",
                              cdp_scenario = "cdp14",
                              region_name = "NSW")

p3

ggplotly(p3)


a <- isp_generation_output |> 
  filter(source == "2024_final",
         scenario == "step change",
         cdp == "cdp14",
         region == "NSW") |> 
  left_join(util_table, by = c("technology" = "technology")) |> 
  mutate(technology = factor(technology, levels = util_table$technology)) |> 
  mutate(gen_load = if_else(str_detect(technology, "load"), 
                            "load", 
                            "generation")) |> 
  ggplot() +
  geom_bar(aes(x = year, 
               y = value,
               fill = reorder(technology, -as.numeric(technology)),
               alpha = gen_load),
           position = "stack",
           stat = "identity",
           show.legend = TRUE)+ 
  scale_y_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) + 
  scale_alpha_manual(values = c("generation" = 1, "load" = 0.5)) + # Adjust alpha for load
  scale_x_continuous(breaks = unique(isp_generation_output$year),
                     labels = unique(isp_generation_output$year))

a




a <- isp_generator_capacity |> 
  filter(source == "2024_final",
         scenario == "step change",
         cdp == "cdp14",
         region == "NSW") |> 
  left_join(util_table, by = c("technology" = "technology")) |> 
  mutate(technology = factor(technology, levels = util_table$technology)) |> 
  group_by(technology) |> 
  arrange(year) |> 
  mutate(net_capacity_added = value - lag(value)) |> 
  ungroup()

p <- a |> 
  ggplot()+
  geom_bar(aes(x = year, 
               y = net_capacity_added,
               fill = reorder(technology, -as.numeric(technology))),
               position = "stack",
               stat = "identity",
               show.legend = TRUE) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) + 
  scale_alpha_manual(values = c("generation" = 1, "load" = 0.5)) + # Adjust alpha for load
  scale_x_continuous(breaks = unique(isp_generation_output$year),
                     labels = unique(isp_generation_output$year))

p




























# Calculate the share of renewables (like the chart)

a <- isp_generation_output |> 
  filter(source == "2024_final",
         scenario == "step change",
         cdp == "cdp14",
         region == "NSW") |> 
  left_join(util_table, by = c("technology" = "technology")) |> 
  mutate(technology = factor(technology, levels = util_table$technology)) |> 
  mutate(gen_load = if_else(str_detect(technology, "load"), 
                            "load", 
                            "generation")) |> 
  group_by(year) |> 
  summarise(share_of_re = sum(value[tech_type_cgr == "renewable"]) / 
              sum(value[tech_type_cgr %in% c("renewable", "coal", "gas")])) |> 
  ungroup()
  






group_by(year) %>%
  summarise(
    total_output = sum(output),
    renewable_output = sum(output[tech_type_cgr == "renewable"]),
    renewable_percentage = (renewable_output / total_output) * 100
  )



isp_generation_output |> 
  select(source, technology) |>
  unique() |> 
  filter(str_detect(technology, "load")) |> 
  view()



  
  

a <- isp_generator_capacity |> 
  filter(region == "NSW",
         source == "2024_final",
         cdp == "cdp14", 
         scenario == "step change") |> 
  left_join(util_table, by = c("technology" = "technology")) |> 
  mutate(re_ff = case_when(tech_type_cgr %in% c("coal", "gas") ~ "fossil",
                           .default = tech_type_cgr)) |> 
  mutate(re_ff = factor(re_ff, levels = c("fossil", "renewable", "storage", "dsp"))) |> 
  group_by(year, re_ff) |> 
  summarise(value = sum(value)) |> 
  ungroup() |> 
  mutate(value = value/1000)


p <- a |> 
  ggplot() +
  geom_area(aes(x = year, y = value, fill = reorder(re_ff, -as.numeric(re_ff))))


p

  
  
  
  
  
  
  






p1 <- chart_generator_capacity(cdp_scenario = "CDP11",
                               region_name = "VIC",
                               isp_scenario = "step change",
                               isp_source = "ISP 2024 - Final")

p1


p2 <- chart_generator_capacity(cdp_scenario = "CDP11",
                              region_name = "NEM",
                              isp_scenario = "step change",
                              isp_source = "ISP 2024 - Final")

p2



p3 <- (p1 + p2) 
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














cdp_scenario <- "CDP11"
region_name <- "VIC"
isp_scenario <- "step change"
isp_source <- "ISP 2024 - Final"

  
  data <- isp_generator_capacity |>
    filter(cdp == cdp_scenario,
           scenario == isp_scenario,
           source == isp_source) |> 
    group_by(technology, year, year_ending) |> 
    summarise(value = sum(value)) |> 
    ungroup() |> 
    mutate(region = "NEM")

  
  stopifnot(region_name %in% unique(isp_generator_capacity$region))
  
  data <- isp_generator_capacity |> 
    filter(region == region_name,
           cdp == cdp_scenario,
           scenario == isp_scenario,
           source == isp_source)


p <- data |> 
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
  scale_x_continuous(breaks = unique(isp_generator_capacity$year),
                     labels = unique(isp_generator_capacity$year)) +
  labs(fill = "Technology",
       title = glue("{region_name} generator capacity"),
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




















