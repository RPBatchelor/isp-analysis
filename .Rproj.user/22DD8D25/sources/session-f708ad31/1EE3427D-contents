


chart_generator_capacity <- function(cdp_scenario,
                                     region_name,
                                     isp_scenario){
  
  stopifnot(region %in% unique(isp_generator_capacity$region))
  stopifnot(cdp_scenario %in% unique(isp_generator_capacity$cdp))
  stopifnot(isp_scenario %in% unique(isp_generator_capacity$scenario))
  
  if(region_name == "NEM"){
    
    data <- isp_generator_capacity |> 
      filter(cdp == cdp_scenario,
             scenario == isp_scenario) |> 
      group_by(technology, year) |> 
      summarise(value = sum(value)) |> 
      ungroup() |> 
      mutate(region = "NEM")
    
  } else {
    
    data <- isp_generator_capacity |> 
      filter(region == region_name,
             cdp == cdp_scenario,
             scenario == isp_scenario)
  }
  
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
         caption = "Source: Draft ISP 2024",
         x = "Year",
         y = "Capacity (GW)") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(color = "gray", size = 0.1)
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"))
  
  return(p)
  
}
