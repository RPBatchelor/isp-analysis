


chart_generation_output <- function(cdp_scenario,
                                    region_name = "NEM",
                                    isp_scenario,
                                    isp_source){
  
  stopifnot(cdp_scenario %in% unique(isp_generation_output$cdp))
  stopifnot(isp_scenario %in% unique(isp_generation_output$scenario))
  stopifnot(isp_source %in% unique(isp_generation_output$source))
  
  if(region_name == "NEM"){
    
    data <- isp_generation_output |> 
      filter(cdp == cdp_scenario,
             scenario == isp_scenario,
             source == isp_source) |> 
      group_by(technology, year, year_ending) |> 
      summarise(value = sum(value)) |> 
      ungroup() |> 
      mutate(region = "NEM")
    
  } else {
    
    stopifnot(region_name %in% unique(isp_generation_output$region))
    
    data <- isp_generation_output |> 
      filter(region == region_name,
             cdp == cdp_scenario,
             scenario == isp_scenario,
             source == isp_source) 
  }
  
  p <- data |> 
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
             show.legend = TRUE) +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_y_continuous(labels = label_number(scale = 1e-3)) +
    scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) + 
    scale_alpha_manual(values = c("generation" = 1, "load" = 0.5)) + # Adjust alpha for load
    scale_x_continuous(breaks = unique(isp_generation_output$year),
                       labels = unique(isp_generation_output$year)) +
    labs(fill = "Technology",
         alpha = "Generation / load",
         title = glue("{region_name} generation output"),
         subtitle = glue("{isp_scenario} scenario"),
         caption = glue("Source: {isp_source}"),
         x = "Year (financial year ending 30-jun-YYYY)",
         y = "Output (TWh)") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))
  

  return(p)
  
}






