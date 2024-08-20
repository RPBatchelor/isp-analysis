
shinyServer(function(input, output, session){
  
  #===== Dynamic listings ======================================================
  
  #----- Region filter -----
  updatePickerInput(session, "region", 
                    choices = unique(region_list$region), 
                    selected = "VIC")
  
  #----- 1. ISP source filter -----

  updateSelectInput(session, "source", 
                    choices = unique(source_scenario_pathway_list$source),
                    selected = "2024_final")
  
  
  #-----2. Scenario filter
  observeEvent(input$source, {
    filtered_scenarios <- source_scenario_pathway_list |> 
      filter(source == input$source) |> 
      pull(scenario) |> 
      unique()
    
    updateSelectInput(session, "scenario", 
                      choices = filtered_scenarios,
                      selected = "step change")
    
    # Clear the pathway dropdown when source changes
    updateSelectInput(session, "pathway", choices = NULL)
  })
  
  
  #----- 3. Pathway filter
  observeEvent(input$scenario, {
    filtered_pathways <- source_scenario_pathway_list |> 
      filter(source == input$source, scenario == input$scenario) |> 
      pull(cdp) |> 
      unique()
    
    updateSelectInput(session, "pathway", 
                      choices = filtered_pathways,
                      selected = "cdp14")
  })
  
  #-----4. Technology filter
  observeEvent(input$source, {
    filtered_technologies <- source_scenario_pathway_list |> 
      filter(source == input$source) |> 
      pull(technology) |> 
      unique()
    
    updatePickerInput(session, "technology", 
                      choices = filtered_technologies, 
                      selected = filtered_technologies)
    
    # updateSelectInput(session, "scenario", choices = NULL)
    # updateSelectInput(session, "pathway", choices = NULL)
  })
  
  
  
  #===== GENERATION CAPACITY ======================================================
  
  # Output generation capacity (MW) chart
 
   generation_capacity_plot <- reactive({
    
    d <- isp_generator_capacity |> 
      filter(source == input$source,
             scenario == input$scenario, 
             cdp == input$pathway,
             technology %in% input$technology,
             region %in% input$region) |> 
      left_join(util_table, by = c("technology" = "technology")) |> 
      mutate(technology = factor(technology, levels = util_table$technology))
    
    p <- d |> 
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
      scale_x_continuous(breaks = unique(d$year),
                         labels = unique(d$year)) +
      labs(fill = "Technology",
           title = glue("{input$region} generator capacity"),
           subtitle = glue("{input$scenario} scenario"),
           caption = glue("Source: {input$source}"),
           x = "Year (financial year ending 30-jun-YYYY)",
           y = "Capacity (GW)") +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))
    
    return(ggplotly(p, tooltip = c("text")) |> 
             plotly::config(displayModeBar = FALSE))
  })
  
  output$generation_capacity_plot <- renderPlotly(generation_capacity_plot())


  
  
  

  
  
  
  
})  # BRACE FOR SHINY SERVER FUNCTIOn 



