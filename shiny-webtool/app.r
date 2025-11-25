################################################################################
#
# ISP Analysis
# Shiny app
# 
# Ryan Batchelor
# 23 November 2025
#
################################################################################


# ===== 01. Load the necessary libraries =======================================

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)
library(bslib)
library(thematic)
library(scales)
library(glue)
library(yaml)



# ===== 02. Setup (settings, load files etc)===================================


# Load all the RDA data files
data_files <- list.files("./data", full.names = TRUE,
                         pattern = "\\.rda$")

for (d in data_files){
  load(d)
}

# Load all R files
r_files <- list.files("./R", full.names = TRUE,
                      pattern = "\\.[Rr]")
for (f in r_files){
  source(f)
}



# Load brand configuration
brand_config <- yaml::read_yaml("./brand/_brand.yml")
brand_colors <- brand_config$color$palette

# Helper function to darken colors for borders
darken_color <- function(hex_color, factor = 0.7) {
  # Remove # if present
  hex_color <- gsub("#", "", hex_color)
  
  # Convert to RGB
  r <- as.integer(paste0("0x", substr(hex_color, 1, 2)))
  g <- as.integer(paste0("0x", substr(hex_color, 3, 4)))
  b <- as.integer(paste0("0x", substr(hex_color, 5, 6)))
  
  # Darken by factor
  r <- as.integer(r * factor)
  g <- as.integer(g * factor)
  b <- as.integer(b * factor)
  
  # Convert back to hex
  sprintf("#%02X%02X%02X", r, g, b)
}



# ===== 03. UI =================================================================


ui <- page_navbar(
  id = "main_navbar",
  title = "ISP Analyser",
  theme = bs_theme(brand = "brand/_brand.yml"),
  
  
  nav_panel(
    title = "Technology view",
    layout_sidebar(
      sidebar = sidebar(
        h4("ISP source data"),
        
        selectInput("source",
                    "Select ISP source",
                    choices = NULL,
                    multiple = FALSE,
                    selected = NULL),
        
        selectInput("scenario",
                    "Select scenario",
                    choices = NULL,
                    multiple = FALSE,
                    selected = NULL),
        
        selectInput("pathway",
                    "Select pathway",
                    choices = NULL,
                    multiple = FALSE,
                    selected = NULL),
        
        checkboxInput("show-only-odp",
                      "Use only optimal development paths",
                      value = FALSE),
        
        h4("Chart inputs"),

        pickerInput("region",
                    "Select region",
                    choices = NULL,
                    multiple = TRUE,
                    selected = c("VIC"),
                    options = list(`actions-box` = TRUE)),

        # Conditional filter for generation tabs (tabs 1-3)
        conditionalPanel(
          condition = "input.technology_tabs == 'Generation capacity (GW)' || input.technology_tabs == 'Generation output (GWh)' || input.technology_tabs == 'Generation net additions'",
          pickerInput("technology",
                      "Select technology type(s)",
                      choices = NULL,
                      multiple = TRUE,
                      selected = NULL,
                      options = list(`actions-box` = TRUE))
        ),

        # Conditional filter for storage tabs (tabs 4-6)
        conditionalPanel(
          condition = "input.technology_tabs == 'Storage capacity (GW)' || input.technology_tabs == 'Storage output (GWh)' || input.technology_tabs == 'Storage net additions (MW)'",
          pickerInput("storage_type",
                      "Select storage type(s)",
                      choices = NULL,
                      multiple = TRUE,
                      selected = NULL,
                      options = list(`actions-box` = TRUE))
        ),

        virtualSelectInput("show_simple_tech",
                           "Select technology display options", 
                           choices = c("Simplified",
                                       "Detailed",
                                       "Re-Fo-St"),
                           width = "100%",
                           dropboxWrapper = "body"),

        checkboxInput("show_dispatchable",
                      "Show dispatchable capacity",
                      value = FALSE),

        checkboxInput("show_total_capacity",
                      "Show total capacity",
                      value = FALSE),
        
        h4("Download data"),

        actionBttn("download_chart_data",
                   "Download chart data",
                   style = "simple",
                   color = "primary",
                   icon = icon("download"),
                   size = "sm"),
        
        actionBttn("download_chart_image",
                   "Download chart image",
                   style = "simple",
                   color = "primary",
                   icon = icon("file-image"),
                   size = "sm"),

        width = 300
      ),
      
      navset_card_tab(
        id = "technology_tabs",
        height = "calc(100vh - 120px)",

        nav_panel(
          title = "Generation capacity (GW)",
          card(
            card_header("Total generation capacity (GW) per year, by technology type"),
            card_body(
              plotlyOutput("generation_capacity_plot", height = "500px")
            ),
            full_screen = TRUE,
            height = "100%"
          )
        ),

        nav_panel(
          title = "Generation output (GWh)",
          card(
            card_header("Total generation output (GWh) per year, by technology type"),
            card_body(
              plotlyOutput("generation_output_plot", height = "500px")
            ),
            full_screen = TRUE,
            height = "100%"
          )
        ),

        nav_panel(
          title = "Generation net additions",
          card(
            card_header("Generation capacity net change per year (MW) (additions / retirements)"),
            card_body(
              plotlyOutput("generation_capacity_growth_plot", height = "500px")
            ),
            full_screen = TRUE,
            height = "100%"
          )
        ),
        
        nav_panel(
          title = "Storage capacity (GW)",
          card(
            card_header("Total storage capacity (GW)"),
            card_body(
              plotlyOutput("storage_capacity_plot", height = "500px")
            ),
            full_screen = TRUE,
            height = "100%"
          )
        ),
        
        nav_panel(
          title = "Storage output (GWh)",
          card(
            card_header("Total storage output (GWh)"),
            card_body(
              plotlyOutput("storage_output_plot", height = "500px")
            ),
            full_screen = TRUE,
            height = "100%"
          )
        ),
        
        nav_panel(
          title = "Storage net additions (MW)",
          card(
            card_header("Storage net change per year (MW)"),
            card_body(
              plotlyOutput("storage_capacity_growth_plot", height = "500px")
            ),
            full_screen = TRUE,
            height = "100%"
          )
        ),
        
      )
    )
  ),
  
  
  nav_panel(
    title = "Storage capacity & output",
    navset_card_tab(
      height = "calc(100vh - 120px)",
      nav_panel(
        title = "Storage capacity (MW)",
        layout_sidebar(
          sidebar = sidebar(
            "SIDEBAR",
            width = 300
          ),
          card(
            card_header("Storage output (GWh)"),
            card_body(
              "CARD BODY"
            ),
            full_screen = TRUE,
            height = "100%"
          )
        )
      ),
      
      nav_panel(
        title = "Generation output (GWh)",
        layout_sidebar(
          sidebar = sidebar(
            "SIDEBAR",
            width = 300
          ),
          card(
            card_header("Generation output (GWh)"),
            card_body(
              "Card body"
            ),
            full_screen = TRUE,
            height = "100%"
          )
        )
      )
 
    )
  ),
  
  
  nav_panel(
    title = "Compare scenarios",
    page_sidebar(
      sidebar = sidebar(
        "SIDE BAR"
      ),
      card(
        card_header("Compare scenarios"),
        card_body(
          "CARD BODY"
        ),
        full_screen = TRUE
      )
    )
  ),
  
  # Insert spacer
  nav_spacer(),
  
  
  nav_panel(
    title = "Settings",
    card(
      card_header("Settings page"),
      card_body(
        "CARD BODY"
      )
    )
  )
) # END OF UI



# ===== 04. SERVER =============================================================

server <- function(input, output, session){
  
  # ----- 01. Dynamic listings -------------------------------------------------
  
  #----- A. Region filter -----
  updatePickerInput(session, "region", 
                    choices = unique(region_list$region), 
                    selected = "VIC")
  
  #----- B. ISP source filter -----
  
  updateSelectInput(session, "source", 
                    choices = unique(source_scenario_pathway_list$source),
                    selected = "2024_final")
  
  
  #-----C. Scenario filter
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
  
  
  #----- D. Pathway filter
  observeEvent(c(input$scenario, input$`show-only-odp`), {
    # Get all pathways for the selected source and scenario
    filtered_pathways <- source_scenario_pathway_list |>
      filter(source == input$source, scenario == input$scenario) |>
      pull(cdp) |>
      unique()

    # If show-only-odp is checked, filter to only the ODP pathway for this ISP source
    if(isTRUE(input$`show-only-odp`)) {
      # Get the ODP pathway for this ISP source (independent of scenario)
      odp_pathway <- odp_table |>
        filter(isp_source == input$source) |>
        pull(odp)

      # Filter to only include the ODP pathway
      if(length(odp_pathway) > 0) {
        filtered_pathways <- intersect(filtered_pathways, odp_pathway)
      }
    }

    # Sort pathways in natural order (cdp1, cdp2, ..., cdp10, cdp11, etc.)
    filtered_pathways <- stringr::str_sort(filtered_pathways, numeric = TRUE)

    # Select the first available pathway (or ODP if filtered)
    selected_pathway <- if(length(filtered_pathways) > 0) filtered_pathways[1] else NULL

    updateSelectInput(session, "pathway",
                      choices = filtered_pathways,
                      selected = selected_pathway)
  })
  
  #-----E. Technology filter (for generation tabs)
  observeEvent(input$source, {
    filtered_technologies <- source_scenario_pathway_list |>
      filter(source == input$source) |>
      pull(technology) |>
      unique()

    updatePickerInput(session, "technology",
                      choices = filtered_technologies,
                      selected = filtered_technologies)
  })

  #-----F. Storage type filter (for storage tabs)
  observeEvent(input$source, {
    filtered_storage_types <- isp_storage_capacity |>
      filter(source == input$source) |>
      pull(storage_category) |>
      unique()

    updatePickerInput(session, "storage_type",
                      choices = filtered_storage_types,
                      selected = filtered_storage_types)
  })
  
  
  

  # Download button - show message that feature is not yet implemented
  observeEvent(input$download_chart_data, {

    # Get information about the currently active tab
    tab_info <- get_active_tab_info(input)

    # Create description based on chart type
    # Handle NULL or missing chart_type
    if (is.null(tab_info$chart_type) || tab_info$chart_type == "") {
      data_description <- "No data available for this tab"
    } else {
      data_description <- switch(
        tab_info$chart_type,
        "generation_capacity" = "generation capacity data (GW) by technology and year",
        "generation_output" = "generation output data (GWh) by technology and year",
        "generation_net_additions" = "net capacity additions/retirements (MW) by technology and year",
        "storage_capacity" = "storage capacity data (GW) by technology and year",
        "storage_output" = "storage output data (GWh) by technology and year",
        "storage_net_additions" = "net storage capacity additions (MW) by technology and year",
        "No data available for this tab"  # default case
      )
    }

    # Build the message
    message_text <- if (!is.null(tab_info$sub_tab)) {
      paste0(
        "You have selected to download data from:\n\n",
        "Main tab: ", tab_info$main_tab, "\n",
        "Sub-tab: ", tab_info$sub_tab, "\n",
        "Chart type: ", ifelse(is.null(tab_info$chart_type), "NULL", tab_info$chart_type), "\n\n",
        "This would generate a data download for: ", data_description
      )
    } else {
      paste0(
        "You have selected to download data from:\n\n",
        "Main tab: ", tab_info$main_tab, "\n",
        "Chart type: ", ifelse(is.null(tab_info$chart_type), "NULL", tab_info$chart_type), "\n\n",
        "This would generate a data download for: ", data_description
      )
    }

    showModal(modalDialog(
      title = "Feature Not Available",
      message_text,
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  

  # =---- 02. Generator capacity chart -----------------------------------------
  
  #Reactive data for the generation capacity chart
  chart_data_gen_capacity <- reactive({
    req(input$source, input$scenario, input$pathway, input$technology, input$region)

    isp_generator_capacity |>
      filter(source == input$source,
             scenario == input$scenario,
             cdp == input$pathway,
             technology %in% input$technology,
             region %in% input$region) |>
      left_join(util_table, by = c("technology" = "technology")) |>
      mutate(technology = factor(technology, levels = util_table$technology)) |>
      mutate(value_gw = value/1000)
  })


  # Generate chart

  generation_capacity_plot <- reactive({

    d <- chart_data_gen_capacity()
    
    p <- d |>
      ggplot() +
      geom_bar(aes(x = year,
                   y = value_gw,
                   fill = reorder(technology, -as.numeric(technology))),
               position = "stack",
               stat = "identity",
               show.legend = TRUE) +
      scale_y_continuous(labels = scales::label_comma()) +
      scale_y_continuous(labels = label_number(scale = 1)) +
      scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) +
      scale_x_continuous(breaks = unique(d$year),
                         labels = unique(d$year)) +
      labs(fill = "Technology",
           # title = glue("Region generator capacity"),
           subtitle = glue("{input$scenario} scenario"),
           caption = glue("Source: {input$source}"),
           x = "Year (financial year ending 30-jun-YYYY)",
           y = "Capacity (GW)") +
      theme_minimal(base_family = "Arial") +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))

    if(input$show_dispatchable == T){
      d2 <- d |>
        filter(dispatchable == T) |>
        group_by(year) |>
        summarise(value_gw = sum(value_gw)) |>
        ungroup()

      p <- p +
        geom_line(data = d2,
                  aes(x = year, y = value_gw),
                  colour = "red4",
                  linetype = "dashed",
                  linewidth = 0.8,
                  show.legend = FALSE)
    }

    if(input$show_total_capacity == T){
      d3 <- d |>
        group_by(year) |>
        summarise(value_gw = sum(value_gw)) |>
        ungroup()

      p <- p +
        geom_text(data = d3,
                  aes(x = year,
                      y = value_gw,
                      label = scales::comma(round(value_gw, 1))),
                  nudge_y = 4, size = 3, colour = "gray30")

    }

    return(ggplotly(p, tooltip = c("value_gw")) |>
             plotly::config(displayModeBar = F))
    
    
  })

  output$generation_capacity_plot <- renderPlotly(generation_capacity_plot())



  # =---- 03. Generator output chart -----------------------------------------
  
  #Reactive data for the generation capacity chart
  chart_data_gen_output <- reactive({
    req(input$source, input$scenario, input$pathway, input$technology, input$region)
    
    isp_generation_output |>
      filter(source == input$source,
             scenario == input$scenario,
             cdp == input$pathway,
             technology %in% input$technology,
             region %in% input$region) |>
      left_join(util_table, by = c("technology" = "technology")) |>
      mutate(technology = factor(technology, levels = util_table$technology)) 
  })
  
  
  # Generate chart
  
  generation_output_plot <- reactive({
    
    d <- chart_data_gen_output()
    
    p <- d |>
      ggplot() +
      geom_bar(aes(x = year,
                   y = value,
                   fill = reorder(technology, -as.numeric(technology))),
               position = "stack",
               stat = "identity",
               show.legend = TRUE) +
      scale_y_continuous(labels = scales::label_comma()) +
      scale_y_continuous(labels = label_number(scale = 1)) +
      scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) +
      scale_x_continuous(breaks = unique(d$year),
                         labels = unique(d$year)) +
      labs(fill = "Technology",
           # title = glue("Region generator capacity"),
           subtitle = glue("{input$scenario} scenario"),
           caption = glue("Source: {input$source}"),
           x = "Year (financial year ending 30-jun-YYYY)",
           y = "Output (GWh)") +
      theme_minimal(base_family = "Arial") +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))
    
    if(input$show_dispatchable == T){
      d2 <- d |>
        filter(dispatchable == T) |>
        group_by(year) |>
        summarise(value = sum(value)) |>
        ungroup()
      
      p <- p +
        geom_line(data = d2,
                  aes(x = year, y = value),
                  colour = "red4",
                  linetype = "dashed",
                  linewidth = 0.8,
                  show.legend = FALSE)
    }
    
    if(input$show_total_capacity == T){
      d3 <- d |>
        group_by(year) |>
        summarise(value = sum(value)) |>
        ungroup()
      
      p <- p +
        geom_text(data = d3,
                  aes(x = year,
                      y = value,
                      label = scales::comma(round(value, 1))),
                  nudge_y = 4, size = 3, colour = "gray30")
      
    }
    
    return(ggplotly(p, tooltip = c("value")) |>
             plotly::config(displayModeBar = F))
    
    
  })
  
  output$generation_output_plot <- renderPlotly(generation_output_plot())
  
  
  # =---- 04. Generator capacity growth (net additions/ subtractions) chart ----
  
  #Reactive data for the generation capacity chart
  chart_data_gen_capacity_change <- reactive({
    req(input$source, input$scenario, input$pathway, input$technology, input$region)
    
    isp_generator_capacity |>
      filter(source == input$source,
             scenario == input$scenario,
             cdp == input$pathway,
             technology %in% input$technology,
             region %in% input$region) |>
      left_join(util_table, by = c("technology" = "technology")) |>
      mutate(technology = factor(technology, levels = util_table$technology)) |> 
      group_by(technology, year) |> 
      summarise(value = sum(value)) |> 
      arrange(year) |> 
      mutate(net_capacity_added = value - lag(value)) |> 
      ungroup()
  })
  
  
  generation_capacity_growth_plot <- reactive({
    
    d <- chart_data_gen_capacity_change()
    
    p <- d |> 
      ggplot() +
      geom_bar(aes(x = year, 
                   y = net_capacity_added, 
                   fill = reorder(technology, -as.numeric(technology))),
               position = "stack",
               stat = "identity",
               show.legend = TRUE) +
      scale_y_continuous(labels = scales::label_comma()) +
      scale_y_continuous(labels = label_number(scale = 1)) +
      scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) + 
      scale_x_continuous(breaks = unique(d$year),
                         labels = unique(d$year)) +
      labs(fill = "Technology",
           # title = glue("Generator capacity net change per year"),
           subtitle = glue("{input$scenario} scenario"),
           caption = glue("Source: {input$source}"),
           x = "Year (financial year ending 30-jun-YYYY)",
           y = "Capacity (MW)") +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))
    
    
    return(ggplotly(p, tooltip = c("net_capacity_added")) |>
             plotly::config(displayModeBar = F))
    
  })

  output$generation_capacity_growth_plot <- renderPlotly(generation_capacity_growth_plot())
  
  

}




# ===== 05. RUN APP ============================================================

shinyApp(ui = ui, server = server)

















