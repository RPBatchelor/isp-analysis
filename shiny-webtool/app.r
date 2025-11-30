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


ui <- tagList(
  # Header section - full width row
  div(
    class = "bg-primary text-white",
    style = "padding: 1.5rem 2rem ",
    # style = "padding: 1.5rem 2rem; border-bottom: 2px solid white;",
    h1("ISP Explorer",
       style = "margin: 0; font-size: 1.8rem; font-weight: 600; text-align: left; color: white;")
  ),

  # Navigation bar - second row
  page_navbar(
    id = "main_navbar",
    title = NULL,
    theme = bs_theme(brand = "brand/_brand.yml"),


    # Home page - first tab with sub-tabs
  nav_panel(
    title = tagList(icon("home"), "Home"),
    value = "home",
    navset_card_tab(
      id = "home_tabs",
      height = "calc(100vh - 120px)",

      # Welcome sub-tab
      nav_panel(
        title = "Welcome",
        page_fluid(
          layout_column_wrap(
            width = 1,
            heights_equal = "row",

            # Main welcome card
            card(
              card_header(
                class = "bg-primary",
                h2("Welcome to the ISP Analyser", style = "margin: 0; color: white; font-size: 1.5rem;")
              ),
              card_body(
                h4("Interactive Analysis of Australia's Integrated System Plan"),
                p("This tool provides comprehensive visualizations and analysis of electricity generation and storage capacity
                  projections from the Australian Energy Market Operator's (AEMO) Integrated System Plan (ISP)."),
                hr(),
                p(strong("What is the ISP?")),
                p("The Integrated System Plan is AEMO's whole-of-system plan for the efficient development of Australia's
                  National Electricity Market over the next 20+ years. It provides actionable information to support
                  investment decisions in generation, storage, and transmission infrastructure."),
                hr(),
                p(em("Select a tab above to begin exploring the ISP data.")),
                p(em("Last updated: 23 November 2025"))
              )
            ),

            # Features overview
            layout_column_wrap(
              width = 1/2,

              card(
                card_header(h4("Technology View")),
                card_body(
                  p(strong("Explore generation and storage trends")),
                  tags$ul(
                    tags$li("Generation capacity and output by technology"),
                    tags$li("Storage capacity and output analysis"),
                    tags$li("Net capacity additions and retirements"),
                    tags$li("Filter by region, scenario, and technology type")
                  )
                )
              ),

              card(
                card_header(h4("Compare Scenarios")),
                card_body(
                  p(strong("Compare different ISP scenarios")),
                  tags$ul(
                    tags$li("Side-by-side scenario comparison"),
                    tags$li("Analyze different development pathways"),
                    tags$li("Understand scenario implications")
                  )
                )
              )
            ),

            # Getting started card
            card(
              card_header(
                class = "bg-secondary",
                h4("Getting Started", style = "margin: 0;")
              ),
              card_body(
                tags$ol(
                  tags$li(strong("Navigate"), " - Use the tabs above to access different views"),
                  tags$li(strong("Select"), " - Choose your ISP source, scenario, and pathway from the sidebar"),
                  tags$li(strong("Filter"), " - Select regions and technologies to customize your analysis"),
                  tags$li(strong("Visualize"), " - Interact with charts (zoom, pan, hover for details)"),
                  tags$li(strong("Download"), " - Export chart data and images for your reports")
                ),
                hr(),
                p(
                  icon("info-circle"),
                  " Tip: Use the 'Technology View' tab to start exploring generation and storage trends across different scenarios."
                )
              )
            )
          )
        )
      ),

      # Definitions sub-tab
      nav_panel(
        title = "Definitions",
        card(
          card_header(
            h3("Glossary of Terms", style = "margin: 0;")
          ),
          card_body(
            p("This section provides definitions for key terms used throughout the ISP Analyser."),
            hr(),

            # Placeholder table structure
            h4("Key Terms"),
            tags$table(
              class = "table table-striped table-hover",
              style = "width: 100%;",
              tags$thead(
                tags$tr(
                  tags$th("Term", style = "width: 25%;"),
                  tags$th("Definition", style = "width: 75%;")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(strong("ISP")),
                  tags$td("Integrated System Plan - AEMO's roadmap for efficient development of the National Electricity Market")
                ),
                tags$tr(
                  tags$td(strong("AEMO")),
                  tags$td("Australian Energy Market Operator")
                ),
                tags$tr(
                  tags$td(strong("Scenario")),
                  tags$td("A plausible future pathway for the energy system based on different assumptions")
                ),
                tags$tr(
                  tags$td(strong("CDP")),
                  tags$td("Candidate Development Path - potential development trajectory for the energy system")
                ),
                tags$tr(
                  tags$td(strong("ODP")),
                  tags$td("Optimal Development Path - the most efficient development pathway for a given scenario")
                ),
                tags$tr(
                  tags$td(strong("GW")),
                  tags$td("Gigawatt - unit of power equal to 1,000 megawatts")
                ),
                tags$tr(
                  tags$td(strong("GWh")),
                  tags$td("Gigawatt-hour - unit of energy equal to 1,000 megawatt-hours")
                ),
                tags$tr(
                  tags$td(tags$em("(More definitions to be added...)")),
                  tags$td(tags$em(""))
                )
              )
            )
          ),
          full_screen = TRUE
        )
      ),

      # Scenario descriptions sub-tab
      nav_panel(
        title = "Scenario descriptions",
        card(
          card_header(
            h3("ISP Scenario Descriptions", style = "margin: 0;")
          ),
          card_body(
            p("This section describes the different scenarios modeled in the ISP, representing plausible futures for Australia's energy system."),
            hr(),

            # Placeholder for scenario descriptions
            h4("2024 ISP Scenarios"),

            tags$div(
              class = "mb-4",
              h5(strong("Step Change")),
              p("The Step Change scenario represents a future where Australia takes strong action on emissions reduction,
                with high consumer energy resource uptake, coordinated policy, and rapid technology development.
                This scenario sees accelerated decarbonization and significant grid transformation.")
            ),

            tags$div(
              class = "mb-4",
              h5(strong("Progressive Change")),
              p("Progressive Change assumes moderate progress toward decarbonization with steady policy support.
                Technology costs decline at expected rates and consumer adoption grows gradually.
                This scenario represents a balanced pathway with measured grid transformation.")
            ),

            tags$div(
              class = "mb-4",
              h5(strong("Green Energy Exports")),
              p("This scenario envisions Australia developing a major green hydrogen export industry alongside
                domestic decarbonization. It features very high renewable energy deployment to support both
                domestic consumption and export production, requiring significant grid expansion.")
            ),

            tags$div(
              class = "mb-4",
              h5(strong("Slow Change")),
              p("Slow Change represents a future with delayed policy action and slower technology adoption.
                Emissions reduction proceeds more gradually with less coordinated policy support.
                Fossil fuel generation remains in the system longer as renewable deployment is more moderate.")
            ),

            hr(),
            p(tags$em("Note: Scenario details vary by ISP release year. The above describes 2024 ISP scenarios.
                      Select different ISP sources in the Technology View to explore other scenario sets."))
          ),
          full_screen = TRUE
        )
      )
    )
  ),


  nav_panel(
    title = tagList(icon("solar-panel"), "Technology view"),
    value = "technology_view",
    layout_sidebar(
      sidebar = sidebar(
        bg = "#F2F2F2",
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
                      value = TRUE),
        
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

        downloadButton("download_chart_data",
                       "Download chart data",
                       class = "btn-primary",
                       icon = icon("download")),

        downloadButton("download_chart_image",
                       "Download chart image",
                       class = "btn-primary",
                       icon = icon("file-image")),

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
    title = tagList(icon("cloud"), "Emissions view"),
    value = "emissions_view",
    layout_sidebar(
      sidebar = sidebar(
        bg = "#F2F2F2",
        h4("ISP source data"),

        selectInput("emissions_source",
                    "Select ISP source",
                    choices = NULL,
                    multiple = FALSE,
                    selected = NULL),

        selectInput("emissions_scenario",
                    "Select scenario",
                    choices = NULL,
                    multiple = FALSE,
                    selected = NULL),

        selectInput("emissions_pathway",
                    "Select pathway",
                    choices = NULL,
                    multiple = FALSE,
                    selected = NULL),

        checkboxInput("emissions-show-only-odp",
                      "Use only optimal development paths",
                      value = TRUE),

        h4("Chart inputs"),

        pickerInput("emissions_region",
                    "Select region",
                    choices = NULL,
                    multiple = TRUE,
                    selected = NULL,
                    options = list(`actions-box` = TRUE)),

        virtualSelectInput("emissions_display_options",
                           "Select display options",
                           choices = NULL,
                           width = "100%",
                           dropboxWrapper = "body"),

        h4("Download data"),

        downloadButton("download_emissions_chart_data",
                       "Download chart data",
                       class = "btn-primary",
                       icon = icon("download")),

        downloadButton("download_emissions_chart_image",
                       "Download chart image",
                       class = "btn-primary",
                       icon = icon("file-image")),

        width = 300
      ),

      navset_card_tab(
        id = "emissions_tabs",
        height = "calc(100vh - 120px)",

        nav_panel(
          title = "Total emissions",
          card(
            card_header("Total emissions (MtCO2e) per year"),
            card_body(
              "Chart content placeholder"
            ),
            full_screen = TRUE,
            height = "100%"
          )
        ),

        nav_panel(
          title = "Annual change",
          card(
            card_header("Annual change in emissions (MtCO2e)"),
            card_body(
              "Chart content placeholder"
            ),
            full_screen = TRUE,
            height = "100%"
          )
        )
      )
    )
  ),
  

  nav_panel(
    title = tagList(icon("code-compare"), "Compare scenarios"),
    value = "compare_scenarios",
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
  
  nav_panel(
    title = tagList(icon("map"), "Compare regions"),
    value = "compare_regions",
    page_sidebar(
      sidebar = sidebar(
        "SIDE BAR"
      ),
      card(
        card_header("Compare regions"),
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
    value = "settings",
    card(
      card_header("Settings page"),
      card_body(
        "CARD BODY"
      )
    )
  )
  ) # END OF page_navbar
) # END OF UI (tagList)



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
  
  
  

  # =---- Download handlers -----------------------------------------------------

  # Download chart data handler
  output$download_chart_data <- downloadHandler(
    filename = function() {
      # Get information about the currently active tab
      tab_info <- get_active_tab_info(input)

      # Require valid inputs
      req(input$source, input$scenario)

      # Create filename based on active tab and selections
      if (is.null(tab_info$chart_type) || tab_info$chart_type == "") {
        filename <- "isp_data.csv"
      } else {
        # Build descriptive filename
        source_part <- gsub("_", "-", input$source)
        scenario_part <- gsub(" ", "-", tolower(input$scenario))
        chart_part <- tab_info$chart_type

        filename <- paste0("isp_", source_part, "_", scenario_part, "_", chart_part, "_",
                          format(Sys.Date(), "%Y%m%d"), ".csv")
      }

      return(filename)
    },

    content = function(file) {
      # Get information about the currently active tab
      tab_info <- get_active_tab_info(input)

      # Validate that we have a valid chart type
      req(tab_info$chart_type, tab_info$data_reactive)

      # Validate we're on a valid tab
      validate(
        need(!is.null(tab_info$chart_type) && tab_info$chart_type != "",
             "Please navigate to a chart tab first"),
        need(!is.null(tab_info$data_reactive) && tab_info$data_reactive != "",
             "No data source available for this tab")
      )

      # Get the reactive data based on the data_reactive name
      data_to_download <- switch(
        tab_info$data_reactive,
        "chart_data_gen_capacity" = chart_data_gen_capacity(),
        "chart_data_gen_output" = chart_data_gen_output(),
        "chart_data_gen_capacity_change" = chart_data_gen_capacity_change(),
        "chart_data_storage_capacity" = chart_data_storage_capacity(),
        "chart_data_storage_output" = chart_data_storage_output(),
        "chart_data_storage_capacity_change" = chart_data_storage_capacity_change(),
        NULL  # default case
      )

      # Validate we have data
      validate(
        need(!is.null(data_to_download), "No data available"),
        need(nrow(data_to_download) > 0, "No data available for current selection")
      )

      # Write the data to CSV
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )


  # Download chart image handler
  output$download_chart_image <- downloadHandler(
    filename = function() {
      # Get information about the currently active tab
      tab_info <- get_active_tab_info(input)

      # Require valid inputs
      req(input$source, input$scenario)

      # Create filename based on active tab and selections
      if (is.null(tab_info$chart_type) || tab_info$chart_type == "") {
        filename <- "isp_chart.svg"
      } else {
        # Build descriptive filename
        source_part <- gsub("_", "-", input$source)
        scenario_part <- gsub(" ", "-", tolower(input$scenario))
        chart_part <- tab_info$chart_type

        filename <- paste0("isp_", source_part, "_", scenario_part, "_", chart_part, "_",
                          format(Sys.Date(), "%Y%m%d"), ".svg")
      }

      return(filename)
    },

    content = function(file) {
      # Get information about the currently active tab
      tab_info <- get_active_tab_info(input)

      # Validate that we have a valid chart type
      req(tab_info$chart_type, tab_info$data_reactive)

      # Validate we're on a valid tab
      validate(
        need(!is.null(tab_info$chart_type) && tab_info$chart_type != "",
             "Please navigate to a chart tab first"),
        need(!is.null(tab_info$data_reactive) && tab_info$data_reactive != "",
             "No data source available for this tab")
      )

      # Get the data for the chart
      chart_data <- switch(
        tab_info$data_reactive,
        "chart_data_gen_capacity" = chart_data_gen_capacity(),
        "chart_data_gen_output" = chart_data_gen_output(),
        "chart_data_gen_capacity_change" = chart_data_gen_capacity_change(),
        "chart_data_storage_capacity" = chart_data_storage_capacity(),
        "chart_data_storage_output" = chart_data_storage_output(),
        "chart_data_storage_capacity_change" = chart_data_storage_capacity_change(),
        NULL
      )

      # Validate we have data
      validate(
        need(!is.null(chart_data), "No data available"),
        need(nrow(chart_data) > 0, "No data available for current selection")
      )

      # Generate the ggplot based on chart type
      plot_to_save <- switch(
        tab_info$chart_type,
        "generation_capacity" = generate_generation_capacity_chart(
          data = chart_data,
          scenario = input$scenario,
          source = input$source,
          show_dispatchable = input$show_dispatchable,
          show_total = input$show_total_capacity,
          util_table = util_table
        ),
        "generation_output" = generate_generation_output_chart(
          data = chart_data,
          scenario = input$scenario,
          source = input$source,
          show_dispatchable = input$show_dispatchable,
          show_total = input$show_total_capacity,
          util_table = util_table
        ),
        "generation_net_additions" = generate_generation_net_additions_chart(
          data = chart_data,
          scenario = input$scenario,
          source = input$source,
          util_table = util_table
        ),
        "storage_capacity" = generate_storage_capacity_chart(
          data = chart_data,
          scenario = input$scenario,
          source = input$source,
          show_dispatchable = input$show_dispatchable,
          show_total = input$show_total_capacity,
          storage_util_table = storage_util_table
        ),
        "storage_output" = generate_storage_output_chart(
          data = chart_data,
          scenario = input$scenario,
          source = input$source,
          show_dispatchable = input$show_dispatchable,
          show_total = input$show_total_capacity,
          storage_util_table = storage_util_table
        ),
        "storage_net_additions" = generate_storage_net_additions_chart(
          data = chart_data,
          scenario = input$scenario,
          source = input$source,
          storage_util_table = storage_util_table
        ),
        NULL
      )

      # Validate we have a plot
      validate(
        need(!is.null(plot_to_save), "No plot available")
      )

      # Save the plot as SVG
      ggsave(file, plot = plot_to_save, device = "svg", width = 12, height = 7, units = "in")
    }
  )


  # =---- 1.1. Generator capacity chart -----------------------------------------
  
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
      # Require group by to remove the breakdown of tech by state displayed on the chart - show as contiguous
      group_by(technology, year, scenario, source, cdp, year_ending, odp, dispatchable) |> 
      summarise(value = sum(value)) |> 
      ungroup() |> 
      mutate(value_gw = value/1000)
  })


  # Generate chart

  generation_capacity_plot <- reactive({

    d <- chart_data_gen_capacity()

    # Use the chart generation function
    p <- generate_generation_capacity_chart(
      data = d,
      scenario = input$scenario,
      source = input$source,
      show_dispatchable = input$show_dispatchable,
      show_total = input$show_total_capacity,
      util_table = util_table
    )

    return(ggplotly(p, tooltip = c("value_gw")) |>
             plotly::config(displayModeBar = F))

  })

  output$generation_capacity_plot <- renderPlotly(generation_capacity_plot())



  # =---- 1.2 Generator output chart -----------------------------------------
  
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
      mutate(technology = factor(technology, levels = util_table$technology)) |> 
    # Require group by to remove the breakdown of tech by state displayed on the chart - show as contiguous
      group_by(technology, year, scenario, source, cdp, year_ending, odp, dispatchable) |> 
      summarise(value = sum(value)) |> 
      ungroup() 
  })
  
  
  # Generate chart

  generation_output_plot <- reactive({

    d <- chart_data_gen_output()

    # Use the chart generation function
    p <- generate_generation_output_chart(
      data = d,
      scenario = input$scenario,
      source = input$source,
      show_dispatchable = input$show_dispatchable,
      show_total = input$show_total_capacity,
      util_table = util_table
    )

    return(ggplotly(p, tooltip = c("value")) |>
             plotly::config(displayModeBar = F))

  })
  
  output$generation_output_plot <- renderPlotly(generation_output_plot())
  
  
  
  
  # =---- 1.3 Generator capacity growth (net additions/ subtractions) chart ----
  
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

    # Use the chart generation function
    p <- generate_generation_net_additions_chart(
      data = d,
      scenario = input$scenario,
      source = input$source,
      util_table = util_table
    )

    return(ggplotly(p, tooltip = c("net_capacity_added")) |>
             plotly::config(displayModeBar = F))

  })

  output$generation_capacity_growth_plot <- renderPlotly(generation_capacity_growth_plot())
  

  
  
  
  # =---- 1.4 Storage capacity chart -----------------------------------------
  
  #Reactive data for the storage capacity chart
  chart_data_storage_capacity <- reactive({
    req(input$source, input$scenario, input$pathway, input$storage_type, input$region)
    
    isp_storage_capacity |>
      filter(source == input$source,
             scenario == input$scenario,
             cdp == input$pathway,
             storage_category %in% input$storage_type,
             region %in% input$region) |>
      left_join(storage_util_table, by = c("storage_category" = "storage_category")) |>
      mutate(storage_category = factor(storage_category, levels = storage_util_table$storage_category)) |>
      # Require group by to remove the breakdown of tech by state displayed on chart - show as a contiguous bar
      group_by(storage_category, year, scenario, source, cdp, year_ending, odp, dispatchable) |> 
      summarise(value = sum(value)) |> 
      ungroup() |> 
      mutate(value_gw = value/1000) 
  })
  
  
  # Generate chart

  storage_capacity_plot <- reactive({

    d <- chart_data_storage_capacity()

    # Use the chart generation function
    p <- generate_storage_capacity_chart(
      data = d,
      scenario = input$scenario,
      source = input$source,
      show_dispatchable = input$show_dispatchable,
      show_total = input$show_total_capacity,
      storage_util_table = storage_util_table
    )

    return(ggplotly(p, tooltip = c("value_gw")) |>
             plotly::config(displayModeBar = F))

  })
  
  output$storage_capacity_plot <- renderPlotly(storage_capacity_plot())
  
  
  
  
  
  # =---- 1.5 Storage output chart -----------------------------------------
  
  #Reactive data for the storage capacity chart
  chart_data_storage_output <- reactive({
    req(input$source, input$scenario, input$pathway, input$storage_type, input$region)
    
    isp_storage_output |>
      filter(source == input$source,
             scenario == input$scenario,
             cdp == input$pathway,
             storage_category %in% input$storage_type,
             region %in% input$region) |>
      left_join(storage_util_table, by = c("storage_category" = "storage_category")) |>
      mutate(storage_category = factor(storage_category, levels = storage_util_table$storage_category)) |>
      # Require group by to remove the breakdown of tech by state displayed on chart - show as a contiguous bar
      group_by(storage_category, year, scenario, source, cdp, year_ending, odp, dispatchable) |> 
      summarise(value = sum(value)) |> 
      ungroup() 
  })
  
  
  # Generate chart

  storage_output_plot <- reactive({

    d <- chart_data_storage_output()

    # Use the chart generation function
    p <- generate_storage_output_chart(
      data = d,
      scenario = input$scenario,
      source = input$source,
      show_dispatchable = input$show_dispatchable,
      show_total = input$show_total_capacity,
      storage_util_table = storage_util_table
    )

    return(ggplotly(p, tooltip = c("value")) |>
             plotly::config(displayModeBar = F))

  })
  
  output$storage_output_plot <- renderPlotly(storage_output_plot())
  
  
  
  
  
  # =---- 1.6 Storage capacity growth (net additions/ subtractions) chart ----

  #Reactive data for the storage capacity change chart
  chart_data_storage_capacity_change <- reactive({
    req(input$source, input$scenario, input$pathway, input$storage_type, input$region)

    isp_storage_capacity |>
      filter(source == input$source,
             scenario == input$scenario,
             cdp == input$pathway,
             storage_category %in% input$storage_type,
             region %in% input$region) |>
      left_join(storage_util_table, by = c("storage_category" = "storage_category")) |>
      mutate(storage_category = factor(storage_category, levels = storage_util_table$storage_category)) |> 
      group_by(storage_category, year) |> 
      summarise(value = sum(value)) |> 
      arrange(year) |> 
      mutate(net_capacity_added = value - lag(value)) |> 
      ungroup()
  })
  
  
  storage_capacity_growth_plot <- reactive({

    d <- chart_data_storage_capacity_change()

    # Use the chart generation function
    p <- generate_storage_net_additions_chart(
      data = d,
      scenario = input$scenario,
      source = input$source,
      storage_util_table = storage_util_table
    )

    return(ggplotly(p, tooltip = c("net_capacity_added")) |>
             plotly::config(displayModeBar = F))

  })
  
  output$storage_capacity_growth_plot <- renderPlotly(storage_capacity_growth_plot())
  
  
    

} # END SERVER BRACE




# ===== 05. RUN APP ============================================================

shinyApp(ui = ui, server = server)

















