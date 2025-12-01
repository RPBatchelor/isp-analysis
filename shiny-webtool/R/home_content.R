################################################################################
#
# Home Page Content
#
# This file contains all the content for the Home tab (Welcome, Definitions,
# Scenario descriptions). Edit this file to update the home page content
# without modifying the main app.r file.
#
# Ryan Batchelor
# 1 December 2025
#
################################################################################


#' Generate Welcome Tab Content
#'
#' @return A shiny UI page_fluid object with welcome content
generate_welcome_content <- function() {
  page_fluid(
    layout_column_wrap(
      width = 1,
      heights_equal = "row",

      # Main welcome card
      card(
        card_header(
          class = "bg-primary",
          h3("Welcome to the ISP Analyser", style = "margin: 0; color: white; font-size: 1.5rem;")
        ),
        card_body(
          h4("Interactive Analysis of Australia's Integrated System Plan"),
          p("This tool provides the ability to explore and analyse generation, storage, and emissions data 
            projections from the Australian Energy Market Operator's (AEMO) Integrated System Plan (ISP)."),
          hr(),
          p(strong("What is the ISP?")),
          p("The Integrated System Plan is AEMO's whole-of-system plan for the efficient development of Australia's
            National Electricity Market over the next 20+ years. It provides actionable information to support
            investment decisions in generation, storage, and transmission infrastructure."),
          hr(),
          p(strong("Note: The author of this application is not affiliated with AEMO, and does not purport to represent it's views")),
          p("ISP data has been extracted, transformed and manipulated based on the official ISP data provided by AEMO"),
          p("The author makes no guarantees or warranties for the accuracy of the data contained herein"),
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
}


#' Generate Definitions Tab Content
#'
#' @return A shiny card object with definitions content
generate_definitions_content <- function() {
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
}


#' Generate Scenario Descriptions Tab Content
#'
#' @return A shiny card object with scenario descriptions content
generate_scenario_descriptions_content <- function() {
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
}
