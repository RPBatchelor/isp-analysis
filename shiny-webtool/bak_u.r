
# Initialize thematic
thematic::thematic_shiny()

theme_set(theme_minimal(base_family = "Arial"))

# Global UI settings
sidebar_width <- 250
overview_box_height <- '270px'
input_col_width <- 2
full_page_width <- 12

# Apply global styling for font
global_styles <- tags$head(
  tags$style(HTML("
    body, h1, h2, h3, h4, h5, h6, p, div, span, input, select, button {
      font-family: Arial, sans-serif !important;
    }
  "))
)

# Sidebar content with input controls
sidebar_content <- sidebar(
  width = sidebar_width,
  title = "ISP Data Explorer",

  h4("Select ISP inputs"),

  pickerInput("region",
              "Select region",
              choices = NULL,
              multiple = TRUE,
              selected = c("VIC"),
              options = list(`actions-box` = TRUE)),

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

  pickerInput("technology",
              "Select technology type(s)",
              choices = NULL,
              multiple = TRUE,
              selected = NULL,
              options = list(`actions-box` = TRUE)),

  checkboxInput("show_dispatchable",
                "Show dispatchable capacity",
                value = FALSE),

  checkboxInput("show_total_capacity",
                "Show total capacity",
                value = FALSE)
)

# Generation capacity & output tab content
generation_capacity_content <- navset_tab(
  id = "generator_capacity_tabs",

  nav_panel("Generation capacity (GW)",
            card(
              card_header("Total generation capacity (GW)"),
              card_body(
                plotlyOutput("generation_capacity_plot", height = "500px")
              )
            )
  ),

  nav_panel("Net annual growth (MW)",
            card(
              card_header("Generator capacity net change per year (MW)"),
              card_body(
                plotlyOutput("generation_capacity_growth_plot", height = "500px")
              )
            )
  ),

  nav_panel("Generation output (GWh)",
            h3("Tab 3 content")
  ),

  nav_panel("Storage capacity (GW)",
            h3("Tab 4 content")
  ),

  nav_panel("Storage net annual growth (MW)",
            h3("Tab 5 content")
  ),

  nav_panel("Storage output (GWh)",
            h3("Tab 6 content")
  )
)

# Compare scenarios tab content
compare_scenarios_content <- div(
  h2("Compare scenarios"),
  p("This section will allow comparison across different sources and scenarios.")
)

# Admin tab content
admin_content <- div(
  h2("Admin"),
  p("Admin settings and controls will go here.")
)

# Read brand from _brand.yml file
app_theme <- bs_theme_brand_read("_brand.yml")

# Main UI with page_sidebar and top-level navigation
ui <- page_sidebar(
  title = "ISP Data Explorer",
  fillable = TRUE,
  theme = app_theme,

  global_styles,

  sidebar = sidebar_content,

  # Top-level navigation
  navset_bar(
    title = "ISP Data Explorer",

    nav_panel("Generation capacity & output",
              generation_capacity_content
    ),

    nav_panel("Compare scenarios",
              compare_scenarios_content
    ),

    nav_spacer(),

    nav_panel("Admin",
              admin_content
    )
  )
)
