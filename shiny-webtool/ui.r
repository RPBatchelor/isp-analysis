
sidebar_width <- 250
overview_box_height <- '270px'


#----- HEADER ------------------------------------------------------------------

header <- dashboardHeader(title = "ISP Data Explorer",
                          titleWidth = sidebar_width)


#----- SIDEBAR -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  width = sidebar_width,
  sidebarMenu(
    id = "tab",
    menuItem(text = "Generator capacity", tabName = "generator_capacity", icon = icon("chart-line")),
    menuItem(text = "Generation output", tabName = "generation_output", icon = icon("chart-bar")),
    menuItem(text = "Compare items", tabName = "compare_items")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "generator_capacity", source(file.path("ui", "ui_generator_capacity.R"), local = TRUE)$value),
    tabItem(tabName = "generation_output", source(file.path("ui", "ui_generation_output.R"), local = TRUE)$value),
    tabItem(tabName = "compare_items", source(file.path("ui", "ui_compare_items.R"), local = TRUE)$value)
  )
)


ui <- dashboardPage(
  title = "ISP Data Explorer",
  header,
  sidebar, 
  body
)