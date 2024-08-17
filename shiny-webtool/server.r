

header <- dashboardHeader()

sidebar <- dashboardSidebar(
  width = 350,
  sidebarMenu(
    id = "tab",
    menuItem(text = "Generator capacity",
             tabName = "generator_capacity",
             icon = NULL),
    menuItem(text = "Generation output",
             tabName = "generation_output"),
    menuItem(text = "Compare items",
             tabName = "compare_items")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "generation_output", source(file.path("ui", "ui_generator_capacity.R"), local = TRUE)$value),
    tabItem(tabName = "generation_output", source(file.path("ui", "ui_generation_output.R"), local = TRUE)$value),
    tabItem(tabName = "compare_items", source(file.path("ui", "ui_compare_items.R"), local = TRUE)$value)
  )
)


ui <- dashboardPage(
  title = "ISP Analyser",
  header,
  sidebar, 
  body
)