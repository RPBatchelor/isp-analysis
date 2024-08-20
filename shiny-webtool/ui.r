
sidebar_width <- 250
overview_box_height <- '270px'


#----- HEADER ------------------------------------------------------------------

header <- dashboardHeader(title = "ISP Data Explorer",
                          titleWidth = sidebar_width)


#----- SIDEBAR -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  width = sidebar_width,
  sidebarMenu(
    menuItem(text = "Generator capacity", tabName = "generator_capacity", icon = icon("chart-line")),
    menuItem(text = "Generation output", tabName = "generation_output", icon = icon("chart-bar")),
    menuItem(text = "Compare items", tabName = "compare_items")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "generator_capacity",                         # Tab item 1
            h2("Generator capacity (MW) overview"),
            
            fluidRow(
              box(title = "Select ISP inputs",
                  width = 12,
                  solidHeader = TRUE,
                  
                  column(3, pickerInput("region",
                                        "Select region",
                                        choices = NULL,
                                        multiple = TRUE,
                                        selected = c("VIC"))),
                  
                  column(3, selectInput("source",
                                        "Select ISP data source",
                                        choices = NULL,
                                        multiple = FALSE,
                                        selected = NULL)),
                  
                  column(3, selectInput("scenario", 
                                        "Select scenario",
                                        choices = NULL,
                                        multiple = FALSE,
                                        selected = NULL)),
                  
                  column(3, selectInput("pathway",
                                        "Select pathway",
                                        choices = NULL,
                                        multiple = FALSE,
                                        selected = NULL)),
                  
                  column(3, pickerInput("technology",
                                        "Select technology type",
                                        choices = NULL,
                                        multiple = TRUE,
                                        selected = NULL))
              )
            ),
            
            fluidRow(
              box(title = "Generator capacity (MW)",
                  width = 12,
                  solidHeader = TRUE,
                  
                  plotlyOutput("generation_capacity_plot", height = "600px"))
            )
          ),                                                     # End of Tab item 1
    
    tabItem(tabName = "generation_output", 
            source(file.path("ui", "ui_generation_output.R"), local = TRUE)$value),
    
    tabItem(tabName = "compare_items", 
            source(file.path("ui", "ui_compare_items.R"), local = TRUE)$value)
  )
)


ui <- dashboardPage(
  title = "ISP Data Explorer",
  skin = "purple",
  header,
  sidebar, 
  body
)