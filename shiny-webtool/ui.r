
# Initialize thematic
thematic::thematic_shiny()


# Global UI settings
sidebar_width <- 250
overview_box_height <- '270px'

input_col_width <- 2
full_page_width <- 12


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
    
#----- GENERATOR CAPACITY -----------------------------------------------------
    tabItem(tabName = "generator_capacity",                            # Page 1
            h2(""),
            fluidRow(
              # Page 1 tabs
              tabBox(title = " ",
                     id = "generator_capacity_tabs",
                     width = full_page_width,
                     
                     tabPanel("Annual capacity", 
                              h3(NULL),
                                       
                                       fluidRow(
                                         box(title = "Select ISP inputs",
                                             width = full_page_width,
                                             solidHeader = TRUE,
                                             
                                             column(input_col_width, pickerInput("region",
                                                                                 "Select region",
                                                                                 choices = NULL,
                                                                                 multiple = TRUE,
                                                                                 selected = c("VIC"),
                                                                                 options = list(`actions-box` = TRUE))),
                                             
                                             column(input_col_width, selectInput("source",
                                                                                 "Select ISP source",
                                                                                 choices = NULL,
                                                                                 multiple = FALSE,
                                                                                 selected = NULL)),
                                             
                                             column(input_col_width, selectInput("scenario", 
                                                                                 "Select scenario",
                                                                                 choices = NULL,
                                                                                 multiple = FALSE,
                                                                                 selected = NULL)),
                                             
                                             column(input_col_width, selectInput("pathway",
                                                                                 "Select pathway",
                                                                                 choices = NULL,
                                                                                 multiple = FALSE,
                                                                                 selected = NULL)),
                                             
                                             column(input_col_width, pickerInput("technology",
                                                                                 "Select technology type(s)",
                                                                                 choices = NULL,
                                                                                 multiple = TRUE,
                                                                                 selected = NULL,
                                                                                 options = list(`actions-box` = TRUE))),
                                             
                                             column(input_col_width, checkboxInput("show_dispatchable",
                                                                                   "Show dispatchable capacity",
                                                                                   value = FALSE),
                                                    checkboxInput("show_total_capacity",
                                                                  "Show total capacity",
                                                                  value = FALSE))
                                         )
                                       ),
                                       
                                       fluidRow(
                                         box(title = "Total generation capacity (GW)",
                                             width = 12,
                                             solidHeader = TRUE,
                                             
                                             plotlyOutput("generation_capacity_plot", height = "500px")))
                              
                              ),
                     
                     tabPanel("Tab 2",
                              h3("Tab 2 content")),
                     
                     tabPanel("Tab 3",
                              h3("Tab 3 content")),
                     
                     tabPanel("Tabe 4",
                              h3("Tab 4 content"))
                
              )
            )
          ),                                                    # End of Page 1
    
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