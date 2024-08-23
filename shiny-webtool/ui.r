
# Initialize thematic
thematic::thematic_shiny()


theme_set(theme_minimal(base_family = "Arial"))

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
    
    menuItem(text = "Generation capacity and output", 
             tabName = "generator_capacity", 
             icon = icon("chart-line")),
    
    menuItem(text = "Generation output", 
             tabName = "generation_output", 
             icon = icon("chart-bar")),
    
    menuItem(text = "Compare across sources", 
             tabName = "compare_items",
             icon = icon("code-compare"))
  )
)


body <- dashboardBody(
  
  # Apply global styling for font
  tags$head(
    tags$style(HTML("
        body, h1, h2, h3, h4, h5, h6, p, div, span, input, select, button {
          font-family: Arial, sans-serif !important;
        }
      "))
  ),
  
  
  
  # Content for each sidebar tab
  tabItems(
    
#----- GENERATOR CAPACITY -----------------------------------------------------
    tabItem(tabName = "generator_capacity",                            # Page 1
            h2(""),
            
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
                                                      options = list(`actions-box` = TRUE))
                         ),
                  
                  column(input_col_width, checkboxInput("show_dispatchable",
                                                        "Show dispatchable capacity",
                                                        value = FALSE),
                         checkboxInput("show_total_capacity",
                                       "Show total capacity",
                                       value = FALSE)
                  

                                   ),
                  

              )
            ),
            
            fluidRow(
              tabBox(title = " ",
                     id = "generator_capacity_tabs",
                     width = full_page_width,
                     
                     tabPanel("Generation capacity (GW)", 
                              h3(NULL),
                               
                               fluidRow(
                                 box(title = "Total generation capacity (GW)",
                                     width = 12,
                                     solidHeader = TRUE,
                                     plotlyOutput("generation_capacity_plot", height = "500px")))
                              ),
                     
                     
                     tabPanel("Net annual growth (MW)",
                              h3(NULL),
                              
                              fluidRow(
                                box(title = "Generator capacity net change per year (MW)",
                                    width = 12,
                                    solidHeader = TRUE,
                                    plotlyOutput("generation_capacity_growth_plot", height = "500px")))
                     ),
                     
                     
                     tabPanel("Generation output (GWh)",
                              h3("Tab 3 content")),
                     
                     tabPanel("Storage capacity (GW)",
                              h3("Tab 4 content")),
                     
                     tabPanel("Storage net annual growth (MW)",
                              h3("Tab 3 content")),
                     
                     tabPanel("Storage output (GWh)",
                              h3("Tab 3 content"))
                
              )
            ),
            
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