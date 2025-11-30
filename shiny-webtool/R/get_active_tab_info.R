################################################################################
#
# Helper function to identify active tab in Shiny app
#
# This function identifies which tab is currently active and returns relevant
# information for downloading data or charts
#
# Ryan Batchelor
# 25 November 2025
#
################################################################################


#' Get Active Tab Information
#'
#' Identifies which tab is currently active in the Shiny app and returns
#' relevant metadata for that tab, including chart type and data source names.
#'
#' @param input The Shiny input object (passed from server function)
#'
#' @return A list containing:
#'   \item{main_tab}{The active main navbar tab}
#'   \item{sub_tab}{The active nested tab (if applicable)}
#'   \item{chart_type}{Descriptive name for the chart type}
#'   \item{data_reactive}{Name of the reactive data object as string}
#'   \item{plot_reactive}{Name of the reactive plot object as string}
#'
#' @examples
#' # In server function:
#' tab_info <- get_active_tab_info(input)
#' if (tab_info$chart_type == "generation_capacity") {
#'   # Handle generation capacity download
#' }

get_active_tab_info <- function(input) {

  # Get the main navbar selection
  main_tab <- input$main_navbar

  # Initialize the result list
  result <- list(
    main_tab = main_tab,
    sub_tab = NULL,
    chart_type = NULL,
    data_reactive = NULL,
    plot_reactive = NULL
  )

  # Check which main tab is active
  if (main_tab == "technology_view") {
    # Get the nested tab selection
    sub_tab <- input$technology_tabs
    result$sub_tab <- sub_tab

    # Map sub-tab to specific chart/data information
    if (sub_tab == "Generation capacity (GW)") {
      result$chart_type <- "generation_capacity"
      result$data_reactive <- "chart_data_gen_capacity"
      result$plot_reactive <- "generation_capacity_plot"

    } else if (sub_tab == "Generation output (GWh)") {
      result$chart_type <- "generation_output"
      result$data_reactive <- "chart_data_gen_output"
      result$plot_reactive <- "generation_output_plot"

    } else if (sub_tab == "Generation net additions") {
      result$chart_type <- "generation_net_additions"
      result$data_reactive <- "chart_data_gen_capacity_change"
      result$plot_reactive <- "generation_capacity_growth_plot"

    } else if (sub_tab == "Storage capacity (GW)") {
      result$chart_type <- "storage_capacity"
      result$data_reactive <- "chart_data_storage_capacity"
      result$plot_reactive <- "storage_capacity_plot"

    } else if (sub_tab == "Storage output (GWh)") {
      result$chart_type <- "storage_output"
      result$data_reactive <- "chart_data_storage_output"
      result$plot_reactive <- "storage_output_plot"

    } else if (sub_tab == "Storage net additions (MW)") {
      result$chart_type <- "storage_net_additions"
      result$data_reactive <- "chart_data_storage_capacity_change"
      result$plot_reactive <- "storage_capacity_growth_plot"
    }

  } else if (main_tab == "home") {
    result$chart_type <- NULL

  } else if (main_tab == "emissions_view") {
    result$chart_type <- NULL

  } else if (main_tab == "compare_scenarios") {
    result$chart_type <- NULL

  } else if (main_tab == "compare_regions") {
    result$chart_type <- NULL

  } else if (main_tab == "settings") {
    result$chart_type <- NULL
  }

  return(result)
}
