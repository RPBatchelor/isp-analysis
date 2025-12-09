################################################################################
#
# Chart Generation Functions
#
# These functions create ggplot objects that can be:
# 1. Converted to plotly for interactive display
# 2. Saved as static images (SVG, PNG, etc.)
#
# Ryan Batchelor
# 30 November 2025
#
################################################################################


#' Generate Generation Capacity Chart
#'
#' @param data The chart data (output from chart_data_gen_capacity reactive)
#' @param scenario Scenario name for subtitle
#' @param source Source name for caption
#' @param show_dispatchable Boolean - show dispatchable capacity line
#' @param show_total Boolean - show total capacity labels
#' @param util_table Utility table with technology colors
#'
#' @return A ggplot object

generate_generation_capacity_chart <- function(data, 
                                               scenario, 
                                               source,
                                               show_dispatchable = FALSE,
                                               show_total = FALSE,
                                               util_table) {

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = value_gw,
                 fill = reorder(technology, -as.numeric(technology))),
             position = "stack",
             stat = "identity",
             show.legend = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) +
    scale_x_continuous(breaks = unique(data$year),
                       labels = unique(data$year)) +
    labs(fill = "Technology",
         subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
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

  if(show_dispatchable){
    d2 <- data |>
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

  if(show_total){
    d3 <- data |>
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

  return(p)
}


#' Generate Generation Output Chart
#'
#' @param data The chart data (output from chart_data_gen_output reactive)
#' @param scenario Scenario name for subtitle
#' @param source Source name for caption
#' @param show_dispatchable Boolean - show dispatchable output line
#' @param show_total Boolean - show total output labels
#' @param util_table Utility table with technology colors
#'
#' @return A ggplot object
generate_generation_output_chart <- function(data, 
                                             scenario, 
                                             source,
                                             show_dispatchable = FALSE,
                                             show_total = FALSE,
                                             util_table) {

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = value,
                 fill = reorder(technology, -as.numeric(technology))),
             position = "stack",
             stat = "identity",
             show.legend = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) +
    scale_x_continuous(breaks = unique(data$year),
                       labels = unique(data$year)) +
    labs(fill = "Technology",
         subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
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

  if(show_dispatchable){
    d2 <- data |>
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

  if(show_total){
    d3 <- data |>
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

  return(p)
}


#' Generate Generation Net Additions Chart
#'
#' @param data The chart data (output from chart_data_gen_capacity_change reactive)
#' @param scenario Scenario name for subtitle
#' @param source Source name for caption
#' @param util_table Utility table with technology colors
#'
#' @return A ggplot object

generate_generation_net_additions_chart <- function(data, 
                                                    scenario, 
                                                    source, 
                                                    util_table) {

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = net_capacity_added,
                 fill = reorder(technology, -as.numeric(technology))),
             position = "stack",
             stat = "identity",
             show.legend = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) +
    scale_x_continuous(breaks = unique(data$year),
                       labels = unique(data$year)) +
    labs(fill = "Technology",
         subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
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

  return(p)
}


#' Generate Storage Capacity Chart
#'
#' @param data The chart data (output from chart_data_storage_capacity reactive)
#' @param scenario Scenario name for subtitle
#' @param source Source name for caption
#' @param show_dispatchable Boolean - show dispatchable capacity line
#' @param show_total Boolean - show total capacity labels
#' @param storage_util_table Utility table with storage colors
#'
#' @return A ggplot object

generate_storage_capacity_chart <- function(data, 
                                            scenario, 
                                            source,
                                            show_dispatchable = FALSE,
                                            show_total = FALSE,
                                            storage_util_table) {

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = value_gw,
                 fill = reorder(storage_category, -as.numeric(storage_category))),
             position = "stack",
             stat = "identity",
             show.legend = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_fill_manual(values = setNames(storage_util_table$colour_label, storage_util_table$storage_category)) +
    scale_x_continuous(breaks = unique(data$year),
                       labels = unique(data$year)) +
    labs(fill = "Storage category",
         subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
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

  if(show_dispatchable){
    d2 <- data |>
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

  if(show_total){
    d3 <- data |>
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

  return(p)
}


#' Generate Storage Output Chart
#'
#' @param data The chart data (output from chart_data_storage_output reactive)
#' @param scenario Scenario name for subtitle
#' @param source Source name for caption
#' @param show_dispatchable Boolean - show dispatchable output line
#' @param show_total Boolean - show total output labels
#' @param storage_util_table Utility table with storage colors
#'
#' @return A ggplot object

generate_storage_output_chart <- function(data, 
                                          scenario, 
                                          source,
                                          show_dispatchable = FALSE,
                                          show_total = FALSE,
                                          storage_util_table) {

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = value,
                 fill = reorder(storage_category, -as.numeric(storage_category))),
             position = "stack",
             stat = "identity",
             show.legend = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_fill_manual(values = setNames(storage_util_table$colour_label, storage_util_table$storage_category)) +
    scale_x_continuous(breaks = unique(data$year),
                       labels = unique(data$year)) +
    labs(fill = "Storage category",
         subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
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

  if(show_dispatchable){
    d2 <- data |>
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

  if(show_total){
    d3 <- data |>
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

  return(p)
}


#' Generate Storage Net Additions Chart
#'
#' @param data The chart data (output from chart_data_storage_capacity_change reactive)
#' @param scenario Scenario name for subtitle
#' @param source Source name for caption
#' @param storage_util_table Utility table with storage colors
#'
#' @return A ggplot object

generate_storage_net_additions_chart <- function(data,
                                                 scenario,
                                                 source,
                                                 storage_util_table) {

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = net_capacity_added,
                 fill = reorder(storage_category, -as.numeric(storage_category))),
             position = "stack",
             stat = "identity",
             show.legend = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_fill_manual(values = setNames(storage_util_table$colour_label, storage_util_table$storage_category)) +
    scale_x_continuous(breaks = unique(data$year),
                       labels = unique(data$year)) +
    labs(fill = "Storage category",
         subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
         x = "Year (financial year ending 30-Jun-YYYY)",
         y = "Storage capacity change (MW)") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))

  return(p)
}


#' Generate Scenario Comparison Total Capacity Chart
#'
#' @param data The chart data (output from comparison_total_capacity_data reactive)
#'
#' @return A ggplot object

generate_scenario_comparison_chart <- function(data) {

  p <- data |>
    ggplot(aes(x = year,
               y = value_gw,
               color = scenario_label,
               group = scenario_label)) +
    geom_line(size = 1) +
    labs(
      x = "Year (financial year ending 30-Jun-YYYY)",
      y = "Total Capacity (GW)",
      color = "Scenario"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9)
    ) +
    scale_y_continuous(labels = scales::comma)

  return(p)
}
