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

  # Dispatchable at bottom of stack, non-dispatchable on top
  tech_order <- util_table |>
    mutate(row = row_number()) |>
    arrange(dispatchable, row) |>
    pull(technology)

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = value_gw,
                 fill = factor(technology, levels = tech_order)),
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
    theme_minimal() +
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

  # Dispatchable at bottom of stack, non-dispatchable on top
  tech_order <- util_table |>
    mutate(row = row_number()) |>
    arrange(dispatchable, row) |>
    pull(technology)

  has_historical <- "opennem" %in% data$source

  all_years  <- sort(unique(data$year))
  x_breaks   <- if (length(all_years) > 15) {
    seq(min(all_years), max(all_years), by = 5)
  } else {
    all_years
  }

  p <- ggplot(data)

  # Shaded background for historical period (drawn first, behind bars)
  if (has_historical) {
    transition_year <- min(data$year[data$source != "opennem"], na.rm = TRUE) - 0.5
    p <- p +
      annotate("rect",
               xmin = -Inf, xmax = transition_year,
               ymin = -Inf, ymax = Inf,
               fill = "gray92", alpha = 1)
  }

  p <- p +
    geom_bar(aes(x = year,
                 y = value,
                 fill = factor(technology, levels = tech_order)),
             position = "stack",
             stat = "identity",
             show.legend = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    labs(fill = "Technology",
         subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
         x = "Year (financial year ending 30-Jun-YYYY)",
         y = "Output (GWh)") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))

  # Divider and labels separating historical actuals from ISP projections
  if (has_historical) {
    y_label <- max(
      data |> group_by(year) |> summarise(tot = sum(value, na.rm = TRUE)) |> pull(tot),
      na.rm = TRUE
    )
    p <- p +
      geom_vline(xintercept = transition_year,
                 linetype = "dashed", colour = "gray50", linewidth = 0.6) +
      annotate("text",
               x = transition_year - 1.5, y = y_label,
               label = "Historical\nactuals", hjust = 1, vjust = 1,
               size = 3, colour = "gray40", fontface = "italic") +
      annotate("text",
               x = transition_year + 1.5, y = y_label,
               label = "ISP projection", hjust = 0, vjust = 1,
               size = 3, colour = "gray40", fontface = "italic")
  }

  if (show_dispatchable) {
    d2 <- data |>
      filter(dispatchable == TRUE) |>
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

  if (show_total) {
    d3 <- data |>
      group_by(year) |>
      summarise(value = sum(value)) |>
      ungroup() |>
      mutate(label_text = scales::comma(round(value, 1)),
             label_y = value + max(value) * 0.03)

    p <- p +
      geom_text(data = d3,
                aes(x = year, y = label_y, label = label_text),
                size = 3, colour = "gray30",
                inherit.aes = FALSE)
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

  # Dispatchable at bottom of stack, non-dispatchable on top
  tech_order <- util_table |>
    mutate(row = row_number()) |>
    arrange(dispatchable, row) |>
    pull(technology)

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = net_capacity_added,
                 fill = factor(technology, levels = tech_order)),
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

  # Dispatchable at bottom of stack, non-dispatchable on top
  storage_order <- storage_util_table |>
    mutate(row = row_number()) |>
    arrange(dispatchable, row) |>
    pull(storage_category)

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = value_gw,
                 fill = factor(storage_category, levels = storage_order)),
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

  # Dispatchable at bottom of stack, non-dispatchable on top
  storage_order <- storage_util_table |>
    mutate(row = row_number()) |>
    arrange(dispatchable, row) |>
    pull(storage_category)

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = value,
                 fill = factor(storage_category, levels = storage_order)),
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
      ungroup() |>
      mutate(label_text = scales::comma(round(value, 1)),
             label_y = value + max(value) * 0.03)

    p <- p +
      geom_text(data = d3,
                aes(x = year,
                    y = label_y,
                    label = label_text),
                size = 3, colour = "gray30",
                inherit.aes = FALSE)
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

  # Dispatchable at bottom of stack, non-dispatchable on top
  storage_order <- storage_util_table |>
    mutate(row = row_number()) |>
    arrange(dispatchable, row) |>
    pull(storage_category)

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = net_capacity_added,
                 fill = factor(storage_category, levels = storage_order)),
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


#' Generate Total Emissions Area Chart
#'
#' @param data The chart data (output from chart_data_emissions reactive)
#' @param scenario Scenario name for subtitle
#' @param source Source name for caption
#'
#' @return A ggplot object
generate_emissions_total_chart <- function(data, scenario, source) {

  region_colors <- c(
    "NEM" = "#2d2d2d",
    "NSW" = "#5c4033",
    "QLD" = "#8b6f47",
    "SA"  = "#a0522d",
    "TAS" = "#7a6457",
    "VIC" = "#4a3c31"
  )

  has_historical <- "opennem" %in% data$source

  all_years <- sort(unique(data$year))
  x_breaks <- if (length(all_years) > 15) {
    seq(min(all_years), max(all_years), by = 5)
  } else {
    all_years
  }

  p <- ggplot(data)

  # Shaded background for historical period
  if (has_historical) {
    transition_year <- min(data$year[data$source != "opennem"], na.rm = TRUE) - 0.5
    p <- p +
      annotate("rect",
               xmin = -Inf, xmax = transition_year,
               ymin = -Inf, ymax = Inf,
               fill = "gray92", alpha = 1)
  }

  p <- p +
    geom_area(aes(x = year, y = value, fill = region),
              alpha = 0.8, position = "stack") +
    scale_fill_manual(values = region_colors) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    labs(fill = "Region",
         subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
         x = "Year (financial year ending 30-Jun-YYYY)",
         y = "Emissions (Mt CO2-e)") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))

  # Divider and labels for historical vs projection
  if (has_historical) {
    y_label <- max(
      data |> group_by(year) |> summarise(tot = sum(value, na.rm = TRUE)) |> pull(tot),
      na.rm = TRUE
    )
    p <- p +
      geom_vline(xintercept = transition_year,
                 linetype = "dashed", colour = "gray50", linewidth = 0.6) +
      annotate("text",
               x = transition_year - 1.5, y = y_label,
               label = "Historical\nactuals", hjust = 1, vjust = 1,
               size = 3, colour = "gray40", fontface = "italic") +
      annotate("text",
               x = transition_year + 1.5, y = y_label,
               label = "ISP projection", hjust = 0, vjust = 1,
               size = 3, colour = "gray40", fontface = "italic")
  }

  return(p)
}


#' Generate Annual Change in Emissions Chart
#'
#' @param data The chart data (output from chart_data_emissions_change reactive)
#' @param scenario Scenario name for subtitle
#' @param source Source name for caption
#'
#' @return A ggplot object
generate_emissions_change_chart <- function(data, scenario, source) {

  region_colors <- c(
    "NEM" = "#2d2d2d",
    "NSW" = "#5c4033",
    "QLD" = "#8b6f47",
    "SA"  = "#a0522d",
    "TAS" = "#7a6457",
    "VIC" = "#4a3c31"
  )

  has_historical <- "opennem" %in% data$source

  all_years <- sort(unique(data$year))
  x_breaks <- if (length(all_years) > 15) {
    seq(min(all_years), max(all_years), by = 5)
  } else {
    all_years
  }

  p <- ggplot(data)

  if (has_historical) {
    transition_year <- min(data$year[data$source != "opennem"], na.rm = TRUE) - 0.5
    p <- p +
      annotate("rect",
               xmin = -Inf, xmax = transition_year,
               ymin = -Inf, ymax = Inf,
               fill = "gray92", alpha = 1)
  }

  p <- p +
    geom_bar(aes(x = year, y = change, fill = region),
             position = "stack", stat = "identity", show.legend = TRUE) +
    geom_hline(yintercept = 0, colour = "gray30", linewidth = 0.4) +
    scale_fill_manual(values = region_colors) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    labs(fill = "Region",
         subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
         x = "Year (financial year ending 30-Jun-YYYY)",
         y = "Change in emissions (Mt CO2-e)") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))

  if (has_historical) {
    y_range <- range(
      data |> group_by(year) |>
        summarise(pos = sum(change[change > 0], na.rm = TRUE),
                  neg = sum(change[change < 0], na.rm = TRUE)) |>
        tidyr::pivot_longer(c(pos, neg)) |> pull(value),
      na.rm = TRUE
    )
    y_label <- y_range[2]
    p <- p +
      geom_vline(xintercept = transition_year,
                 linetype = "dashed", colour = "gray50", linewidth = 0.6) +
      annotate("text",
               x = transition_year - 1.5, y = y_label,
               label = "Historical\nactuals", hjust = 1, vjust = 1,
               size = 3, colour = "gray40", fontface = "italic") +
      annotate("text",
               x = transition_year + 1.5, y = y_label,
               label = "ISP projection", hjust = 0, vjust = 1,
               size = 3, colour = "gray40", fontface = "italic")
  }

  return(p)
}


#' Generate Emissions Intensity Chart
#'
#' @param data The chart data (output from chart_data_emissions_intensity reactive)
#' @param scenario Scenario name for subtitle
#' @param source Source name for caption
#'
#' @return A ggplot object
generate_emissions_intensity_chart <- function(data, scenario, source) {

  has_historical <- "opennem" %in% data$source

  all_years <- sort(unique(data$year))
  x_breaks <- if (length(all_years) > 15) {
    seq(min(all_years), max(all_years), by = 5)
  } else {
    all_years
  }

  p <- ggplot(data)

  if (has_historical) {
    transition_year <- min(data$year[data$source != "opennem"], na.rm = TRUE) - 0.5
    p <- p +
      annotate("rect",
               xmin = -Inf, xmax = transition_year,
               ymin = -Inf, ymax = Inf,
               fill = "gray92", alpha = 1)
  }

  p <- p +
    geom_line(aes(x = year, y = intensity),
              colour = "#4a3c31", linewidth = 1) +
    geom_point(aes(x = year, y = intensity),
               colour = "#4a3c31", size = 2) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    labs(subtitle = glue("{scenario} scenario"),
         caption = glue("Source: {source}"),
         x = "Year (financial year ending 30-Jun-YYYY)",
         y = "Emissions intensity (kg CO2-e / MWh)") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))

  if (has_historical) {
    y_label <- max(data$intensity, na.rm = TRUE)
    p <- p +
      geom_vline(xintercept = transition_year,
                 linetype = "dashed", colour = "gray50", linewidth = 0.6) +
      annotate("text",
               x = transition_year - 1.5, y = y_label,
               label = "Historical\nactuals", hjust = 1, vjust = 1,
               size = 3, colour = "gray40", fontface = "italic") +
      annotate("text",
               x = transition_year + 1.5, y = y_label,
               label = "ISP projection", hjust = 0, vjust = 1,
               size = 3, colour = "gray40", fontface = "italic")
  }

  return(p)
}
