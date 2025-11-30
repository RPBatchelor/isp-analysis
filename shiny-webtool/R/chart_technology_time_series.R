################################################################################
#
# Unified chart function for ISP technology time series data
# Handles generation and storage, capacity and output
#
# Ryan Batchelor
# 30 November 2025
#
################################################################################


#' Create time series chart for generation or storage data
#'
#' @param tech_type Character: "generation" or "storage"
#' @param data_type Character: "capacity" or "output"
#' @param cdp_scenario Character: CDP scenario name (e.g., "cdp1", "cdp2")
#' @param region_name Character: Region name or "NEM" for all regions combined
#' @param isp_scenario Character: ISP scenario name (e.g., "step change")
#' @param isp_source Character: ISP source (e.g., "2024_final")
#' @param technology_filter Character vector or "all": Technologies to include
#' @param show_dispatchable Logical: Whether to overlay dispatchable capacity line
#'
#' @return ggplot object
#'
chart_technology_time_series <- function(tech_type = c("generation", "storage"),
                                         data_type = c("capacity", "output"),
                                         show_dispatchable = FALSE,
                                         show_total_capacoty = FALSE,
                                         input_data) {



  d <- input_data
  
  p <- d |> 
    ggplot() +
    



  # ----- 06. Set chart parameters based on data type --------------------------

  # Set y-axis label and scale
  if(data_type == "capacity") {
    y_label <- "Capacity (GW)"
    y_scale <- 1e-3  # Convert MW to GW
  } else {
    y_label <- "Output (TWh)"
    y_scale <- 1e-3  # Convert GWh to TWh
  }

  # Set title
  chart_title <- glue("{region_name} {tech_type} {data_type}")


  # ----- 07. Join with utility table and prepare for plotting -----------------

  data <- data |>
    left_join(util_table, by = setNames("technology", tech_col)) |>
    mutate(technology = factor(technology, levels = util_table$technology)) |>
    mutate(value_scaled = value * y_scale)


  # ----- 08. Create base plot -------------------------------------------------

  p <- data |>
    ggplot() +
    geom_bar(aes(x = year,
                 y = value_scaled,
                 fill = reorder(technology, -as.numeric(technology))),
             position = "stack",
             stat = "identity",
             show.legend = TRUE) +
    scale_y_continuous(labels = label_number(scale = 1)) +
    scale_fill_manual(values = setNames(util_table$colour_label, util_table$technology)) +
    scale_x_continuous(breaks = unique(data_source$year),
                       labels = unique(data_source$year)) +
    labs(fill = "Technology",
         title = chart_title,
         subtitle = glue("{isp_scenario} scenario"),
         caption = glue("Source: {isp_source}"),
         x = "Year (financial year ending 30-jun-YYYY)",
         y = y_label) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(color = "gray", linewidth = 0.1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1))


  # ----- 09. Add optional dispatchable line -----------------------------------

  if(show_dispatchable == TRUE) {
    series <- data |>
      filter(dispatchable == TRUE) |>
      group_by(year) |>
      summarise(value_scaled = sum(value_scaled), .groups = "drop")

    p <- p +
      geom_line(data = series,
                aes(x = year, y = value_scaled, linetype = "Dispatchable"),
                colour = "red4",
                linewidth = 0.8) +
      scale_linetype_manual(name = "Dispatchable", values = c("Dispatchable" = "dashed")) +
      guides(fill = guide_legend(order = 1),
             linetype = guide_legend(order = 2))
  }


  # ----- 10. Return plot ------------------------------------------------------

  return(p)

}
