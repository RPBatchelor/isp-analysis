################################################################################
#
# Scenario Comparison Helper Functions
#
# Functions for filtering and comparing multiple ISP scenarios dynamically
#
# Ryan Batchelor
# 1 December 2025
#
################################################################################


#' Filter ISP data for multiple scenario combinations
#'
#' @param data The ISP dataset (e.g., isp_generator_capacity)
#' @param scenarios A data frame with columns: source, scenario, cdp
#'                  Optionally include scenario_label for labeling
#' @param add_label Logical. If TRUE and scenario_label doesn't exist, creates one
#'
#' @return Filtered dataset with optional scenario_label column
filter_scenarios <- function(data, scenarios, add_label = TRUE) {

  # Create scenario label if it doesn't exist
  if (add_label && !"scenario_label" %in% names(scenarios)) {
    scenarios <- scenarios |>
      mutate(scenario_label = paste(source, scenario, cdp, sep = " | "))
  }

  # Filter and join
  result <- data |>
    semi_join(scenarios, by = c("source", "scenario", "cdp"))

  # Add labels if requested
  if (add_label && "scenario_label" %in% names(scenarios)) {
    result <- result |>
      left_join(scenarios |> select(source, scenario, cdp, scenario_label),
                by = c("source", "scenario", "cdp"))
  }

  return(result)
}


#' Build scenarios from user selections (for Shiny)
#' Useful when users select scenarios through UI inputs
#'
#' @param sources Vector of source selections
#' @param scenarios Vector of scenario selections
#' @param cdps Vector of CDP selections
#' @param labels Optional vector of custom labels
#'
#' @return Tibble with scenario combinations and labels
build_scenarios_from_selections <- function(sources, scenarios, cdps, labels = NULL) {

  # Ensure all vectors are same length
  n <- length(sources)
  if (length(scenarios) != n || length(cdps) != n) {
    stop("sources, scenarios, and cdps must be same length")
  }

  # Create scenarios tibble
  result <- tibble(
    source = sources,
    scenario = scenarios,
    cdp = cdps
  )

  # Add labels if provided, otherwise generate them
  if (!is.null(labels) && length(labels) == n) {
    result$scenario_label <- labels
  } else {
    result$scenario_label <- paste(sources, scenarios, cdps, sep = " | ")
  }

  return(result)
}


#' Get all available scenario combinations from a dataset
#'
#' @param data The ISP dataset
#'
#' @return Tibble of unique source/scenario/cdp combinations
get_available_scenarios <- function(data) {
  data |>
    distinct(source, scenario, cdp) |>
    arrange(source, scenario, cdp)
}
