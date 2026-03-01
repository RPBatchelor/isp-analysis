################################################################################
#
# Unified ISP Data Processing Framework
#
# Single function to process any ISP data type across all ISP versions,
# replacing the duplicated code in scripts 02-08.
#
# RBatchelor
# December 2024
#
################################################################################


#' Process ISP Data for a Specific Year and Data Type
#'
#' Generic function to process ISP data for modern ISP versions (2022+, 2026)
#' that have consistent structure
#'
#' @param data_type Type of data to process (e.g., "generation_capacity")
#' @param isp_source ISP version (e.g., "2024_final", "2026_draft")
#' @param base_path Path to data files
#' @param read_function Function to use for reading Excel files
#' @return Processed data frame
#'
process_modern_isp_year <- function(data_type, isp_source, base_path, read_function) {

  # Get configuration
  config <- get_data_config(data_type, isp_source)

  # Get list of files
  list_files <- list.files(base_path, pattern = ".xlsx")

  # Read all files
  all_data <- map(list_files, ~ {
    read_function(
      path = paste0(base_path, .x),
      sheet = config$sheet_name,
      range = config$cell_range,
      scenario = str_extract(.x, pattern = "-\\D*-"),
      output_unit = config$unit,
      source_data = isp_source
    )
  })

  # Combine and apply standard transformations
  result <- bind_rows(all_data) |>
    mutate(
      year = str_replace(year, "_", "-"),
      scenario = str_trim(str_remove_all(scenario, "-")),
      scenario = str_to_lower(scenario),
      cdp = str_to_lower(cdp)
    ) |>
    create_year_ending()

  # Group and summarize based on value column type
  value_col <- config$value_column

  # Handle column name variations - storage data may have 'technology' column
  # that should be renamed to 'storage_category'
  if (value_col == "storage_category" && "technology" %in% names(result) && !"storage_category" %in% names(result)) {
    result <- result |> rename(storage_category = technology)
  }

  if (value_col == "technology") {
    result <- result |>
      group_by(cdp, region, technology, year, unit, scenario, source, year_ending) |>
      summarise(value = sum(value), .groups = "drop")
  } else if (value_col == "storage_category") {
    result <- result |>
      group_by(cdp, region, storage_category, year, unit, scenario, source, year_ending) |>
      summarise(value = sum(value), .groups = "drop")
  } else if (value_col == "station") {
    result <- result |>
      group_by(cdp, region, station, year, unit, scenario, source, year_ending) |>
      summarise(value = sum(value), .groups = "drop")
  } else {
    # For emissions and other types with just region
    result <- result |>
      group_by(cdp, region, year, unit, scenario, source, year_ending) |>
      summarise(value = sum(value), .groups = "drop")
  }

  return(result)
}


#' Process 2022 ISP Data
#'
#' Processes 2022 ISP data which has a slightly different structure
#'
#' @param data_type Type of data to process
#' @param read_function Function to use for reading Excel files
#' @return Processed data frame
#'
process_2022_data <- function(data_type, read_function) {

  config <- get_data_config(data_type, "2022_final")
  base_path <- "raw-data/2022_final/Final ISP Results/Scenarios/"

  list_files <- list.files(base_path, pattern = ".xlsx")

  all_data <- map(list_files, ~ {
    read_function(
      path = paste0(base_path, .x),
      sheet = config$sheet_name,
      range = config$cell_range,
      scenario = str_extract(.x, pattern = "-\\D*-"),
      output_unit = config$unit,
      source_data = "2022_final"
    )
  })

  result <- bind_rows(all_data) |>
    mutate(
      scenario = str_trim(str_remove_all(scenario, "-")),
      scenario = str_to_lower(scenario),
      cdp = str_to_lower(cdp)
    ) |>
    create_year_ending()

  # Remove columns that don't exist in other years
  if ("existing_and_committed" %in% names(result)) {
    result <- result |> select(-existing_and_committed)
  }
  if ("total" %in% names(result)) {
    result <- result |> select(-total)
  }

  # Rename technology to storage_category for storage types
  value_col <- config$value_column
  if (value_col == "storage_category" && "technology" %in% names(result)) {
    result <- result |> rename(storage_category = technology)
  }

  return(result)
}


#' Process 2018 ISP Data
#'
#' Processes 2018 ISP data which has the most different structure
#'
#' @param data_type Type of data to process
#' @param read_function Function to use for reading Excel files (2018 version)
#' @return Processed data frame
#'
process_2018_data <- function(data_type, read_function) {

  # 2018 doesn't have storage or emissions data
  if (!data_type %in% c("generation_capacity", "generation_output")) {
    return(NULL)
  }

  config <- get_data_config(data_type, "2018_final")
  base_path <- "raw-data/2018_final/2018 ISP Generation and Transmission Outlooks/"

  # Filter out transmission files
  list_files <- list.files(base_path, pattern = ".xlsx") |>
    as_tibble() |>
    filter(!str_detect(value, "Transmission")) |>
    pull(value)

  all_data <- map(list_files, ~ {
    read_function(
      path = paste0(base_path, .x),
      sheet = config$sheet_name,
      range = config$cell_range,
      scenario = str_extract(.x, pattern = "-\\s.*"),
      output_unit = config$unit,
      source_data = "2018_final"
    )
  })

  bind_rows(all_data) |>
    mutate(
      cdp = "default",
      scenario = str_remove_all(scenario, "-"),
      scenario = str_replace(scenario, ".xlsx", ""),
      scenario = str_trim(scenario),
      scenario = str_to_lower(scenario)
    ) |>
    create_year_ending()
}


#' Process All ISP Years for a Data Type
#'
#' Master function that processes all ISP years for a given data type
#'
#' @param data_type Type of data to process (e.g., "generation_capacity", "storage_output")
#' @param include_years Vector of ISP years to include (default: all)
#' @return Combined data frame with all years
#' @export
#'
process_all_isp_years <- function(data_type,
                                   include_years = c("2026_draft", "2024_final",
                                                     "2022_final", "2020_final",
                                                     "2018_final")) {

  message(paste("\n========================================"))
  message(paste("Processing", data_type))
  message(paste("========================================\n"))

  results <- list()

  # 2026 Draft
  if ("2026_draft" %in% include_years) {
    message("Processing 2026_draft...")
    tryCatch({
      results$combined_2026_draft <- process_modern_isp_year(
        data_type = data_type,
        isp_source = "2026_draft",
        base_path = "raw-data/2026_draft/Cores/",
        read_function = read_isp_capacity_generation
      )
      message("  ✓ 2026_draft complete")
    }, error = function(e) {
      message(paste("  ✗ 2026_draft failed:", e$message))
      results$combined_2026_draft <<- NULL
    })
  }

  # 2024 Final
  if ("2024_final" %in% include_years) {
    message("Processing 2024_final...")
    tryCatch({
      results$combined_2024_final <- process_modern_isp_year(
        data_type = data_type,
        isp_source = "2024_final",
        base_path = "raw-data/2024_final/Core/",
        read_function = read_isp_capacity_generation
      )
      message("  ✓ 2024_final complete")
    }, error = function(e) {
      message(paste("  ✗ 2024_final failed:", e$message))
      results$combined_2024_final <<- NULL
    })
  }

  # 2022 Final
  if ("2022_final" %in% include_years) {
    message("Processing 2022_final...")
    tryCatch({
      results$combined_2022_final <- process_2022_data(
        data_type = data_type,
        read_function = read_isp_capacity_generation
      )
      message("  ✓ 2022_final complete")
    }, error = function(e) {
      message(paste("  ✗ 2022_final failed:", e$message))
      results$combined_2022_final <<- NULL
    })
  }

  # 2020 Final
  if ("2020_final" %in% include_years) {
    message("Processing 2020_final...")
    tryCatch({
      config <- get_data_config(data_type, "2020_final")

      combined_2020 <- process_2020_files(
        data_type = data_type,
        read_function = read_isp_capacity_generation_2020,
        output_unit = config$unit
      )

      # Apply filters specific to data type
      if (data_type %in% c("generation_output", "storage_capacity", "storage_output")) {
        combined_2020 <- combined_2020 |>
          filter(!region %in% c("Total", "Total including DSP", "Total excluding storage",
                                "NEM", "Region")) |>
          filter(!str_detect(region, "Pump Load")) |>
          drop_na(region)
      } else if (data_type == "generation_capacity") {
        combined_2020 <- combined_2020 |>
          select(-existing_committed_in_2021) |>
          filter(!region %in% c("Total", "Total including DSP", "NEM", "Region")) |>
          drop_na(region)
      } else if (data_type == "emissions") {
        combined_2020 <- combined_2020 |>
          rename(region = emissions) |>
          select(-total)
      }

      # Rename technology to storage_category for storage types
      value_col <- config$value_column
      if (value_col == "storage_category" && "technology" %in% names(combined_2020)) {
        combined_2020 <- combined_2020 |>
          filter(region != "Total Storage Capacity") |>
          rename(storage_category = technology)
      }

      # Lowercase scenario names for consistency
      combined_2020 <- combined_2020 |>
        mutate(scenario = str_to_lower(scenario))

      results$combined_2020_final <- combined_2020
      message("  ✓ 2020_final complete")

    }, error = function(e) {
      message(paste("  ✗ 2020_final failed:", e$message))
      results$combined_2020_final <<- NULL
    })
  }

  # 2018 Final
  if ("2018_final" %in% include_years) {
    message("Processing 2018_final...")
    tryCatch({
      results$combined_2018_final <- process_2018_data(
        data_type = data_type,
        read_function = read_isp_capacity_generation_2018
      )
      if (!is.null(results$combined_2018_final)) {
        message("  ✓ 2018_final complete")
      } else {
        message("  - 2018_final not applicable for this data type")
      }
    }, error = function(e) {
      message(paste("  ✗ 2018_final failed:", e$message))
      results$combined_2018_final <<- NULL
    })
  }

  # Combine all results
  message("\nCombining all years...")
  combined <- bind_rows(compact(results))

  # Apply final transformations
  message("Applying final transformations...")

  # Apply name standardization (normalises legacy spelling variants)
  if ("technology" %in% names(combined)) {
    combined <- standardize_technology_names(combined)
  }
  if ("storage_category" %in% names(combined)) {
    combined <- standardize_storage_names(combined)
  }

  # Add coerced category columns (maps all versions → ISP 2026 canonical names)
  if ("technology" %in% names(combined)) {
    combined <- add_coerced_tech_cat(combined, col = "technology")
  }
  if ("storage_category" %in% names(combined)) {
    combined <- add_coerced_storage_cat(combined)
  }

  # Apply common transformations
  combined <- combined |>
    mutate(year = as.numeric(year(year_ending))) |>
    determine_odp()

  message(paste("✓ Processing complete!", nrow(combined), "rows"))
  message(paste("========================================\n"))

  return(combined)
}
