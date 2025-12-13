################################################################################
#
# ISP Processing Helper Functions
#
# Common functions used across ISP data processing scripts to reduce duplication
# and centralize logic.
#
# RBatchelor
# December 2024
#
################################################################################


#' Get 2020 ISP File List
#'
#' Creates a file list for 2020 ISP data which uses scenario-based folder structure
#'
#' @param scenario_name Name of the scenario (e.g., "Central", "Step Change")
#' @return Tibble with file information including scenario
#'
get_2020_file_list <- function(scenario_name) {
  folder <- paste0("raw-data/2020_final/", scenario_name, "/")
  files <- list.files(folder, pattern = ".xlsx") |>
    as_tibble() |>
    mutate(scenario = scenario_name)
  return(files)
}


#' Standardize Technology Names
#'
#' Applies technology name standardization to match ISP 2024 conventions
#'
#' @param df Data frame with a 'technology' column
#' @return Data frame with standardized technology names
#'
standardize_technology_names <- function(df) {
  if (!"technology" %in% names(df)) {
    return(df)
  }

  df |>
    mutate(
      # First convert to lowercase for consistency
      technology = str_to_lower(technology),
      # Then apply name mappings
      technology = case_when(
        technology %in% names(technology_name_map) ~ unname(technology_name_map[technology]),
        .default = technology
      )
    )
}


#' Standardize Storage Category Names
#'
#' Applies storage category name standardization
#'
#' @param df Data frame with a 'storage_category' column
#' @return Data frame with standardized storage category names
#'
standardize_storage_names <- function(df) {
  if (!"storage_category" %in% names(df)) {
    return(df)
  }

  df |>
    mutate(
      # First convert to lowercase for consistency
      storage_category = str_to_lower(storage_category),
      # Then apply name mappings
      storage_category = case_when(
        storage_category %in% names(storage_name_map) ~ unname(storage_name_map[storage_category]),
        .default = storage_category
      )
    )
}


#' Determine Optimal Development Pathway (ODP)
#'
#' Marks which scenarios represent the optimal development pathway for each ISP
#'
#' @param df Data frame with 'source' and 'cdp' columns
#' @return Data frame with 'odp' boolean column added
#'
determine_odp <- function(df) {
  df |>
    mutate(odp = case_when(
      source == "2018_final" & cdp == "default" ~ TRUE,
      source == "2020_final" & cdp == "dp1" ~ TRUE,
      source == "2022_final" & cdp == "cdp8" ~ TRUE,
      source == "2024_final" & cdp == "cdp14" ~ TRUE,
      source == "2026_draft" & cdp == "cdp4" ~ TRUE,
      .default = FALSE
    ))
}


#' Create Year Ending Date
#'
#' Creates a year_ending date column from year string based on ISP version
#'
#' @param df Data frame with 'year' and 'source' columns
#' @return Data frame with 'year_ending' date column added
#'
create_year_ending <- function(df) {
  # Join with year config to get the correct substring positions
  df_with_config <- df |>
    left_join(isp_year_config, by = c("source" = "isp_source"))

  # Extract year using the configured positions
  df_with_config |>
    mutate(year_ending = dmy(paste0("30-Jun-",
                                    substr(year, year_start, year_end)))) |>
    select(-year_start, -year_end)
}


#' Apply Common ISP Data Transformations
#'
#' Applies standard transformations used across all ISP data processing:
#' - Lowercase conversion for scenario and cdp
#' - Year ending date creation
#' - Numeric year extraction
#' - ODP determination
#'
#' @param df Data frame with ISP data
#' @return Transformed data frame
#'
apply_common_transformations <- function(df) {
  df |>
    mutate(
      scenario = str_to_lower(scenario),
      cdp = str_to_lower(cdp)
    ) |>
    create_year_ending() |>
    mutate(year = as.numeric(year(year_ending))) |>
    determine_odp()
}


#' Process 2020 ISP Files
#'
#' Processes 2020 ISP files for a given data type, handling the unique
#' 2020 folder structure and both regular and counterfactual scenarios
#'
#' @param data_type Data type to process (e.g., "generation_capacity")
#' @param read_function Function to use for reading Excel files
#' @param output_unit Unit of measurement for the data
#' @return Combined data frame with all 2020 scenarios
#'
process_2020_files <- function(data_type, read_function, output_unit) {

  # Get configuration for this data type
  config <- isp_data_config |>
    filter(data_type == !!data_type, str_detect(isp_source, "2020"))

  config_regular <- config |> filter(isp_source == "2020_final")
  config_cf <- config |> filter(isp_source == "2020_final_cf")

  # Build file list for all scenarios
  list_files <- map(isp_2020_scenarios, get_2020_file_list) |>
    bind_rows() |>
    mutate(
      file_path = paste0("raw-data/2020_final/", scenario, "/", value),
      cdp = str_extract(file_path, "\\(\\S{1,3}\\)")
    ) |>
    select(file_path, scenario, cdp)

  # Process regular scenarios
  all_data <- pmap(
    list(list_files$file_path, list_files$scenario, list_files$cdp),
    function(file_path, scenario, cdp) {
      read_function(
        path = file_path,
        sheet = config_regular$sheet_name,
        range = config_regular$cell_range,
        scenario = scenario,
        cdp = cdp,
        output_unit = output_unit,
        source_data = "2020_final"
      )
    }
  )

  # Process counterfactuals
  list_files_cf <- map(isp_2020_scenarios, get_2020_file_list) |>
    bind_rows() |>
    filter(str_detect(value, "DP1")) |>
    mutate(
      file_path = paste0("raw-data/2020_final/", scenario, "/", value),
      cdp = "counterfactual"
    ) |>
    select(file_path, scenario, cdp)

  counterfactual <- pmap(
    list(list_files_cf$file_path, list_files_cf$scenario, list_files_cf$cdp),
    function(file_path, scenario, cdp) {
      read_function(
        path = file_path,
        sheet = config_cf$sheet_name,
        range = config_cf$cell_range,
        scenario = scenario,
        cdp = cdp,
        output_unit = output_unit,
        source_data = "2020_final"
      )
    }
  )

  # Combine and clean
  bind_rows(all_data, counterfactual) |>
    mutate(
      cdp = str_remove_all(cdp, "\\("),
      cdp = str_remove_all(cdp, "\\)"),
      cdp = str_trim(cdp),
      cdp = str_to_lower(cdp)
    )
}


#' Get Configuration for Data Type and ISP Version
#'
#' Retrieves sheet name and cell range from configuration
#'
#' @param data_type Type of data (e.g., "generation_capacity")
#' @param isp_source ISP version (e.g., "2024_final")
#' @return List with sheet_name and cell_range
#'
get_data_config <- function(data_type, isp_source) {
  config <- isp_data_config |>
    filter(
      data_type == !!data_type,
      isp_source == !!isp_source
    )

  if (nrow(config) == 0) {
    stop(paste("No configuration found for", data_type, isp_source))
  }

  list(
    sheet_name = config$sheet_name[1],
    cell_range = config$cell_range[1],
    unit = config$unit[1],
    value_column = config$value_column[1]
  )
}


#' Save ISP Data to Files
#'
#' Saves processed ISP data to both .rda and .csv formats
#'
#' @param data Data frame to save
#' @param data_name Name for the data object (e.g., "isp_generator_capacity")
#' @param compress_level Compression level for .rda file (default "xz")
#'
save_isp_data <- function(data, data_name, compress_level = "xz") {
  # Create the object with the specified name
  assign(data_name, data)

  # Save to .rda
  save(list = data_name,
       file = paste0("shiny-webtool/data/", data_name, ".rda"),
       compress = compress_level,
       envir = environment())

  # Save to .csv
  write_csv(data, file = paste0("data/", data_name, ".csv"))

  message(paste("Saved", data_name, "to .rda and .csv"))
}
