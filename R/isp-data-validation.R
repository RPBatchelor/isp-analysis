################################################################################
#
# ISP Data Validation Functions
#
# Functions to validate ISP data files and processed data to catch errors early
# and provide helpful error messages.
#
# RBatchelor
# December 2024
#
################################################################################


#' Validate ISP File Exists
#'
#' Checks if a file path exists and is readable
#'
#' @param file_path Path to file
#' @return TRUE if valid, stops with error if not
#'
validate_file_exists <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  if (!file.access(file_path, 4) == 0) {
    stop(paste("File is not readable:", file_path))
  }

  return(TRUE)
}


#' Validate Excel Sheet Exists
#'
#' Checks if a sheet exists in an Excel file
#'
#' @param file_path Path to Excel file
#' @param sheet_name Name of sheet to check
#' @return TRUE if valid, stops with error if not
#'
validate_sheet_exists <- function(file_path, sheet_name) {
  validate_file_exists(file_path)

  sheets <- readxl::excel_sheets(file_path)

  if (!sheet_name %in% sheets) {
    stop(paste0(
      "Sheet '", sheet_name, "' not found in ", file_path, "\n",
      "Available sheets: ", paste(sheets, collapse = ", ")
    ))
  }

  return(TRUE)
}


#' Validate ISP Directory Structure
#'
#' Checks if the expected directory structure exists for an ISP version
#'
#' @param isp_source ISP version (e.g., "2024_final")
#' @return List with validation results
#'
validate_isp_directory <- function(isp_source) {

  path_config <- isp_paths_config |>
    filter(isp_source == !!isp_source)

  if (nrow(path_config) == 0) {
    return(list(
      valid = FALSE,
      message = paste("No path configuration found for", isp_source)
    ))
  }

  base_path <- path_config$base_path[1]

  if (!dir.exists(base_path)) {
    return(list(
      valid = FALSE,
      message = paste("Directory not found:", base_path),
      suggestion = "Data may not be downloaded yet. Run script 01-download-aemo-isp-source-data.R"
    ))
  }

  # Check for files
  files <- list.files(base_path, pattern = ".xlsx", recursive = TRUE)

  if (length(files) == 0) {
    return(list(
      valid = FALSE,
      message = paste("No Excel files found in:", base_path),
      suggestion = "Check if data was extracted from zip file correctly"
    ))
  }

  return(list(
    valid = TRUE,
    message = paste("Found", length(files), "Excel file(s) in", base_path),
    file_count = length(files)
  ))
}


#' Validate Processed ISP Data
#'
#' Checks processed data for common issues
#'
#' @param df Processed data frame
#' @param data_type Type of data being validated
#' @return List with validation results
#'
validate_processed_data <- function(df, data_type) {

  issues <- list()

  # Check if data frame is empty
  if (nrow(df) == 0) {
    issues$empty_data <- "Data frame has zero rows"
  }

  # Check for required columns
  required_cols <- c("source", "scenario", "year", "value", "unit")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    issues$missing_columns <- paste("Missing columns:", paste(missing_cols, collapse = ", "))
  }

  # Check for NA values in critical columns
  if ("value" %in% names(df)) {
    na_count <- sum(is.na(df$value))
    if (na_count > 0) {
      pct <- round(100 * na_count / nrow(df), 1)
      issues$na_values <- paste0(na_count, " NA values (", pct, "%) in 'value' column")
    }
  }

  # Check for negative values where they shouldn't be
  if ("value" %in% names(df) && !data_type %in% c("emissions")) {
    neg_count <- sum(df$value < 0, na.rm = TRUE)
    if (neg_count > 0) {
      issues$negative_values <- paste0(neg_count, " negative values in 'value' column")
    }
  }

  # Check year range is reasonable
  if ("year" %in% names(df)) {
    year_range <- range(df$year, na.rm = TRUE)
    if (year_range[1] < 2000 || year_range[2] > 2100) {
      issues$year_range <- paste("Unusual year range:", paste(year_range, collapse = "-"))
    }
  }

  # Check for duplicate rows
  dup_count <- sum(duplicated(df))
  if (dup_count > 0) {
    issues$duplicates <- paste(dup_count, "duplicate rows found")
  }

  # Return validation results
  if (length(issues) == 0) {
    return(list(
      valid = TRUE,
      message = "Data validation passed",
      row_count = nrow(df),
      year_range = if ("year" %in% names(df)) range(df$year, na.rm = TRUE) else NULL
    ))
  } else {
    return(list(
      valid = FALSE,
      issues = issues,
      row_count = nrow(df)
    ))
  }
}


#' Validate and Report
#'
#' Validates processed data and prints a report
#'
#' @param df Processed data frame
#' @param data_type Type of data
#' @param stop_on_error Whether to stop execution if validation fails
#'
validate_and_report <- function(df, data_type, stop_on_error = FALSE) {

  message("\n--- Data Validation Report ---")
  validation <- validate_processed_data(df, data_type)

  if (validation$valid) {
    message("✓ Validation passed")
    message(paste("  -", validation$row_count, "rows"))
    if (!is.null(validation$year_range)) {
      message(paste("  - Year range:",
                    paste(validation$year_range, collapse = " to ")))
    }
  } else {
    message("✗ Validation found issues:")
    for (issue_name in names(validation$issues)) {
      message(paste("  -", validation$issues[[issue_name]]))
    }

    if (stop_on_error) {
      stop("Data validation failed. See issues above.")
    }
  }
  message("------------------------------\n")

  invisible(validation)
}


#' Check Configuration Coverage
#'
#' Checks if configuration exists for all expected data types and ISP versions
#'
#' @return Data frame showing configuration coverage
#' @export
#'
check_config_coverage <- function() {

  data_types <- c("generation_capacity", "generation_output",
                  "storage_capacity", "storage_output",
                  "retirements", "emissions")

  isp_versions <- c("2026_draft", "2024_final", "2022_final",
                    "2020_final", "2018_final")

  coverage <- expand_grid(
    data_type = data_types,
    isp_source = isp_versions
  ) |>
    left_join(
      isp_data_config |>
        filter(!str_detect(isp_source, "_cf")) |>
        select(data_type, isp_source) |>
        distinct() |>
        mutate(configured = TRUE),
      by = c("data_type", "isp_source")
    ) |>
    mutate(configured = replace_na(configured, FALSE))

  message("\n=== Configuration Coverage ===\n")

  coverage_wide <- coverage |>
    pivot_wider(names_from = isp_source, values_from = configured)

  print(coverage_wide)

  missing_count <- sum(!coverage$configured)
  total_count <- nrow(coverage)

  message(paste0(
    "\nConfigured: ", total_count - missing_count, "/", total_count,
    " (", round(100 * (total_count - missing_count) / total_count, 1), "%)"
  ))

  message("==============================\n")

  invisible(coverage)
}
