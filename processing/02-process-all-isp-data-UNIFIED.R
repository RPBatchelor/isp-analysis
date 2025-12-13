################################################################################
#
# Unified ISP Data Processing Script
#
# This script replaces the old scripts 02-08 with a single unified approach.
# Processes all ISP data types using the new framework.
#
# OLD SCRIPTS (can be deprecated):
#   - 02-load-isp-generation-capacity.R   (244 lines)
#   - 03-load-isp-generation-output.R     (244 lines)
#   - 04-storage-capacity.R               (252 lines)
#   - 05-storage_output.R                 (254 lines)
#   - 06-retirements.R                    (102 lines)
#   - 08-emissions.R                      (209 lines)
#
# NEW APPROACH:
#   - This file                           (70 lines)
#   - Configuration-driven, validated, consistent
#
# RBatchelor
# December 2024
#
################################################################################


# =============================================================================
# Setup
# =============================================================================

message("\n")
message("================================================================================")
message("             UNIFIED ISP DATA PROCESSING PIPELINE")
message("================================================================================")
message("")

# Source required functions
source("processing/00-utils.R")  # Load configuration objects
source("R/extract-isp-generation-storage-data.R")  # Load read functions
source("R/isp-processing-helpers.R")
source("R/process-isp-data-unified.R")
source("R/isp-data-validation.R")

# Check configuration coverage
message("\nChecking configuration coverage...")
check_config_coverage()


# =============================================================================
# Process All Data Types
# =============================================================================

# 1. Generation Capacity
message("\n[1/6] Processing Generation Capacity...")
isp_generator_capacity <- process_all_isp_years(data_type = "generation_capacity")
validate_and_report(isp_generator_capacity, "generation_capacity")
save_isp_data(isp_generator_capacity, "isp_generator_capacity")


# 2. Generation Output
message("\n[2/6] Processing Generation Output...")
isp_generation_output <- process_all_isp_years(data_type = "generation_output")
validate_and_report(isp_generation_output, "generation_output")
save_isp_data(isp_generation_output, "isp_generation_output")


# 3. Storage Capacity
message("\n[3/6] Processing Storage Capacity...")
isp_storage_capacity <- process_all_isp_years(data_type = "storage_capacity")
validate_and_report(isp_storage_capacity, "storage_capacity")
save_isp_data(isp_storage_capacity, "isp_storage_capacity")


# 4. Storage Output
message("\n[4/6] Processing Storage Output...")
isp_storage_output <- process_all_isp_years(data_type = "storage_output")
validate_and_report(isp_storage_output, "storage_output")
save_isp_data(isp_storage_output, "isp_storage_output")


# 5. Retirements (2024 & 2026 only)
message("\n[5/6] Processing Retirements...")
isp_retirements <- process_all_isp_years(
  data_type = "retirements",
  include_years = c("2026_draft", "2024_final")
)
validate_and_report(isp_retirements, "retirements")
save_isp_data(isp_retirements, "isp_retirements")


# 6. Emissions (2020, 2022, 2024, 2026)
message("\n[6/6] Processing Emissions...")
isp_emissions <- process_all_isp_years(
  data_type = "emissions",
  include_years = c("2026_draft", "2024_final", "2022_final", "2020_final")
)
validate_and_report(isp_emissions, "emissions")
save_isp_data(isp_emissions, "isp_emissions")


# =============================================================================
# Summary
# =============================================================================

message("\n")
message("================================================================================")
message("                         PROCESSING COMPLETE")
message("================================================================================")
message("")
message("Datasets processed:")
message("  ✓ Generation Capacity  : ", nrow(isp_generator_capacity), " rows")
message("  ✓ Generation Output    : ", nrow(isp_generation_output), " rows")
message("  ✓ Storage Capacity     : ", nrow(isp_storage_capacity), " rows")
message("  ✓ Storage Output       : ", nrow(isp_storage_output), " rows")
message("  ✓ Retirements          : ", nrow(isp_retirements), " rows")
message("  ✓ Emissions            : ", nrow(isp_emissions), " rows")
message("")
message("Output files saved to:")
message("  - shiny-webtool/data/*.rda (for Shiny app)")
message("  - data/*.csv (for analysis)")
message("")
message("================================================================================")
message("\n")


# Cleanup
rm(isp_generator_capacity, isp_generation_output,
   isp_storage_capacity, isp_storage_output,
   isp_retirements, isp_emissions)
