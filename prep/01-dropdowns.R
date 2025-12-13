################################################################################
#
# Create Lookup Tables for Shiny App Dropdowns
#
# This script creates the lookup/reference tables that the Shiny app uses
# to populate dropdown menus and filters.
#
# Dependencies: Requires processed ISP data files to exist
#
# RBatchelor
# December 2024
#
################################################################################


# =============================================================================
# Load Required Data
# =============================================================================

message("\nCreating Shiny app lookup tables...")

# Load the processed ISP data
load("shiny-webtool/data/isp_generator_capacity.rda")
load("shiny-webtool/data/odp_table.rda")


# =============================================================================
# Create Lookup Tables
# =============================================================================

# ISP source list (simple list of all ISP versions)
isp_source_list <- odp_table |>
  pull(isp_source)

message("  ✓ isp_source_list: ", length(isp_source_list), " ISP versions")

save(isp_source_list, file = "shiny-webtool/data/isp_source_list.rda")


# Source-Scenario-Pathway list (for dropdown filters)
# Contains all unique combinations of source, scenario, cdp, region, and technology
source_scenario_pathway_list <- isp_generator_capacity |>
  distinct(source, scenario, cdp, region, technology) |>
  arrange(source, scenario, cdp, region, technology)

message("  ✓ source_scenario_pathway_list: ", nrow(source_scenario_pathway_list), " rows")

save(source_scenario_pathway_list, file = "shiny-webtool/data/source_scenario_pathway_list.rda")


# Region list (for region filter)
region_list <- isp_generator_capacity |>
  distinct(region) |>
  arrange(region)

message("  ✓ region_list: ", nrow(region_list), " regions")

save(region_list, file = "shiny-webtool/data/region_list.rda")


message("✓ All lookup tables saved to shiny-webtool/data/\n")