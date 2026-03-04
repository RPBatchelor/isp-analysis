

# ==============================================================================
# Extract CDP summary tables from each ISP source
# ==============================================================================


# Draft 2026 table
table_26 <- read_xlsx(
  path  = "raw-data/2026_draft/Cores/20251210_2026 ISP - Step Change - Core.xlsx",
  sheet = "CDPs",
  range = "A3:W27"
)

# 2024 table
table_24 <- read_xlsx(
  path  = "raw-data/2024_final/Core/2024 ISP - Step Change - Core.xlsx",
  sheet = "CDPs",
  range = "A4:R30"
)

# 2022 table
table_22 <- read_xlsx(
  path  = "raw-data/2022_final/Draft ISP Results/Scenarios/2022 Draft ISP results workbook - Step Change - Draft ISP Inputs.xlsx",
  sheet = "CDPs",
  range = "A4:H20"
) |>
  rename("CDP" = "...1")


# ==============================================================================
# Combine into a nested tibble and save
# ==============================================================================

cdp_tables <- tibble(
  isp_source = c("2022_final", "2024_final", "2026_draft"),
  cdp_table  = list(table_22, table_24, table_26)
)

save(cdp_tables, file = "shiny-webtool/data/cdp_tables.rda")


isp_list <- c("2018_final",
              "2020_final",
              "2022_final",
              "2024_final",
              "2026_draft")



