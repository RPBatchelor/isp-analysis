################################################################################
#
# Technology Crosswalk
#
# Defines the canonical ISP 2026 technology categories and maps all legacy ISP
# version names and OpenNEM fueltech codes back to them.
#
# Design principles:
#   - ISP 2026 Draft is the definitive source of canonical category names.
#   - Original technology / fueltech / storage_category columns are NEVER
#     modified. The crosswalk adds a new coerced_tech_cat / coerced_storage_cat
#     column that is safe to use for cross-source comparisons.
#   - Unknown names pass through unchanged with a warning, so new ISP versions
#     do not silently break charts.
#
# Ryan Batchelor
# March 2026
#
################################################################################


# ===== Generation / output technology crosswalk ================================
#
# Covers:
#   - ISP 2026 canonical names (self-mapping)
#   - Legacy ISP names from 2018, 2020, 2022, 2024 versions
#   - OpenNEM fueltech codes (snake_case)
#
# Canonical names are the 18 technology categories present in ISP 2026 Draft
# generation output data, plus mid-merit gas with ccs and hydrogen turbine
# which appear in earlier ISPs and may return in future projections.

isp_tech_crosswalk <- tibble::tribble(
  ~raw_name,                               ~coerced_tech_cat,

  # ── ISP 2026 canonical names (self-mapping) ──────────────────────────────
  "black coal",                            "black coal",
  "brown coal",                            "brown coal",
  "mid-merit gas",                         "mid-merit gas",
  "mid-merit gas with ccs",               "mid-merit gas with ccs",
  "flexible gas",                          "flexible gas",
  "flexible gas with ccs",                "flexible gas with ccs",
  "other renewable fuels",                "other renewable fuels",
  "hydro",                                "hydro",
  "wind",                                 "wind",
  "offshore wind",                        "offshore wind",
  "utility-scale solar",                  "utility-scale solar",
  "distributed pv",                       "distributed pv",
  "utility-scale storage",               "utility-scale storage",
  "utility-scale storage load",          "utility-scale storage load",
  "coordinated cer storage",             "coordinated cer storage",
  "coordinated cer storage load",        "coordinated cer storage load",
  "passive cer storage",                 "passive cer storage",
  "passive cer storage load",            "passive cer storage load",
  "dsp",                                 "dsp",

  # ── ISP 2024 / 2022 / 2020 / 2018 names not in 2026 ─────────────────────
  # Gas: pre-2024 ISPs used "peaking gas+liquids" for what 2026 calls "flexible gas"
  "peaking gas+liquids",                  "flexible gas",

  # Renewables: older ISPs listed these separately; 2026 uses "other renewable fuels"
  "biomass",                              "other renewable fuels",
  "solar thermal",                        "other renewable fuels",
  "hydrogen turbine",                     "other renewable fuels",

  # Rooftop solar: older ISPs used a longer label
  "rooftop and other small-scale solar", "distributed pv",

  # Storage (generation-side): 2018 ISP labelled charging/discharging separately
  "utility storage generation",          "utility-scale storage",
  "distributed storage generation",      "passive cer storage",

  # Storage load names: various pre-2026 conventions
  "utility storage load",                "utility-scale storage load",   # 2018, 2024
  "distributed storage load",            "passive cer storage load",     # 2018, 2022
  "behind the meter storage load",       "passive cer storage load",     # 2020
  "dispatchable storage load",           "utility-scale storage load",   # 2020
  "coordinated der storage load",        "coordinated cer storage load", # 2022

  # ── OpenNEM fueltech codes ────────────────────────────────────────────────
  # Coal
  "coal_black",                          "black coal",
  "coal_brown",                          "brown coal",

  # Gas — mapped to the ISP 2026 gas categories by operational profile:
  #   CCGT / steam → mid-merit (baseload-ish gas)
  #   OCGT / recip / WCMG / distillate → flexible (peaking)
  "gas_ccgt",                            "mid-merit gas",
  "gas_steam",                           "mid-merit gas",
  "gas_ocgt",                            "flexible gas",
  "gas_recip",                           "flexible gas",
  "gas_wcmg",                            "flexible gas",
  "distillate",                          "flexible gas",

  # Renewables
  # Note: "hydro" and "wind" are already listed in the ISP canonical block above
  # with identical mappings, so they are omitted here to keep raw_name unique.
  "bioenergy_biomass",                   "other renewable fuels",
  "solar_utility",                       "utility-scale solar",
  "solar_rooftop",                       "distributed pv",

  # Storage — battery_charging / pumps are load (negative generation)
  "battery_discharging",                 "utility-scale storage",
  "battery_charging",                    "utility-scale storage load",
  "pumps",                               "utility-scale storage load",
  "battery",                             "utility-scale storage"       # net battery
)


# ===== Storage capacity / output category crosswalk ===========================
#
# Covers the storage_category column in isp_storage_capacity / isp_storage_output.
# ISP 2026 uses "shallow/medium/deep utility-scale storage" where earlier ISPs
# used "shallow/medium/deep storage".  Snowy 2.0, Borumba, coordinated/passive
# CER storage are self-mapping.

isp_storage_crosswalk <- tibble::tribble(
  ~raw_name,                              ~coerced_storage_cat,

  # ── ISP 2026 canonical storage categories (self-mapping) ─────────────────
  "shallow utility-scale storage",        "shallow utility-scale storage",
  "medium utility-scale storage",         "medium utility-scale storage",
  "deep utility-scale storage",           "deep utility-scale storage",
  "snowy 2.0",                            "snowy 2.0",
  "borumba",                              "borumba",
  "coordinated cer storage",              "coordinated cer storage",
  "passive cer storage",                  "passive cer storage",

  # ── Pre-2026 storage names ────────────────────────────────────────────────
  "shallow storage",                      "shallow utility-scale storage",
  "medium storage",                       "medium utility-scale storage",
  "deep storage",                         "deep utility-scale storage",
  "distributed storage",                  "passive cer storage",           # 2022
  "behind the meter storage",             "passive cer storage"            # 2020
)


# ===== Helper functions =======================================================

#' Add Coerced Technology Category
#'
#' Adds a `coerced_tech_cat` column that maps the raw technology / fueltech
#' column to ISP 2026 canonical category names.  The original column is
#' preserved unchanged.
#'
#' Works for both ISP data (col = "technology") and OpenNEM data
#' (col = "fueltech").
#'
#' @param df   Data frame containing the column to map.
#' @param col  Name of the column to map from (default "technology").
#' @return     df with an additional `coerced_tech_cat` character column.
#'
add_coerced_tech_cat <- function(df, col = "technology") {

  if (!col %in% names(df)) {
    warning(paste0("add_coerced_tech_cat: column '", col, "' not found — returning unchanged."))
    return(df)
  }

  # Named vector for O(1) lookup – avoids left_join column-conflict issues
  crosswalk_vec <- setNames(isp_tech_crosswalk$coerced_tech_cat,
                            isp_tech_crosswalk$raw_name)

  pass_through <- setdiff(unique(df[[col]]), names(crosswalk_vec))
  if (length(pass_through) > 0) {
    warning(paste0(
      "add_coerced_tech_cat: these values are not in the crosswalk and will ",
      "pass through unchanged:\n  ",
      paste(sort(pass_through), collapse = ", ")
    ))
  }

  df |>
    mutate(coerced_tech_cat = coalesce(unname(crosswalk_vec[.data[[col]]]),
                                       .data[[col]]))
}


#' Add Coerced Storage Category
#'
#' Adds a `coerced_storage_cat` column that maps the raw `storage_category`
#' column to ISP 2026 canonical storage category names.
#'
#' @param df   Data frame containing a `storage_category` column.
#' @return     df with an additional `coerced_storage_cat` character column.
#'
add_coerced_storage_cat <- function(df) {

  if (!"storage_category" %in% names(df)) {
    warning("add_coerced_storage_cat: column 'storage_category' not found — returning unchanged.")
    return(df)
  }

  crosswalk_vec <- setNames(isp_storage_crosswalk$coerced_storage_cat,
                            isp_storage_crosswalk$raw_name)

  pass_through <- setdiff(unique(df$storage_category), names(crosswalk_vec))
  if (length(pass_through) > 0) {
    warning(paste0(
      "add_coerced_storage_cat: these storage categories are not in the crosswalk ",
      "and will pass through unchanged:\n  ",
      paste(sort(pass_through), collapse = ", ")
    ))
  }

  df |>
    mutate(coerced_storage_cat = coalesce(unname(crosswalk_vec[storage_category]),
                                          storage_category))
}
