# =============================================================================
# 02_epc_certificates.R
# One row per EPC certificate with its recommendations attached, ID dummies,
# potential-minus-current differences, and the single ID grouping applied.
#
# IN : data/raw/epcs/<local-authority>/certificates.csv, recommendations.csv
#      data/manual/improvement_ID_stats_q.csv  (fills missing indicative costs)
# OUT: data/intermediate/epcs/<local-authority>.csv  (per-LA, restartable)
#      data/intermediate/master_epcs_clean.csv
# =============================================================================
source(here::here("R", "setup.R"))

SELECT_MAIN      <- c("LMK_KEY", "BUILDING_REFERENCE_NUMBER", "INSPECTION_DATE", "TRANSACTION_TYPE")
SELECT_CURRENT   <- c("CURRENT_ENERGY_RATING", "CURRENT_ENERGY_EFFICIENCY", "ENERGY_CONSUMPTION_CURRENT",
                      "LIGHTING_COST_CURRENT", "HEATING_COST_CURRENT", "HOT_WATER_COST_CURRENT",
                      "ENVIRONMENT_IMPACT_CURRENT", "CO2_EMISSIONS_CURRENT")
SELECT_POTENTIAL <- c("POTENTIAL_ENERGY_RATING", "POTENTIAL_ENERGY_EFFICIENCY", "ENERGY_CONSUMPTION_POTENTIAL",
                      "LIGHTING_COST_POTENTIAL", "HEATING_COST_POTENTIAL", "HOT_WATER_COST_POTENTIAL",
                      "ENVIRONMENT_IMPACT_POTENTIAL", "CO2_EMISSIONS_POTENTIAL")
SELECT_ENERGY    <- c("HOT_WATER_ENERGY_EFF", "FLOOR_ENERGY_EFF", "WINDOWS_ENERGY_EFF", "WALLS_ENERGY_EFF",
                      "ROOF_ENERGY_EFF", "MAINHEAT_ENERGY_EFF", "MAINHEATC_ENERGY_EFF", "LIGHTING_ENERGY_EFF")
SELECT_CONTROLS  <- c("ENERGY_TARIFF", "MAIN_FUEL", "PHOTO_SUPPLY", "GLAZED_TYPE", "MAINS_GAS_FLAG", "TENURE",
                      "PROPERTY_TYPE", "BUILT_FORM", "ADDRESS", "POSTCODE", "TOTAL_FLOOR_AREA",
                      "NUMBER_HABITABLE_ROOMS", "NUMBER_HEATED_ROOMS", "EXTENSION_COUNT",
                      "CONSTRUCTION_AGE_BAND", "POSTTOWN", "UPRN")
SELECTION <- c(SELECT_MAIN, SELECT_CURRENT, SELECT_POTENTIAL, SELECT_ENERGY, SELECT_CONTROLS)

# Hand-completed average cost per (ID, year), used where a recommendation has no cost
cost_fill <- fread(man("improvement_ID_stats_q.csv"))[, .(IMPROVEMENT_ID, YEAR, FILL_COST = Average_Cost)]

dir.create(inter("epcs"), showWarnings = FALSE)
epc_dirs <- list.dirs(raw("epcs"), recursive = FALSE)

for (d in epc_dirs) {
  out_file <- inter("epcs", paste0(basename(d), ".csv"))
  if (file.exists(out_file)) next              # restartable
  message("certificates: ", basename(d))

  epcs <- fread(file.path(d, "certificates.csv"), select = SELECTION)
  rec  <- fread(file.path(d, "recommendations.csv"),
                select = c("LMK_KEY", "IMPROVEMENT_ITEM", "IMPROVEMENT_ID", "IMPROVEMENT_ID_TEXT", "INDICATIVE_COST"))
  rec  <- rec[!is.na(IMPROVEMENT_ID) & IMPROVEMENT_ID != ""]
  rec[, IMPROVEMENT_ID := as.integer(IMPROVEMENT_ID)]

  # Cost mid-point; fill missing from the (ID, year) average
  x <- gsub("[£, ]", "", as.character(rec$INDICATIVE_COST))
  lo <- as.numeric(sub("-.*$", "", x)); hi <- as.numeric(ifelse(grepl("-", x), sub("^.*-", "", x), NA))
  rec[, INDICATIVE_COST_AVG := 0.5 * (lo + fifelse(is.na(hi), lo, hi))]
  rec[epcs, YEAR := year(as.IDate(i.INSPECTION_DATE)), on = "LMK_KEY"]
  rec[cost_fill, FILL := i.FILL_COST, on = c("IMPROVEMENT_ID", "YEAR")]
  rec[is.na(INDICATIVE_COST_AVG), INDICATIVE_COST_AVG := round(FILL)]

  # Keep recommendations in the order the EPC lists them (IMPROVEMENT_ITEM).
  # Strategy 1 ("follow the EPC") depends on this order.
  setorder(rec, LMK_KEY, IMPROVEMENT_ITEM)
  rec[, IMPROVEMENT_ID_TEXT := gsub(",", "", IMPROVEMENT_ID_TEXT)]
  agg <- rec[, .(IMPROVEMENTS_IDs     = paste(IMPROVEMENT_ID, collapse = ","),
                 IMPROVEMENTS_TXTs    = paste(IMPROVEMENT_ID_TEXT, collapse = ","),
                 INDICATIVE_COSTS     = paste(INDICATIVE_COST, collapse = ","),
                 INDICATIVE_COST_AVG  = paste(INDICATIVE_COST_AVG, collapse = ",")), by = LMK_KEY]

  # Raw ID dummies (collapsed later, once, for the whole register)
  for (id in seq_len(TOTAL_IDS)) agg[, (paste0("ID_", id)) := 0L]
  for (id in unique(rec$IMPROVEMENT_ID[rec$IMPROVEMENT_ID %between% c(1, TOTAL_IDS)])) {
    agg[unique(rec[IMPROVEMENT_ID == id, .(LMK_KEY)]), (paste0("ID_", id)) := 1L, on = "LMK_KEY"]
  }

  epcs <- merge(epcs, agg, by = "LMK_KEY", all.x = TRUE)
  setorder(epcs, UPRN, INSPECTION_DATE)
  fwrite(epcs, out_file)
}

# ---- Stack all local authorities ----------------------------------------------
epc <- rbindlist(lapply(list.files(inter("epcs"), full.names = TRUE), fread), use.names = TRUE, fill = TRUE)

# ---- Clean -----------------------------------------------------------------------
setnames(epc, c("PROPERTY_TYPE", "ADDRESS", "POSTCODE"), c("PROPERTY_TYPE_EPC", "ADDRESS_EPC", "POSTCODE_EPC"),
         skip_absent = TRUE)
epc[, INSPECTION_DATE := as.IDate(INSPECTION_DATE)]
epc[, `:=`(
  DIFF_ENERGY_EFFICIENCY  = POTENTIAL_ENERGY_EFFICIENCY  - CURRENT_ENERGY_EFFICIENCY,   # SAP points
  DIFF_ENERGY_CONSUMPTION = ENERGY_CONSUMPTION_POTENTIAL - ENERGY_CONSUMPTION_CURRENT,  # kWh/m2/yr
  DIFF_CO2_EMISSIONS      = CO2_EMISSIONS_POTENTIAL      - CO2_EMISSIONS_CURRENT,       # tCO2/yr
  DIFF_ENVIRONMENT_IMPACT = ENVIRONMENT_IMPACT_POTENTIAL - ENVIRONMENT_IMPACT_CURRENT,
  DIFF_ENERGY_COST        = (LIGHTING_COST_POTENTIAL + HEATING_COST_POTENTIAL + HOT_WATER_COST_POTENTIAL) -
                            (LIGHTING_COST_CURRENT   + HEATING_COST_CURRENT   + HOT_WATER_COST_CURRENT)  # GBP/yr
)]
epc[DIFF_ENERGY_EFFICIENCY > 0, POUND_PER_EPC := sum_values(INDICATIVE_COST_AVG) / DIFF_ENERGY_EFFICIENCY]

collapse_id_dummies(epc)   # single, documented ID grouping (R/helpers.R)

fwrite(epc, inter("master_epcs_clean.csv"))
