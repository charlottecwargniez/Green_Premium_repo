# =============================================================================
# Table: hedonic price models (Eq. 2)
# Paper: Methods & Results, src/tables/hedonic_results.tex
#
#   (1) REAL_PRICE ~ EPC | structural attributes
#   (2) + year and month-of-year FE
#   (3) + monthly 10-year gilt yield
#   (4) + active sale listings in the postcode area (previous month)
#
# Structural attributes enter as fixed effects, as in the paper: total floor
# area, property type, habitable rooms, extension count, built form, age band.
#
# Changes from the thesis code (see README, "Changes"):
#  * SEs: explicit clustering (VCOV_MAIN in R/setup.R). fixest's default had
#    clustered on the first FE, i.e. on TOTAL_FLOOR_AREA.
#  * The gilt yield and listings enter as continuous controls (they were
#    entered after "|", i.e. as one dummy per distinct value).
#  * Listings are merged on postcode area x month. The old merge used the exact
#    transaction date, so only sales completed on the 1st of a month matched,
#    and postcode areas were mis-coded (B -> BA, W -> WA, ...).
#
# IN : data/intermediate/master_dataset_all.csv, zoopla_active_sale.csv
#      data/raw/IRLTLT01GBM156N.csv, RPI_deflator.csv
# OUT: output/tables/hedonic_results.{tex,csv}
# =============================================================================
source(here::here("R", "setup.R"))
library(fixest)

CONTROLS_FE <- "TOTAL_FLOOR_AREA + PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + BUILT_FORM + CONSTRUCTION_AGE_BAND"

md <- prepare_transactions(fread(inter("master_dataset_all.csv"), select = c(
  "UPRN", "TRANSACTION_DATE", "INSPECTION_DATE", "PRICE", "POSTCODE_EPC", "CURRENT_ENERGY_EFFICIENCY",
  "TOTAL_FLOOR_AREA", "PROPERTY_TYPE", "NUMBER_HABITABLE_ROOMS", "EXTENSION_COUNT", "BUILT_FORM",
  "CONSTRUCTION_AGE_BAND")))

md <- merge(md, load_gilt_yield(), by = "year_month", all.x = TRUE)
listings <- fread(inter("zoopla_active_sale.csv"), select = c("pcu_area", "year_month", "active_L1_sale"))
md <- merge(md, listings, by = c("pcu_area", "year_month"), all.x = TRUE)

m <- list(
  feols(as.formula(paste("REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY |", CONTROLS_FE)), md, vcov = VCOV_MAIN),
  feols(as.formula(paste("REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY |", CONTROLS_FE, "+ YEAR_f + MONTH_f")), md, vcov = VCOV_MAIN),
  feols(as.formula(paste("REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY + gilt_10y |", CONTROLS_FE, "+ YEAR_f + MONTH_f")), md, vcov = VCOV_MAIN),
  feols(as.formula(paste("REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY + gilt_10y + active_L1_sale |", CONTROLS_FE, "+ YEAR_f + MONTH_f")),
        md[!is.na(active_L1_sale)], vcov = VCOV_MAIN)
)
names(m) <- c("Baseline", "Time FE", "Interest rate", "Listings")

etable(m, keep = "CURRENT_ENERGY_EFFICIENCY", fitstat = ~ ar2 + n)
etable(m, keep = "CURRENT_ENERGY_EFFICIENCY", fitstat = ~ ar2 + n, tex = TRUE,
       title = "Hedonic price models", label = "tab:hedonic_model",
       notes = "Dependent variable: transaction price in Jan-2019 GBP. Standard errors clustered by postcode district.",
       file = tab("hedonic_results.tex"), replace = TRUE)
fwrite(rbindlist(lapply(names(m), function(n) {
  ct <- coeftable(m[[n]])
  data.table(model = n, term = rownames(ct), estimate = ct[, 1], se = ct[, 2], p = ct[, 4],
             n = nobs(m[[n]]), adj_r2 = fitstat(m[[n]], "ar2")[[1]])
})), tab("hedonic_results.csv"))
