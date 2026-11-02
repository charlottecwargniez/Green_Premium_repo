# =============================================================================
# Table: impact of each improvement ID on five EPC outcomes (Eq. 1)
# Paper: Methods & Results, src/tables/ID_impacts.tex
#
#   (potential - current)_j = sum_m beta_m * ID_m + year FE + e,   j = 1..5
#
# Sample: certificates with >= 1 recommendation, a UPRN, current score < 100
# and a positive potential gain.
#
# IN : data/intermediate/master_epcs_clean.csv
# OUT: output/tables/id_impacts_estimates.csv   (numeric; used by retrofit_costs.R)
#      output/tables/id_impacts.{csv,tex}       (formatted with stars)
# =============================================================================
source(here::here("R", "setup.R"))
library(fixest)

OUTCOMES <- c(
  DIFF_ENERGY_EFFICIENCY  = "EPC score",
  DIFF_ENERGY_CONSUMPTION = "Consumption (kWh/m2)",
  DIFF_CO2_EMISSIONS      = "CO2 (t/yr)",
  DIFF_ENVIRONMENT_IMPACT = "Environmental",
  DIFF_ENERGY_COST        = "Bill (GBP/yr)"
)

hdr <- names(fread(inter("master_epcs_clean.csv"), nrows = 0))
id_cols <- grep("^ID_\\d+$", hdr, value = TRUE)
epc <- load_master_epc(select = c("UPRN", "INSPECTION_DATE", "POSTCODE_EPC", "IMPROVEMENTS_IDs",
                                  "CURRENT_ENERGY_EFFICIENCY", names(OUTCOMES), id_cols))

epc <- epc[!is.na(UPRN) & !is.na(IMPROVEMENTS_IDs) & IMPROVEMENTS_IDs != "" &
           CURRENT_ENERGY_EFFICIENCY < 100 & DIFF_ENERGY_EFFICIENCY > 0]
epc[, `:=`(YEAR = year(INSPECTION_DATE), pcu_district = postcode_district(POSTCODE_EPC))]

# Drop ID columns that are never recommended in the sample
id_cols <- id_cols[vapply(id_cols, function(c) any(epc[[c]] == 1), logical(1))]

fml <- as.formula(paste0("c(", paste(names(OUTCOMES), collapse = ","), ") ~ ",
                         paste(id_cols, collapse = " + "), " | YEAR"))
models <- as.list(feols(fml, data = epc, vcov = VCOV_MAIN))   # one model per outcome

# ---- Numeric estimates (long) --------------------------------------------------
est <- rbindlist(lapply(seq_along(OUTCOMES), function(i) {
  ct <- coeftable(models[[i]])
  data.table(term = rownames(ct), outcome = names(OUTCOMES)[i],
             estimate = ct[, 1], se = ct[, 2], p = ct[, 4])
}))
est[, IMPROVEMENT_ID := as.integer(sub("ID_", "", term))]
fwrite(est, tab("id_impacts_estimates.csv"))

# ---- Formatted table -------------------------------------------------------------
id_text <- fread(inter("improvement_ID_text.csv"))
est[, cell := sprintf("%.2f%s (%.2f)", estimate, stars(p), se)]
wide <- dcast(est, IMPROVEMENT_ID ~ outcome, value.var = "cell")
setcolorder(wide, c("IMPROVEMENT_ID", names(OUTCOMES)))
setnames(wide, names(OUTCOMES), unname(OUTCOMES))
wide <- merge(id_text, wide, by = "IMPROVEMENT_ID")
fwrite(wide, tab("id_impacts.csv"))

nobs_row <- c("", "Observations", vapply(models, function(m) format(nobs(m), big.mark = ","), ""))
tex <- knitr::kable(rbind(wide, as.list(nobs_row)), format = "latex", booktabs = TRUE,
                    caption = paste("Impacts of improvements (OLS with inspection-year FE;",
                                    "SE clustered by postcode district)."))
write_tex(tex, tab("id_impacts.tex"))
