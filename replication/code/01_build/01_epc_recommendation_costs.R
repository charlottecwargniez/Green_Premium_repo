# =============================================================================
# 01_epc_recommendation_costs.R
# Average indicative cost of each EPC improvement ID, by inspection year.
#
# IN : data/raw/epcs/<local-authority>/certificates.csv, recommendations.csv
# OUT: data/intermediate/improvement_ID_text.csv   (ID -> description)
#      data/intermediate/improvement_ID_stats.csv  (ID x YEAR mean/sd/count of cost)
#
# NOTE: data/manual/improvement_ID_stats_q.csv is a hand-completed version of
# improvement_ID_stats.csv (gaps for IDs/years with no cost were filled by hand).
# It is the file used downstream, and is shipped with the package. See README.
# =============================================================================
source(here::here("R", "setup.R"))

epc_dirs <- list.dirs(raw("epcs"), recursive = FALSE)
stopifnot(length(epc_dirs) > 0)

# ---- Inspection dates and recommendations for every certificate ---------------
dates <- rbindlist(lapply(epc_dirs, function(d) {
  fread(file.path(d, "certificates.csv"), select = c("LMK_KEY", "INSPECTION_DATE"))
}))

recoms <- rbindlist(lapply(epc_dirs, function(d) {
  message("recommendations: ", basename(d))
  r <- fread(file.path(d, "recommendations.csv"),
             select = c("LMK_KEY", "IMPROVEMENT_ID", "IMPROVEMENT_ID_TEXT", "INDICATIVE_COST"))
  r[!is.na(IMPROVEMENT_ID) & IMPROVEMENT_ID != ""]
}), fill = TRUE)
recoms[, IMPROVEMENT_ID := as.integer(IMPROVEMENT_ID)]

# ---- ID descriptions (first text seen for each ID) -----------------------------
id_text <- recoms[, .SD[1], by = IMPROVEMENT_ID][, .(IMPROVEMENT_ID, IMPROVEMENT_ID_TEXT)]
setorder(id_text, IMPROVEMENT_ID)
fwrite(id_text, inter("improvement_ID_text.csv"))

# ---- Parse indicative cost: "£100 - £350" -> mid-point 225 ---------------------
parse_cost_mid <- function(x) {
  x <- gsub("[£, ]", "", as.character(x))
  lo <- as.numeric(sub("-.*$", "", x))
  hi <- as.numeric(ifelse(grepl("-", x), sub("^.*-", "", x), NA))
  hi <- fifelse(is.na(hi), lo, hi)
  0.5 * (lo + hi)
}
recoms[, INDICATIVE_COST_AVG := parse_cost_mid(INDICATIVE_COST)]
recoms[dates, INSPECTION_DATE := i.INSPECTION_DATE, on = "LMK_KEY"]
recoms[, YEAR := year(as.IDate(INSPECTION_DATE))]

# ---- Yearly statistics per ID ----------------------------------------------------
stats <- recoms[IMPROVEMENT_ID %between% c(1, TOTAL_IDS), .(
  Average_Cost = mean(INDICATIVE_COST_AVG, na.rm = TRUE),
  SD_Cost      = sd(INDICATIVE_COST_AVG, na.rm = TRUE),
  Count        = .N
), by = .(YEAR, IMPROVEMENT_ID)]
setorder(stats, IMPROVEMENT_ID, YEAR)
fwrite(stats, inter("improvement_ID_stats.csv"))
