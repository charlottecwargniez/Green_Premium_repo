# =============================================================================
# 04_master_dataset.R
# Match each property transaction (2007+) to the latest EPC lodged for the same
# UPRN on or before the transaction date.
#
# IN : data/intermediate/master_registry.csv, master_epcs_clean.csv
# OUT: data/intermediate/master_dataset_all.csv  (one row per matched transaction)
# =============================================================================
source(here::here("R", "setup.R"))

reg <- fread(inter("master_registry.csv"))
setnames(reg, "uprn", "UPRN")
reg <- reg[!is.na(UPRN) & TRANSACTION_DATE > as.IDate("2007-01-01")]   # EPCs start in 2007/08
reg[, TRANSACTION_DATE := as.IDate(TRANSACTION_DATE)]

epc <- load_master_epc()
epc <- epc[!is.na(UPRN)]

# Latest certificate with INSPECTION_DATE <= TRANSACTION_DATE (rolling join)
epc_keys <- epc[, .(UPRN, INSPECTION_DATE, LMK_KEY, match_date = INSPECTION_DATE)]
setkey(epc_keys, UPRN, match_date)
reg[, match_date := TRANSACTION_DATE]
setkey(reg, UPRN, match_date)
md <- epc_keys[reg, roll = Inf]          # last EPC at or before the sale
md <- md[!is.na(LMK_KEY)]
md[, match_date := NULL]

# Attach certificate attributes (drop columns already present)
epc[, c("UPRN", "INSPECTION_DATE") := NULL]
md <- merge(md, epc, by = "LMK_KEY", all.x = TRUE)

setcolorder(md, c("transactionid", "LMK_KEY", "UPRN", "TRANSACTION_DATE", "INSPECTION_DATE"))
setorder(md, UPRN, TRANSACTION_DATE)
md[, EPC_AGE_YEARS := as.numeric(TRANSACTION_DATE - INSPECTION_DATE) / 365.25]  # for robustness checks

fwrite(md, inter("master_dataset_all.csv"))
