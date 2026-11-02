# =============================================================================
# 03_land_registry.R
# HM Land Registry Price Paid Data with UPRNs attached.
#
# IN : data/raw/registry/pp-<year>.csv           (Price Paid, no header)
#      data/raw/ppdid_uprn_usrn.csv              (transaction id -> UPRN lookup)
# OUT: data/intermediate/master_registry.csv
#
# The UPRN lookup ends in early 2022. For later transactions, UPRNs are filled
# by exact address match (PAON, SAON, postcode) on earlier matched transactions.
# =============================================================================
source(here::here("R", "setup.R"))

# Price Paid column order (https://www.gov.uk/guidance/about-the-price-paid-data)
PPD_COLS <- c("transactionid", "PRICE", "TRANSACTION_DATE", "POSTCODE", "PROPERTY_TYPE", "NEWBUILD",
              "ESTATE", "PRIMARY", "SECONDARY", "STREET", "LOCALITY", "TOWN", "BOROUGH", "COUNTY",
              "TRANSACTION_CAT", "RECORD_STATUS")

clean_address <- function(x) {
  x <- toupper(trimws(gsub("\\.", "", x)))
  x <- gsub("FLAT ", "FLAT", x); gsub("APARTMENT ", "APARTMENT", x)
}

lookup <- fread(raw("ppdid_uprn_usrn.csv"), select = c("transactionid", "uprn"))

files <- list.files(raw("registry"), pattern = "\\.csv$", full.names = TRUE)
reg <- rbindlist(lapply(files, function(f) {
  message("registry: ", basename(f))
  d <- fread(f, header = FALSE, col.names = PPD_COLS)
  d[, c("LOCALITY", "RECORD_STATUS") := NULL]
  d[, `:=`(PRIMARY = clean_address(PRIMARY), SECONDARY = clean_address(SECONDARY),
           STREET = clean_address(STREET), TOWN = clean_address(TOWN))]
  d
}))
reg[, TRANSACTION_DATE := as.IDate(substr(TRANSACTION_DATE, 1, 10))]
reg <- merge(reg, lookup, by = "transactionid", all.x = TRUE)

# Address-based UPRN fill for transactions after the lookup's coverage
lookup_end <- reg[!is.na(uprn), max(TRANSACTION_DATE)]
addr_map <- unique(reg[!is.na(uprn), .(PRIMARY, SECONDARY, POSTCODE, uprn_addr = uprn)],
                   by = c("PRIMARY", "SECONDARY", "POSTCODE"), fromLast = TRUE)
reg[is.na(uprn) & TRANSACTION_DATE > lookup_end,
    uprn := addr_map[.SD, on = c("PRIMARY", "SECONDARY", "POSTCODE"), x.uprn_addr]]

fwrite(reg, inter("master_registry.csv"))
