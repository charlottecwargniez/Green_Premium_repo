# =============================================================================
# 05_zoopla_listings.R
# Zoopla rental and sale listings (licensed, NOT redistributable; see README):
#   (1) stack the yearly files into one listing-level file per market;
#   (2) count ACTIVE listings per postcode area and month.
#
# IN : data/raw/zoopla/rent/*.csv, data/raw/zoopla/sale/*.csv
# OUT: data/intermediate/zoopla_all_{rent,sale}.csv
#      data/intermediate/zoopla_active_{rent,sale}.csv  (pcu_area x month)
#
# A listing is "active" in month m if it was on the market on the first day of m
# or came off the market in the ACTIVE_WINDOW days before it.
# =============================================================================
source(here::here("R", "setup.R"))

ACTIVE_WINDOW <- 30  # days

KEEP <- c("property_id", "property_type", "start_date", "end_date", "price_first", "price_last",
          "num_bed_max", "num_floors_max", "num_bathrooms_max", "num_receptions_max",
          "pcu", "pcu_district", "pcu_area", "duration", "zoopla_year", "lad20cd")

for (market in c("rent", "sale")) {
  files <- list.files(raw("zoopla", market), pattern = "\\.csv$", full.names = TRUE)
  stopifnot(length(files) > 0)
  z <- rbindlist(lapply(files, fread, select = KEEP), use.names = TRUE, fill = TRUE)
  z[, `:=`(start_date = as.IDate(start_date), end_date = as.IDate(end_date))]
  z <- unique(z, by = c("property_id", "start_date"))      # first record per listing spell
  setorder(z, property_id, start_date, end_date)
  z[, year_month := format(end_date, "%Y-%m")]
  fwrite(z, inter(paste0("zoopla_all_", market, ".csv")))

  # ---- Active listings per area and month (non-equi join, no daily loop) ----
  months <- data.table(date = seq(as.IDate(format(min(z$start_date), "%Y-%m-01")),
                                  max(z$end_date), by = "month"))
  months[, window_start := date - ACTIVE_WINDOW]
  act <- z[months, on = .(start_date <= date, end_date >= window_start),
           .(date = i.date, pcu_area = x.pcu_area), allow.cartesian = TRUE, nomatch = 0L
          ][, .(active = .N), by = .(pcu_area, date)]
  # complete panel (areas with no active listing in a month get 0) so the lag is one month
  act <- act[CJ(pcu_area = unique(act$pcu_area), date = months$date), on = .(pcu_area, date)]
  act[is.na(active), active := 0L]
  setorder(act, pcu_area, date)
  act[, active_L1 := shift(active, 1), by = pcu_area]       # previous month, same area
  act[, year_month := format(date, "%Y-%m")]
  setnames(act, c("active", "active_L1"), paste0(c("active_", "active_L1_"), market))
  fwrite(act, inter(paste0("zoopla_active_", market, ".csv")))
}
