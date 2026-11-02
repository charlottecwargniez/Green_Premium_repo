# =============================================================================
# Table: listing-price growth and active sale listings
# Paper: Methods & Results, src/tables/elasticity_results.tex
#
#   ln(P_k) - ln(P_{k-1}) = lambda * ln(active sales) + gilt yield terms
#                            + ln(duration) | rooms + type + area + year + month
#
# estimated on Zoopla SALE listings of properties listed more than once.
# P is the last asking price of the listing (Zoopla), not a transaction price.
#
# INTERPRETATION: lambda is an elasticity. lambda = -0.02 means a 1% rise in
# active listings is associated with a 0.02% (not 2%) lower price.
#
# Changes from the thesis code: listings are merged on postcode area x month
# (the old merge used the exact end date against first-of-month counts), and
# SEs are clustered (VCOV_MAIN).
#
# IN : data/intermediate/zoopla_all_sale.csv, zoopla_active_sale.csv
#      data/raw/IRLTLT01GBM156N.csv
# OUT: output/tables/elasticity_results.{tex,csv}
# =============================================================================
source(here::here("R", "setup.R"))
library(fixest)

z <- fread(inter("zoopla_all_sale.csv"))
TYPES <- c(`Detached house` = "D", `Town house` = "D", Flat = "F", Maisonette = "F", Studio = "F",
           Bungalow = "O", `Semi-detached bungalow` = "O", `Semi-detached house` = "S",
           `End terrace house` = "S", `Terraced house` = "T")
z <- z[property_type %in% names(TYPES)]
z[, PROPERTY_TYPE := TYPES[property_type]]
z[, NUMBER_HABITABLE_ROOMS := num_bed_max + num_receptions_max + 1]
z[, month := as.integer(substr(year_month, 6, 7))]

act <- fread(inter("zoopla_active_sale.csv"), select = c("pcu_area", "year_month", "active_sale"))
z <- merge(z, act, by = c("pcu_area", "year_month"), all.x = TRUE)
z <- merge(z, load_gilt_yield(), by = "year_month", all.x = TRUE)

z[, `:=`(ln_price = log(price_last), ln_active_sale = log(active_sale), ln_duration = log(duration))]
z <- z[is.finite(ln_price) & is.finite(ln_duration) & is.finite(ln_active_sale)]

# Repeat listings: growth in asking price between consecutive listings of a property
setorder(z, property_id, start_date)
z <- z[, if (.N > 1) .SD, by = property_id]
z[, ln_growth_price := ln_price - shift(ln_price), by = property_id]
z[, pcu_district := toupper(pcu_district)]

m <- feols(ln_growth_price ~ ln_active_sale + gilt_10y + gilt_10y_L3 + gilt_10y_L3:duration + ln_duration |
             NUMBER_HABITABLE_ROOMS + PROPERTY_TYPE + pcu_area + zoopla_year + month,
           data = z, vcov = VCOV_MAIN)
print(summary(m))

etable(m, tex = TRUE, fitstat = ~ r2 + ar2 + n, title = "Asking-price growth and active sale listings",
       label = "table:coefficients",
       notes = "Zoopla sale listings, repeat-listed properties. SEs clustered by postcode district.",
       file = tab("elasticity_results.tex"), replace = TRUE)
ct <- coeftable(m)
fwrite(data.table(term = rownames(ct), estimate = ct[, 1], se = ct[, 2], p = ct[, 4], n = nobs(m)),
       tab("elasticity_results.csv"))
