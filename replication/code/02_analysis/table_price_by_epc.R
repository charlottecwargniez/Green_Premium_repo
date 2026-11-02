# =============================================================================
# Table: average transaction price by EPC rating and property type
# Paper: Data section, src/tables/price_by_epc&property_type.tex
#
# IN : data/intermediate/master_dataset_all.csv
# OUT: output/tables/price_by_epc_property_type.{csv,tex}
# =============================================================================
source(here::here("R", "setup.R"))

md <- prepare_transactions(fread(inter("master_dataset_all.csv"),
  select = c("UPRN", "TRANSACTION_DATE", "INSPECTION_DATE", "PRICE", "POSTCODE_EPC",
             "CURRENT_ENERGY_RATING", "PROPERTY_TYPE_EPC")))

md <- md[!is.na(CURRENT_ENERGY_RATING) & CURRENT_ENERGY_RATING %in% LETTERS[1:7]]

tab_dt <- dcast(md[, .(avg_price = mean(REAL_PRICE)), by = .(CURRENT_ENERGY_RATING, PROPERTY_TYPE_EPC)],
                CURRENT_ENERGY_RATING ~ PROPERTY_TYPE_EPC, value.var = "avg_price")
setnames(tab_dt, "CURRENT_ENERGY_RATING", "EPC Rating")
fwrite(tab_dt, tab("price_by_epc_property_type.csv"))

tex <- knitr::kable(tab_dt, format = "latex", booktabs = TRUE, digits = 1,
                    caption = "Average transaction price (Jan-2019 GBP) by EPC rating and property type.")
write_tex(tex, tab("price_by_epc_property_type.tex"))
