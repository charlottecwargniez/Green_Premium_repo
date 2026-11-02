# =============================================================================
# Retrofit costs of reaching EPC band C (score 69) under four strategies
# Paper: Methods & Results
#   src/tables/EPC69_results.tex                    (cumulative costs, all strategies)
#   src/tables/Improvements_Cap69_byYield_sorted.tex (needed measures, yield strategy)
#   src/tables/Improvements_Cap69_by{EPC,IMPACT,PRICE}.tex (appendix)
#   src/tables/Table5_retrofit_descriptive.tex       (already / unable / able groups)
#
# For each dwelling's LATEST certificate, recommended measures are taken in a
# strategy-specific order and accumulated until the predicted score reaches the
# cap (or, for dwellings whose potential is below the cap, until the potential):
#   epc    : the order listed on the certificate          (Strategy 1)
#   impact : largest predicted EPC gain first
#   price  : cheapest first
#   yield  : largest EPC gain per pound first
# Per-measure EPC/CO2/energy impacts come from table_id_impacts.R; per-measure
# costs are the latest-year average indicative cost (data/manual).
#
# IN : data/intermediate/master_epcs_clean.csv, master_dataset_all.csv,
#      improvement_ID_text.csv; data/manual/improvement_ID_stats_q.csv;
#      output/tables/id_impacts_estimates.csv
# OUT: output/tables/epc69_results.{csv,tex}
#      output/tables/improvements_cap69_by_{epc,impact,price,yield}.{csv,tex}
#      output/tables/table5_retrofit_descriptive.{csv,tex}
#      data/intermediate/costs_per_uprn.csv   (used by the density figure and map)
# =============================================================================
source(here::here("R", "setup.R"))

# ---- Per-measure cost and impacts ---------------------------------------------
cost <- fread(man("improvement_ID_stats_q.csv"))
cost <- cost[cost[, .I[which.max(YEAR)], by = IMPROVEMENT_ID]$V1]          # latest year per raw ID
cost[, IMPROVEMENT_ID := collapse_id(IMPROVEMENT_ID)]
cost <- cost[, .(COST = mean(as.numeric(Average_Cost), na.rm = TRUE)), by = IMPROVEMENT_ID]

est <- fread(tab("id_impacts_estimates.csv"))
imp <- dcast(est, IMPROVEMENT_ID ~ outcome, value.var = "estimate")
setnames(imp, c("DIFF_ENERGY_EFFICIENCY", "DIFF_ENERGY_CONSUMPTION", "DIFF_CO2_EMISSIONS",
                "DIFF_ENVIRONMENT_IMPACT", "DIFF_ENERGY_COST"),
         c("IMPACT_EPC", "IMPACT_CONS", "IMPACT_CO2", "IMPACT_ENV", "IMPACT_BILL"))

measures <- merge(cost[is.finite(COST)], imp, by = "IMPROVEMENT_ID")       # measures with both
measures <- merge(fread(inter("improvement_ID_text.csv")), measures, by = "IMPROVEMENT_ID")
measures[, YIELD := IMPACT_EPC / COST]
fwrite(measures, inter("improvements_complete.csv"))

# ---- Latest certificate per dwelling -------------------------------------------
epc <- load_master_epc(select = c("UPRN", "INSPECTION_DATE", "POSTCODE_EPC", "PROPERTY_TYPE_EPC",
  "CURRENT_ENERGY_EFFICIENCY", "POTENTIAL_ENERGY_EFFICIENCY", "DIFF_ENERGY_EFFICIENCY",
  "DIFF_ENERGY_CONSUMPTION", "DIFF_CO2_EMISSIONS", "DIFF_ENVIRONMENT_IMPACT",
  "POUND_PER_EPC", "IMPROVEMENTS_IDs"))
epc <- epc[!is.na(UPRN)]
epc <- epc[epc[, .I[which.max(INSPECTION_DATE)], by = UPRN]$V1]

already    <- epc[CURRENT_ENERGY_EFFICIENCY >= EPC_CAP]
candidates <- epc[CURRENT_ENERGY_EFFICIENCY <  EPC_CAP & DIFF_ENERGY_EFFICIENCY > 0]
candidates[, ABLE := POTENTIAL_ENERGY_EFFICIENCY >= EPC_CAP]
message(sprintf("already >= %d: %s | below cap and improvable: %s (able %s, unable %s)", EPC_CAP,
  format(nrow(already), big.mark = ","), format(nrow(candidates), big.mark = ","),
  format(candidates[(ABLE), .N], big.mark = ","), format(candidates[!(ABLE), .N], big.mark = ",")))

# ---- Long table: one row per (dwelling, recommended measure) -------------------
to_long <- function(dt, cap) {
  long <- dt[, .(IMPROVEMENT_ID = collapse_id(unlist(split_ids(IMPROVEMENTS_IDs)))),
             by = .(UPRN, INITIAL_EPC = CURRENT_ENERGY_EFFICIENCY)]
  long[, CAP := as.numeric(cap)]
  long <- unique(long, by = c("UPRN", "IMPROVEMENT_ID"))      # duplicates created by grouping
  long[, rec_order := seq_len(.N), by = UPRN]
  merge(long, measures, by = "IMPROVEMENT_ID")                 # measures without estimate/cost dropped
}

# Keep measures up to and including the first that reaches the cap.
run_strategy <- function(long, strategy) {
  ord <- switch(strategy,
    epc    = long[order(UPRN, rec_order)],
    impact = long[order(UPRN, -IMPACT_EPC)],
    price  = long[order(UPRN, COST)],
    yield  = long[order(UPRN, -YIELD)])
  ord[, EPC_after := INITIAL_EPC + cumsum(IMPACT_EPC), by = UPRN]
  ord[, reached_before := shift(cumsum(EPC_after >= CAP), fill = 0L) > 0, by = UPRN]
  ord[!(reached_before)]
}

summarise_strategy <- function(kept, label) {
  per_uprn <- kept[, .(cost = sum(COST), n = .N, epc_final = last(EPC_after),
                       reached = last(EPC_after) >= last(CAP)), by = UPRN]
  data.table(
    Strategy            = label,
    `N. Retrofits`      = round(per_uprn[(reached), mean(n)]),
    `Household (GBP)`   = round(per_uprn[, mean(cost)]),
    `CO2 (MtCO2/yr)`    = round(kept[, sum(IMPACT_CO2)] / 1e6, 1),
    `Env`               = round(kept[, sum(IMPACT_ENV)] / 1e6, 1),
    `Energy (GWh/m2)`   = round(kept[, sum(IMPACT_CONS)] / 1e6, 1),
    `Total Cost (GBP)`  = round(kept[, sum(COST)]),
    dwellings           = nrow(per_uprn),
    avg_epc_final       = round(per_uprn[, mean(epc_final)])
  )
}

by_measure_table <- function(kept, label) {
  t <- kept[, .(Count = .N), by = IMPROVEMENT_ID][measures, on = "IMPROVEMENT_ID", nomatch = 0L]
  t[, TOTAL_COSTS := Count * COST]
  setorder(t, -TOTAL_COSTS)
  out <- t[, .(ID = IMPROVEMENT_ID, Count, Description = IMPROVEMENT_ID_TEXT, `COST (GBP)` = COST,
               `dEPC` = round(IMPACT_EPC), `Energy (kWh/m2)` = round(IMPACT_CONS, 3),
               `CO2 (t/yr)` = round(IMPACT_CO2, 3), `TOTAL COSTS (GBP)` = round(TOTAL_COSTS))]
  stem <- paste0("improvements_cap", EPC_CAP, "_by_", label)
  fwrite(out, tab(paste0(stem, ".csv")))
  write_tex(knitr::kable(out, format = "latex", booktabs = TRUE,
            caption = sprintf("Measures needed to reach EPC %d, ordered by total cost ('%s' strategy).", EPC_CAP, label)),
            tab(paste0(stem, ".tex")))
}

# ---- Strategies at the cap (all dwellings below the cap) -----------------------
long69 <- to_long(candidates, EPC_CAP)
strategies <- c(epc = "By EPC", impact = "By Impact", price = "By Price", yield = "By Yield")
kept69 <- lapply(names(strategies), function(s) run_strategy(long69, s)); names(kept69) <- names(strategies)
for (s in names(strategies)) by_measure_table(kept69[[s]], s)

# Linear benchmark: cost and impacts scale with the share of the potential gain needed
candidates[, RISE := pmin(EPC_CAP, POTENTIAL_ENERGY_EFFICIENCY) - CURRENT_ENERGY_EFFICIENCY]
candidates[, share := RISE / DIFF_ENERGY_EFFICIENCY]
linear <- candidates[, data.table(
  Strategy = "Linear Est.", `N. Retrofits` = NA_real_,
  `Household (GBP)`  = round(mean(RISE * POUND_PER_EPC, na.rm = TRUE)),
  `CO2 (MtCO2/yr)`   = round(sum(share * DIFF_CO2_EMISSIONS, na.rm = TRUE) / 1e6, 1),
  `Env`              = round(sum(share * DIFF_ENVIRONMENT_IMPACT, na.rm = TRUE) / 1e6, 1),
  `Energy (GWh/m2)`  = round(sum(share * DIFF_ENERGY_CONSUMPTION, na.rm = TRUE) / 1e6, 1),
  `Total Cost (GBP)` = round(sum(RISE * POUND_PER_EPC, na.rm = TRUE)),
  dwellings = .N, avg_epc_final = NA_real_)]

res69 <- rbind(rbindlist(Map(summarise_strategy, kept69, strategies)), linear)
setorder(res69, `Total Cost (GBP)`)
fwrite(res69, tab("epc69_results.csv"))
write_tex(knitr::kable(res69[, !c("dwellings", "avg_epc_final")], format = "latex", booktabs = TRUE,
          format.args = list(big.mark = ","),
          caption = sprintf("Cumulative costs and impacts of reaching EPC %d.", EPC_CAP)),
          tab("epc69_results.tex"))

# ---- Costs per dwelling (Strategy 1) for the density figure and the map --------
costs_uprn <- kept69$epc[, .(BY_EPC = sum(COST)), by = UPRN]
for (s in c("impact", "price", "yield"))
  costs_uprn[kept69[[s]][, .(v = sum(COST)), by = UPRN], (paste0("BY_", toupper(s))) := i.v, on = "UPRN"]
costs_uprn <- merge(costs_uprn, candidates[, .(UPRN, INSPECTION_DATE, POSTCODE_EPC, ABLE,
                                               CURRENT_ENERGY_EFFICIENCY, POTENTIAL_ENERGY_EFFICIENCY)],
                    by = "UPRN")
fwrite(costs_uprn, inter("costs_per_uprn.csv"))

# ---- Table 5: already / unable / able ------------------------------------------
# Dwellings unable to reach the cap are costed up to their own potential.
unable_long <- to_long(candidates[!(ABLE)], NA_real_)
unable_long[candidates, CAP := i.POTENTIAL_ENERGY_EFFICIENCY, on = "UPRN"]
kept_unable <- lapply(names(strategies), function(s) run_strategy(unable_long, s))
able_ids    <- candidates[(ABLE), UPRN]
kept_able   <- lapply(kept69, function(k) k[UPRN %in% able_ids])

# Property price: transaction matched to the latest certificate (nominal GBP)
price <- fread(inter("master_dataset_all.csv"), select = c("UPRN", "INSPECTION_DATE", "TRANSACTION_DATE", "PRICE"))
price <- price[order(UPRN, -INSPECTION_DATE, -TRANSACTION_DATE)][, .SD[1], by = UPRN]

group_stats <- function(dt, kept_list, label) {
  s <- if (is.null(kept_list)) NULL else rbindlist(Map(summarise_strategy, kept_list, strategies))
  pr <- price[UPRN %in% dt$UPRN, mean(PRICE)]
  data.table(
    Group = label,
    `Number of Houses` = nrow(dt),
    `Total Retrofit Costs (GBP bn, Strategy 1)` = if (is.null(s)) NA else round(s[1, `Total Cost (GBP)`] / 1e9, 1),
    `Reduced Emissions (MtCO2/yr, range)` = if (is.null(s)) NA else
      paste(round(max(s$`CO2 (MtCO2/yr)`), 1), "to", round(min(s$`CO2 (MtCO2/yr)`), 1)),
    `Average Retrofit Cost (GBP, Strategy 1)` = if (is.null(s)) NA else s[1, `Household (GBP)`],
    `Avg. pre-retrofit EPC`  = round(mean(dt$CURRENT_ENERGY_EFFICIENCY)),
    `Avg. post-retrofit EPC` = if (is.null(s)) NA else s[1, avg_epc_final],
    `Average Property Price (GBP)` = round(pr)
  )
}
t5 <- rbind(group_stats(already, NULL, "Already Achieved"),
            group_stats(candidates[!(ABLE)], kept_unable, "Unable to Reach"),
            group_stats(candidates[(ABLE)], kept_able, paste("Able to Reach EPC", EPC_CAP)))
t5 <- transpose(t5, keep.names = "Statistic", make.names = "Group")
fwrite(t5, tab("table5_retrofit_descriptive.csv"))
write_tex(knitr::kable(t5, format = "latex", booktabs = TRUE,
          caption = sprintf("Dwellings by capacity to reach EPC %d.", EPC_CAP)),
          tab("table5_retrofit_descriptive.tex"))
