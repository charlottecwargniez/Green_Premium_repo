# =============================================================================
# Figure: average retrofit cost (Strategy 1) per local authority district
# Paper: Discussion, src/figures/average_costs_per_district.png
#
# IN : data/intermediate/costs_per_uprn.csv
#      data/raw/ONSPD_AUG_2024_UK.csv                 (postcode -> LAD lookup)
#      data/raw/LAD_MAY_2022_UK_BFE_V3.{shp,...}     (LAD boundaries)
# OUT: output/figures/average_costs_per_district.png
#      output/tables/average_costs_per_district.csv
# =============================================================================
source(here::here("R", "setup.R"))
library(sf)

ONSPD_FILE <- raw("ONSPD_AUG_2024_UK.csv")
LAD_SHAPE  <- raw("LAD_MAY_2022_UK_BFE_V3.shp")
LAD_CODE   <- "LAD22CD"
# NOTE: ONSPD Aug-2024 uses 2023 LAD codes (Cumberland, Westmorland & Furness,
# North Yorkshire and Somerset were reorganised in April 2023), while the May-2022
# boundaries use the old codes, so those areas show as missing. Using a boundary
# file that matches the ONSPD release (e.g. LAD Dec-2023) removes the gap.

costs <- fread(inter("costs_per_uprn.csv"), select = c("UPRN", "BY_EPC", "POSTCODE_EPC"))
costs[, pcds := postcode_pcds(POSTCODE_EPC)]

# `pcds` is the one-space format used by EPC postcodes ("BS10 6LU"); `pcd` is a
# fixed-width 7-character code and does not match most EPC postcodes.
onspd <- fread(ONSPD_FILE, select = c("pcds", "oslaua"))
costs <- merge(costs, onspd, by = "pcds", all.x = TRUE)
message(sprintf("postcodes matched to a LAD: %.1f%%", 100 * costs[, mean(!is.na(oslaua))]))

by_lad <- costs[!is.na(oslaua), .(avg_cost = mean(BY_EPC), dwellings = .N), by = oslaua]
setnames(by_lad, "oslaua", LAD_CODE)
fwrite(by_lad, tab("average_costs_per_district.csv"))

districts <- st_read(LAD_SHAPE, quiet = TRUE)
map_data <- merge(districts, by_lad, by = LAD_CODE, all.x = TRUE)
map_data <- map_data[substr(map_data[[LAD_CODE]], 1, 1) %in% c("E", "W"), ]   # England & Wales

p <- ggplot(map_data) +
  geom_sf(aes(fill = avg_cost), colour = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Average cost (£)", na.value = "grey") +
  theme_minimal()
ggsave(fig("average_costs_per_district.png"), p, width = 10, height = 8, dpi = 300)
