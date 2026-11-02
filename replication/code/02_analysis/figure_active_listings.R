# =============================================================================
# Figure: average monthly active rental and sale listings
# Paper: Data section, src/figures/active_listings.png
# (No script for this figure existed in the thesis repository; this one is new.
#  Check it against the published figure before submission.)
#
# IN : data/intermediate/zoopla_active_{rent,sale}.csv
# OUT: output/figures/active_listings.png
# =============================================================================
source(here::here("R", "setup.R"))

act <- rbind(
  fread(inter("zoopla_active_rent.csv"))[, .(date, pcu_area, active = active_rent, market = "Rent")],
  fread(inter("zoopla_active_sale.csv"))[, .(date, pcu_area, active = active_sale, market = "Sale")]
)
avg <- act[, .(active = mean(active)), by = .(date = as.IDate(date), market)]

p <- ggplot(avg, aes(date, active, colour = market)) +
  geom_line() +
  labs(x = NULL, y = "Average active listings per postcode area", colour = NULL) +
  theme_classic() + theme(legend.position = "bottom")
ggsave(fig("active_listings.png"), p, width = 8, height = 5)
