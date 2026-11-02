# =============================================================================
# Figure: density of absolute (a) and relative (b) retrofit costs, Strategy 1
# Paper: Methods & Results, src/figures/Combined_Density_Costs.png
#
# IN : data/intermediate/costs_per_uprn.csv, master_dataset_all.csv
# OUT: output/figures/Combined_Density_Costs.png
#      output/tables/density_costs_stats.csv  (numbers quoted in the text)
# =============================================================================
source(here::here("R", "setup.R"))
library(patchwork)

costs <- fread(inter("costs_per_uprn.csv"), select = c("UPRN", "BY_EPC"))
setnames(costs, "BY_EPC", "Cost")
mean_cost <- costs[, mean(Cost)]       # = "Household" cost of Strategy 1 in epc69_results

# Price and age band of the transaction matched to the latest certificate (nominal GBP)
md <- fread(inter("master_dataset_all.csv"),
            select = c("UPRN", "INSPECTION_DATE", "TRANSACTION_DATE", "PRICE", "CONSTRUCTION_AGE_BAND"))
md <- md[order(UPRN, -INSPECTION_DATE, -TRANSACTION_DATE)][, .SD[1], by = UPRN]
costs <- merge(costs, md, by = "UPRN")

# Numbers quoted in the text (before winsorising)
stats <- costs[, .(dwellings = .N, pct_below_10k = 100 * mean(Cost < 10000),
                   pct_below_3500 = 100 * mean(Cost < 3500), mean_cost_all = mean_cost)]
fwrite(stats, tab("density_costs_stats.csv")); print(stats)

# Winsorise at the 99th percentile
p99 <- quantile(costs$Cost, 0.99)
costs[Cost > p99, Cost := p99]
costs[, price_range := cut(PRICE, c(0, 1e5, 5e5, 1e6, 1e7, Inf),
                           labels = c("0-100k", "100-500k", "500k-1M", "1-10M", "10M+"), right = FALSE)]
costs[, per_cost := 100 * Cost / PRICE]

# Construction age band -> three buckets
age_bucket <- function(x) {
  # first year in labels of the form "1950" or "England and Wales: 1950-1966"; other labels -> NA
  ok <- grepl("^\\d{4}$|^England and Wales: \\d{4}", x)
  yr <- fifelse(ok, suppressWarnings(as.integer(substr(sub("^England and Wales: ", "", x), 1, 4))), NA_integer_)
  fcase(grepl("before 1900", x), "before 1900",
        x %in% c("England and Wales: 1996-2002"), "after 2000",
        !is.na(yr) & yr < 1900, "before 1900",
        !is.na(yr) & yr <= 2000, "1900-2000",
        !is.na(yr) & yr > 2000, "after 2000",
        default = NA_character_)
}
costs[, age := factor(age_bucket(CONSTRUCTION_AGE_BAND), levels = c("before 1900", "1900-2000", "after 2000"))]

pa <- ggplot(costs, aes(Cost, fill = price_range)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean_cost, colour = "red", linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = mean_cost, y = Inf, vjust = 1.5, hjust = 1.05, angle = 90, colour = "red",
           label = sprintf("Average retrofit cost (£%s)", format(round(mean_cost), big.mark = ","))) +
  labs(x = "Retrofitting Cost (£)", y = "Density", fill = "Price Range") +
  theme_classic()

pb <- ggplot(costs[!is.na(age)], aes(per_cost, fill = age)) +
  geom_density(alpha = 0.5) +
  coord_cartesian(xlim = c(0, 15)) +
  labs(x = "Relative Retrofitting Cost (%)", y = "Density", fill = "Construction Age Band") +
  theme_classic()

ggsave(fig("Combined_Density_Costs.png"), pa / pb + plot_annotation(tag_levels = "a"), width = 10, height = 8)
