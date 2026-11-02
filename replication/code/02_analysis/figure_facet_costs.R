# =============================================================================
# Figure: average yearly indicative cost by improvement ID
# Paper: Data section, src/figures/Facet_costs.png
#
# IN : data/intermediate/improvement_ID_stats.csv
# OUT: output/figures/Facet_costs.png
# =============================================================================
source(here::here("R", "setup.R"))

s <- fread(inter("improvement_ID_stats.csv"))[!is.na(Average_Cost) & !is.na(Count)]
s[, IMPROVEMENT_ID := collapse_id(IMPROVEMENT_ID)]
# Count-weighted average across raw IDs that share a group
s <- s[, .(Average_Cost = sum(Average_Cost * Count) / sum(Count)), by = .(IMPROVEMENT_ID, YEAR)]
setorder(s, IMPROVEMENT_ID, YEAR)

p <- ggplot(s, aes(YEAR, Average_Cost, colour = factor(IMPROVEMENT_ID), group = IMPROVEMENT_ID)) +
  geom_line() + geom_point() +
  facet_wrap(~ IMPROVEMENT_ID, scales = "free_y", ncol = 5) +
  labs(x = "Year", y = "Average Costs (£, nominal)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 14),
        strip.text = element_text(size = 14), legend.position = "none")
ggsave(fig("Facet_costs.png"), p, width = 16, height = 22)
