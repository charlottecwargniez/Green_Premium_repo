# =============================================================================
# 00_master.R -- reproduces every table and figure in the paper
#
# Usage: open replication/replication.Rproj (or setwd("replication")), then
#   source("00_master.R")
# Each step can also be run on its own; scripts communicate only through files
# in data/intermediate and output/.
#
# Set RUN_BUILD <- FALSE to skip the (slow, ~hours, ~64 GB RAM) data build when
# data/intermediate is already populated.
# =============================================================================
source(here::here("R", "setup.R"))

RUN_BUILD <- TRUE

run <- function(path) {
  message("\n==== ", path, " ====")
  t0 <- Sys.time()
  sys.source(here(path), envir = new.env())        # fresh environment per script
  message(sprintf("     done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

if (RUN_BUILD) {
  run("code/01_build/01_epc_recommendation_costs.R")
  run("code/01_build/02_epc_certificates.R")
  run("code/01_build/03_land_registry.R")
  run("code/01_build/04_master_dataset.R")
  run("code/01_build/05_zoopla_listings.R")
}

# Paper exhibits, in order of appearance
run("code/02_analysis/figure_facet_costs.R")        # Figure: Facet_costs
run("code/02_analysis/table_price_by_epc.R")        # Table: price by EPC x property type
run("code/02_analysis/figure_active_listings.R")    # Figure: active_listings
run("code/02_analysis/table_id_impacts.R")          # Table: ID impacts (Eq. 1)
run("code/02_analysis/retrofit_costs.R")            # Tables: EPC69 results, by-yield, Table 5, appendix
run("code/02_analysis/figure_density_costs.R")      # Figure: Combined_Density_Costs
run("code/02_analysis/table_hedonic.R")             # Table: hedonic results
run("code/02_analysis/table_elasticity.R")          # Table: elasticity
run("code/02_analysis/figure_costs_district.R")     # Figure: average_costs_per_district

writeLines(capture.output(sessionInfo()), here("output", "sessionInfo.txt"))
