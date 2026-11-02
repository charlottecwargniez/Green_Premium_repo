# =============================================================================
# setup.R -- packages, paths and shared parameters
# Sourced at the top of every script in code/. Safe to source repeatedly.
# =============================================================================

suppressPackageStartupMessages({
  library(here)        # project-relative paths (root = folder containing .here)
  library(data.table)
  library(ggplot2)
})

# ---- Paths ------------------------------------------------------------------
# All paths are relative to the replication/ folder. Nothing outside it is read
# or written. Raw inputs go in data/raw (see README), hand-curated inputs in
# data/manual, derived files in data/intermediate.
PATH_RAW     <- here("data", "raw")
PATH_MANUAL  <- here("data", "manual")
PATH_INT     <- here("data", "intermediate")
PATH_TABLES  <- here("output", "tables")
PATH_FIGURES <- here("output", "figures")
for (p in c(PATH_INT, PATH_TABLES, PATH_FIGURES)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

raw   <- function(...) file.path(PATH_RAW, ...)
man   <- function(...) file.path(PATH_MANUAL, ...)
inter <- function(...) file.path(PATH_INT, ...)
tab   <- function(...) file.path(PATH_TABLES, ...)
fig   <- function(...) file.path(PATH_FIGURES, ...)

# ---- Shared parameters --------------------------------------------------------
EPC_CAP        <- 69     # minimum standard studied in the paper (band C)
TOTAL_IDS      <- 63     # number of EPC improvement IDs in the register
PRICE_TRIM_Q   <- 0.99   # transactions above this real-price quantile are dropped
DEFLATOR_BASE  <- c(year = 2019, month = 1)  # prices expressed in Jan-2019 pounds

# Standard errors. The paper's tables were produced with fixest/lm defaults
# (fixest clusters on the FIRST fixed effect, which in the hedonic models was
# TOTAL_FLOOR_AREA). We now cluster explicitly by postcode district.
# Set to "hetero" for heteroskedasticity-robust (what the table notes claim).
VCOV_MAIN <- ~pcu_district

source(here("R", "helpers.R"))
