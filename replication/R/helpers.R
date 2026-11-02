# =============================================================================
# helpers.R -- functions shared by the build and analysis scripts
# This is the ONLY place where improvement-ID groupings, postcode parsing and
# deflation are defined, so every script uses the same definitions.
# =============================================================================

# ---- Improvement-ID grouping -------------------------------------------------
# Several EPC improvement IDs describe the same measure (e.g. 12-18 are variants
# of "upgrade heating controls"). They are collapsed onto one representative ID.
# This mapping reproduces the grouping shown in the paper's ID-impacts table
# (rows "1-3", "11-18", "20-21", "23&39", "24&30", "25&31", "27,29,32,40-42",
# "28&43", "37-38", "59&61", "60&62").
ID_COLLAPSE <- c(
  `2` = 1, `3` = 1,
  `12` = 11, `13` = 11, `14` = 11, `15` = 11, `16` = 11, `17` = 11, `18` = 11,
  `21` = 20,
  `24` = 30,
  `31` = 25,
  `28` = 43,
  `29` = 27, `32` = 27, `40` = 27, `41` = 27, `42` = 27,
  `38` = 37,
  `39` = 23,
  `61` = 59,
  `62` = 60
)

# Vectorised: map raw improvement IDs to their group ID.
collapse_id <- function(id) {
  id  <- as.integer(id)
  out <- unname(ID_COLLAPSE[as.character(id)])
  as.integer(ifelse(is.na(out), id, out))
}

# Collapse ID_<n> dummy columns in place: target := target OR any source, then
# drop the source columns. Missing columns are treated as all-zero.
collapse_id_dummies <- function(dt) {
  for (target in unique(ID_COLLAPSE)) {
    tcol <- paste0("ID_", target)
    scol <- intersect(paste0("ID_", names(ID_COLLAPSE)[ID_COLLAPSE == target]), names(dt))
    if (!length(scol)) next
    if (!tcol %in% names(dt)) dt[, (tcol) := 0L]
    dt[, (tcol) := as.integer(Reduce(`|`, lapply(c(tcol, scol), function(c) get(c) %in% c(1, TRUE))))]
    dt[, (scol) := NULL]
  }
  invisible(dt)
}

# Split "1,5,35" into integer IDs.
split_ids <- function(x) lapply(strsplit(as.character(x), "[,|]"), as.integer)

# Sum "250,4000.5,25" -> 4275.5 (vectorised over a character column).
sum_values <- function(x) vapply(strsplit(as.character(x), "[,|]"),
                                 function(v) sum(as.numeric(v), na.rm = TRUE), numeric(1))

# ---- Postcodes ----------------------------------------------------------------
# Postcode AREA is the leading 1-2 letters ("B" for "B1 1AA", "BS" for "BS10 6LU").
# (The thesis code took the first two letters of the whole postcode, which turned
#  B -> BA, W -> WA, E -> EA, etc.)
postcode_area <- function(pc) sub("^([A-Z]{1,2}).*$", "\\1", toupper(trimws(pc)))
# Postcode DISTRICT is the outward code ("BS10" for "BS10 6LU").
postcode_district <- function(pc) sub("\\s.*$", "", toupper(trimws(pc)))
# Normalise to the single-space format used by ONSPD's `pcds` column.
postcode_pcds <- function(pc) {
  pc <- gsub("\\s+", "", toupper(pc))
  paste(substr(pc, 1, nchar(pc) - 3), substr(pc, nchar(pc) - 2, nchar(pc)))
}

# ---- Prices -------------------------------------------------------------------
# RPI deflator built directly from the ONS RPI index (series CHAW) rather than a
# hand-edited spreadsheet. Returns YEAR, MONTH, DEFLATOR (= RPI_t / RPI_base).
load_rpi_deflator <- function(path = raw("RPI_deflator.csv")) {
  rpi <- fread(path, select = c("YEAR", "MONTH", "RPI"))
  base <- rpi[YEAR == DEFLATOR_BASE[["year"]] & MONTH == DEFLATOR_BASE[["month"]], RPI]
  stopifnot(length(base) == 1)
  rpi[, .(YEAR, MONTH, DEFLATOR = RPI / base)]
}

# Add YEAR, MONTH, REAL_PRICE (Jan-2019 GBP) to a transaction table and drop the
# top (1 - PRICE_TRIM_Q) share of real prices. Used for every price-based exhibit.
prepare_transactions <- function(dt, trim = TRUE) {
  dt <- dt[!is.na(UPRN) & !is.na(INSPECTION_DATE)]
  dt[, `:=`(YEAR = year(TRANSACTION_DATE), MONTH = month(TRANSACTION_DATE))]
  dt <- merge(dt, load_rpi_deflator(), by = c("YEAR", "MONTH"), all.x = TRUE)
  dt[, REAL_PRICE := PRICE / DEFLATOR]
  dt <- dt[is.finite(REAL_PRICE)]
  if (trim) dt <- dt[REAL_PRICE <= quantile(REAL_PRICE, PRICE_TRIM_Q)]
  dt[, `:=`(YEAR_f = factor(YEAR), MONTH_f = factor(MONTH),
            pcu_area = postcode_area(POSTCODE_EPC),
            pcu_district = postcode_district(POSTCODE_EPC),
            year_month = sprintf("%d-%02d", YEAR, MONTH))]
  setorder(dt, UPRN, TRANSACTION_DATE)
  dt[]
}

# Monthly long-term gilt yield (FRED IRLTLT01GBM156N), in PERCENT, keyed by "YYYY-MM".
load_gilt_yield <- function(path = raw("IRLTLT01GBM156N.csv")) {
  ir <- fread(path)
  setnames(ir, c("DATE", "IRLTLT01GBM156N"), c("date", "gilt_10y"))
  ir[, year_month := substr(as.character(date), 1, 7)]
  setorder(ir, date)
  ir[, gilt_10y_L3 := shift(gilt_10y, 3)]
  ir[, .(year_month, gilt_10y, gilt_10y_L3)]
}

# ---- EPC master file --------------------------------------------------------
# Loads the cleaned EPC register (one row per certificate) built by
# code/01_build/02_epc_certificates.R. Optional column selection keeps memory down.
load_master_epc <- function(select = NULL) {
  epc <- fread(inter("master_epcs_clean.csv"), select = select)
  epc[]
}

# ---- Output helpers -----------------------------------------------------------
stars <- function(p) fifelse(p < 0.001, "***", fifelse(p < 0.01, "**", fifelse(p < 0.05, "*", "")))

write_tex <- function(x, path) { writeLines(x, path); message("wrote ", path) }
