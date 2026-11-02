# Replication package: retrofit costs and the green premium in England and Wales

This folder reproduces every table and figure in the paper submitted to the
*Journal of Environmental Economics and Management*. It contains only the code
behind the paper. The rest of this repository (one level up) is the thesis
code it was extracted from, and is not needed.

> **Status: draft.** The code runs end to end on synthetic data. It has not yet
> been run on the full data, and several fixes (listed under "Changes") will move
> the numbers in the paper. Items marked **TODO** need the author's input.

## Quick start

1. Install R (tested with R 4.5) and open `replication.Rproj`, or `setwd()` to this folder.
2. Install packages: `renv::restore()` if `renv.lock` is present, otherwise
   `install.packages(c("here","data.table","ggplot2","fixest","knitr","kableExtra","patchwork","sf"))`.
3. Put the raw data in `data/raw/` as described below.
4. Run `source("00_master.R")`. Tables go to `output/tables/`, figures to `output/figures/`.

All paths are relative (via `here`). Nothing outside this folder is read or written.

## Exhibits and the scripts that produce them

| Paper exhibit | Overleaf file | Script | Output |
|---|---|---|---|
| Figure: average yearly cost by improvement | `src/figures/Facet_costs.png` | `code/02_analysis/figure_facet_costs.R` | `output/figures/Facet_costs.png` |
| Table: price by EPC rating and property type | `src/tables/price_by_epc&property_type.tex` | `code/02_analysis/table_price_by_epc.R` | `price_by_epc_property_type.tex` |
| Figure: active listings | `src/figures/active_listings.png` | `code/02_analysis/figure_active_listings.R` (new, **TODO** check against the published figure) | `active_listings.png` |
| Table: impacts of improvements (Eq. 1) | `src/tables/ID_impacts.tex` | `code/02_analysis/table_id_impacts.R` | `id_impacts.tex`, `id_impacts_estimates.csv` |
| Table: cumulative costs to reach EPC 69 | `src/tables/EPC69_results.tex` | `code/02_analysis/retrofit_costs.R` | `epc69_results.tex` |
| Table: measures needed, yield strategy | `src/tables/Improvements_Cap69_byYield_sorted.tex` | `code/02_analysis/retrofit_costs.R` | `improvements_cap69_by_yield.tex` |
| Appendix tables: EPC / impact / price strategies | `src/tables/Improvements_Cap69_by{EPC,IMPACT,PRICE}.tex` | `code/02_analysis/retrofit_costs.R` | `improvements_cap69_by_{epc,impact,price}.tex` |
| Figure: density of retrofit costs | `src/figures/Combined_Density_Costs.png` | `code/02_analysis/figure_density_costs.R` | `Combined_Density_Costs.png`, `density_costs_stats.csv` |
| Table 5: dwellings by capacity to reach EPC C | `src/tables/Table5_retrofit_descriptive.tex` | `code/02_analysis/retrofit_costs.R` | `table5_retrofit_descriptive.tex` |
| Table: hedonic price models | `src/tables/hedonic_results.tex` | `code/02_analysis/table_hedonic.R` | `hedonic_results.tex` |
| Table: listings elasticity | `src/tables/elasticity_results.tex` | `code/02_analysis/table_elasticity.R` | `elasticity_results.tex` |
| Figure: average cost per district | `src/figures/average_costs_per_district.png` | `code/02_analysis/figure_costs_district.R` | `average_costs_per_district.png` |

`EPCertificate.png`, `decision_model.png` and `CPI-RPI.png` are illustrations, not code output.

The data build (`code/01_build/`) runs first and writes to `data/intermediate/`:

| Step | Script | Main output |
|---|---|---|
| 1 | `01_epc_recommendation_costs.R` | `improvement_ID_stats.csv`, `improvement_ID_text.csv` |
| 2 | `02_epc_certificates.R` | `master_epcs_clean.csv` (one row per certificate) |
| 3 | `03_land_registry.R` | `master_registry.csv` (transactions with UPRN) |
| 4 | `04_master_dataset.R` | `master_dataset_all.csv` (transaction + latest prior EPC) |
| 5 | `05_zoopla_listings.R` | `zoopla_all_*.csv`, `zoopla_active_*.csv` |

## Data

| Input (in `data/raw/`) | Source | Access | Licence / redistribution |
|---|---|---|---|
| `epcs/<LA>/certificates.csv`, `recommendations.csv` | Energy Performance of Buildings Register, domestic, all local authorities: <https://epc.opendatacommunities.org/> (**TODO** download date) | Free registration | Open Government Licence, except address fields (Royal Mail / OS copyright). Do not redistribute addresses. |
| `registry/pp-YYYY.csv` | HM Land Registry Price Paid Data, yearly files 2008-2023: <https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads> (**TODO** download date) | Public | OGL (address data: Royal Mail / OS copyright) |
| `ppdid_uprn_usrn.csv` | Price Paid transaction ID to UPRN lookup. **TODO: source, version and date** | **TODO** | **TODO** |
| `RPI_deflator.csv` | ONS RPI all-items index, series CHAW: <https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/chaw/mm23>. Only columns `YEAR`, `MONTH`, `RPI` are used. | Public | OGL |
| `IRLTLT01GBM156N.csv` | OECD long-term (10-year) government bond yield for the UK, via FRED: <https://fred.stlouisfed.org/series/IRLTLT01GBM156N>. Monthly, percent. | Public | Public domain (FRED/OECD) |
| `zoopla/{rent,sale}/*.csv` | Zoopla Property Data 2016-2022, via the Urban Big Data Centre, University of Glasgow (ESRC) | **Licensed.** Apply to UBDC: <https://www.ubdc.ac.uk/> | **Cannot be redistributed.** Replicators must obtain it from UBDC. |
| `ONSPD_AUG_2024_UK.csv` | ONS Postcode Directory, August 2024: <https://geoportal.statistics.gov.uk/> | Public | OGL |
| `LAD_MAY_2022_UK_BFE_V3.*` | Local Authority Districts, May 2022, full-extent boundaries: <https://geoportal.statistics.gov.uk/> | Public | OGL |

`data/manual/improvement_ID_stats_q.csv` ships with this package. It is the
average indicative cost per improvement ID and year (output of build step 1),
with gaps for some IDs and years **filled in by hand**. **TODO:** document which
cells were filled and the rule used, or replace the manual step with code.

Computational requirements: the EPC register has about 25M certificates (15.6M
dwellings). The build needs roughly **TODO** GB of RAM and **TODO** hours; the
analysis scripts need **TODO**.

## Changes from the thesis code

These fixes mean the numbers produced here will differ from the current draft.
The table and figure files in the Overleaf should be regenerated from `output/`.

1. **One improvement-ID grouping** (`R/helpers.R`, `ID_COLLAPSE`). It matches the
   grouping in the paper's ID-impacts table. Previously four different mappings existed.
2. **Postcode area.** This now takes the leading 1-2 letters. Previously the first
   two letters of the whole postcode were used, so B became BA, W became WA, and E became EA.
3. **Listings merged on postcode area x month** (hedonic and elasticity). The old
   merge matched the transaction day to first-of-month counts, which kept only sales
   completed on the 1st of a month.
4. **Active-listing counts per area.** The old code grouped by the string
   `"pcu_area"`, which put everything in one national group, and lagged by one day
   rather than one month.
5. **Standard errors are clustered explicitly** by postcode district (`VCOV_MAIN`
   in `R/setup.R`). fixest's default had clustered the hedonic models on
   `TOTAL_FLOOR_AREA`, and the `lm` models used iid errors. The hedonic table notes
   should be updated: there are no household fixed effects, and the errors are clustered.
6. **Interest rate and listings enter the hedonic models as continuous controls.**
   They previously sat after `|`, which made them fixed effects with one dummy per value.
7. **ID-impacts table** is built from the regression objects. The old CSV output had
   its first rows overwritten by column labels.
8. **District map** joins ONSPD on `pcds` (one-space format) instead of `pcd`
   (fixed width), which failed for most postcodes.
9. **Average-cost line** in the density figure is computed, not hard-coded (it was 9058).
10. **RPI deflator** is computed from the RPI index in code, not taken from a
    hand-edited column. The result is identical: prices are in Jan-2019 pounds.
11. **Retrofit-cost algorithm** (one function per strategy):
    - Measures are kept up to and including the first one that reaches the cap.
    - Measures without a cost or impact estimate are dropped. Previously they turned
      the cumulative sum into NA, which silently dropped every later measure.
    - The ad-hoc "EPC <= 100" filters, applied only in some strategies, are removed.
    - "Already achieved" is now defined as current score >= 69. Previously some
      dwellings with a negative potential gain were counted as already achieved.
12. **Certificate matching** uses a rolling join (latest EPC on or before the sale).
    `EPC_AGE_YEARS` is saved for robustness checks.
13. **EPC build** writes `master_epcs_clean.csv` rather than overwriting its own
    input, so it can be re-run safely.
14. The thesis-only parts are removed: DiD, RDD, first-difference and IV models, the
    NPV model, log regressions and exploratory plots.

## Open items for the author (TODO)

- Fill in the data sources, dates and lookup provenance marked TODO above.
- Check the new active-listings figure against the published one.
- **Paper text vs tables:**
  - The hedonic numbers in the text (£486.72, £207.80, 10.4M) differ from the table.
  - The elasticity is -0.02, so a 1% rise in listings means a 0.02% lower price, not 2%.
  - Table 5 shows £8.3bn where the text implies about £83bn.
- The ID-impacts table in the paper shows an "Efficiency Gradings" row that the
  regression code does not include. Confirm whether it was a control.
