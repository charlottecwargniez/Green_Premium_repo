
# Determines impacts of retrofits (improvement IDs) using OLS regression:
      # dependent variable: difference between current and potential EPC
      # independent variables: Improvement IDs recommended to reach potential EPC / time fixed effects

# the data therein is stored in Master EPC - thus according to EPC certificates for dwellings. 

# Folder: 

# INPUT -> master_epc 
# OUTPUT -> table2a_ImprovementsOLS.csv / table2b_ImprovementsOLS.csv /

#Load Data 
source("functions/clean_masterEPC.R")

improvements_IDs <- fread("data/cleaned/improvement_ID_text.csv") # load the list of IDs
improvements_IDs$IMPROVEMENT_ID <- as.numeric(improvements_IDs$IMPROVEMENT_ID)

# Load Helper Functions 
source("functions/help_func.R") 

# Compute adjusted efficiency
master_epc[, DIFF_ENERGY_EFFICIENCY := POTENTIAL_ENERGY_EFFICIENCY - CURRENT_ENERGY_EFFICIENCY] # EPC score
master_epc[, POUND_PER_EPC := sapply(INDICATIVE_COST_AVG, sum_values)/DIFF_ENERGY_EFFICIENCY] # linear estimation of cost of raising EPC
master_epc[, DIFF_ENERGY_CONSUMPTION := ENERGY_CONSUMPTION_POTENTIAL - ENERGY_CONSUMPTION_CURRENT] # ELECTRICITY in kWh
master_epc[, DIFF_CO2_EMISSIONS := CO2_EMISSIONS_POTENTIAL - CO2_EMISSIONS_CURRENT] # CARBON EMISSIONS in tons of CO2
master_epc[, DIFF_ENVIRONMENT_IMPACT := ENVIRONMENT_IMPACT_POTENTIAL - ENVIRONMENT_IMPACT_CURRENT] # Environmental friendliness
master_epc[, DIFF_ENERGY_COST := LIGHTING_COST_POTENTIAL + HEATING_COST_POTENTIAL + HOT_WATER_COST_POTENTIAL - LIGHTING_COST_CURRENT - HEATING_COST_CURRENT - HOT_WATER_COST_CURRENT] # ENERGY COST (Light, water and heating)

# Gen time variables for regression controls
master_epc <- master_epc |>
  mutate (
    YEAR  = as.numeric(substr(INSPECTION_DATE,1,4)),
    MONTH  = as.numeric(substr(INSPECTION_DATE,6,7))
  )

# Level variables
master_epc[, `:=`(YEAR_f = factor(YEAR), MONTH_f = factor(MONTH))]


# Replace Improvement IDs based on duplicates
improvements_IDs <- improvements_IDs %>%
  mutate(IMPROVEMENT_ID = sapply(IMPROVEMENT_ID, replace_improvement_id))

master_epc <- master_epc %>%
  mutate(IMPROVEMENTS_IDs = lapply(IMPROVEMENTS_IDs, function(x) sapply(x, replace_improvement_id)))


# Find all covariates that start with "ID_"
covariates <- grep("^ID_", names(master_epc), value = TRUE)

# Define OLS regression formulas
list_of_formulas_1 <- list(
  formula_1 <- as.formula(paste("DIFF_ENERGY_EFFICIENCY ~ 0 + YEAR_f +", paste(covariates, collapse = " + "))),
  formula_2 <- as.formula(paste("DIFF_ENERGY_CONSUMPTION ~ 0 + YEAR_f +", paste(covariates, collapse = " + "))),
  formula_3 <- as.formula(paste("DIFF_CO2_EMISSIONS ~ 0 + YEAR_f +", paste(covariates, collapse = " + "))),
  formula_4 <- as.formula(paste("DIFF_ENVIRONMENT_IMPACT ~ 0 + YEAR_f +", paste(covariates, collapse = " + "))),
  formula_5 <- as.formula(paste("DIFF_ENERGY_COST ~ 0 + YEAR_f +", paste(covariates, collapse = " + ")))
)

# Run the series of regressions above
reg_output_list_1 <- lapply(list_of_formulas_1, function(formula) {
  run_regression(formula, master_epc, cluster_se, pattern_controls)
})


# Custom save the regression output to a CSV file
custom_write_csv(reg_output_list_1,"output/tables/table2a_ImprovementsOLS.csv")

# Find all covariates that end with "_EFF"
covariates <- grep("_EFF$", names(master_epc), value = TRUE)

# Define OLS regression formulas
list_of_formulas_2 <- list(
  formula_1 <- as.formula(paste("CURRENT_ENERGY_EFFICIENCY ~ + ", paste(covariates, collapse = " + "))),
  formula_2 <- as.formula(paste("ENERGY_CONSUMPTION_CURRENT ~ + ", paste(covariates, collapse = " + "))),
  formula_3 <- as.formula(paste("CO2_EMISSIONS_CURRENT ~ + ", paste(covariates, collapse = " + "))),
  formula_4 <- as.formula(paste("ENVIRONMENT_IMPACT_CURRENT ~ + ", paste(covariates, collapse = " + "))),
  formula_5 <- as.formula(paste("DIFF_ENERGY_COST ~ 0 + YEAR_f +", paste(covariates, collapse = " + ")))
)

# Run the series of regressions above
reg_output_list_2 <- lapply(list_of_formulas_2, function(formula) {
  run_regression(formula, master_epc, cluster_se, pattern_controls)
})

# Custom save the regression output to a CSV file
custom_write_csv(reg_output_list_2,"output/tables/table2b_ImprovementsOLS.csv")

