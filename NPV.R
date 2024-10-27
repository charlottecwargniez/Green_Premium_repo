# IMPORT LIBRARIES -------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(plm)
library(stringr)
library(data.table)
library(rdrobust)
library(rdmulti)
library(parallel)
library(broom)
library(magrittr)  # For the pipe operator
library(viridis)  # For viridis color palette

library(e1071) # to save in latex tables 
library(xtable)
library(stargazer)
library(kableExtra)

library(sf) # spatial visualization 
library(ggmap)
library(osmdata)
library(mapproj)

library(lubridate)

# Clear the environment
rm(list = ls())

# Load Data ---------------------------------------------------------------------
setwd('/Users/charlottewargniez/Desktop/GreenPremium')

# Load master_dataset
master_dataset <- as.data.table(fread('data/cleaned/master_dataset_all.csv'))

#Load retrofit costs 
costs_per_UPRN <- as.data.table(fread('data/cleaned/costs_uprn_epc.csv'))

#Load Zoopla Rent data 
zoopla_rent <- as.data.table(fread('data/cleaned/zoopla_rent_predict.csv'))

#variable and fixed electricity prices
electricity <- as.data.table(fread('data/cleaned/electricity_prices.csv'))

# Load active listing sale 
active_sale <- as.data.table(fread('data/cleaned/zoopla_active_sale.csv'))

# Helper Functions -----------------------------------------------------------

# Create a function to copy values across df
copy_id_value <- function(dt, from_id, to_id, column) {
  dt[IMPROVEMENT_ID == to_id, (column) := dt[IMPROVEMENT_ID == from_id, get(column)]]
}

# Function to sum costs based on preprocessed IDs
sum_impacts <- function(ids_split, impact_column, impacts_table) {
  # Sum costs where IMPROVEMENT_ID is in ids_split
  sapply(ids_split, function(ids) sum(impacts_table[IMPROVEMENT_ID %in% ids, as.numeric(get(impact_column))], na.rm = TRUE))
}

# Function to keep occurrences larger than 1
keep_occurrences <- function(data, var1, occurrence) {
  setDT(data)
  n_occur <- data[, .N, by = var1][N > occurrence]
  data <- data[var1 %in% n_occur$var1]
  rm(n_occur)
  return(data)
}

# Create a function to format the regression output
format_reg_output <- function(reg_output) {
  reg_output %>%
    mutate(
      p.value = ifelse(`Pr(>|t|)` < 0.01, "***",
                       ifelse(`Pr(>|t|)` < 0.05, "**",
                              ifelse(`Pr(>|t|)` < 0.1, "*", ""))),
      estimate = sprintf("%.2f", Estimate),
      std.error = sprintf("(%.2f)", Std..Error)
    ) %>%
    select(estimate, std.error, p.value) %>%
    mutate(estimate = paste0(estimate, p.value)) %>%
    select(estimate, std.error)
}

# Create a function to write a custom csv
custom_write_csv <- function(reg_output_list,csv_file_name) {
  # Apply the formatting function to each regression output
  formatted_reg_output <- lapply(reg_output_list, format_reg_output)
  
  # Combine the formatted regression outputs into a single data frame
  combined_reg_output <- do.call(cbind, formatted_reg_output)
  
  # Save the combined regression output to a CSV file
  write.csv(combined_reg_output, csv_file_name,
            row.names = TRUE)
}

# Function to run regression and extract coefficients
run_regression <- function(formula, data, cluster_se, pattern_controls) {
  # Perform OLS regression
  reg_output <- lm(formula, data = data)
  # Extract coefficients summary
  reg_summary <- summary(reg_output)
  coeffs_std <- data.frame(reg_summary$coefficients)
  # Filter out coefficients based on pattern_controls
  #coeffs_std <- coeffs_std[!grepl(pattern = pattern_controls, rownames(coeffs_std)), ]
  # Add cluster SE if needed (this part may need adjustment depending on how cluster_se is structured)
  coeffs_std$cluster <- cluster_se
  return(coeffs_std)
}

# Define a function to find positions of elements in vec1 that do not match any in vec2
find_unmatched_positions <- function(vec1, vec2) {
  match_results <- match(vec1, vec2)  # Find matches
  na_positions <- which(is.na(match_results)) # Positions of NA values indicate unmatched elements
  return(list(na_positions))
}

# Function to propagate values based on relationships
propagate_values <- function(dt, relationships) {
  for (rel in relationships) {
    cols <- rel
    
    # Find rows where any of the columns in the relationship is 1
    rows_to_update <- dt[,(cols), with=FALSE][, Reduce(`|`, lapply(.SD, `==`, 1))]
    
    # Set all related columns to 1 in the selected rows
    for (col in cols) {
      dt[rows_to_update, (col) := 1]
    }
  }
}


# Replace IMPROVEMENT ID duplicatess
replace_improvement_id <- function(id) {
  if (id %in% c(12, 13, 14, 15, 17, 18)) return(11)
  if (id == 21) return(20)
  if (id == 24) return(30)
  if (id == 31) return(25)
  if (id %in% c(29, 32)) return(27)
  if (id == 38) return(37)
  if (id == 41) return(40)
  if (id == 61) return(59)
  if (id == 62) return(60)
  return(id)
}


# Define a function to format p-values
format_p_value <- function(p) {
  if (p < 0.001) {
    return(sprintf("%.3f ***", p))
  } else if (p < 0.01) {
    return(sprintf("%.3f **", p))
  } else if (p < 0.05) {
    return(sprintf("%.3f *", p))
  } else {
    return(sprintf("%.3f", p))
  }
}



# Global Variables -------------------------------------------


# # ------------------------ CBA (Basic) ---------------------------------------
#   ## Marginal price of EPC retrofit --------
#   
#   # marginal per uprn
#   costs_per_UPRN[, mar_cost := BY_EPC/RISE_IN_EPC]
#   setnames(costs_per_UPRN, 'CURRENT_ENERGY_EFFICIENCY.x', 'CURRENT_ENERGY_EFFICIENCY')
#   costs_per_UPRN[, CURRENT_ENERGY_EFFICIENCY := as.numeric(CURRENT_ENERGY_EFFICIENCY)]
#   
#   # Fit the linear model
#   model <- lm(BY_EPC ~ CURRENT_ENERGY_EFFICIENCY, data = costs_per_UPRN)
#   
#   # Get predicted values and standard deviation of residuals
#   costs_per_UPRN <- costs_per_UPRN %>%
#     mutate(
#       fitted_values = predict(model), 
#       residuals = residuals(model),
#       std_dev = sd(residuals)  # Standard deviation of residuals
#     )
#   
#   # Plotting mar_cost by CURRENT_ENERGY_EFFICIENCY
#   marginal_costs <- ggplot(costs_per_UPRN, aes(x = CURRENT_ENERGY_EFFICIENCY, y = mar_cost)) +
#     geom_point(color = "blue", alpha = 0.5) +
#     labs(
#       title = "Marginal Cost by Current Energy Efficiency",
#       x = "Current Energy Efficiency (EPC Score)",
#       y = "Marginal Cost (£ per unit EPC rise)"
#     ) +
#     theme_minimal()
#   ggsave("output/plots/marginal_costs_scatter.png", marginal_costs)
#   
#   # Plotting mar_cost by CURRENT_ENERGY_EFFICIENCY line of best fit and standard deviation
#   retrofit_line <- ggplot(costs_per_UPRN, aes(x = CURRENT_ENERGY_EFFICIENCY, y = BY_EPC)) +
#     geom_line(aes(y = fitted_values), color = "red", size = 1) +  # Line of best fit
#     geom_ribbon(aes(ymin = fitted_values - std_dev, ymax = fitted_values + std_dev), 
#                 fill = "lightblue", alpha = 0.3) +  # Standard deviation bands
#     labs(
#       title = "Costs to Retrofitting to 69% with Standard Deviation Bands",
#       x = "Current Energy Efficiency (EPC Score)",
#       y = "£"
#     ) +
#     theme_minimal()
#   ggsave("output/plots/marginal_costs_line.png", retrofit_line)
#   
#   
#   ## Marginal price of Green Premium --------
  
  
# ------------------------ NPV model (Valuation) ---------------------------------------

  ## Interest Rates ------------------------------
  
  # Load interest rates
  interest_rates <- fread("data/IRLTLT01GBM156N.csv")
  setnames(interest_rates,"IRLTLT01GBM156N","base_rate")
  interest_rates[, date_to_merge := as.character(DATE)]
  interest_rates[, date_to_merge := substr(date_to_merge, 1, nchar(date_to_merge) - 3)]
  interest_rates[, DATE := NULL]
  
  #calculate sigma i (St.dev) - for rolling 12 months 
  interest_rates[, date := as.Date(date_to_merge, format = "%Y-%m")]
  interest_rates[, sigma := frollapply(base_rate, 12, sd, fill = NA, align = "right"), by = .(data.table::year(date))]
  interest_rates[, date := NULL]

  
  ## Clean Price paid data --------------
  
  # keep only columsn of interest 
  setnames(master_dataset, 'PROPERTY_TYPE_EPC', 'property_type')
  master_dataset <- master_dataset[, .(UPRN, TRANSACTION_DATE, TOTAL_FLOOR_AREA,PRICE, POSTCODE, PROPERTY_TYPE, BUILDING_REFERENCE_NUMBER, CURRENT_ENERGY_EFFICIENCY, ENERGY_CONSUMPTION_CURRENT, NUMBER_HABITABLE_ROOMS)]
  
  # Drop transactions without Inspection Date or UPRN
  master_dataset  <- master_dataset [!is.na(UPRN)]
  print(paste(length(unique(master_dataset$UPRN)), "unique UPRNs found"))
  
  # Calculate numerical year difference 
  master_dataset[, TRANSACTION_DATE := as.Date(TRANSACTION_DATE, format = "%Y-%m-%d")]
  master_dataset[, YEAR := as.numeric(format(TRANSACTION_DATE, "%Y"))]
  master_dataset[, year_num := decimal_date(TRANSACTION_DATE)]
  master_dataset[, duration := shift(year_num, type = "lead") - year_num, by = UPRN]
  master_dataset <- master_dataset[YEAR %in% c(2015:2022)] #keep only 2016-22 years 
  
  # Calculate price differential (y(t))
  master_dataset[, price_diff := shift(PRICE, type = "lead") - PRICE, by = UPRN]
  
  #Drop all Nas (last recorded transactions per UPRN)
  master_dataset <- master_dataset[!is.na(price_diff)]
  
  #calculate proxy for EPC (x(t))
  master_dataset[, x_t := 1-(CURRENT_ENERGY_EFFICIENCY/100)]
  
  ## first two digits of post_code (pcu_area)
  master_dataset[, pcu_area := toupper(gsub("[^A-Za-z]", "", POSTCODE))]
  master_dataset[, pcu_area := substr(pcu_area, 1, 2)]
  
  #adjust date to merge with interest rates
  master_dataset$date_to_merge <- format(as.Date(master_dataset$TRANSACTION_DATE), "%Y-%m")
  
  
  ## Clean Zoopla -------------------------------
  
  #reset year for merge
  setnames(zoopla_rent, "zoopla_year", "YEAR")
  
  
  ## Merge all ----------------------------
  
  #merge by property type and pcu_area
  master_dataset <- merge(x = master_dataset, y = zoopla_rent, by = c("PROPERTY_TYPE", "pcu_area", "YEAR"), all.x = TRUE)
  
  #merge with interest rates 
  master_dataset <- merge(x = master_dataset, y = interest_rates, by = "date_to_merge", all.x = TRUE)
  
  
# -------- NPV Regression --------------------------------------------------------
  
  ## SET UP ----------------
  
  #constants 
  EPC_cap <- 69 
  
  delta <- 3.636/100
  minus_delta <- 1 - delta
  
  #variable consumption (annual)
  electricity <- electricity[, Rg := var_price / shift(var_price)]
  
  #merge electricity prices 
  master_dataset <- merge(x = master_dataset, y = electricity, by = "YEAR", all.x = TRUE)
  master_dataset <- master_dataset[YEAR %in% c(2016:2022)] #keep only 2016-22 years 
  
  #standing charges 
  setnames(master_dataset, 'fixed_price', 'kappa_0')
  
  #kappa1
  master_dataset[, kappa_1 := var_price*ENERGY_CONSUMPTION_CURRENT]
  
  # rho and St
  master_dataset[, rho := (1+Rg)/(1+base_rate)]
  master_dataset[, s_t := (1-rho^duration)/(1-rho)]
  
  # g-bar
  master_dataset[, g_coeff := (1-delta^duration)/minus_delta]
  master_dataset[, g_bar := g_coeff*average_price_over_size*NUMBER_HABITABLE_ROOMS]
  
  # two independent variables 
  master_dataset[, interm_cashflow := g_bar - (kappa_0 * s_t*duration)]
  master_dataset[, energy_returns := kappa_1 * s_t * x_t]
  
  #independent variable 
  master_dataset[, m_bar := 1/ ((1+base_rate)^duration)] 
  master_dataset[, y_t := m_bar * price_diff]
  
  ## LINEAR MODEL ----------------
  
  # Fit the linear model
  model <- lm(y_t ~ interm_cashflow + energy_returns, data = master_dataset)
              
  # Display summary of the model
  summary_model <- summary(model)
  
  # Fix effects the linear model
  model_fixed <- feols(y_t ~ interm_cashflow + energy_returns | TOTAL_FLOOR_AREA + POSTCODE + NUMBER_HABITABLE_ROOMS, data = master_dataset)
  summary(model_fixed)
  summary_fixed <- summary(model_fixed)
  
  library(texreg)
  texreg(
    list(model),  # List of models
    custom.model.names = c("Model 1"),  # Custom label for the model
    caption = "NPV Regressions Results: Hedonic Price Model with and without Fixed Effects",
    label = "tab:hedonic_model",
    booktabs = TRUE,
    use.packages = FALSE,
    file = "output/tables/NPV_model.tex"  # Specify the file name to save the LaTeX output
  )
  
  ## NPV for all values ----------------
  
  #extract coefficient 
  # coefficients <- as.numeric(summary_fixed$coefficients)
  coefficients <- summary_model$coefficients
  intercept <- coefficients[1, 1]
  beta_1 <- coefficients[2, 1]
  beta_2 <- coefficients[3, 1]
  # beta_1 <- coefficients[1]
  # beta_2 <- coefficients[2]
  master_dataset[, NPV := beta_1*interm_cashflow + beta_2*energy_returns]
  
  filtered_dataset <- master_dataset[!is.na(NPV)]
  filtered_dataset[, decision := ifelse(NPV>0, TRUE, FALSE)]
  
  # Create the summary table
  summary_table <- filtered_dataset[, .(
    number_of_homes = .N,
    average_transaction_price = mean(PRICE, na.rm = TRUE),
    average_energy_efficiency = mean(CURRENT_ENERGY_EFFICIENCY, na.rm = TRUE)
  ), by = decision]
  
  
  # Regression with active sales (add to hedonic) ------------------- 
  
  # zoopla get year (take average listing per year )
  active_sale[, YEAR := lubridate::year(date)]
  active_sale[, avg_listing := mean(active_L1_sale), by = .(YEAR, pcu_area)]
  active_sale <- unique(active_sale[, .(YEAR, pcu_area, avg_listing)])
  
  #remove non macthign pcu_areas
  master_dataset <- master_dataset[pcu_area %in% unique(active_sale$pcu_area)]
  
  #select only master_datset between 2016 and 2022
  master_dataset[, YEAR := as.numeric(format(TRANSACTION_DATE, "%Y"))]
  master_dataset <- master_dataset[YEAR %in% c(2016:2022)]
  
  #merge with master_dataset 
  master_dataset <- merge(x = master_dataset, y = active_sale, by = c("YEAR","pcu_area"), all.x = TRUE)
  
  #run regression 
  model_2b_listings <- feols(REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY | TOTAL_FLOOR_AREA + PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + BUILT_FORM + CONSTRUCTION_AGE_BAND  + avg_listing, data = master_dataset, panel.id=c("UPRN", "XT_SET2"))
  