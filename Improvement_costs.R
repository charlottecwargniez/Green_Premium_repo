
# --------------------------- CONFIGURATION ------------------------------------
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

# Clear the environment
rm(list = ls())


# SELECTION EQUATIONS ----------------------------------------------------------
# Selections for EPCs
SELECT_MAIN <- c("LMK_KEY","BUILDING_REFERENCE_NUMBER","INSPECTION_DATE","TRANSACTION_TYPE")
SELECT_CURRENT <- c("CURRENT_ENERGY_RATING","CURRENT_ENERGY_EFFICIENCY","ENERGY_CONSUMPTION_CURRENT","LIGHTING_COST_CURRENT","HEATING_COST_CURRENT","HOT_WATER_COST_CURRENT","ENVIRONMENT_IMPACT_CURRENT","CO2_EMISSIONS_CURRENT")
SELECT_POTENTIAL <- c("POTENTIAL_ENERGY_RATING","POTENTIAL_ENERGY_EFFICIENCY","ENERGY_CONSUMPTION_POTENTIAL","LIGHTING_COST_POTENTIAL","HEATING_COST_POTENTIAL","HOT_WATER_COST_POTENTIAL","ENVIRONMENT_IMPACT_POTENTIAL","CO2_EMISSIONS_POTENTIAL")
SELECT_ENERGY <- c("HOT_WATER_ENERGY_EFF","FLOOR_ENERGY_EFF","WINDOWS_ENERGY_EFF","WALLS_ENERGY_EFF","ROOF_ENERGY_EFF","MAINHEAT_ENERGY_EFF","MAINHEATC_ENERGY_EFF","LIGHTING_ENERGY_EFF")  # empty: "SHEATING_ENERGY_EFF"
SELECT_ENV <- c("HOT_WATER_ENV_EFF","FLOOR_ENV_EFF","WINDOWS_ENV_EFF","WALLS_ENV_EFF","SHEATING_ENV_EFF","ROOF_ENV_EFF","MAINHEAT_ENV_EFF","MAINHEATC_ENV_EFF","LIGHTING_ENV_EFF")
SELECT_CONTROLS <- c("ENERGY_TARIFF","MAIN_FUEL","PHOTO_SUPPLY","GLAZED_TYPE","MAINS_GAS_FLAG","TENURE","PROPERTY_TYPE","BUILT_FORM","ADDRESS","POSTCODE","TOTAL_FLOOR_AREA","NUMBER_HABITABLE_ROOMS","NUMBER_HEATED_ROOMS","EXTENSION_COUNT","CONSTRUCTION_AGE_BAND","POSTTOWN","UPRN")
SELECT_DESCRIPTIONS <- c("HOTWATER_DESCRIPTION","FLOOR_DESCRIPTION","WINDOWS_DESCRIPTION","WALLS_DESCRIPTION","SECONDHEAT_DESCRIPTION","ROOF_DESCRIPTION","MAINHEAT_DESCRIPTION","MAINHEATCONT_DESCRIPTION","LIGHTING_DESCRIPTION")
SELECT_ADDRESSES_EPC <- c("BUILDING_REFERENCE_NUMBER","ADDRESS","POSTCODE","POSTTOWN","UPRN")
SELECT_RECOMMENDATIONS <- c("LMK_KEY","IMPROVEMENT_ID","IMPROVEMENT_ID_TEXT","INDICATIVE_COST") # not needed: ,"IMPROVEMENT_ITEM"
SELECT_ADDRESSES_CLEANED <- c("PRIMARY","SECONDARY","TERTIARY","STREET")

# FINAL SELECTION FOR EPC DATA
SELECTION <- c(SELECT_MAIN,SELECT_CURRENT,SELECT_POTENTIAL,SELECT_ENERGY,SELECT_CONTROLS)

# Create dummies
SELECT_ENERGY_d <- paste(SELECT_ENERGY,"_d",sep="") # keep track of extra-columns

# Selections for Registry
SELECT_REGISTRY <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V12","V13","V14","V15")
REGISTRY_VARIABLE_NAMES <- c("transactionid","PRICE","TRANSACTION_DATE","POSTCODE","PROPERTY_TYPE","NEWBUILD","ESTATE","PRIMARY","SECONDARY","STREET","TOWN","BOROUGH","COUNTY","TRANSACTION_CAT")
SELECT_ADDRESSES_REG <- c("ADDRESS","POSTCODE","PRIMARY","SECONDARY","STREET") # "BUILDING_REFERENCE_NUMBER","TOWN","BOROUGH","COUNTY"

# Variables: Levels Description
# PROPERTY_TYPE :: detached, semi-detached, terraced, flat/maisonette, other
# ESTATE_TYPE :: freehold, leasehold
# TRANSACTION_CAT :: standard/private (A), additional (B)




# Load Data  ----------------------------------------------------------
setwd('/Users/charlottewargniez/Desktop/GreenPremium')

# Read the first chunk
chunk1 <- as.data.table(fread("data/cleaned/master_epcs.csv", nrows = 1e6, skip = 0, header = TRUE))
column_names <- colnames(chunk1)

# Read the second chunk
chunk2 <- as.data.table(fread("data/cleaned/master_epcs.csv", nrows = 2e6, skip = 1e6, header = FALSE))
chunk3 <- as.data.table(fread("data/cleaned/master_epcs.csv", nrows = 2e6, skip = 3e6, header = FALSE))
chunk4 <- as.data.table(fread("data/cleaned/master_epcs.csv", nrows = 2e6, skip = 5e6, header = FALSE))
chunk5 <- as.data.table(fread("data/cleaned/master_epcs.csv", nrows = 2e6, skip = 7e6, header = FALSE))
chunk6 <- as.data.table(fread("data/cleaned/master_epcs.csv", skip = 9e6, header = FALSE))

#Change header to match
setnames(chunk2, column_names)
setnames(chunk3, column_names)
setnames(chunk4, column_names)
setnames(chunk5, column_names)
setnames(chunk6, column_names)

# Combine the chunks if needed
master_epc <- rbind(chunk1, chunk2, chunk3, chunk4, chunk5, chunk6)

# Remove unecessary chunks 
rm(chunk1)
rm(chunk2)
rm(chunk3)
rm(chunk4)
rm(chunk5)
rm(chunk6)

#Remove observations where Improvements ID NaN or empty
master_epc <- master_epc[!is.na(master_epc$IMPROVEMENTS_IDs) & master_epc$IMPROVEMENTS_IDs != "", ]

# Remove rows with UPRN is NA 
master_epc <- master_epc [!is.na(UPRN)]
master_epc[, UPRN := as.numeric(UPRN)]

# Remove rows with EPC score > 100
master_epc <- master_epc[CURRENT_ENERGY_EFFICIENCY <100]


# # Calculate time between inspections
# master_epc[, date_as_numeric := as.numeric(INSPECTION_DATE)]
# master_epc[, time_between_inspections := difftime(INSPECTION_DATE, shift(INSPECTION_DATE, type = "lag"), units = "days"), by = UPRN]
# 
# master_epc<- master_epc[time_between_inspections > 0]

# GLOBAL VARIABLES ----------------------------------------------------------
# REGRESSION PARAMETERS
controls <- "EXTENSION_COUNT + PROPERTY_TYPE + TOTAL_FLOOR_AREA + CONSTRUCTION_AGE_BAND + MAIN_FUEL + NUMBER_HABITABLE_ROOMS + ESTATE"
time_fe <- "YEAR_f + MONTH_f"
cluster_se <- c("BOROUGH") # Clustered Standard Errors: choose among TOWN, BOROUGH, COUNTY (GREATER LONDON, includes all E09)
pattern_controls <- "MAIN_FUEL|YEAR|MONTH|PROPERTY|CONSTRUCTION|ESTATE|EXTENSION|NUMBER_HABITABLE|TOTAL_FLOOR"
bandwidth <- 5 # Bandwidth Selection for Regression Discontinuity Design
CI_level <- 1.96 # Confidence Interval level

# RECOMMENDATIONS and IMPROVEMENTS
total_IDs <- 63 # total number of IMPROVEMENT_IDs

# Table to save costs per UPRN to reach cap 
costs_per_UPRN <- data.table()

#Create output table for cumulative costs 
cumulative_costs_table <- data.table()

# Load Deflator 
deflator <- as.data.table(fread('data/RPI_deflator.csv'))
deflator <- deflator[, .(YEAR, MONTH, `From 2019-Jan`)]
deflator[,XT_SET2 :=YEAR*100+MONTH]

# Helper Functions ----------------------------------------------------------

# Cleans a single address string by trimming, removing special characters, and converting to uppercase
clean_address <- function(address) {
  address %>%
    str_trim() %>% # Remove leading and trailing whitespace
    str_remove_all("\\.") %>% # Remove periods
    str_to_upper() # Convert to uppercase
}

# Define a function to find positions of elements in vec1 that do not match any in vec2
find_unmatched_positions <- function(vec1, vec2) {
  match_results <- match(vec1, vec2)  # Find matches
  na_positions <- which(is.na(match_results)) # Positions of NA values indicate unmatched elements
  return(list(na_positions))
}

# Function to run regression and extract coefficients
run_regression <- function(formula, data, cluster_se, pattern_controls) {
  
  # Perform SGD
  #sgd_model <- sgd(formula, data, model = "lm", sgd.control = list(reltol = 1e-8, npasses = 50))
  # coeffs_std <- coefficients(sgd_model)
  # transform into data.frame with correct regressor names
  
  # Perform OLS
  reg_output <- lm(formula, data = data)
  coeffs_std <- data.frame(summary(reg_output)$coefficients, cluster = cluster_se)
  
  # Keep essentials
  coeffs_std[!grepl(pattern = pattern_controls, rownames(coeffs_std)), ]
}

# Function to keep occurrences larger than 1
keep_occurrences <- function(data, var1, occurrence) {
  setDT(data)
  n_occur <- data[, .N, by = var1][N > occurrence]
  data <- data[var1 %in% n_occur$var1] 
  rm(n_occur)
  return(data)
}

# Function to calculate the mode
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to round 1st decimals if not whole number
custom_round <- function(Value){
  Value <- if_else(abs(Value - round(Value)) < .Machine$double.eps^0.5, as.character(as.integer(Value)), sprintf("%.1f", Value))
}

# Function to sum numeric values in a character vector
sum_values <- function(vector) {
  # Split the vector by commas and convert to numeric
  numeric_values <- as.numeric(unlist(strsplit(vector, ",")))
  
  # Sum the numeric values
  total_sum <- sum(numeric_values, na.rm = TRUE)
  
  return(total_sum)
}

# Create a function to format the regression output
format_reg_output <- function(reg_output) {
  reg_output %>%
    mutate(
      p.value = ifelse(Pr...t.. < 0.01, "***",
                       ifelse(Pr...t.. < 0.05, "**",
                              ifelse(Pr...t.. < 0.1, "*", ""))),
      estimate = sprintf("%.2f", Estimate),
      std.error = sprintf("(%.2f)", Std..Error)
    ) %>%
    select(estimate, std.error, p.value) %>%
    mutate(estimate = paste0(estimate, p.value)) %>%
    select(estimate) # , std.error
}

# Custom cbind function that pads vectors with NAs
cbind_pad <- function(..., fill = NA) {
  # Collect all input arguments into a list
  args <- list(...)
  
  # Determine the maximum number of rows needed
  nrow <- max(sapply(args, NROW))
  
  # Pad each input to the maximum length
  padded_args <- lapply(args, function(x) {
    if (is.null(dim(x))) {  
      # If x is a vector, convert it to a single-column matrix
      x <- matrix(x, ncol = 1) 
    }
    # Calculate the number of rows to add
    rows_to_add <- nrow - NROW(x)
    if (rows_to_add > 0) { 
      # Pad the matrix with the fill value (default is NA)
      x <- rbind(x, matrix(fill, nrow = rows_to_add, ncol = NCOL(x)))
    }
    return(x)
  })
  
  # Combine the padded matrices using cbind
  do.call(cbind, padded_args) 
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

# Create a function to copy values across df
copy_id_value <- function(dt, from_id, to_id, column) {
  dt[IMPROVEMENT_ID == to_id, (column) := dt[IMPROVEMENT_ID == from_id, get(column)]]
}

# Helper function to split IDs
split_ids <- function(ids) {
  as.numeric(unlist(strsplit(ids, ",")))
}


# Replace IMPROVEMENT ID duplicates sassa
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


gc()





# -------------------- DESCRIPTIVE STATS -------------------------------------

# Creating the histogram
distribution <- ggplot(master_epc, aes(x = CURRENT_ENERGY_EFFICIENCY)) +
  geom_histogram(aes(y = (..count..) / sum(..count..) * 100), 
                 binwidth = 1, color = "black", fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Current Energy Efficiency Ratings",
       x = "Current Energy Efficiency Rating",
       y = "Percentage of Total Observations (%)") +
  theme_minimal()

# Create a new column for the inspection number for each UPRN
master_epc[, inspection_number := seq_len(.N), by = UPRN]

# Plotting the distribution of CURRENT_ENERGY_EFFICIENCY by inspection number
ggplot(master_epc, aes(x = factor(inspection_number), y = CURRENT_ENERGY_EFFICIENCY)) +
  geom_boxplot(aes(group = inspection_number), fill = "blue", alpha = 0.7) +
  labs(title = "Variation in EPC Ratings Across Inspections",
       x = "Inspection Number",
       y = "Current Energy Efficiency Rating") +
  theme_minimal()

# -------------------------- MAIN EXECUTION -----------------------------------

# COST of REACHING EPC ---------------------------------------------------------
  
  ## Gen file with Improvement IDs, Costs and IMPACT CATEGORIES
  improvements_IDs <- fread("data/cleaned/improvement_ID_text.csv") # load the list of IDs
  improvements_IDs$IMPROVEMENT_ID <- as.numeric(improvements_IDs$IMPROVEMENT_ID)
  
  # Add avg latest cost per improvement
  costs_improvements <- as.data.table(fread("data/cleaned/improvement_ID_stats_q.csv"))
  costs_improvements <- costs_improvements[costs_improvements[, .I[which.max(YEAR)], by = .(IMPROVEMENT_ID)]$V1]
  costs_improvements <- costs_improvements[, .(IMPROVEMENT_ID,Average_Cost)]
  costs_improvements[, Average_Cost := custom_round(as.numeric(Average_Cost))] # costs_improvements[, Average_Cost := sprintf("%.1f", Average_Cost)]
  setnames(costs_improvements,"Average_Cost","RETROFIT_COST")
  
  # Merge ID, texts and IMPACT variables
  improvements_IDs <- merge(x = improvements_IDs, y = costs_improvements, by = "IMPROVEMENT_ID", all.x = T)
  rm(costs_improvements)
  
  # Add avg return on EPC, emissions, consumption, environment and cost per improvement
  impact_improvements <- fread("output/tables/reg_EPC_1.csv", header = FALSE) 
  impact_improvements <- impact_improvements[-1] # drop first line
  
  # Rename and clean multiple columns
  impact_names = c("IMPACT_EPC", "IMPACT_CONS", "IMPACT_CO2", "IMPACT_ENV","IMPACT_COST")
  setnames(impact_improvements, old = c("V2", "V3", "V4", "V5","V6"), new = impact_names)
  setnames(impact_improvements, old = c("V1"), new = "IMPROVEMENT_ID")
  impact_improvements <- impact_improvements[, lapply(.SD, function(x) gsub("\\*", "", x))] # remove stars in estimates
  impact_improvements$IMPROVEMENT_ID <- gsub("ID_","",as.character(impact_improvements$IMPROVEMENT_ID)) # remove characters
  impact_improvements$IMPROVEMENT_ID <- as.numeric(impact_improvements$IMPROVEMENT_ID)
  
  # Merge ID, texts and IMPACT variables
  improvements_IDs <- merge(x = improvements_IDs, y = impact_improvements, by = "IMPROVEMENT_ID", all.x = T)
  rm(impact_improvements)
  
  # Fill in missing values from duplicated dummies
  # for (columns in all_of(impact_names)) {
  #   print(columns)
  #   copy_id_value(improvements_IDs, from_id = 11, to_id = 12, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 11, to_id = 13, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 11, to_id = 14, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 11, to_id = 15, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 11, to_id = 17, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 11, to_id = 18, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 20, to_id = 21, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 24, to_id = 30, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 25, to_id = 31, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 27, to_id = 29, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 27, to_id = 32, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 37, to_id = 38, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 40, to_id = 41, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 59, to_id = 61, column = columns)
  #   copy_id_value(improvements_IDs, from_id = 60, to_id = 62, column = columns)
  # }
  # 
  
  ## Optional: delete rows with NA (if not copied above) !!!!!!!!!!!!
  improvements_IDs <- improvements_IDs [!is.na(IMPACT_EPC)]
  improvements_IDs <- improvements_IDs[IMPROVEMENT_ID!=42]
  
  fwrite(improvements_IDs,"output/tables/improvements_complete.csv") # Output file
  
  ## Clean master_epc ----------------------------------------------
  
  # Cleaning: Identify columns starting with "ID_"
  cols_to_remove <- grep("^ID_", names(master_epc), value = TRUE)
  
  # Remove the identified columns
  master_epc[, (cols_to_remove) := NULL]

  # By group transaction date and unique ID, keep observations with latest inspection date
  master_epc <- master_epc[master_epc[, .I[which.max(INSPECTION_DATE)], by = .(UPRN)]$V1]
  
  # Count how many properties can raise their EPCs
  print(paste("UPRNs able to raise their EPC:", master_epc[DIFF_ENERGY_EFFICIENCY != 0, .N]))
  
  master_epc <- master_epc[DIFF_ENERGY_EFFICIENCY != 0]
  
  ## BACK OF THE ENVELOPPE ESTIMATION (underestimated) -------------------------
  # Compute pound per epc when improvements are possible (i.e. linear estimation of cost of raising EPC) to avoid dividing by 0
  master_epc[DIFF_ENERGY_EFFICIENCY > 0, POUND_PER_EPC := sapply(INDICATIVE_COST_AVG, sum_values)/DIFF_ENERGY_EFFICIENCY]
  
  # Compute IMPACTs per epc when improvements are possible (i.e. linear estimation of cost of raising EPC) to avoid dividing by 0
  master_epc[DIFF_ENERGY_EFFICIENCY > 0, CONS_PER_EPC := DIFF_ENERGY_CONSUMPTION/DIFF_ENERGY_EFFICIENCY]
  master_epc[DIFF_ENERGY_EFFICIENCY > 0, ENV_PER_EPC := DIFF_ENVIRONMENT_IMPACT/DIFF_ENERGY_EFFICIENCY]
  master_epc[DIFF_ENERGY_EFFICIENCY > 0, CO2_PER_EPC := DIFF_CO2_EMISSIONS/DIFF_ENERGY_EFFICIENCY]
  
  for (EPC_cap in c(21,39,55,69,81,92)) {
    
    # Count how many properties can improve their EPCs
    retrofitable_props <- sum(master_epc$DIFF_ENERGY_EFFICIENCY > 0 & EPC_cap > master_epc$CURRENT_ENERGY_EFFICIENCY)
    
    # Compute the maximum possible rise in EPC between either the threshold and POTENTIAL_ENERGY_EFFICIENCY  
    master_epc[EPC_cap > CURRENT_ENERGY_EFFICIENCY, RISE_IN_EPC := ifelse(POTENTIAL_ENERGY_EFFICIENCY > EPC_cap, EPC_cap - CURRENT_ENERGY_EFFICIENCY, DIFF_ENERGY_EFFICIENCY)]
    
    # Compute cost of reaching the EPC_cap (or getting as close as possible if not reachable)
    master_epc[DIFF_ENERGY_EFFICIENCY > 0 & EPC_cap > CURRENT_ENERGY_EFFICIENCY, 
               paste0("COST_OF_REACHING_EPC_", EPC_cap) := RISE_IN_EPC * POUND_PER_EPC]
    
    # Summing costs in the dynamic column
    sum_positive <- round(master_epc[get(paste0("COST_OF_REACHING_EPC_", EPC_cap)) > 0, 
                                     sum(get(paste0("COST_OF_REACHING_EPC_", EPC_cap))) ] / 10^9, 3) # in billions with 3 decimals
    
    print(paste("RETROFIT COST TO", EPC_cap, ": b£", sum_positive, "for", retrofitable_props, "properties"))
    
    #Same for Impacts 
    master_epc[DIFF_ENERGY_EFFICIENCY > 0 & EPC_cap > CURRENT_ENERGY_EFFICIENCY, 
               paste0("CONS_REACHING", EPC_cap) := RISE_IN_EPC * CONS_PER_EPC]
    master_epc[DIFF_ENERGY_EFFICIENCY > 0 & EPC_cap > CURRENT_ENERGY_EFFICIENCY, 
               paste0("ENV_REACHING", EPC_cap) := RISE_IN_EPC * ENV_PER_EPC]
    master_epc[DIFF_ENERGY_EFFICIENCY > 0 & EPC_cap > CURRENT_ENERGY_EFFICIENCY, 
               paste0("CO2_REACHING", EPC_cap) := RISE_IN_EPC * CO2_PER_EPC]
    
  }
  
  # Descriptive Statistics 
  average_cost <- master_epc[COST_OF_REACHING_EPC_69  > 0, mean(COST_OF_REACHING_EPC_69), by = UPRN]
  average_cost <- round(mean(average_cost$V1))
  average_improvements <- "NA"
  sum_impact_co2 <- round(master_epc[CO2_REACHING69 <0, sum(CO2_REACHING69) ]) 
  sum_impact_env <- round(master_epc[ENV_REACHING69> 0, sum(ENV_REACHING69) ]) 
  sum_impact_cons <- round(master_epc[CONS_REACHING69 <0, sum(CONS_REACHING69) ]) 
  sum_impact_costs <- round(master_epc[COST_OF_REACHING_EPC_69> 0, sum(COST_OF_REACHING_EPC_69) ])
  EPC_cap <- 69 
  
  # Save in new column 
  retro_fit_linear <- cbind("Linear Est.", EPC_cap,  average_improvements, average_cost, sum_impact_co2, sum_impact_env,sum_impact_cons,sum_impact_costs)
  print(retro_fit_linear)
  
  
  
  ## Costs of years ------------------------------
  
  # Load the data
  data <- fread("data/cleaned/improvement_ID_stats_wide.csv")

  # Reshape data to long format
  dt_long <- data %>%
    pivot_longer(cols = starts_with("Year_"), names_to = "Year", values_to = "Value") %>%
    pivot_wider(names_from = Category, values_from = Value) %>%
    na.omit()
  setDT(dt_long)
  
  # Clean Year column to make it numeric
  dt_long[, YEAR := as.numeric(gsub("Year_", "", Year))]
  dt_long[, Year := NULL]
  setorder(dt_long, IMPROVEMENT_ID, YEAR)
  dt_long <- dt_long[!is.na(Average_Cost)] # remove if cost in NA
  
  # Apply the function to replace duplicate Improvement_IDs
  dt_long[, IMPROVEMENT_ID := sapply(IMPROVEMENT_ID, replace_improvement_id)]
  dt_long <- dt_long[!is.na(Average_Cost)]
  dt_long <- dt_long[!is.na(Count)]
  
  dt_long[, Count := as.numeric(Count)]
  dt_long[, SD_Cost := as.numeric(SD_Cost)]
  dt_long[, Average_Cost := as.numeric(Average_Cost)]
  
  # Calculate weighted averages for each IMPROVEMENT_ID and YEAR
  dt_long <- dt_long[, .(
    Average_Cost = sum(Average_Cost * Count) / sum(Count),
    Count = sum(Count)
  ), by = .(IMPROVEMENT_ID, YEAR)]

  
  # # Adjust Prices for deflator
  # deflator[, yearly_RIP := mean(`From 2019-Jan`), by = YEAR] # Take average RPI per year
  # deflator <- unique(deflator, by = "YEAR") # Remove duplicates in the YEAR column, keeping the first occurrence
  # dt_long <- merge(x = dt_long, y = deflator, by = "YEAR", all.x = T)
  # dt_long[, MEAN_REAL := Average_Cost/(`yearly_RIP`+1)] #estimate inflated controlled prices
  
  # Calculate the first observation mean price for each Improvement_ID
  dt_long[, First_Observation := Average_Cost[YEAR == min(YEAR)], by = IMPROVEMENT_ID]
  
  # Calculate change from the first observation (in % change)
  dt_long[, Change_from_First := 100*(Average_Cost - First_Observation)/First_Observation, by = IMPROVEMENT_ID]
  
  # Create the plot
  ggplot(dt_long, aes(x = YEAR, group = IMPROVEMENT_ID)) +
    geom_line(aes(y = Average_Cost, color = as.factor(IMPROVEMENT_ID))) +
    #geom_ribbon(aes(ymin = Average_Cost - SD_Cost, ymax = Average_Cost + SD_Cost, fill = as.factor(IMPROVEMENT_ID)), alpha = 0.3) +
    geom_text(aes(y = Average_Cost, label = IMPROVEMENT_ID), hjust = -0.1, vjust = 0, size = 3, check_overlap = TRUE) +
    labs(
      x = "Year",
      y = "Mean Price",
      title = "Mean Price with SD across Years",
      color = "Improvement ID",
      fill = "Improvement ID"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Plot changes from the first observation for each Improvement_ID
  plot <- ggplot(dt_long, aes(x = YEAR, y = Change_from_First, color = as.factor(IMPROVEMENT_ID), group = IMPROVEMENT_ID)) +
    geom_line() +
    geom_point() +
    geom_text(aes(y = Change_from_First, label = IMPROVEMENT_ID), hjust = -1, vjust = 0, size = 3, check_overlap = TRUE) +
    labs(
      x = "Year",
      y = "Change from First Observation Price (%)",
      color = "Improvement ID"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  ggsave("output/plots/Change_from_First_Observation_Plot.png", plot = plot, width = 10, height = 6) # Save the plot to a file
  
  # Identify if the line is always zero
  dt_long[, Always_Zero := all(Change_from_First == 0), by = IMPROVEMENT_ID]
  
  # Assign colors based on whether the line is always zero or not
  dt_long[, Line_Color := ifelse(Always_Zero, "grey", as.factor(IMPROVEMENT_ID))]
  
  # Manually define 12 distinct colors
  distinct_colors <- c("red", "yellow", "orange", "green", "blue", "pink", "purple", "magenta")
  
  # Generate colors for non-zero lines
  non_zero_ids <- unique(dt_long[Always_Zero==FALSE]$IMPROVEMENT_ID)
  color_mapping <- setNames(rep(distinct_colors, length.out = length(non_zero_ids)), non_zero_ids)
  
  # Add grey for always zero lines
  color_mapping <- c(color_mapping, grey = "grey")
  
  # Plot with colors based on change status
  ggplot(dt_long, aes(x = YEAR, y = Change_from_First, group = IMPROVEMENT_ID, color = Line_Color)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = color_mapping) + 
    labs(
      x = "Year",
      y = "Change from First Observation Price",
      title = "Change in Price from First Observation by Improvement ID",
      color = "Improvement ID"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ## Change in Improvement Costs across years -------------------
  
  # Filter data to include only lines with changes from the first observation
  dt_filtered <- dt_long[Always_Zero == FALSE]
  
  # Plot
  ggplot(dt_filtered, aes(x = YEAR, group = IMPROVEMENT_ID)) +
    geom_line(aes(y = Average_Cost, color = as.factor(IMPROVEMENT_ID))) +
    #geom_ribbon(aes(ymin = Average_Cost - SD_Cost, ymax = Average_Cost + SD_Cost, fill = as.factor(IMPROVEMENT_ID)), alpha = 0.3) +
    geom_text(aes(y = Average_Cost, label = IMPROVEMENT_ID), hjust = -0.1, vjust = 0, size = 3, check_overlap = TRUE) +
    labs(
      x = "Year",
      y = "Mean Price",
      title = "Mean Price with SD across Years",
      color = "Improvement ID",
      fill = "Improvement ID"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Plot with colors based on change status
  ggplot(dt_filtered, aes(x = YEAR, y = Change_from_First, group = IMPROVEMENT_ID, color = Line_Color)) +
    geom_line() +
    geom_point() +
    labs(
      x = "Year",
      y = "Change from First Observation Price (%)",
      title = "Change in Price from First Observation by Improvement ID",
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ## Facet Plot of All Improvement ID change in costs --------------
  
  #Average Yearly Costs
  facet_price <- ggplot(dt_long, aes(x = YEAR, y = Average_Cost)) +
    geom_line(aes(color = as.factor(IMPROVEMENT_ID), group = IMPROVEMENT_ID)) +
    geom_point(aes(color = as.factor(IMPROVEMENT_ID))) +
    facet_wrap(~ IMPROVEMENT_ID, scales = "free_y", ncol = 5, nrow = 9) +
    labs(
      x = "Year",
      y = "Average Costs (£)",
      color = "Improvement ID"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      strip.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "none"
    )
  ggsave("output/plots/Facet_costs.png", plot = facet_price,width = 16, height = 22 ) # Save the plot to a file
  
  #Change from first observation 
  facet_change <- ggplot(dt_long, aes(x = YEAR, y = Change_from_First)) +
    geom_line(aes(color = as.factor(IMPROVEMENT_ID), group = IMPROVEMENT_ID)) +
    geom_point(aes(color = as.factor(IMPROVEMENT_ID))) +
    facet_wrap(~ IMPROVEMENT_ID, scales = "fixed") +
    labs(
      x = "Year",
      y = "Change from First Observation Price (%)",
      color = "Improvement ID"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  ggsave("output/plots/Facet_changefromfirst.png", plot = facet_change, width = 16, height = 8) # Save the plot to a file
  
  
  
# ESTIMATION WHICH INCLUDES EACH IMPACT PER IMPROVEMENT ---------------------
  improvements_IDs <- as.data.table(fread("data/cleaned/improvements_complete.csv"))
  
  #Replace Improvement IDs based on duplicates 
  improvements_IDs[, IMPROVEMENT_ID := sapply(IMPROVEMENT_ID, replace_improvement_id)]
  improvements_IDs <- unique(improvements_IDs, by = c("IMPROVEMENT_ID")) #remove created duplicates 

  EPC_cap <- 69 #Set an EPC cap/threshold to cross

  # Compute RISE_IN_EPC (if negative then CURRENT_ENERGY_EFFICIENCY > EPC_cap)
  master_epc[,RISE_IN_EPC := ifelse(POTENTIAL_ENERGY_EFFICIENCY > EPC_cap,EPC_cap - CURRENT_ENERGY_EFFICIENCY,DIFF_ENERGY_EFFICIENCY)]

  # Remove observations where RISE_IN_EPC <= 0
  already_acheived <- master_epc[RISE_IN_EPC <= 0]
  master_epc <- master_epc[RISE_IN_EPC > 0]
  unable_reach <- master_epc[POTENTIAL_ENERGY_EFFICIENCY < EPC_cap] #houses that cannot reach EPC cap 
  
  # Create new frame from IMPROVEMENT_IDs
  separated_improvements <- master_epc[, .(CURRENT_ENERGY_EFFICIENCY, IMPROVEMENT_ID = unlist(strsplit(IMPROVEMENTS_IDs, ","))), by = UPRN]
  separated_improvements$IMPROVEMENT_ID <- as.numeric(separated_improvements$IMPROVEMENT_ID)
  setDT(separated_improvements)
  
  # Take care of IMPROVEMENT ID duplicate recommendations 
  separated_improvements[, IMPROVEMENT_ID := sapply(IMPROVEMENT_ID, replace_improvement_id)]
  separated_improvements <- unique(separated_improvements) #remove created duplicates 
  
  # Create a "count" for each improvement in order of recommendation 
  separated_improvements[, row_index := sequence(.N), by = UPRN]
  
  # Left join on improvement Ids (add impacts to each improvement IDs) and order by UPRN
  separated_improvements <- merge(x = separated_improvements, y = improvements_IDs, by = "IMPROVEMENT_ID", all.x = TRUE, allow.cartesian = FALSE)
  separated_improvements <-separated_improvements[order(UPRN,row_index),]
  setnames(separated_improvements, "CURRENT_ENERGY_EFFICIENCY", "INITIAL_EPC")
  
  ## 1. Improvement IDs in order of recommendation (EPC data) -----------------------
  
  # Sum the raise in EPC for each additional improvement 
  separated_improvements[, cumulative_sum := cumsum(IMPACT_EPC), by = UPRN]
  
  # Remove rows where rise > 100% (not necessary or possible) 
  separated_improvements <- separated_improvements[cumulative_sum <= 100]
  
  # Create new column with EPC at each improvement 
  separated_improvements[, EPC_sum := cumulative_sum + INITIAL_EPC]
  
  # # determine additional retrofitting costs to reach 69 to 81 (Band C to Band D)
  # separated_improvements <- separated_improvements[EPC_sum >= 69] #keep only start at EPC C
  
  # Dummy to track Improvments until EPC cap reached
  separated_improvements[, ID_d := ifelse(EPC_sum >= EPC_cap, 2, 1)] #2 if reached and 1 if not 
  first_reach <- separated_improvements[EPC_sum >= EPC_cap, .SD[1], by = UPRN]
  separated_improvements[first_reach, ID_d := 1, on = .(UPRN, row_index)] # value of 1 for first observation where cap reached (improvement needed)
  
  #Filter rows where ID_d is 1 (all improvement necessary)
  filtered_improvements <- separated_improvements[ID_d == 1] 
  
  # Cummulative Costs of achieving EPC cap 
  filtered_improvements[, IMPACT_CO2 := as.numeric(IMPACT_CO2)]
  filtered_improvements[, IMPACT_ENV := as.numeric(IMPACT_ENV)]
  filtered_improvements[, RETROFIT_COST := as.numeric(RETROFIT_COST)]
  filtered_improvements[, IMPACT_CONS := as.numeric(IMPACT_CONS)]
  sum_impact_co2 <- filtered_improvements[,sum(IMPACT_CO2)] #CO2
  sum_impact_env <- filtered_improvements[,sum(IMPACT_ENV)] #ENv
  sum_impact_cons <- filtered_improvements[,sum(IMPACT_CONS)] #Consumption
  sum_impact_costs <- filtered_improvements[,sum(RETROFIT_COST)] #Costs
  
  # Sum up costs per UPRNs 
  costs_per_UPRN <- filtered_improvements[, .(BY_EPC = sum(RETROFIT_COST)), by = UPRN]
  average_cost <- round(costs_per_UPRN[, mean(BY_EPC)]) # Average costs per UPRN 
    
  # Average number of improvements needed (nearest integer)
  average_improvements <- round(first_reach[, mean(row_index)])
  
  #Save Stats 
  retro_fit <- cbind("ordered by EPC", EPC_cap,  average_improvements, average_cost, sum_impact_co2, sum_impact_env,sum_impact_cons, sum_impact_costs)
  print(retro_fit)
  
  setorder(costs_per_UPRN, count)
  
  fwrite(costs_per_UPRN,paste0("output/tables/costs_cap_",EPC_cap,".csv")) # Output file
  
  # Save the table to a LaTeX file
  latex_table <- costs_per_UPRN %>%
    kbl(format = "latex", booktabs = TRUE, 
        caption = paste("Total Improvements to Reach EPC Cap", EPC_cap, ", ordered by EPC recommendations"),
        row.names = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
  
  writeLines(latex_table, paste0("output/tables/Improvements_Cap", EPC_cap, "_byrec.tex"))
  
  separated_improvements[, cumulative_sum := NULL]
  separated_improvements[, EPC_sum := NULL]
  separated_improvements[, ID_d := NULL]
  rm(counts)
  rm(first_reach)
  rm(filtered_improvements)
  rm(costs_total)
  
  ## 2. Nuances IDs Selection (order of greatest EPC Impact) ----------------------
  
  # Sort the data.table by IMPACT_EPC in descending order
  separated_improvements <- separated_improvements[order(UPRN, -IMPACT_EPC),]
  
  # Create a "count" for each improvement in order of recommendation 
  separated_improvements[, row_index := sequence(.N), by = UPRN]
  gc()
  
  # Sum the raise in EPC for each additional improvement 
  separated_improvements[, cumulative_sum := cumsum(IMPACT_EPC), by = UPRN]
  
  # Create new column with EPC at each improvement 
  separated_improvements[, EPC_sum := cumulative_sum + INITIAL_EPC]
  
  # Dummy to track Improvments until EPC cap reached
  separated_improvements[, ID_d := ifelse(EPC_sum >= EPC_cap, 2, 1)] #2 if reached and 1 if not 
  first_reach <- separated_improvements[EPC_sum >= EPC_cap, .SD[1], by = UPRN]
  separated_improvements[first_reach, ID_d := 1, on = .(UPRN, row_index)] # value of 1 for first observation where cap reached (improvement needed)
  
  #Filter rows where ID_d is 1 (all improvement necessary)
  filtered_improvements <- separated_improvements[ID_d == 1] 
  
  # Cummulative Costs of achieving EPC cap 
  filtered_improvements[, IMPACT_CO2 := as.numeric(IMPACT_CO2)]
  filtered_improvements[, IMPACT_ENV := as.numeric(IMPACT_ENV)]
  filtered_improvements[, RETROFIT_COST := as.numeric(RETROFIT_COST)]
  filtered_improvements[, IMPACT_CONS := as.numeric(IMPACT_CONS)]
  sum_impact_co2 <- filtered_improvements[,sum(IMPACT_CO2)] #CO2
  sum_impact_env <- filtered_improvements[,sum(IMPACT_ENV)] #ENv
  sum_impact_cons <- filtered_improvements[,sum(IMPACT_CONS)] #Consumption
  sum_impact_costs <- filtered_improvements[,sum(RETROFIT_COST)] #Costs
  
  # Sum up costs per UPRNs 
  impact_per_UPRN <- filtered_improvements[, .(BY_IMPACT = sum(RETROFIT_COST)), by = UPRN]
  costs_per_UPRN <- merge(costs_per_UPRN, impact_per_UPRN, by = "UPRN", all.x = TRUE)
  average_cost <- round(costs_per_UPRN[, mean(BY_IMPACT)]) # Average costs per UPRN 
  
  # Average number of improvements needed (nearest integer)
  average_improvements <- round(first_reach[, mean(row_index)])
  
  # Average number of improvements needed (nearest integer)
  average_improvements <- round(first_reach[, mean(row_index)])
  
  #Save Stats 
  retro_fit_impact <- cbind("Ordered by Impact", EPC_cap,  average_improvements, average_cost, sum_impact_co2, sum_impact_env,sum_impact_cons, sum_impact_costs)
  print(retro_fit_impact)
  
  # Identify & count improvements needed 
  counts <- filtered_improvements[, .(count = .N), by = IMPROVEMENT_ID]
  costs_total <- merge(x = counts, y = improvements_IDs, by = "IMPROVEMENT_ID", all.x = TRUE)
  costs_total[, TOTAL_COSTS := as.numeric(count)*as.numeric(RETROFIT_COST)]
  
  fwrite(costs_total,paste0("output/tables/costs_cap_",EPC_cap,"byimpact.csv")) # Output file
  
  # Save the table to a LaTeX file
  latex_table <- costs_total %>%
    kbl(format = "latex", booktabs = TRUE, 
        caption = paste("Total Improvements to Reach EPC Cap", EPC_cap, ", ordered by highest Impact"),
        row.names = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
  
  writeLines(latex_table, paste0("output/tables/Improvements_Cap", EPC_cap, "_byIMPACT.tex"))
  
  separated_improvements[, cumulative_sum := NULL]
  separated_improvements[, EPC_sum := NULL]
  separated_improvements[, ID_d := NULL]
  rm(counts)
  rm(first_reach)
  rm(filtered_improvements)
  rm(costs_total)
  gc()
  
  
  ## 3. Nuances IDs Selection (order of lowest price) ----------------------
  
  # Sort the data.table by COST RETROFIR in ascending order
  separated_improvements <- separated_improvements[order(UPRN, RETROFIT_COST),]
  
  # Create a "count" for each improvement in order of recommendation 
  separated_improvements[, row_index := sequence(.N), by = UPRN]
  
  # Sum the raise in EPC for each additional improvement 
  separated_improvements[, cumulative_sum := cumsum(IMPACT_EPC), by = UPRN]
  
  # Create new column with EPC at each improvement 
  separated_improvements[, EPC_sum := cumulative_sum + INITIAL_EPC]
  
  # Dummy to track Improvements until EPC cap reached
  separated_improvements[, ID_d := ifelse(EPC_sum >= EPC_cap, 2, 1)] #2 if reached and 1 if not 
  first_reach <- separated_improvements[EPC_sum >= EPC_cap, .SD[1], by = UPRN]
  separated_improvements[first_reach, ID_d := 1, on = .(UPRN, row_index)] # value of 1 for first observation where cap reached (improvement needed)
  
  #Filter rows where ID_d is 1 (all improvement necessary)
  filtered_improvements <- separated_improvements[ID_d == 1] 
  
  # Cummulative Costs of achieving EPC cap 
  filtered_improvements[, IMPACT_CO2 := as.numeric(IMPACT_CO2)]
  filtered_improvements[, IMPACT_ENV := as.numeric(IMPACT_ENV)]
  filtered_improvements[, RETROFIT_COST := as.numeric(RETROFIT_COST)]
  filtered_improvements[, IMPACT_CONS := as.numeric(IMPACT_CONS)]
  sum_impact_co2 <- filtered_improvements[,sum(IMPACT_CO2)] #CO2
  sum_impact_env <- filtered_improvements[,sum(IMPACT_ENV)] #ENv
  sum_impact_cons <- filtered_improvements[,sum(IMPACT_CONS)] #Consumption
  sum_impact_costs <- filtered_improvements[,sum(RETROFIT_COST)] #Costs
  
  # Sum up costs per UPRNs 
  impact_per_UPRN <- filtered_improvements[, .(BY_PRICE = sum(RETROFIT_COST)), by = UPRN]
  costs_per_UPRN <- merge(costs_per_UPRN, impact_per_UPRN, by = "UPRN", all.x = TRUE)
  average_cost <- round(costs_per_UPRN[, mean(BY_PRICE)]) # Average costs per UPRN 
  
  # Average number of improvements needed (nearest integer)
  average_improvements <- round(first_reach[, mean(row_index)])
  
  #Save Stats 
  retro_fit_price <- cbind("Ordered by Price", EPC_cap,  average_improvements, average_cost, sum_impact_co2, sum_impact_env,sum_impact_cons, sum_impact_costs)
  print(retro_fit_price)
  
  # Identify & count improvements needed 
  counts <- filtered_improvements[, .(count = .N), by = IMPROVEMENT_ID]
  costs_total <- merge(x = counts, y = improvements_IDs, by = "IMPROVEMENT_ID", all.x = TRUE)
  costs_total[, TOTAL_COSTS := as.numeric(count)*as.numeric(RETROFIT_COST)]
  
  fwrite(costs_total,paste0("output/tables/costs_cap_",EPC_cap,"byprice.csv")) # Output file
  
  # Save the table to a LaTeX file
  latex_table <- costs_total %>%
    kbl(format = "latex", booktabs = TRUE, 
        caption = paste("Total Improvements to Reach EPC Cap", EPC_cap, ", ordered by Price"),
        row.names = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
  
  writeLines(latex_table, paste0("output/tables/Improvements_Cap", EPC_cap, "_byPRICE.tex"))
  
  separated_improvements[, cumulative_sum := NULL]
  separated_improvements[, EPC_sum := NULL]
  separated_improvements[, ID_d := NULL]
  rm(counts)
  rm(first_reach)
  rm(filtered_improvements)
  rm(costs_total)
  gc()
  
  
  
  ## 4. Nuances IDs Selection (order of yield - EPC/Price) ----------------------
  
  # Calculate Yield (EPC)
  separated_improvements[, yield := IMPACT_EPC/RETROFIT_COST]
  
  # Sort the data.table by yield in descending order
  separated_improvements <- separated_improvements[order(UPRN, -yield),]
  
  # Create a "count" for each improvement in order of recommendation 
  separated_improvements[, row_index := sequence(.N), by = UPRN]
  
  # Sum the raise in EPC for each additional improvement 
  separated_improvements[, cumulative_sum := cumsum(IMPACT_EPC), by = UPRN]
  
  # Create new column with EPC at each improvement 
  separated_improvements[, EPC_sum := cumulative_sum + INITIAL_EPC]
  
  # Remove rows where resulting EPC > 100% (not necessary or possible) 
  separated_improvements <- separated_improvements[EPC_sum <= 100]
  
  # Dummy to track Improvements until EPC cap reached
  separated_improvements[, ID_d := ifelse(EPC_sum >= EPC_cap, 2, 1)] #2 if reached and 1 if not 
  first_reach <- separated_improvements[EPC_sum >= EPC_cap, .SD[1], by = UPRN]
  separated_improvements[first_reach, ID_d := 1, on = .(UPRN, row_index)] # value of 1 for first observation where cap reached (improvement needed)
  
  #Filter rows where ID_d is 1 (all improvement necessary)
  filtered_improvements <- separated_improvements[ID_d == 1] 
  
  # Costs of achieving EPC cap 
  filtered_improvements[, IMPACT_CO2 := as.numeric(IMPACT_CO2)]
  filtered_improvements[, IMPACT_ENV := as.numeric(IMPACT_ENV)]
  filtered_improvements[, RETROFIT_COST := as.numeric(RETROFIT_COST)]
  filtered_improvements[, IMPACT_CONS := as.numeric(IMPACT_CONS)]
  sum_impact_co2 <- filtered_improvements[,sum(IMPACT_CO2)] #CO2
  sum_impact_env <- filtered_improvements[,sum(IMPACT_ENV)] #Env. 
  sum_impact_cons <- filtered_improvements[,sum(IMPACT_CONS)] #Consumption
  sum_impact_costs <- filtered_improvements[,sum(RETROFIT_COST)] #Costs
  
  # Sum up costs per UPRNs 
  impact_per_UPRN <- filtered_improvements[, .(BY_YIELD = sum(RETROFIT_COST)), by = UPRN]
  costs_per_UPRN <- merge(costs_per_UPRN, impact_per_UPRN, by = "UPRN", all.x = TRUE)
  average_cost <- round(costs_per_UPRN[, mean(BY_YIELD)]) # Average costs per UPRN 
  
  # Average number of improvements needed (nearest integer)
  average_improvements <- round(first_reach[, mean(row_index)])
  
  #Save Stats 
  retro_fit_yield <- cbind("Ordered by Yield", EPC_cap,  average_improvements, average_cost, sum_impact_co2, sum_impact_env,sum_impact_cons, sum_impact_costs)
  print(retro_fit_yield)
  
  # Identify & count improvements needed 
  counts <- filtered_improvements[, .(count = .N), by = IMPROVEMENT_ID]
  costs_total <- merge(x = counts, y = improvements_IDs, by = "IMPROVEMENT_ID", all.x = TRUE)
  costs_total[, TOTAL_COSTS := as.numeric(count)*as.numeric(RETROFIT_COST)]
  
  fwrite(costs_total,paste0("output/tables/costs_cap_",EPC_cap,"byyield.csv")) # Output file
  
  # Save the table to a LaTeX file
  latex_table <- costs_total %>%
    kbl(format = "latex", booktabs = TRUE, 
        caption = paste("Total Improvements to Reach EPC Cap", EPC_cap, ", ordered by Yield"),
        row.names = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
  
  writeLines(latex_table, paste0("output/tables/Improvements_Cap", EPC_cap, "_byYIELD.tex"))
  
  separated_improvements[, cumulative_sum := NULL]
  separated_improvements[, EPC_sum := NULL]
  separated_improvements[, ID_d := NULL]
  rm(counts)
  rm(first_reach)
  rm(filtered_improvements)
  rm(costs_total)
  separated_improvements[, yield := NULL]
  gc()
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  #save costs_uprn with POSTCODE (important for descriptive stats later)
  connect <- master_epc[, .(UPRN, POSTCODE_EPC)]
  connect <- master_epc[, .(UPRN, POSTCODE_EPC)]
  connect[, pcu_area := toupper(gsub("[^A-Za-z]", "", POSTCODE_EPC))]
  connect[, pcu_area := substr(pcu_area, 1, 2)]
  
  costs_per_UPRN <- merge(x = costs_per_UPRN , y = connect, by = "UPRN", all.x = TRUE)
  fwrite(costs_per_UPRN, "data/cleaned/costs_per_uprn.csv")
  
# ESTIMATION FOR EPC_CAP = POTENTIAL ENERGY EFFICIENCY (UNABLE TO REACH)-------------
  rm(separated_improvements)
  
  unable_reach[, EPC_cap := POTENTIAL_ENERGY_EFFICIENCY] #Set an EPC cap/threshold to cross
  
  # Create new frame from IMPROVEMENT_IDs
  separated_improvements <- unable_reach[, .(CURRENT_ENERGY_EFFICIENCY, IMPROVEMENT_ID = unlist(strsplit(IMPROVEMENTS_IDs, ","))), by = UPRN]
  separated_improvements$IMPROVEMENT_ID <- as.numeric(separated_improvements$IMPROVEMENT_ID)
  setDT(separated_improvements)
  
  # Take care of IMPROVEMENT ID duplicate recommendations 
  separated_improvements[, IMPROVEMENT_ID := sapply(IMPROVEMENT_ID, replace_improvement_id)]
  separated_improvements <- unique(separated_improvements) #remove created duplicates 
  
  # Create a "count" for each improvement in order of recommendation 
  separated_improvements[, row_index := sequence(.N), by = UPRN]
  
  # Left join on improvement Ids (add impacts to each improvement IDs) and order by UPRN
  separated_improvements <- merge(x = separated_improvements, y = improvements_IDs, by = "IMPROVEMENT_ID", all.x = TRUE, allow.cartesian = FALSE)
  separated_improvements <-separated_improvements[order(UPRN,row_index),]
  setnames(separated_improvements, "CURRENT_ENERGY_EFFICIENCY", "INITIAL_EPC")
  
  # Left join the reach_cap for each UPRN
  epc_caps <- unable_reach[, .(UPRN, EPC_cap)]
  separated_improvements <- merge(x = separated_improvements, y = epc_caps, by = "UPRN", all.x = TRUE, allow.cartesian = FALSE)
  separated_improvements <-separated_improvements[order(UPRN,row_index),]
  
  ## 1. Improvement IDs in order of recommendation (EPC data) -----------------------
  
  # Sum the raise in EPC for each additional improvement 
  separated_improvements[, cumulative_sum := cumsum(IMPACT_EPC), by = UPRN]
  
  # Create new column with EPC at each improvement 
  separated_improvements[, EPC_sum := cumulative_sum + INITIAL_EPC]
  
  # Create new column with sum costs of retrofit
  separated_improvements[, sum_retrofit := cumsum(RETROFIT_COST), by = UPRN]
  
  # Dummy to track Improvments until EPC cap reached or maximum spending (both £3,500 and £10,000)
  separated_improvements[, ID_d := ifelse(EPC_sum >= EPC_cap, 2, 1)] #2 if reached and 1 if not 
  first_reach <- separated_improvements[EPC_sum >= EPC_cap, .SD[1], by = UPRN]
  separated_improvements[first_reach, ID_d := 1, on = .(UPRN, row_index)] # value of 1 for first observation where cap reached (improvement needed)
  #separated_improvements[, ID_d := ifelse(sum_retrofit > max_spending, 2, 1)]
  
  #Filter rows where ID_d is 1 (all improvement necessary)
  filtered_improvements <- separated_improvements[ID_d == 1] 
  
  # Cummulative Costs of achieving EPC cap 
  filtered_improvements[, IMPACT_CO2 := as.numeric(IMPACT_CO2)]
  filtered_improvements[, IMPACT_ENV := as.numeric(IMPACT_ENV)]
  filtered_improvements[, RETROFIT_COST := as.numeric(RETROFIT_COST)]
  filtered_improvements[, IMPACT_CONS := as.numeric(IMPACT_CONS)]
  sum_impact_co2 <- filtered_improvements[,sum(IMPACT_CO2)] #CO2
  sum_impact_env <- filtered_improvements[,sum(IMPACT_ENV)] #ENv
  sum_impact_cons <- filtered_improvements[,sum(IMPACT_CONS)] #Consumption
  sum_impact_costs <- filtered_improvements[,sum(RETROFIT_COST)] #Costs
  
  # Sum up costs per UPRNs 
  costs_per_UPRN <- filtered_improvements[, .(BY_EPC = sum(RETROFIT_COST)), by = UPRN]
  average_cost <- round(costs_per_UPRN[, mean(BY_EPC)]) # Average costs per UPRN 
  
  # Average number of improvements needed (nearest integer)
  average_improvements <- round(first_reach[, mean(row_index)])
  
  #average EPC reached 
  last_improvement <- filtered_improvements[ID_d == 1, .SD[.N], by = UPRN] # Identify the last improvement per UPRN where ID_d == 1
  EPC_avg <- round(last_improvement[, mean(EPC_sum, na.rm = TRUE)]) # Rounded to 2 decimal places
    
  #Save Stats 
  retro_fit <- cbind("ordered by EPC", EPC_avg,  average_improvements, average_cost, sum_impact_co2, sum_impact_env,sum_impact_cons, sum_impact_costs)
  print(retro_fit)
  
  # Identify & count improvements needed 
  counts <- filtered_improvements[, .(count = .N), by = IMPROVEMENT_ID]
  costs_total <- merge(x = counts, y = improvements_IDs, by = "IMPROVEMENT_ID", all.x = TRUE)
  costs_total[, TOTAL_COSTS := as.numeric(count)*as.numeric(RETROFIT_COST)]
  total_costs <- costs_total[,sum(TOTAL_COSTS)]
  
  fwrite(costs_total,paste0("output/tables/UNABLETOREACH_costs_cap.csv")) # Output file
  
  separated_improvements[, cumulative_sum := NULL]
  separated_improvements[, EPC_sum := NULL]
  separated_improvements[, ID_d := NULL]
  rm(counts)
  rm(first_reach)
  rm(filtered_improvements)
  rm(costs_total)
  
  ## 2. Nuances IDs Selection (order of greatest EPC Impact) ----------------------
  
  # Sort the data.table by IMPACT_EPC in descending order
  separated_improvements <- separated_improvements[order(UPRN, -IMPACT_EPC),]
  
  # Create a "count" for each improvement in order of recommendation 
  separated_improvements[, row_index := sequence(.N), by = UPRN]
  gc()
  
  # Sum the raise in EPC for each additional improvement 
  separated_improvements[, cumulative_sum := cumsum(IMPACT_EPC), by = UPRN]
  
  # Create new column with EPC at each improvement 
  separated_improvements[, EPC_sum := cumulative_sum + INITIAL_EPC]
  
  # Dummy to track Improvments until EPC cap reached
  separated_improvements[, ID_d := ifelse(EPC_sum >= EPC_cap, 2, 1)] #2 if reached and 1 if not 
  first_reach <- separated_improvements[EPC_sum >= EPC_cap, .SD[1], by = UPRN]
  separated_improvements[first_reach, ID_d := 1, on = .(UPRN, row_index)] # value of 1 for first observation where cap reached (improvement needed)
  #separated_improvements[, ID_d := ifelse(sum_retrofit > max_spending, 2, 1)]
  
  #Filter rows where ID_d is 1 (all improvement necessary)
  filtered_improvements <- separated_improvements[ID_d == 1] 
  
  # Cummulative Costs of achieving EPC cap 
  filtered_improvements[, IMPACT_CO2 := as.numeric(IMPACT_CO2)]
  filtered_improvements[, IMPACT_ENV := as.numeric(IMPACT_ENV)]
  filtered_improvements[, RETROFIT_COST := as.numeric(RETROFIT_COST)]
  filtered_improvements[, IMPACT_CONS := as.numeric(IMPACT_CONS)]
  sum_impact_co2 <- filtered_improvements[,sum(IMPACT_CO2)] #CO2
  sum_impact_env <- filtered_improvements[,sum(IMPACT_ENV)] #ENv
  sum_impact_cons <- filtered_improvements[,sum(IMPACT_CONS)] #Consumption
  sum_impact_costs <- filtered_improvements[,sum(RETROFIT_COST)] #Costs
  
  # Sum up costs per UPRNs 
  impact_per_UPRN <- filtered_improvements[, .(BY_IMPACT = sum(RETROFIT_COST)), by = UPRN]
  costs_per_UPRN <- merge(costs_per_UPRN, impact_per_UPRN, by = "UPRN", all.x = TRUE)
  average_cost <- round(costs_per_UPRN[, mean(BY_IMPACT)]) # Average costs per UPRN 
  
  # Average number of improvements needed (nearest integer)
  average_improvements <- round(first_reach[, mean(row_index)])
  
  #average EPC reached 
  last_improvement <- filtered_improvements[ID_d == 1, .SD[.N], by = UPRN] # Identify the last improvement per UPRN where ID_d == 1
  EPC_avg <- round(last_improvement[, mean(EPC_sum, na.rm = TRUE)]) 
  
  #Save Stats 
  retro_fit_impact <- cbind("Ordered by Impact", EPC_avg,  average_improvements, average_cost, sum_impact_co2, sum_impact_env,sum_impact_cons, sum_impact_costs)
  print(retro_fit_impact)
  
  # Identify & count improvements needed 
  counts <- filtered_improvements[, .(count = .N), by = IMPROVEMENT_ID]
  costs_total <- merge(x = counts, y = improvements_IDs, by = "IMPROVEMENT_ID", all.x = TRUE)
  costs_total[, TOTAL_COSTS := as.numeric(count)*as.numeric(RETROFIT_COST)]
  
  fwrite(costs_total,paste0("output/tables/UNABLETOREACH_byimpact_cap.csv")) # Output file
  
  separated_improvements[, cumulative_sum := NULL]
  separated_improvements[, EPC_sum := NULL]
  separated_improvements[, ID_d := NULL]
  rm(counts)
  rm(first_reach)
  rm(filtered_improvements)
  rm(costs_total)
  gc()
  
  
  ## 3. Nuances IDs Selection (order of lowest price) ----------------------
  
  # Sort the data.table by COST RETROFIR in ascending order
  separated_improvements <- separated_improvements[order(UPRN, RETROFIT_COST),]
  
  # Create a "count" for each improvement in order of recommendation 
  separated_improvements[, row_index := sequence(.N), by = UPRN]
  
  # Sum the raise in EPC for each additional improvement 
  separated_improvements[, cumulative_sum := cumsum(IMPACT_EPC), by = UPRN]
  
  # Create new column with EPC at each improvement 
  separated_improvements[, EPC_sum := cumulative_sum + INITIAL_EPC]
  
  # Dummy to track Improvements until EPC cap reached
  separated_improvements[, ID_d := ifelse(EPC_sum >= EPC_cap, 2, 1)] #2 if reached and 1 if not 
  first_reach <- separated_improvements[EPC_sum >= EPC_cap, .SD[1], by = UPRN]
  separated_improvements[first_reach, ID_d := 1, on = .(UPRN, row_index)] # value of 1 for first observation where cap reached (improvement needed)
  #separated_improvements[, ID_d := ifelse(sum_retrofit > max_spending, 2, 1)]
  
  #Filter rows where ID_d is 1 (all improvement necessary)
  filtered_improvements <- separated_improvements[ID_d == 1] 
  
  # Cummulative Costs of achieving EPC cap 
  filtered_improvements[, IMPACT_CO2 := as.numeric(IMPACT_CO2)]
  filtered_improvements[, IMPACT_ENV := as.numeric(IMPACT_ENV)]
  filtered_improvements[, RETROFIT_COST := as.numeric(RETROFIT_COST)]
  filtered_improvements[, IMPACT_CONS := as.numeric(IMPACT_CONS)]
  sum_impact_co2 <- filtered_improvements[,sum(IMPACT_CO2)] #CO2
  sum_impact_env <- filtered_improvements[,sum(IMPACT_ENV)] #ENv
  sum_impact_cons <- filtered_improvements[,sum(IMPACT_CONS)] #Consumption
  sum_impact_costs <- filtered_improvements[,sum(RETROFIT_COST)] #Costs
  
  # Sum up costs per UPRNs 
  impact_per_UPRN <- filtered_improvements[, .(BY_PRICE = sum(RETROFIT_COST)), by = UPRN]
  costs_per_UPRN <- merge(costs_per_UPRN, impact_per_UPRN, by = "UPRN", all.x = TRUE)
  average_cost <- round(costs_per_UPRN[, mean(BY_PRICE)]) # Average costs per UPRN 
  
  # Average number of improvements needed (nearest integer)
  average_improvements <- round(first_reach[, mean(row_index)])
  
  #average EPC reached 
  last_improvement <- filtered_improvements[ID_d == 1, .SD[.N], by = UPRN] # Identify the last improvement per UPRN where ID_d == 1
  EPC_avg <- round(last_improvement[, mean(EPC_sum, na.rm = TRUE)]) 
  
  #Save Stats 
  retro_fit_price <- cbind("Ordered by Price", EPC_avg,  average_improvements, average_cost, sum_impact_co2, sum_impact_env,sum_impact_cons, sum_impact_costs)
  print(retro_fit_price)
  
  # Identify & count improvements needed 
  counts <- filtered_improvements[, .(count = .N), by = IMPROVEMENT_ID]
  costs_total <- merge(x = counts, y = improvements_IDs, by = "IMPROVEMENT_ID", all.x = TRUE)
  costs_total[, TOTAL_COSTS := as.numeric(count)*as.numeric(RETROFIT_COST)]
  
  fwrite(costs_total,paste0("output/tables/UNABLETOREACHcosts_capbyprice.csv")) # Output file

  
  separated_improvements[, cumulative_sum := NULL]
  separated_improvements[, EPC_sum := NULL]
  separated_improvements[, ID_d := NULL]
  rm(counts)
  rm(first_reach)
  rm(filtered_improvements)
  rm(costs_total)
  gc()
  
  
  
  ## 4. Nuances IDs Selection (order of yield - EPC/Price) ----------------------
  
  # Calculate Yield (EPC)
  separated_improvements[, yield := IMPACT_EPC/RETROFIT_COST]
  
  # Sort the data.table by yield in descending order
  separated_improvements <- separated_improvements[order(UPRN, -yield),]
  
  # Create a "count" for each improvement in order of recommendation 
  separated_improvements[, row_index := sequence(.N), by = UPRN]
  
  # Sum the raise in EPC for each additional improvement 
  separated_improvements[, cumulative_sum := cumsum(IMPACT_EPC), by = UPRN]
  
  # Create new column with EPC at each improvement 
  separated_improvements[, EPC_sum := cumulative_sum + INITIAL_EPC]
  
  # Dummy to track Improvements until EPC cap reached
  separated_improvements[, ID_d := ifelse(EPC_sum >= EPC_cap, 2, 1)] #2 if reached and 1 if not 
  first_reach <- separated_improvements[EPC_sum >= EPC_cap, .SD[1], by = UPRN]
  separated_improvements[first_reach, ID_d := 1, on = .(UPRN, row_index)] # value of 1 for first observation where cap reached (improvement needed)
  #separated_improvements[, ID_d := ifelse(sum_retrofit > max_spending, 2, 1)]
  
  #Filter rows where ID_d is 1 (all improvement necessary)
  filtered_improvements <- separated_improvements[ID_d == 1] 
  
  # Costs of achieving EPC cap 
  filtered_improvements[, IMPACT_CO2 := as.numeric(IMPACT_CO2)]
  filtered_improvements[, IMPACT_ENV := as.numeric(IMPACT_ENV)]
  filtered_improvements[, RETROFIT_COST := as.numeric(RETROFIT_COST)]
  filtered_improvements[, IMPACT_CONS := as.numeric(IMPACT_CONS)]
  sum_impact_co2 <- filtered_improvements[,sum(IMPACT_CO2)] #CO2
  sum_impact_env <- filtered_improvements[,sum(IMPACT_ENV)] #Env. 
  sum_impact_cons <- filtered_improvements[,sum(IMPACT_CONS)] #Consumption
  sum_impact_costs <- filtered_improvements[,sum(RETROFIT_COST)] #Costs
  
  # Sum up costs per UPRNs 
  impact_per_UPRN <- filtered_improvements[, .(BY_YIELD = sum(RETROFIT_COST)), by = UPRN]
  costs_per_UPRN <- merge(costs_per_UPRN, impact_per_UPRN, by = "UPRN", all.x = TRUE)
  average_cost <- round(costs_per_UPRN[, mean(BY_YIELD)]) # Average costs per UPRN 
  
  # Average number of improvements needed (nearest integer)
  average_improvements <- round(first_reach[, mean(row_index)])
  
  #average EPC reached 
  last_improvement <- filtered_improvements[ID_d == 1, .SD[.N], by = UPRN] # Identify the last improvement per UPRN where ID_d == 1
  EPC_avg <- round(last_improvement[, mean(EPC_sum, na.rm = TRUE)]) 
  
  #Save Stats 
  retro_fit_yield <- cbind("Ordered by Yield", EPC_avg,  average_improvements, average_cost, sum_impact_co2, sum_impact_env,sum_impact_cons, sum_impact_costs)
  print(retro_fit_yield)
  
  # Identify & count improvements needed 
  counts <- filtered_improvements[, .(count = .N), by = IMPROVEMENT_ID]
  costs_total <- merge(x = counts, y = improvements_IDs, by = "IMPROVEMENT_ID", all.x = TRUE)
  costs_total[, TOTAL_COSTS := as.numeric(count)*as.numeric(RETROFIT_COST)]
  
  fwrite(costs_total,paste0("output/tables/UNABLETOREACHcosts_cap_byyield.csv")) # Output file
  
  separated_improvements[, cumulative_sum := NULL]
  separated_improvements[, EPC_sum := NULL]
  separated_improvements[, ID_d := NULL]
  rm(counts)
  rm(first_reach)
  rm(filtered_improvements)
  rm(costs_total)
  separated_improvements[, yield := NULL]
  gc()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# DESCRIPTIVE STATISTICS -------------------------------------------------------
  
  ## Descriptive Across made, achieved, and unable to reach EPC_cap -----------------------
  
  # Convert matrices to data.tables if necessary
  if (class(retro_fit)[1] == "matrix") retro_fit <- as.data.table(retro_fit)
  if (class(retro_fit_impact)[1] == "matrix") retro_fit_impact <- as.data.table(retro_fit_impact)
  if (class(retro_fit_price)[1] == "matrix") retro_fit_price <- as.data.table(retro_fit_price)
  if (class(retro_fit_yield)[1] == "matrix") retro_fit_yield <- as.data.table(retro_fit_yield)
  
  # Bind them together
  cumulative_costs_table <- rbind(retro_fit, retro_fit_impact, retro_fit_price, retro_fit_yield)
  
  
  # Convert columns to numeric where applicable
  cumulative_costs_table[, `:=`(
    EPC_cap = as.numeric(EPC_cap),
    average_improvements = as.numeric(average_improvements),
    sum_impact_co2 = as.numeric(sum_impact_co2),
    sum_impact_env = as.numeric(sum_impact_env),
    sum_impact_cons = as.numeric(sum_impact_cons),
    sum_impact_costs = round(as.numeric(sum_impact_costs))
  )]
  
  master_epc <- merge(x = master_epc, y = prices, by = c("UPRN", "INSPECTION_DATE"), all.x = TRUE)
  
  
  ## Properties that can achieve EPC cap ---------------------
  
  # Merge with master_dataset 
  master_dataset <- as.data.table(fread('data/cleaned/master_dataset_all.csv'))
  master_epc <- merge(x = costs_per_UPRN, y = prices, by = c("UPRN", "INSPECTION_DATE"), all.x = TRUE)
  
  # r
  
  
  ## Geographical Distribution of Costs ---------------------
  
  # Create base map of england 
  uk_tbl <- map_data("world", region = "UK") %>%
    as_tibble()
  
  # uk_base <- uk_tbl %>%
  #   ggplot(aes(long, lat, map_id = region)) + 
  #   geom_map(
  #     map = world_tbl,
  #     color = "white", fill = "gray30", linewidth= 0.3
  #   ) + 
  #   coord_map("ortho", orientation = c(54.5, -3.0, 0)) # Fix projection (orthogonal projection and focused on UK)
  
  # Create a data frame
  geo_data <- data.frame(
    lat = costs_per_UPRN$LATITUDE,  # Example latitudes
    long = costs_per_UPRN$LONGITUDE, # Example longitudes
    value = costs_per_UPRN$BY_EPC       # Retrofitting costs by EPC recommendations
  )
  geo_data_clean <- geo_data %>%
    filter(!is.na(value))

  # Plot with base map and overlay data
  ggplot(data = uk_tbl, aes(x = long, y = lat)) +
    geom_polygon(fill = "grey80", color = "gray0", linewidth = 0.3) +
    geom_point(data = geo_data_clean, aes(x = long, y = lat, color = value), size = 2) +
    coord_map("ortho", orientation = c(54.5, -3.0, 0)) +
    scale_fill_gradient(low = "white", high = "red") +  # Color gradient from white to red +
    theme_minimal() +
    labs(title = "UK Map with Retrofitting Costs by EPC Recommendations")

  
  
  
  
  
  
  
  ## Density Plot of Cost per UPRN -----------------
  #use EPC recommendations value 
  
  # Merge UPRNs from master_epc with indidual costs (Costs_per_UPRN) with master_dataset 
  master_dataset <- as.data.table(fread("data/cleaned/master_dataset_all.csv"))  #load master_dataset
  master_dataset[, UPRN := as.numeric(UPRN)]  
  master_dataset <- master_dataset[master_dataset[, .I[which.max(INSPECTION_DATE)], by = .(UPRN)]$V1]  #keep observations with latest inspection date
  master_dataset <- master_dataset[, .(UPRN, PROPERTY_TYPE, PRICE)] #keep only columns of interest 
  costs_per_UPRN <- merge(x = costs_per_UPRN, y = master_dataset, by = "UPRN", all.x = TRUE, allow.cartesian = FALSE)
  setorder(costs_per_UPRN, UPRN)
  
  # remove NAs
  costs_per_UPRN <- costs_per_UPRN[!is.na(PRICE)]
  
  # Create price range categories (adjust the breaks as per your dataset)
  costs_per_UPRN[, price_range := cut(PRICE, 
                                      breaks = c(0, 100000, 200000, 300000, 400000, 500000, Inf), 
                                      labels = c("0-100k", "100-200k", "200-300k", "300-400k", "400-500k", "500k+"),
                                      right = FALSE)]
  
  # Create a density plot with different shading for different price ranges
  density_price <- ggplot(costs_per_UPRN, aes(x = BY_EPC, fill = price_range)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 8945, color = "red", linetype = "dashed", size = 0.5) +
    annotate("text", x = 8945, y = 0.00014, label = "Average retrofit costs", vjust = -0.5, angle = 90, color = "red", size = 4) +
    labs(
      x = "Retrofitting Cost (£)",
      y = "Density",
      fill = "Price Range"
    ) +
    theme_minimal()
  
  # Save the density plot to a file
  ggsave("output/plots/Density_Average_Retrofitting_Costs.png", density_price, width = 8, height = 6)
  
  # Create a density plot with different shading for different property types
ggplot(costs_per_UPRN, aes(x = BY_EPC, fill = PROPERTY_TYPE)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Average Retrofitting Costs by Property Type",
    x = "Average Retrofitting Cost (£)",
    y = "Density",
    fill = "Property Type"
  ) +
  theme_minimal()
  ## Save file with costs per UPRN (EPC) and energy efficiency ------
  
  #select only columns of interest 
  costs_per_UPRN <- costs_per_UPRN[, .(UPRN, BY_EPC)]

  #merge with EPC information 
  f_data <- master_epc[, .(UPRN, CURRENT_ENERGY_EFFICIENCY, POTENTIAL_ENERGY_RATING, DIFF_ENERGY_EFFICIENCY, RISE_IN_EPC)]
  costs_per_UPRN <- merge(x = costs_per_UPRN, y = f_data, by = "UPRN", all.x = TRUE, allow.cartesian = FALSE) 
  
  # save as new file (later for CBA)
  fwrite(costs_per_UPRN, "data/cleaned/costs_uprn_epc.csv") #Output File
  

  
# SAVE RESULTS ------------------------------------
 
  # CUMULATIVE COSTS --------------------------------------------------------- 
  # Convert matrices to data.tables if necessary
  if (class(retro_fit)[1] == "matrix") retro_fit <- as.data.table(retro_fit)
  #if (class(retro_fit)[1] == "matrix") retro_fit_linear <- as.data.table(retro_fit_linear)
  if (class(retro_fit_impact)[1] == "matrix") retro_fit_impact <- as.data.table(retro_fit_impact)
  if (class(retro_fit_price)[1] == "matrix") retro_fit_price <- as.data.table(retro_fit_price)
  if (class(retro_fit_yield)[1] == "matrix") retro_fit_yield <- as.data.table(retro_fit_yield)
  
  # Bind them together
  cumulative_costs_table <- rbind(retro_fit, retro_fit_impact, retro_fit_price, retro_fit_yield)
  
  
  # Convert columns to numeric where applicable
  cumulative_costs_table[, `:=`(
    EPC_cap = as.numeric(EPC_cap),
    average_improvements = as.numeric(average_improvements),
    sum_impact_co2 = as.numeric(sum_impact_co2),
    sum_impact_env = as.numeric(sum_impact_env),
    sum_impact_cons = as.numeric(sum_impact_cons),
    sum_impact_costs = round(as.numeric(sum_impact_costs))
  )]
  
  # Order by total costs (ascending)
  cumulative_costs_table <- cumulative_costs_table[order(sum_impact_costs)]
  
  fwrite(cumulative_costs_table,paste0("output/tables/UNABLETOREACH_cumulative_costs_", max_spending,".csv")) # Output file
  
  # Save the table to a LaTeX file
  latex_table <- cumulative_costs_table %>%
    kbl(format = "latex", booktabs = TRUE, 
        caption = paste("Cumulative Costs for EPC Cap", EPC_cap),
        row.names = FALSE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
  
  writeLines(latex_table, paste0("output/tables/EPC", EPC_cap, "_results.tex"))
  
  

# Descriptive Statistics (Between able and unable to reach) -----------------
  
  # Already Achieved
  summary_already_achieved <- already_acheived[, .(
    `Number of Houses` = uniqueN(UPRN),
    `Average EPC` = round(mean(CURRENT_ENERGY_EFFICIENCY, na.rm = TRUE)),
    `Average Size` = mean(TOTAL_FLOOR_AREA, na.rm = TRUE)
  )]
  summary_already_achieved[, Category := "Already Achieved"]
  
  # Unable to Reach
  summary_unable_to_reach <- unable_reach[, .(
    `Number of Houses` = uniqueN(UPRN),
    `Average EPC` = round(mean(CURRENT_ENERGY_EFFICIENCY, na.rm = TRUE)),
    `Average Size` = round(mean(TOTAL_FLOOR_AREA, na.rm = TRUE))
  )]
  summary_unable_to_reach[, Category := "Unable to Reach"]
  
  # Master EPC
  summary_master_epc <- master_epc[, .(
    `Number of Houses` = uniqueN(UPRN),
    `Average EPC` = round(mean(CURRENT_ENERGY_EFFICIENCY, na.rm = TRUE)),
    `Average Size` = mean(TOTAL_FLOOR_AREA, na.rm = TRUE)
  )]
  summary_master_epc[, Category := "Master EPC"]
  
  #Calculate the percentage distribution of PROPERTY_TYPE_EPC within each category
  
  # Already Achieved
  total_observations <- already_acheived[, .N]
  percentage_already_achieved <- already_acheived[, .N, by = PROPERTY_TYPE_EPC][
    , Percentage := round((N / total_observations) * 100, 2)]
  
  # Unable to Reach
  total_observations <- unable_reach[, .N]
  percentage_unable_to_reach <- unable_reach[, .N, by = PROPERTY_TYPE_EPC][
    , Percentage := round((N / total_observations) * 100, 2)]
  
  # Master EPC
  total_observations <- master_epc[, .N]
  percentage_master_epc <- master_epc[, .N, by = PROPERTY_TYPE_EPC][
    , Percentage := round((N / total_observations) * 100, 2)]
  
  # Combine summaries and densities into a single data table
  final_summary <- data.table(
    Statistic = c("Number of Houses", "Average EPC", "Average Retrofit Cost", paste("Percentage", percentage_already_achieved$PROPERTY_TYPE_EPC)),
    Already_Achieved = c(summary_already_achieved$`Number of Houses`, 
                         summary_already_achieved$`Average EPC`, 
                         summary_already_achieved$`Average Retrofit Cost`,
                         percentage_already_achieved$Percentage),
    Unable_to_Reach = c(summary_unable_to_reach$`Number of Houses`, 
                        summary_unable_to_reach$`Average EPC`, 
                        summary_unable_to_reach$`Average Retrofit Cost`,
                        percentage_unable_to_reach$Percentage),
    Master_EPC = c(summary_master_epc$`Number of Houses`, 
                   summary_master_epc$`Average EPC`, 
                   summary_master_epc$`Average Retrofit Cost`,
                   percentage_master_epc$Percentage)
  )
  
  
  # Output as LaTeX table
  latex_table <-kable(final_summary, format = "latex", booktabs = TRUE, 
                      col.names = c("Statistic", "Already Achieved", "Unable to Reach", "Master EPC"))
  writeLines(latex_table, paste0("output/tables/descriptive_retrofits.tex"))
  
# -------------------------- REGRESSION ------------------------------------
# Costs per UPRN - want to determine what factors influence the price to reach EPC cap 
# per property and influence of certain characteristics. 
  
  # Define the formula for the regression model
  formula <- as.formula("BY_EPC ~ CURRENT_ENERGY_EFFICIENCY + PRICE")
  
  # Fit the linear model
  model1 <- lm(formula, data = costs_per_UPRN)
  model1_summary <- tidy(model)   # Tidy the model output
 
  # Apply the function to the p.value column
  model1_summary$p_value_formatted <- sapply(model1_summary$p.value, format_p_value)
  
  # Prepare data for kable
  model1_summary_latex <- model1_summary %>%
    select(term, estimate, std.error, statistic, p_value_formatted) %>%
    rename(`Term` = term,
           `Estimate` = Estimate,
           `Standard Error` = Standard Error,
           `t Value` = t Value,
           `p Value` = p Value)
  
  # Create the LaTeX table
  latex_table <- model1_summary %>%
    kbl(format = "latex", booktabs = TRUE, 
        caption = "Regression Results") %>%
    kable_styling(latex_options = c("hold_position", "repeat_header", "scale_down")) 
    add_header_above(c("Variable" = 1, "Coefficient" = 1, "Standard Error" = 1, "t Value" = 1, "p Value" = 1))
  
  # Save the LaTeX table to a file
  writeLines(latex_table, paste0("output/tables/Regression_costsbyEPC.tex"))
  
  # Calculate Variance Inflation Factors
  vif(model)
  
  # Plot residuals
  plot(model$residuals)
  