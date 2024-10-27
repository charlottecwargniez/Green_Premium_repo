
# FOLDER ARCHITECTURE ----------------------------------------------------------
 
  # -FOLDER/
  #   |- Green Premium.r
  #   |- data/
  #   |     |- epcs/      :: folder containing raw EPC data (certificates and recommendations per area)
  #   |     |- registry/  :: folder containing raw Registry data
  #   |     |- epcid_uprn_usrn 
  #   |     |- ppdid_uprn_usrn
  #   |     |- osopenuprn
  #   |     |- cleaned/
  #   |       |- epcs/      :: folder containing processed EPC data
  #   |       |- registry/  :: folder containing processed Registry data
  #   |       |- costs/     :: folder containing processed Costs data
  #   |- output/
  #         |- plots/
  #         | |- costs/      :: folder containing output plots for costs
  #         |- tables/

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

# Clear the environment
rm(list = ls())

# --------------------------- CONFIGURATION ------------------------------------

# Set flags to execute sections of code (0 no, 1 yes)
gen_ppd_epc_uprn_msk <- 0 
gen_keys <- 0
gen_cost_IDs <- 0
gen_cost_avg <- 1
gen_certificates <- 0
gen_registy <- 0
gen_master_regepcs <- 0
gen_master_data <- 0
clean_master_epc <- 0
pre_analysis <- 0
analysis <- 0
analysis_EPC <- 0
cost_of_retrofit <- 0
descriptive <- 0

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


# ------------------------ GLOBAL VARIABLES ------------------------------------

registry_files <- length(list.files("data/registry",full.names=TRUE)) # pattern="*.csv"
numCores <- detectCores()  # example: mclapply(1:10, function(x) x^2, mc.cores = numCores)
desktop_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop") # Get the desktop path

# REGRESSION PARAMETERS
controls <- "EXTENSION_COUNT + PROPERTY_TYPE + TOTAL_FLOOR_AREA + CONSTRUCTION_AGE_BAND + MAIN_FUEL + NUMBER_HABITABLE_ROOMS + ESTATE"
time_fe <- "YEAR_f + MONTH_f"
cluster_se <- c("BOROUGH") # Clustered Standard Errors: choose among TOWN, BOROUGH, COUNTY (GREATER LONDON, includes all E09)
pattern_controls <- "MAIN_FUEL|YEAR|MONTH|PROPERTY|CONSTRUCTION|ESTATE|EXTENSION|NUMBER_HABITABLE|TOTAL_FLOOR"
bandwidth <- 5 # Bandwidth Selection for Regression Discontinuity Design
CI_level <- 1.96 # Confidence Interval level

# RECOMMENDATIONS and IMPROVEMENTS
total_IDs <- 63 # total number of IMPROVEMENT_IDs


# ------------------------ Helper Functions ------------------------------------

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

# Function to sum costs based on preprocessed IDs
sum_impacts <- function(ids_split, impact_column, impacts_table) {
  # Sum costs where IMPROVEMENT_ID is in ids_split
  sapply(ids_split, function(ids) sum(impacts_table[IMPROVEMENT_ID %in% ids, as.numeric(get(impact_column))], na.rm = TRUE))
}


# -------------------------- Main Execution ------------------------------------

# GENERATE MASK ----------------------------------------------------------------
if (gen_ppd_epc_uprn_msk == 1){
  
  # MASK REGISTRY --------------------------------------------------------------
  # load mapping between registry transactions and UPRN
  ppdid_uprn <- as.data.table(fread("data/ppdid_uprn_usrn.csv"))
  ppdid_uprn[, c("parentuprn","usrn") := NULL]
  
  filenames <- list.files("data/registry",full.names = TRUE) #pattern="*.csv" # find all folders
  count <- 1 # Init
  
  for (files in filenames[count:length(filenames)]) {
    print(paste(count,files)) #display progress
    
    # load current dataset
    df <- as.data.table(fread(file = files, header = FALSE))
    df <- select(df,c("V1","V3")) # Selection equation
    
    # rename columns with actual variable names
    df <- df %>%
      rename_with(~ all_of(c("transactionid","TRANSACTION_DATE")),c("V1","V3"))
    
    # Perform a left join to update 'TRANSACTION_DATE' in ppdid_uprn with values from df
    ppdid_uprn[df, TRANSACTION_DATE := i.TRANSACTION_DATE, on = .(transactionid)]
    count <- count + 1 # increase counter
    
  } # end for loop on files
  
  # keep mask with transaction dates
  ppdid_uprn <- ppdid_uprn[!is.na(TRANSACTION_DATE)]
  write_csv(ppdid_uprn, "data/cleaned/ppdid_uprn.csv") # output file
  rm(ppdid_uprn,df) # free memory
  
  # MASK EPCs --------------------------------------------------------------
  # load mapping between registry transactions and UPRN
  epcid_uprn <- as.data.table(fread("data/epcid_uprn_usrn.csv"))
  epcid_uprn[, c("parentuprn","usrn") := NULL] # Selection equation
  setnames(epcid_uprn, "lmk_key", "LMK_KEY")
  
  # # Keep EPCs where buildings which are present in registry data?
  # epcid_uprn <- epcid_uprn[epcid_uprn$uprn%in%ppdid_uprn$uprn,]
  
  filenames <- list.files("data/epcs",full.names=TRUE) #pattern="*.csv" # find all folders
  count <- 1 # Init: start variable: from 1 to 345 files in total
  
  for (files in filenames[count:length(filenames)]) {
    print(paste(count,files)) #display progress
    
    #load current dataset
    df <- as.data.table(fread(file = paste(files,"/certificates.csv",sep=""), header = TRUE))
    df <-  df[, .(LMK_KEY, INSPECTION_DATE)] # Selection equation
    
    # Perform a left join to update 'INSPECTION_DATE' in epcid_uprn with values from df
    epcid_uprn[df, INSPECTION_DATE := i.INSPECTION_DATE, on = .(LMK_KEY)]
    count <- count + 1 # increase counter
    
  }  # end for loop on files
  
  # keep mask with inspection dates
  epcid_uprn <- epcid_uprn[!is.na(INSPECTION_DATE)]
  write_csv(epcid_uprn, "data/cleaned/epcid_uprn.csv") # output file
  rm(epcid_uprn,df) # free memory
  
  # MERGE MASKS ----------------------------------------------------------------
  epcid_uprn <- as.data.table(fread("data/cleaned/epcid_uprn.csv"))
  ppdid_uprn <- as.data.table(fread("data/cleaned/ppdid_uprn.csv"))
  
  ppdid_uprn[, TRANSACTION_DATE := as.IDate(TRANSACTION_DATE)]
  epcid_uprn[, INSPECTION_DATE := as.IDate(INSPECTION_DATE)]
  
  ppdid_uprn <- merge(x = ppdid_uprn, y = epcid_uprn, by = "uprn")
  
  # Filter EPCs post transactions
  ppdid_uprn <- ppdid_uprn[as.Date(TRANSACTION_DATE) > as.Date(INSPECTION_DATE)]
  
  # By group transaction date and unique ID, keep observations with latest inspection date
  ppdid_uprn <- ppdid_uprn[ppdid_uprn[, .I[which.max(INSPECTION_DATE)], by = .(uprn, TRANSACTION_DATE)]$V1]
  
  write_csv(ppdid_uprn, "data/cleaned/ppd_epc_uprn_msk.csv") # output file
  rm(ppdid_uprn,epcid_uprn) # free memory
  
  # Cleaning -------------------------------------------------------------------
  ppd_epc_uprn_msk <- as.data.table(fread("data/cleaned/ppd_epc_uprn_msk.csv"))
  
  ppd_epc_uprn_msk <- ppd_epc_uprn_msk |>
    mutate (
      YEAR  = as.numeric(substr(INSPECTION_DATE,1,4))
    )
  table(ppd_epc_uprn_msk$YEAR)
  
  # keep repeat transactions (i.e. houses which transacted several times)
  ppd_epc_uprn_msk <- keep_occurrences(ppd_epc_uprn_msk,ppd_epc_uprn_msk$uprn,1)
  rm(ppd_epc_uprn_msk) # free memory
}


# GEN KEYS ---------------------------------------------------------------------
if (gen_keys == 1){
  # Append all certificates in master spreadsheet
  filenames <- list.files("data/epcs",full.names=TRUE) #pattern="*.csv" # find all folders
  count <- 1 # Init
  
  # Preallocate a list to store individual data frames
  list_of_dfs <- vector("list", length(filenames) - count + 1)
  
  # Loop through filenames and read the data into the list
  for (i in seq_along(filenames[count:length(filenames)])) {
    files <- filenames[count + i - 1]
    print(paste(count + i - 1, paste(files, "/certificates.csv", sep = ""))) # display progress
    
    temp_df <- fread(file = paste(files, "/certificates.csv", sep = ""), header = TRUE)
    temp_df <- temp_df[, .(LMK_KEY, INSPECTION_DATE)] # Selection equation
    list_of_dfs[[i]] <- temp_df
    
    rm(temp_df) # free memory
  }
  
  # Combine all data frames at once
  full_df <- rbindlist(list_of_dfs, use.names = TRUE, fill = TRUE)
  rm(list_of_dfs) # free memory
  
  # Adjust count to reflect total files processed
  count <- count + length(filenames[count:length(filenames)])
  
  fwrite(full_df,"data/cleaned/epcid_lmk_dates.csv" )
  rm(full_df) # free memory
  
  
} # end if on gen_keys


# GENERATE ALL IMPROVEMENT COSTS -----------------------------------------------
if (gen_cost_IDs == 1){
  # Append all Improvements
  filenames <- list.files("data/epcs",full.names=TRUE) #pattern="*.csv" # find all folders
  count <- 1 # Init

  # Preallocate a list to store individual data frames
  list_of_recoms <- vector("list", length(filenames) - count + 1)
  
  # Loop through filenames and read the data into the list
  for (i in seq_along(filenames[count:length(filenames)])) {
    files <- filenames[count + i - 1]
    
    # Load data
    temp_recoms <- fread(file = paste(files, "/recommendations.csv", sep = ""), header = TRUE)
    print(paste(count + i - 1, paste(files, "/recommendations.csv", sep = ""))) # display progress
    
    # Select only IDs, Item, and Cost, remove unnecessary columns and lines
    temp_recoms[, c("IMPROVEMENT_SUMMARY_TEXT", "IMPROVEMENT_DESCR_TEXT") := NULL]
    temp_recoms <- temp_recoms[!(is.na(IMPROVEMENT_ID) | IMPROVEMENT_ID == ""), ] # Remove observations where IMPROVEMENT_ID is missing
    
    # Store the data frame in the list
    list_of_recoms[[i]] <- temp_recoms
    
    rm(temp_recoms) # free memory
  }
  
  # Combine all data frames at once
  full_recoms <- rbindlist(list_of_recoms, use.names = TRUE, fill = TRUE)
  rm(list_of_recoms) # free memory
  
  # Adjust count to reflect total files processed
  count <- count + length(filenames[count:length(filenames)])
  
  # Export all Improvement ID texts and save output file
  first_obs_per_ID <- full_recoms[, .SD[1], by = IMPROVEMENT_ID] # Keep only the first observation per group of IDs
  first_obs_per_ID <- first_obs_per_ID[, .(IMPROVEMENT_ID, IMPROVEMENT_ID_TEXT)] # Selection
  first_obs_per_ID <- first_obs_per_ID[order(IMPROVEMENT_ID),] # Sort
  fwrite(first_obs_per_ID,"data/cleaned/improvement_ID_text.csv") # Output file
  rm(first_obs_per_ID) # free memory
  
  # Keep essentials
  full_recoms[,c("IMPROVEMENT_ID_TEXT"):= NULL]
  fwrite(full_recoms,"data/cleaned/all_improvements.csv") # Output file
  table(full_recoms$IMPROVEMENT_ID)
  
  # Compute distribution for each Improvement
  full_recoms <- fread("data/cleaned/all_improvements.csv")
  for (id in 1:total_IDs) {
    print(paste("Improvement ID_",id,sep=""))
    
    # Select data
    recoms <- full_recoms[IMPROVEMENT_ID == id]
    
    # Cleaning
    recoms$INDICATIVE_COST <- gsub("£","",as.character(recoms$INDICATIVE_COST)) # delete pound symbol
    recoms$INDICATIVE_COST <- gsub(",","",as.character(recoms$INDICATIVE_COST)) # delete commas
    recoms$INDICATIVE_COST <- gsub(" ","",as.character(recoms$INDICATIVE_COST)) # delete spaces
    
    # Split INDICATIVE_COST column into LOW and HIGH
    recoms <- recoms %>% 
      separate(INDICATIVE_COST,sep = "-",into = c("INDICATIVE_COST_LOW","INDICATIVE_COST_HIGH"),remove = FALSE)
    
    # Replace NA generated above with LOW boundary
    recoms$INDICATIVE_COST_HIGH <- ifelse(is.na(recoms$INDICATIVE_COST_HIGH), recoms$INDICATIVE_COST_LOW, recoms$INDICATIVE_COST_HIGH)
    
    # Change to numeric and compute average
    recoms$INDICATIVE_COST_HIGH = as.numeric(as.character(recoms$INDICATIVE_COST_HIGH)) 
    recoms$INDICATIVE_COST_LOW = as.numeric(as.character(recoms$INDICATIVE_COST_LOW))
    recoms$INDICATIVE_COST_AVG = 0.5*(recoms$INDICATIVE_COST_HIGH + recoms$INDICATIVE_COST_LOW)
    
    # Drop unused variables
    recoms[, c("INDICATIVE_COST_LOW","INDICATIVE_COST_HIGH","INDICATIVE_COST") := NULL]
    
    # Output file
    fwrite(recoms,paste("data/cleaned/costs/ID_",id,".csv",sep=""))
    
    rm(recoms) # free memory
  } # end loop on total_IDs
  
  rm(full_recoms)
} # end if on gen_cost_IDs


# GENERATE COST AVERAGES -------------------------------------------------------
if (gen_cost_avg == 1 ) {
  
  # Load inspection dates
  #epcid_lmk_dates <- as.data.table(fread("data/cleaned/epcid_lmk_dates.csv"))
  #epcid_lmk_dates <- as.data.table(fread("data/cleaned/epcid_uprn.csv"))
  #epcid_lmk_dates[, c("uprn") := NULL] # drop uprn
  
  # Load improvement list
  improvements <- as.data.table(fread("data/cleaned/improvement_ID_text.csv"))
  
  count <- 1 # Init
  plot <- 0 # Export graphs
  
  for (id in count:total_IDs) {
    print(paste("Improvement ID_",id,sep=""))
    
    # Load data
    recoms <- as.data.table(fread(paste("data/cleaned/costs/ID_",id,".csv",sep="")))
    
    # check if the improvement was suggested
    if (nrow(recoms) > 0) {
      # Merge with INSPECTION_DATE
      recoms[epcid_lmk_dates, INSPECTION_DATE := i.INSPECTION_DATE, on = .(LMK_KEY)]
      
      # Keep year
      recoms <- recoms |>
        mutate (YEAR  = as.numeric(substr(INSPECTION_DATE,1,4)))
      
      # Calculate yearly averages and standard deviations
      yearly_stats <- recoms %>%
        group_by(YEAR) %>%
        summarize(
          Average_Cost = mean(INDICATIVE_COST_AVG, na.rm = TRUE),
          SD_Cost = sd(INDICATIVE_COST_AVG, na.rm = TRUE),
          Count = n(),  # Adding count of observations per year
          IMPROVEMENT_ID = id # Saving ID number for future merge
        )
      
      if (count == 1){ # Append datasets
        full_yearly_stats <- yearly_stats # First time
      } else {
        full_yearly_stats <- rbind(full_yearly_stats,yearly_stats) # Append
      }
      
      if (plot == 1){
        
        improv_text <- ifelse(any(improvements$IMPROVEMENT_ID == id),
                              improvements$IMPROVEMENT_ID_TEXT[improvements$IMPROVEMENT_ID == id],
                              NA)
        
        # Create box plots over time (years)
        box_plot <- ggplot(recoms, aes(x = factor(YEAR), y = INDICATIVE_COST_AVG)) +
          geom_boxplot() +
          #facet_wrap(~YEAR, scales="free_x") +  # Create separate plots for each year
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x labels for better readability
          labs(title = paste("Costs Over Time for ID_",id,": ",improv_text,sep=""),
               x = "Years",
               y = "Costs") +
          theme_minimal()  # Applies a minimal theme for aesthetics
        
        # Save the box plot
        ggsave(paste("output/plots/costs/box_ID_",id,".png",sep=""), plot = box_plot, width = 8, height = 6, dpi = 300)
        rm(box_plot) # free memory
      } # end if on plot
      
      rm(recoms,yearly_stats) # free memory
    }
    count <- count + 1
  } # end loop on ids
  
  fwrite(full_yearly_stats,"data/cleaned/improvement_ID_stats.csv") # Output file
  
  # Merge yearly stats with improvements IDs and name
  improvements <- merge(x = improvements, y = full_yearly_stats, by = "IMPROVEMENT_ID", all.x = T)
  
  ## Format descriptive statistics for output table ----------------------------
  # Pivot to longer structure
  temp_data <- full_yearly_stats %>%
    pivot_longer(
      cols = c(Average_Cost, SD_Cost, Count),
      names_to = "Category",
      values_to = "Value"
    )
  
  # Conditionally format the values
  temp_data <- temp_data %>%
    mutate(Value = custom_round(Value))
  
  # Pivot to wide format
  temp_data <- temp_data %>%
    pivot_wider(
      names_from = YEAR,
      values_from = Value,
      names_prefix = "Year_"
    )
  
  fwrite(temp_data,"output/tables/improvement_ID_stats_wide.csv") # Output file
  rm(improvements,full_yearly_stats,temp_data) # free memory
  
} # end gen_cost_avg


# GENERATE EPC DATA ------------------------------------------------------------
if (gen_certificates != 0){   
  filenames <- list.files("data/epcs",full.names=TRUE) #pattern="*.csv" # find all folders
  count <- 1 # Init: start variable: from 1 to 345 files in total
  
  # load mapping between epcs and UPRN
  #epcid_uprn <- as.data.table(fread("data/epcid_uprn_usrn.csv"))
  #epcid_uprn[,c("parentuprn","usrn") := NULL)
  #setnames(epcid_uprn, "lmk_key", "LMK_KEY")
  
  # load averaged costs for recommendations
  improvements_all <- fread("data/cleaned/improvement_ID_stats_q.csv") # load the manually filled file
  improvements_all[, .(IMPROVEMENT_ID,Average_Cost,YEAR)]
  setnames(improvements_all, "Average_Cost", "INDICATIVE_COST_AVG")
  
  # append all certificates in master spreadsheet
  for (files in filenames[count:length(filenames)]) {
    print(paste(count,gsub(" ","",paste(files,"/certificates.csv")))) #display progress
    
    #load current dataset
    epcs <- as.data.table(fread(file = paste(files,"/certificates.csv",sep=""), header = TRUE))
    epcs <- select(epcs,all_of(SELECTION)) # Selection equation

    # Perform a left join to store 'uprn' in epcs with values from epcid_uprn
    #epcs[epcid_uprn, uprn := i.uprn, on = .(LMK_KEY)]
    ## NOTE: some UPRNS are set to NA and need to be pulled from epcid_uprn so keep uprn and UPRN
    
    # ADD RECOMMENDATIONS ------------------------------------------------------
    # Load corresponding Recommendations
    print(gsub(" ","",paste(files,"/recommendations.csv"))) #display progress
    recoms <- as.data.table(fread(file = paste(files,"/recommendations.csv",sep=""), header = TRUE))
    recoms <- select(recoms,all_of(SELECT_RECOMMENDATIONS)) # Selection equation
    recoms <- recoms[!(is.na(IMPROVEMENT_ID) | IMPROVEMENT_ID == ""), ] # Remove observations where IMPROVEMENT_ID is missing
      
    # cleaning
    recoms$INDICATIVE_COST <- gsub("£","",as.character(recoms$INDICATIVE_COST)) # delete pound symbol
    recoms$INDICATIVE_COST <- gsub(",","",as.character(recoms$INDICATIVE_COST)) # delete commas
    recoms$INDICATIVE_COST <- gsub(" ","",as.character(recoms$INDICATIVE_COST)) # delete spaces
    recoms$IMPROVEMENT_ID_TEXT <- gsub(",", "", recoms$IMPROVEMENT_ID_TEXT) # delete commas
      
    # split INDICATIVE_COST column into LOW and HIGH
    recoms <- recoms %>% 
      separate(INDICATIVE_COST,sep = "-",into = c("INDICATIVE_COST_LOW","INDICATIVE_COST_HIGH"),remove = FALSE)
      
    # replace NA generated above with LOW boundary
    recoms$INDICATIVE_COST_HIGH <- ifelse(is.na(recoms$INDICATIVE_COST_HIGH), recoms$INDICATIVE_COST_LOW, recoms$INDICATIVE_COST_HIGH)
      
    # change to numeric
    recoms$INDICATIVE_COST_HIGH = as.numeric(as.character(recoms$INDICATIVE_COST_HIGH)) 
    recoms$INDICATIVE_COST_LOW = as.numeric(as.character(recoms$INDICATIVE_COST_LOW))
    recoms$INDICATIVE_COST_AVG = 0.5*(recoms$INDICATIVE_COST_HIGH + recoms$INDICATIVE_COST_LOW)
    
    # Filling missing costs estimates with modes per files (NOTE THAT SHOULD BE TIME DEPENDENT)
    setDT(recoms)
      
    # Store LMK_KEYs and INSPECTION_DATE only
    epcs_2 <- epcs[, .(LMK_KEY,INSPECTION_DATE)] # Selection equation
    epcs_2 <- epcs_2 |>
      mutate (YEAR  = as.numeric(substr(INSPECTION_DATE,1,4)))
      
    # Add YEAR to recommendations
    recoms <- merge(recoms, epcs_2, by = "LMK_KEY",all.x = TRUE)
    rm(epcs_2)
      
    # Create a lookup table from improvements to get INDICATIVE_COST_AVG where needed
    improvements_lookup <- improvements_all[, .(INDICATIVE_COST_AVG), by = .(IMPROVEMENT_ID, YEAR)]
      
    # Update recoms where INDICATIVE_COST_AVG is NA
    recoms[is.na(INDICATIVE_COST_AVG), INDICATIVE_COST_AVG := as.numeric(round(improvements_all[.SD, on=.(IMPROVEMENT_ID, YEAR), x.INDICATIVE_COST_AVG]))]
    recoms[INDICATIVE_COST_AVG=="", INDICATIVE_COST_AVG := as.numeric(round(improvements_all[.SD, on=.(IMPROVEMENT_ID, YEAR), x.INDICATIVE_COST_AVG]))]
    
    rm(improvements_lookup) # free memory
    
    # # Compute the mode of INDICATIVE_COST for each IMPROVEMENT_ID
    # recoms[, mode_cost := get_mode(INDICATIVE_COST_AVG), by = IMPROVEMENT_ID]
    
    # # Replace NA values in INDICATIVE_COST with the computed mode
    # recoms[is.na(INDICATIVE_COST_AVG), INDICATIVE_COST_AVG := mode_cost]
    # recoms[INDICATIVE_COST_AVG=="", INDICATIVE_COST_AVG := mode_cost]
    
    # collate strings & sum variables by groups (LMK KEY)
    recoms <- recoms %>%
     group_by(LMK_KEY) %>%
     dplyr::summarise(IMPROVEMENTS_IDs = paste(IMPROVEMENT_ID, collapse = ","),
                      IMPROVEMENTS_TXTs = paste(IMPROVEMENT_ID_TEXT, collapse = ","),
                      INDICATIVE_COSTS = paste(INDICATIVE_COST, collapse = ","),
                      #INDICATIVE_COST_LOW = paste(INDICATIVE_COST_LOW, collapse = ","),
                      #INDICATIVE_COST_HIGH = paste(INDICATIVE_COST_HIGH, collapse = ","),
                      INDICATIVE_COST_AVG = paste(INDICATIVE_COST_AVG, collapse = ",")
                      )
    
    # Convert recoms to data.table
    recoms <- as.data.table(recoms)
      
    # Split the 'IMPROVEMENTS_IDs' into a list of numeric IDs
    recoms[, IMPROVEMENTS_IDs_2 := lapply(strsplit(IMPROVEMENTS_IDs, ",", fixed = TRUE), as.integer)]
      
    # Create a long-format data.table of IDs and their present IMPROVEMENT_IDs
    improvement_long <- recoms[, .(IMPROVEMENT_ID = unlist(IMPROVEMENTS_IDs_2)), by = LMK_KEY]
      
    # Add dummy variables initialized to 0
    dummy_names <- paste0("ID_", 1:total_IDs)
    recoms[, (dummy_names) := 0]
      
    # Efficient update: Use a binary join to set dummies
    for (id in 1:total_IDs) {
      dummy_col <- paste0("ID_", id)
      recoms[improvement_long[IMPROVEMENT_ID == id], on=.(LMK_KEY), (dummy_col) := 1]
    }
      
    # Delete unused column with improvements
    recoms[, c("IMPROVEMENTS_IDs_2") := NULL]
      
    # Merge EPC with recommendations
    epcs <- merge(epcs, recoms, by = "LMK_KEY",all.x = TRUE)
      
    rm(recoms,improvement_long) # delete temp frames

    # sort by building number and inspection date
    epcs <- epcs[order(UPRN, INSPECTION_DATE,POSTCODE,POSTTOWN),]
    #epcs <- epcs[order(epcs$UPRN, epcs$INSPECTION_DATE,epcs$POSTCODE,epcs$POSTTOWN),]
    
    # OUTPUT FILE --------------------------------------------------------------
    write_csv(epcs, gsub(" ","",paste("data/cleaned/epcs/",str_sub(files,20,-1),".csv"))) # equivalent to adding sep="" in paste
    
    count <- count + 1 # increase counter
  }  # end loop on filenames

  rm(epcs,epcid_uprn,improvements_all) # delete temp frames
} # end generate certificates



# GENERATE REGISTRY DATA -------------------------------------------------------
if (gen_registy != 0){ 
  filenames <- list.files("data/registry",full.names = TRUE) # find all folders
  count <- 1 # Init  
  
  # load mapping between registry transactions and UPRN
  ppid_uprn <- as.data.table(fread("data/ppdid_uprn_usrn.csv"))
  ppid_uprn[, c("parentuprn","usrn") := NULL]
  
  # append and clean all registry
  for (files in filenames[count:length(filenames)]) {
    print(paste(count,files)) # display progress
    
    # load current dataset
    df <- fread(file = files, header = FALSE)
    # Selection equation
    df <- select(df,all_of(SELECT_REGISTRY))
    
    # rename columns with actual variable names
    df <- df %>%
      rename_with(~ all_of(REGISTRY_VARIABLE_NAMES),SELECT_REGISTRY)
    
    # merge with uprn until Jan 2022
    df <- merge(x = df, y = ppid_uprn, by = "transactionid", all.x = TRUE)

    # gen and clean ADDRESSES
    df <- df |>
      mutate(
        PRIMARY = clean_address(PRIMARY),
        SECONDARY = clean_address(SECONDARY),
        STREET = clean_address(STREET),
        TOWN = clean_address(TOWN),
        SECONDARY = gsub("FLAT ","FLAT",SECONDARY),
        SECONDARY = gsub("APARTMENT ","APARTMENT",SECONDARY),
        PRIMARY = gsub("FLAT ","FLAT",PRIMARY),
        PRIMARY = gsub("APARTMENT ","APARTMENT",PRIMARY),
        ADDRESS = paste(PRIMARY,SECONDARY,STREET,POSTCODE,TOWN,sep=", "), # create a unique identifier per property
        ADDRESS = gsub(", ,",",",as.character(ADDRESS)), # delete empty spot in ADDRESS
        )
    
    if (count >= registry_files - 1) {
      # load all other datasets and look for uprns
      filenames2 <- list.files("data/cleaned/registry", full.names = TRUE)
      for (files2 in filenames2[1:count-1]) {
        print(files2) #display progress
        # load current dataset
        df2 <- fread(file = files2, header = TRUE)
        df2 <- df2[!is.na(uprn)]
        df2 <- df2[, .(PRIMARY,SECONDARY,POSTCODE,uprn)] 
        
        # Perform a left join to update 'UPRN' in df with values from epcid_uprn
        df[df2, uprn := i.uprn, on = .(PRIMARY,SECONDARY,POSTCODE)]
        
      } # end for loop on filenames2
      rm(df2) # free memory
      
    } # end if on count
    
    # save cleaned file
    write_csv(df, paste("data/cleaned/",substr(files,6,nchar(files)),sep=""))
    
    rm(df) # delete temp frames
    count <- count + 1 # increase counter
    
  } # end loop on filenames
  
  rm(ppid_uprn)
  
} # end generate registry



# GEN MASTER REG AND EPC -------------------------------------------------------
if (gen_master_regepcs == 1) {

   for (folder in c("epcs","registry")) { 
     print(folder)
     filenames <- list.files(paste("data/cleaned/",folder,sep=""),full.names = FALSE) #pattern="*.csv" # find all folders
     count <- 1 # Init  
     
     # Initialize list to store individual data frames
     list_of_dfs <- vector("list", length(filenames))
     
     # Loop through filenames and read the data
     for (i in seq_along(filenames)) {
       files <- filenames[i]
       print(paste(i, files))
       temp_df <- fread(paste0("data/cleaned/", folder, "/", files)) # Load mapped registry
       list_of_dfs[[i]] <- temp_df
       rm(temp_df)
     }
     
     # Combine all data frames at once
     full_df <- rbindlist(list_of_dfs, use.names = TRUE, fill = TRUE)
     rm(list_of_dfs) # free memory
     
     # Output file
     fwrite(full_df,paste("data/cleaned/master_",folder,".csv",sep="") )
     rm(full_df) # free memory
   } # end loop on folder
  
} # end if on gen_master_regepcs



# GEN MASTER DATASET ---------------------------------------------------------
if (gen_master_data == 1) {
  
  # Load EPC and REGISTRY data -------------------------------------------------
  master_epcid <- as.data.table(fread("data/cleaned/master_epcs.csv"))
  master_ppdid <- as.data.table(fread("data/cleaned/master_registry.csv"))
  
  # Load and filter EPC and REGISTRY data
  master_epcid <- master_epcid[, .(UPRN,INSPECTION_DATE,LMK_KEY)]
  master_ppdid <- master_ppdid[, .(uprn,TRANSACTION_DATE,transactionid)]
  
  # Convert to date format
  master_ppdid$TRANSACTION_DATE <- as.Date(master_ppdid$TRANSACTION_DATE)
  #master_epcid$INSPECTION_DATE <- as.Date(master_epcid$INSPECTION_DATE)
  
  # Keep post 2007 transactions
  master_ppdid  <- master_ppdid [TRANSACTION_DATE > as.Date("2007-01-01")] # date from which EPCs were introduced
  setnames(master_ppdid, "uprn", "UPRN")
  
  # Drop transactions without UPRN
  master_ppdid  <- master_ppdid [!is.na(UPRN)]
  
  # Pre-calculate the latest inspection for each UPRN up to each transaction date
  latest_inspection <- master_epcid[master_ppdid, on = .(UPRN, INSPECTION_DATE <= TRANSACTION_DATE), 
                                    .(UPRN, TRANSACTION_DATE, latest_inspection = max(x.INSPECTION_DATE), LMK_KEY = x.LMK_KEY[which.max(x.INSPECTION_DATE)]),
                                    by = .EACHI]
  
  # Join this information back to master_ppdid to update the original table
  master_ppdid <- master_ppdid[latest_inspection, on = .(UPRN, TRANSACTION_DATE), 
                               `:=` (INSPECTION_DATE = i.latest_inspection, LMK_KEY = i.LMK_KEY)]
  
  rm(master_epcid) # free memory
  
  # Merge UPRN to LATITUDE and LONGITUDE
  coordinates <- as.data.table(fread("data/osopenuprn_202312.csv"))
  coordinates <- coordinates[coordinates$UPRN%in%master_ppdid$UPRN,] # keep only common UPRNs
  coordinates [, c("X_COORDINATE","Y_COORDINATE") := NULL]
  
  master_ppdid <- merge(x = master_ppdid, y = coordinates, by = c("UPRN"), all.x = "T")
  
  fwrite(master_ppdid,"data/cleaned/master_dataset.csv") # Output file
  rm(master_ppdid,coordinates) # free memory
  
} # end if on gen_master_data


# CLEAN MASTER EPC -----------------------------------------------------------------
if (clean_master_epc == 1){
  
  # Load EPC dataset
  master_dataset <- as.data.table(fread("data/cleaned/master_epcs.csv"))
  
  # rename EPC variables
  for (vars in c("PROPERTY_TYPE","ADDRESS","POSTCODE")) {
    setnames(master_dataset, vars, paste(vars,"_EPC",sep=""),skip_absent=TRUE)
  }
  
  # Compute adjusted efficiency
  master_dataset[, DIFF_ENERGY_EFFICIENCY := POTENTIAL_ENERGY_EFFICIENCY - CURRENT_ENERGY_EFFICIENCY] # EPC score
  master_dataset[DIFF_ENERGY_EFFICIENCY > 0, POUND_PER_EPC := sapply(INDICATIVE_COST_AVG, sum_values)/DIFF_ENERGY_EFFICIENCY] # linear estimation of cost of raising EPC (avoids dividing by 0)
  master_dataset[, DIFF_ENERGY_CONSUMPTION := ENERGY_CONSUMPTION_POTENTIAL - ENERGY_CONSUMPTION_CURRENT] # ELECTRICITY in kWh
  master_dataset[, DIFF_CO2_EMISSIONS := CO2_EMISSIONS_POTENTIAL - CO2_EMISSIONS_CURRENT] # CARBON EMISSIONS in tons of CO2
  master_dataset[, DIFF_ENVIRONMENT_IMPACT := ENVIRONMENT_IMPACT_POTENTIAL - ENVIRONMENT_IMPACT_CURRENT] # Environmental friendliness
  master_dataset[, ENERGY_COST_POTENTIAL := LIGHTING_COST_POTENTIAL + HEATING_COST_POTENTIAL + HOT_WATER_COST_POTENTIAL] # ENERGY COST (Light, water and heating)
  master_dataset[, ENERGY_COST_CURRENT := LIGHTING_COST_CURRENT + HEATING_COST_CURRENT + HOT_WATER_COST_CURRENT] # ENERGY COST (Light, water and heating)
  master_dataset[, DIFF_ENERGY_COST := ENERGY_COST_POTENTIAL - ENERGY_COST_CURRENT] # ENERGY COST (Light, water and heating)
  #master_dataset[DIFF_ENERGY_EFFICIENCY > 0, KWH_PER_EPC := DIFF_ENERGY_CONSUMPTION/DIFF_ENERGY_EFFICIENCY] # linear estimation of savings from raising EPC (avoids dividing by 0)
  
  # Recode columns into specific 5-point scales
  for (columns in all_of(SELECT_ENERGY)) {
    print(paste("cleaning", columns))
    columns_f <- paste(columns, "_f", sep = "")
    master_dataset[, (columns_f) := NA_real_] # Initialize the new column as numeric
    
    master_dataset[get(columns) == "Very Poor", (columns_f) := -2]
    master_dataset[get(columns) == "Poor", (columns_f) := -1]
    master_dataset[get(columns) == "Average", (columns_f) := 0]
    master_dataset[get(columns) == "Good", (columns_f) := 1]
    master_dataset[get(columns) == "Very Good", (columns_f) := 2]
    #master_dataset[, (columns_f) := as.numeric(as.character(columns_f))] # Ensure not a factor
    
  } # end for loop on columns
  
  # Clean repetitions between dummies ------------------------------------------
  # Upgrade heating controls
  master_dataset[, ID_11 := as.numeric(ID_11 | ID_12 | ID_13 | ID_14 | ID_15 | ID_17 | ID_18) ]
  master_dataset[, c("ID_12", "ID_13", "ID_14", "ID_15", "ID_17", "ID_18") := NULL]
  # Replace boiler with new condensing boiler
  master_dataset[, ID_20 := as.numeric(ID_20 | ID_21) ] 
  master_dataset[, c("ID_21") := NULL]
  # Fan assisted storage heaters and dual immersion cylinder
  master_dataset[, ID_24 := as.numeric(ID_24 | ID_30) ] 
  master_dataset[, c("ID_30") := NULL]
  # Fan assisted storage heaters
  master_dataset[, ID_25 := as.numeric(ID_25 | ID_31) ] 
  master_dataset[, c("ID_31") := NULL]
  # Change heating to gas condensing boiler
  master_dataset[, ID_27 := as.numeric(ID_27 | ID_29| ID_32) ] 
  master_dataset[, c("ID_29","ID_32") := NULL]
  # Install condensing boiler
  master_dataset[, ID_37 := as.numeric(ID_37 | ID_38) ] 
  master_dataset[, c("ID_38") := NULL]
  # Change room heaters to condensing boiler
  master_dataset[, ID_40 := as.numeric(ID_40 | ID_41) ] 
  master_dataset[, c("ID_41") := NULL]
  # High heat retention storage heaters and dual immersion cylinder
  master_dataset[, ID_59 := as.numeric(ID_59 | ID_61) ] 
  master_dataset[, c("ID_61") := NULL]
  # High heat retention storage heaters
  master_dataset[, ID_60 := as.numeric(ID_60 | ID_62) ] 
  master_dataset[, c("ID_62") := NULL]
  
  # Sort master_dataset
  master_dataset <- master_dataset[order(UPRN,INSPECTION_DATE),]
  
  # # Look for changes in EPCs for a given property
  # master_dataset[, change_EPC := 0] # Initialize column with 0
  # master_dataset[, change_EPC := ifelse(shift(INSPECTION_DATE, type = "lag", fill = INSPECTION_DATE[1]) != INSPECTION_DATE, 1, 0), by = UPRN]
  # 
  # # Recode improvements and costs
  # master_dataset[, `:=`(
  #   IMPROVEMENTS_IDs = strsplit(IMPROVEMENTS_IDs, ","),
  #   INDICATIVE_COST_AVG = strsplit(INDICATIVE_COST_AVG, ",")
  # )]
  # 
  # # Initialize a new column to store unmatched positions
  # filtered_indices <- which(master_dataset$change_EPC == 1)
  # master_dataset[, `:=`(ESTIMATED_COST_AVG = 0)]
  # 
  # # Ensure that the first index is not included if it can lead to an out-of-bounds error
  # if (1 %in% filtered_indices) {
  #   filtered_indices <- filtered_indices[-1]
  # }
  # 
  # # Compute values only for the necessary rows
  # master_dataset[filtered_indices, ESTIMATED_COST_AVG := {
  #   unmatches <- mapply(find_unmatched_positions, master_dataset$IMPROVEMENTS_IDs[.I-1], master_dataset$IMPROVEMENTS_IDs[.I])
  #   sums <- mapply(function(costs, idx) sum(as.numeric(costs[idx])), master_dataset$INDICATIVE_COST_AVG[.I-1], unmatches)
  #   sums
  # }, by = .I]
  
  fwrite(master_dataset,"data/cleaned/master_epcs.csv") # Output file
  
} # end if on clean_master_epc


# PRE_ANALYSIS -----------------------------------------------------------------
if (pre_analysis == 1){
  
  # Load data
  master_dataset <- as.data.table(fread("data/cleaned/master_dataset.csv"))
  
  # Drop transactions without Inspection Date or UPRN
  master_dataset  <- master_dataset [!is.na(UPRN)]
  master_dataset  <- master_dataset [!is.na(INSPECTION_DATE)]
  print(paste(length(unique(master_dataset$UPRN)), "unique UPRNs found"))
  
  # Read and clean the registry CSV file
  registry <- as.data.table(fread("data/cleaned/master_registry.csv"))
  registry <- registry[registry$transactionid%in%master_dataset$transactionid,] # Drop transactions absent from master_dataset
  registry [, c("TRANSACTION_DATE","uprn") := NULL]
  
  # Merge with registry data
  master_dataset <- merge(x = master_dataset, y = registry, by = "transactionid", all.x = T)
  rm(registry)
  
  # Read and clean the EPC CSV file
  epcs <- as.data.table(fread("data/cleaned/master_epcs.csv"))
  epcs <- epcs[epcs$LMK_KEY%in%master_dataset$LMK_KEY,] # Drop EPCs absent from master_dataset
  #epcs <- epcs[, .SD, .SDcols = !grepl("^ID_", names(epcs))]  # removes the dummies
  epcs[, c("INSPECTION_DATE","UPRN") := NULL]
  
  # Merge with EPC data
  master_dataset <- merge(x = master_dataset, y = epcs, by = "LMK_KEY", all.x = T)
  rm(epcs)
  
  # Specifying new order and merging with the rest of the columns
  new_order <- c("transactionid","LMK_KEY","LATITUDE","LONGITUDE","UPRN", "TRANSACTION_DATE", "INSPECTION_DATE")
  master_dataset <- master_dataset[, c(new_order, setdiff(names(master_dataset), new_order)), with = FALSE]
  rm(new_order)
  
  # Gen time variables for regression controls
  master_dataset <- master_dataset |>
    mutate (
      YEAR  = as.numeric(substr(TRANSACTION_DATE,1,4)),
      MONTH = as.numeric(substr(TRANSACTION_DATE,6,7))
    )
  
  # Gen general regression variables
  master_dataset[, `:=`(
    after_2022 = as.integer(YEAR >= 2022)
  )]
  
  # Level variables
  master_dataset[, `:=`(YEAR_f = factor(YEAR), MONTH_f = factor(MONTH))]
  
  # Sort master_dataset
  master_dataset <- master_dataset[order(UPRN,TRANSACTION_DATE,INSPECTION_DATE),]
  
  # Look for changes in EPCs for a given property
  master_dataset[, change_EPC := 0] # Initialize column with 0
  master_dataset[, change_EPC := ifelse(shift(INSPECTION_DATE, type = "lag", fill = INSPECTION_DATE[1]) != INSPECTION_DATE, 1, 0), by = UPRN]
  
  # Initialize new columns to 0
  master_dataset[, c("post_treat", "treat", "post") := .(0, 0, 0)]
  
  # Diff-in-diff modelling
  master_dataset[, `:=`(
    post_treat = cumsum(change_EPC),
    post = as.integer(seq_len(.N) > 1)
  ), by = UPRN]
  # we need to flag when it changes, 010, if it changes 011, if i belongs to treated 111 (as opposed to 000), if it changed multiple times 012
  
  # Recreate treat variable as well
  master_dataset[, `:=`(
    treat = as.integer(any(post_treat > 0))
  ), by = UPRN]
  
  # Recode improvements and costs
  master_dataset[, `:=`(
    IMPROVEMENTS_IDs = strsplit(IMPROVEMENTS_IDs, ","),
    INDICATIVE_COST_AVG = strsplit(INDICATIVE_COST_AVG, ",")
  )]
  
  # Initialize a new column to store unmatched positions
  filtered_indices <- which(master_dataset$change_EPC == 1)
  master_dataset[, `:=`(ESTIMATED_COST_AVG = 0)]
  
  # Ensure that the first index is not included if it can lead to an out-of-bounds error
  if (1 %in% filtered_indices) {
    filtered_indices <- filtered_indices[-1]
  }
  
  # Compute values only for the necessary rows
  master_dataset[filtered_indices, ESTIMATED_COST_AVG := {
    unmatches <- mapply(find_unmatched_positions, master_dataset$IMPROVEMENTS_IDs[.I-1], master_dataset$IMPROVEMENTS_IDs[.I])
    sums <- mapply(function(costs, idx) sum(as.numeric(costs[idx])), master_dataset$INDICATIVE_COST_AVG[.I-1], unmatches)
    sums
  }, by = .I]
  
  ####
  
  # Output files
  fwrite(master_dataset,"data/cleaned/master_dataset_all.csv") 
  
  # keep repeat transactions (i.e. houses which transacted several times)
  master_dataset <- keep_occurrences(master_dataset,master_dataset$UPRN,1)
  print(paste(length(unique(master_dataset$UPRN)), "unique UPRNs found"))

  #fwrite(master_dataset,"data/cleaned/master_dataset_dup.csv") # Output file
  rm(master_dataset, filtered_indices) # free memory
  
} # end if on pre_analysis


# ANALYSIS ---------------------------------------------------------------------
if (analysis == 1){
  
  # Load data
  master_dataset <- as.data.table(fread("data/cleaned/master_dataset_all.csv"))
  
  # Load deflator from RPI data and merge
  # compute variables:
  # repetition of transactions per property (avg ownerwship time)
  # time between latest epc and the transaction (record difference)
  # speak about the transaction type for the epc (why the EPC triggered): anticipation of price increase in an incentive to commit to EPC change
  # backward looking pricing of improvements
  # quantify the utility bills savings (from impact column in EPC)
  # exogenous energy prices drive EPC, improvements and therefore house prices
  # look at EPC score or electricity price to define a control and treatment group  based (or interact with decile dummy)
  # what drives people to adopt improvements (3rd paper)
  # staircasing your EPC: where and when it matters? (RDD)
  # PSM: equivalent to DiD but with the entire dataset (decide control/treatment)
  
  
  # Compute adjusted and real prices
  master_dataset[, ADJUSTED_PRICE := PRICE - ESTIMATED_COST_AVG ] # + energy savings
  #master_dataset[, REAL_PRICE := PRICE / DEFLATOR ] 
  #master_dataset[, REAL_ADJUSTED_PRICE := ADJUSTED_PRICE / DEFLATOR ]
  master_dataset[, ln_PRICE := log(PRICE)] # logged prices
  #master_dataset[, ln_REAL_PRICE := log(REAL_PRICE)] # logged prices
  
  # Select regressions
  reg_glm <- 0 # if running FE regs (within)
  reg_rdd <- 0 # if running RDD
  
  # Regressions ----------------------------------------------------------------
  # use monthly RPI and correct prices (more relevant to real estate)
  # heterogeneity analysis: interact on property_type, deciles of property_prices (compute deciles in year of transaction)
  
  master_dataset[, `:=`(YEAR_f = factor(YEAR), MONTH_f = factor(MONTH))]
  
  # Define OLS regression formulas
  list_of_formulas <- list(
  formula_1 <- PRICE ~ CURRENT_ENERGY_EFFICIENCY,
  formula_2 <- as.formula(paste("PRICE ~ CURRENT_ENERGY_EFFICIENCY", controls, time_fe, sep="+")),
  formula_3 <- as.formula(paste("PRICE ~ CURRENT_ENERGY_EFFICIENCY + CURRENT_ENERGY_EFFICIENCY:after_2022 + after_2022", controls, time_fe, sep="+"))
  # formula_4 <- as.formula(paste("ln_PRICE ~ CURRENT_ENERGY_RATING", controls, time_fe, sep="+")),
  # formula_5 <- PRICE ~ CURRENT_ENERGY_RATING,
  # formula_6 <- as.formula(paste("PRICE ~ CURRENT_ENERGY_RATING", controls, time_fe, sep="+"))
  )
  
  # Run the series of regressions above
  reg_output_list <- lapply(list_of_formulas, function(formula) {
    run_regression(formula, master_dataset, cluster_se, pattern_controls)
  })
  
  # Custom save the regression output to a CSV file
  custom_write_csv(reg_output_list,"output/tables/reg_Price_1.csv")
  
  # Define OLS regression formulas
  list_of_formulas <- list(
  formula_1 <- as.formula(paste("PRICE ~ change_EPC", controls, time_fe, sep="+")),
  formula_2 <- as.formula(paste("PRICE ~ change_EPC:after_2022 + change_EPC", controls, time_fe, sep="+")),
  formula_3 <- as.formula(paste("ADJUSTED_PRICE ~ change_EPC", controls, time_fe, sep="+")),
  formula_4 <- as.formula(paste("ADJUSTED_PRICE ~ change_EPC:after_2022 + change_EPC", controls, time_fe, sep="+"))
  )
  
  # Run the series of regressions above
  reg_output_list <- lapply(list_of_formulas, function(formula) {
    run_regression(formula, master_dataset, cluster_se, pattern_controls)
  })
  
  # Custom save the regression output to a CSV file
  custom_write_csv(reg_output_list,"output/tables/reg_Price_2.csv")
  
  # Define OLS regression formulas
  list_of_formulas <- list(
  formula_1 <- as.formula(paste("PRICE ~ post_treat + treat + post", controls, time_fe, sep="+")),
  formula_2 <- as.formula(paste("ADJUSTED_PRICE ~ post_treat + treat + post", controls, time_fe, sep="+"))
  )
  # Run the series of regressions above
  reg_output_list <- lapply(list_of_formulas, function(formula) {
    run_regression(formula, master_dataset, cluster_se, pattern_controls)
  })
  
  # Custom save the regression output to a CSV file
  custom_write_csv(reg_output_list,"output/tables/reg_Price_3.csv")
  
  rm(list_of_formulas,reg_output_list) # free memory
  
  # OLS with Diff-in-diff and interactions: PRICE ~ post_treat:PROPERTY_TYPE + treat:PROPERTY_TYPE + post:PROPERTY_TYPE + post_treat + treat + post + controls
  formula <- as.formula(paste("PRICE ~ post_treat:PROPERTY_TYPE + treat:PROPERTY_TYPE + post:PROPERTY_TYPE + post_treat + treat + post", controls, time_fe, sep="+"))
  print(run_regression(formula, master_dataset, cluster_se, pattern_controls))
  
  
  if (reg_glm != 0) {
    # FE model "within": PRICE ~ CURRENT_ENERGY_EFFICIENCY + controls
    formula <- as.formula(paste("PRICE ~ CURRENT_ENERGY_EFFICIENCY", controls, time_fe, sep="+"))
    reg_output <- plm(formula, data = master_dataset, index = c("UPRN", "xtset"), model = "within")
    coeffs_std <- data.frame(summary(reg_output)$coefficients, cluster = cluster_se)
    print(coeffs_std[!grepl(pattern = pattern_controls, rownames(coeffs_std)), ])
  
    # FE model "within" with Diff-in-diff: PRICE ~ post_treat + treat + post + controls
    formula <- as.formula(paste("PRICE ~ post_treat + treat + post", controls, time_fe, sep="+"))
    reg_output <- plm(formula, data = master_dataset, index = c("UPRN", "xtset"), model = "within")
    coeffs_std <- data.frame(summary(reg_output)$coefficients, cluster = cluster_se)
    print(coeffs_std[!grepl(pattern = pattern_controls, rownames(coeffs_std)), ])
    
    # FE model "within" with Diff-in-diff and interactions: PRICE ~ post_treat:PROPERTY_TYPE + post:PROPERTY_TYPE + post_treat + treat + post + controls
    formula <- as.formula(paste("PRICE ~ post_treat:PROPERTY_TYPE + treat:PROPERTY_TYPE + post:PROPERTY_TYPE + post_treat + treat + post", controls, time_fe, sep="+"))
    reg_output <- plm(formula, data = master_dataset, index = c("UPRN", "xtset"), model = "within")
    coeffs_std <- data.frame(summary(reg_output)$coefficients, cluster = cluster_se)
    print(coeffs_std[!grepl(pattern = pattern_controls, rownames(coeffs_std)), ])
  } # end if on reg_glm
  
  if (reg_rdd != 0) {
  # RDD with EPC cutoff values corresponding to bands
  # A: >= 92 ; B: 81-91 ; C: 69-80 ; D: 55-68 ; E: 39-54 ; F: 21-38 ; G: 01-20
  # ESTATE: "F", "L"  OR PROPERTY_TYPE: "D","F","O","S","T"
  
    for (cutoff in c(21,39,55,69,81,92)){ # loop on all cutoffs/thresholds above
      print(paste("RDD at the",cutoff,"EPC threshold for property type:",property_type)) # display progress
      
      master_dataset[, cutoff_d := NA] # Reset Cutoff value
      
      # Bandwidth Selection
      master_dataset[CURRENT_ENERGY_EFFICIENCY >= cutoff - bandwidth - 1 & CURRENT_ENERGY_EFFICIENCY <= cutoff - 1, cutoff_d := 0] # & PROPERTY_TYPE == property_type
      master_dataset[CURRENT_ENERGY_EFFICIENCY >= cutoff & CURRENT_ENERGY_EFFICIENCY <= cutoff + bandwidth, cutoff_d := 1] # & PROPERTY_TYPE == property_type
      
      sub_master_dataset <- master_dataset[!is.na(cutoff_d)] # Restrict data to sub sample defined as per cutoff
        
      # Regression with controls
      formula <- PRICE ~ cutoff_d + PROPERTY_TYPE + EXTENSION_COUNT + TOTAL_FLOOR_AREA + MAIN_FUEL + YEAR_f + MONTH_f + CONSTRUCTION_AGE_BAND 
      print(run_regression(formula, sub_master_dataset, cluster_se, pattern_controls))
        
      #formula <- ADJUSTED_PRICE ~ cutoff_d + EXTENSION_COUNT + TOTAL_FLOOR_AREA + MAIN_FUEL + YEAR + MONTH + CONSTRUCTION_AGE_BAND # as.formula(paste("ADJUSTED_PRICE ~ cutoff_d", controls, time_fe, sep="+"))
      #print(run_regression(formula, sub_master_dataset, cluster_se, pattern_controls))
        
      rm(sub_master_dataset) # free memory
    } # end for loop on cutoff
  } # end if on reg_rdd
  
  ## FIX RD_multi !!
  
  # # multi RDD with several cutoffs
  # Y <- master_dataset$PRICE
  # X <- master_dataset$CURRENT_ENERGY_EFFICIENCY
  # Z <- subset(master_dataset, select = c(TOTAL_FLOOR_AREA,PROPERTY_TYPE.y))
  # #cvec <- master_dataset$CURRENT_ENERGY_RATING
  # cvec <- c(45,54,69,81)
  # # aux <- rdms(Y,X,cvec,range = matrix(c(54,80,70,90),ncol=2))
  # aux <- rdms(Y,X,cvec,covs_mat = Z)

} # end analysis

# ANALYSIS EPC ---------------------------------------------------------------------
if (analysis_EPC == 1){
  
  ## to-do list:
  # sort id for different solar panel power
  # fix non dummy estimations for 3rd series of models
  
  # Load EPC dataset
  master_dataset <- as.data.table(fread("data/cleaned/master_epcs.csv"))
  
  # Gen time variables for regression controls
  master_dataset <- master_dataset |>
    mutate (
      YEAR  = as.numeric(substr(INSPECTION_DATE,1,4)),
      MONTH  = as.numeric(substr(INSPECTION_DATE,6,7))
    )
  
  # Level variables
  master_dataset[, `:=`(YEAR_f = factor(YEAR), MONTH_f = factor(MONTH))]
  
  # Find all covariates that start with "ID_"
  covariates <- grep("^ID_", names(master_dataset), value = TRUE)
  
  # Define OLS regression formulas
  list_of_formulas <- list(
  formula_1 <- as.formula(paste("DIFF_ENERGY_EFFICIENCY ~ 0 + YEAR_f +", paste(covariates, collapse = " + "))),
  formula_2 <- as.formula(paste("DIFF_ENERGY_CONSUMPTION ~ 0 + YEAR_f +", paste(covariates, collapse = " + "))),
  formula_3 <- as.formula(paste("DIFF_CO2_EMISSIONS ~ 0 + YEAR_f +", paste(covariates, collapse = " + "))),
  formula_4 <- as.formula(paste("DIFF_ENVIRONMENT_IMPACT ~ 0 + YEAR_f +", paste(covariates, collapse = " + "))),
  formula_5 <- as.formula(paste("DIFF_ENERGY_COST ~ 0 + YEAR_f +", paste(covariates, collapse = " + ")))
  )
  
  # Consider adding property size and type as covariates
  
  # Run the series of regressions above
  reg_output_list <- lapply(list_of_formulas, function(formula) {
    run_regression(formula, master_dataset, cluster_se, pattern_controls)
  })
  
  # Custom save the regression output to a CSV file
  custom_write_csv(reg_output_list,"output/tables/reg_EPC_1.csv")
  
  # Compute additional variables
  master_dataset[, ENERGY_COST_POTENTIAL := LIGHTING_COST_POTENTIAL + HEATING_COST_POTENTIAL + HOT_WATER_COST_POTENTIAL] # ENERGY COST (Light, water and heating)
  master_dataset[, ENERGY_COST_CURRENT := LIGHTING_COST_CURRENT + HEATING_COST_CURRENT + HOT_WATER_COST_CURRENT]
  
  # Define OLS regression formulas
  list_of_formulas <- list(
    formula_1 <- as.formula(paste("POTENTIAL_ENERGY_EFFICIENCY ~ 0 + ENERGY_CONSUMPTION_CURRENT + YEAR_f + TOTAL_FLOOR_AREA +", paste(covariates, collapse = " + "))),
    formula_2 <- as.formula(paste("ENERGY_CONSUMPTION_POTENTIAL ~ 0 + ENERGY_CONSUMPTION_CURRENT + YEAR_f + TOTAL_FLOOR_AREA +", paste(covariates, collapse = " + "))),
    formula_3 <- as.formula(paste("CO2_EMISSIONS_POTENTIAL ~ 0 + CO2_EMISSIONS_CURRENT + YEAR_f + TOTAL_FLOOR_AREA +", paste(covariates, collapse = " + "))),
    formula_4 <- as.formula(paste("ENVIRONMENT_IMPACT_POTENTIAL ~ 0 + ENVIRONMENT_IMPACT_CURRENT + YEAR_f + TOTAL_FLOOR_AREA +", paste(covariates, collapse = " + "))),
    formula_5 <- as.formula(paste("ENERGY_COST_POTENTIAL ~ 0 + ENERGY_COST_CURRENT + YEAR_f + TOTAL_FLOOR_AREA +", paste(covariates, collapse = " + ")))
  )
  
  # Run the series of regressions above
  reg_output_list <- lapply(list_of_formulas, function(formula) {
    run_regression(formula, master_dataset, cluster_se, pattern_controls)
  })
  
  # Custom save the regression output to a CSV file
  custom_write_csv(reg_output_list,"output/tables/reg_EPC_2.csv")
  
  # Find all covariates that end with "_EFF"
  covariates <- grep("_EFF$", names(master_dataset), value = TRUE)

  # Define OLS regression formulas
  list_of_formulas <- list(
  formula_1 <- as.formula(paste("CURRENT_ENERGY_EFFICIENCY ~ ", paste(covariates, collapse = " + "))),
  formula_2 <- as.formula(paste("ENERGY_CONSUMPTION_CURRENT ~ ", paste(covariates, collapse = " + "))),
  formula_3 <- as.formula(paste("CO2_EMISSIONS_CURRENT ~ ", paste(covariates, collapse = " + "))),
  formula_4 <- as.formula(paste("ENVIRONMENT_IMPACT_CURRENT ~ ", paste(covariates, collapse = " + "))),
  formula_5 <- as.formula(paste("ENERGY_COST_CURRENT ~ ", paste(covariates, collapse = " + ")))
  )
  
  # Run the series of regressions above
  reg_output_list <- lapply(list_of_formulas, function(formula) {
    run_regression(formula, master_dataset, cluster_se, pattern_controls)
  })
  
  # Custom save the regression output to a CSV file
  custom_write_csv(reg_output_list,"output/tables/reg_EPC_3.csv")
  
  # Find all covariates that end with "_EFF"
  covariates <- grep("_EFF_f$", names(master_dataset), value = TRUE)
  
  # Define OLS regression formulas
  list_of_formulas <- list(
  formula_1 <- as.formula(paste("CURRENT_ENERGY_EFFICIENCY ~ ", paste(covariates, collapse = " + "))),
  formula_2 <- as.formula(paste("ENERGY_CONSUMPTION_CURRENT ~ ", paste(covariates, collapse = " + "))),
  formula_3 <- as.formula(paste("CO2_EMISSIONS_CURRENT ~ ", paste(covariates, collapse = " + "))),
  formula_4 <- as.formula(paste("ENVIRONMENT_IMPACT_CURRENT ~ ", paste(covariates, collapse = " + "))),
  formula_5 <- as.formula(paste("ENERGY_COST_CURRENT ~ ", paste(covariates, collapse = " + ")))
  )
  
  # Run the series of regressions above
  reg_output_list <- lapply(list_of_formulas, function(formula) {
    run_regression(formula, master_dataset, cluster_se, pattern_controls)
  })
  
  # Custom save the regression output to a CSV file
  custom_write_csv(reg_output_list,"output/tables/reg_EPC_4.csv")
  
  rm(list_of_formulas,reg_output_list) # free memory
  
  # # argument: EPC score is function of components but also costs and consumption throughout greenness of the grid
  # # OLS: CURRENT_ENERGY_EFFICIENCY ~ EPC components and costs + FEs
  # formula <- CURRENT_ENERGY_EFFICIENCY ~ HOT_WATER_ENERGY_EFF + FLOOR_ENERGY_EFF + WALLS_ENERGY_EFF + ROOF_ENERGY_EFF + MAINHEAT_ENERGY_EFF + LIGHTING_ENERGY_EFF + ENERGY_CONSUMPTION_CURRENT + LIGHTING_COST_CURRENT + HEATING_COST_CURRENT + HOT_WATER_COST_CURRENT + ENVIRONMENT_IMPACT_CURRENT + CO2_EMISSIONS_CURRENT
  # print(run_regression(formula, master_dataset, cluster_se, pattern_controls))
  
} # end if on analysis_EPC

# COST of REACHING EPC ---------------------------------------------------------
if (cost_of_retrofit == 1){
  
  ## Gen file with Improvement IDs, Costs and IMPACT CATEGORIES
  improvements_IDs <- fread("data/cleaned/improvement_ID_text.csv") # load the list of IDs
  improvements_IDs$IMPROVEMENT_ID <- as.numeric(improvements_IDs$IMPROVEMENT_ID)
  
  # Add avg latest cost per improvement
  costs_improvements <- fread("data/cleaned/improvement_ID_stats_q.csv")
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
  for (columns in all_of(impact_names)) {
    print(columns)
    copy_id_value(improvements_IDs, from_id = 11, to_id = 12, column = columns)
    copy_id_value(improvements_IDs, from_id = 11, to_id = 13, column = columns)
    copy_id_value(improvements_IDs, from_id = 11, to_id = 14, column = columns)
    copy_id_value(improvements_IDs, from_id = 11, to_id = 15, column = columns)
    copy_id_value(improvements_IDs, from_id = 11, to_id = 17, column = columns)
    copy_id_value(improvements_IDs, from_id = 11, to_id = 18, column = columns)
    copy_id_value(improvements_IDs, from_id = 20, to_id = 21, column = columns)
    copy_id_value(improvements_IDs, from_id = 24, to_id = 30, column = columns)
    copy_id_value(improvements_IDs, from_id = 25, to_id = 31, column = columns)
    copy_id_value(improvements_IDs, from_id = 27, to_id = 29, column = columns)
    copy_id_value(improvements_IDs, from_id = 27, to_id = 32, column = columns)
    copy_id_value(improvements_IDs, from_id = 37, to_id = 38, column = columns)
    copy_id_value(improvements_IDs, from_id = 40, to_id = 41, column = columns)
    copy_id_value(improvements_IDs, from_id = 59, to_id = 61, column = columns)
    copy_id_value(improvements_IDs, from_id = 60, to_id = 62, column = columns)
  }
  
  fwrite(improvements_IDs,"output/tables/improvements_complete.csv") # Output file
  
  ## COMPUTE COST OF RETROFITTING ----------------------------------------------
  # Load EPC dataset
  master_dataset <- as.data.table(fread("data/cleaned/master_epcs.csv"))
  
  # Cleaning: Identify columns starting with "ID_"
  cols_to_remove <- grep("^ID_", names(master_dataset), value = TRUE)
  
  # Remove the identified columns
  master_dataset[, (cols_to_remove) := NULL]

  # By group transaction date and unique ID, keep observations with latest inspection date
  master_dataset <- master_dataset[master_dataset[, .I[which.max(INSPECTION_DATE)], by = .(UPRN)]$V1]
  
  # Count how many properties can raise their EPCs
  print(paste("UPRNs able to raise their EPC:", master_dataset[DIFF_ENERGY_EFFICIENCY != 0, .N]))
  
  ## BACK OF THE ENVELOPPE ESTIMATION (underestimated) -------------------------
  # Compute pound per epc when improvements are possible (i.e. linear estimation of cost of raising EPC) to avoid dividing by 0
  master_dataset[DIFF_ENERGY_EFFICIENCY > 0, POUND_PER_EPC := sapply(INDICATIVE_COST_AVG, sum_values)/DIFF_ENERGY_EFFICIENCY]

  # EPC thresholds for cost estimation of retrofitting
  # A: >= 92 ; B: 81-91 ; C: 69-80 ; D: 55-68 ; E: 39-54 ; F: 21-38 ; G: 01-20

  for (EPC_cap in c(21,39,55,69,81,92)) {
    master_dataset[, COST_OF_REACHING_EPC := NA_real_] # Initialize the new column as numeric
    retrofitable_props <- sum(master_dataset$DIFF_ENERGY_EFFICIENCY > 0 & EPC_cap > master_dataset$CURRENT_ENERGY_EFFICIENCY) # count how many properties can improve their EPCs


    # Compute the maximum possible rise in EPC between either the threshold and POTENTIAL_ENERGY_EFFICIENCY
    master_dataset[EPC_cap > CURRENT_ENERGY_EFFICIENCY, RISE_IN_EPC := ifelse(POTENTIAL_ENERGY_EFFICIENCY > EPC_cap,EPC_cap - CURRENT_ENERGY_EFFICIENCY,DIFF_ENERGY_EFFICIENCY)]

    # Compute cost of reaching the EPC_cap (or getting as close as possible if not reachable)
    master_dataset[DIFF_ENERGY_EFFICIENCY > 0 & EPC_cap > CURRENT_ENERGY_EFFICIENCY, COST_OF_REACHING_EPC := RISE_IN_EPC  * POUND_PER_EPC ]

    # Summing costs in COST_OF_REACHING_EPC (either when costs are positive or diff non zero)
    sum_positive <- round(master_dataset[COST_OF_REACHING_EPC > 0, sum(COST_OF_REACHING_EPC)]/10^9,3) # in billions with 3 decimals
    print(paste("RETROFIT COST TO",EPC_cap,": b£", sum_positive,"for",retrofitable_props,"properties"))
  }
  
  ## ESTIMATION WHICH INCLUDES EACH IMPACT PER IMPROVEMENT ---------------------
  EPC_cap <- 69 # Set an EPC cap/threshold to cross
  
  # Load complete results
  improvements_IDs <- fread("output/tables/improvements_complete.csv") # load the complete IDs
  
  # Compute RISE_IN_EPC (if negative then CURRENT_ENERGY_EFFICIENCY > EPC_cap)
  master_dataset[,RISE_IN_EPC := ifelse(POTENTIAL_ENERGY_EFFICIENCY > EPC_cap,EPC_cap - CURRENT_ENERGY_EFFICIENCY,DIFF_ENERGY_EFFICIENCY)]
  
  # Remove observations where RISE_IN_EPC <= 0
  master_dataset <- master_dataset[RISE_IN_EPC > 0]
  
  # Truncature for testing only
  #master_dataset <- master_dataset[1:1000]
  
  # Helper function to split IDs
  split_ids <- function(ids) {
    as.numeric(unlist(strsplit(ids, ",")))
  }
  
  # Pre-split the IDs once
  split_ids_list <- lapply(master_dataset$IMPROVEMENTS_IDs, split_ids)
  
  # Function to sum impacts based on IDs until rise_in_epc is crossed and return both the sum and count
  sum_EPC_until_cap <- function(ids, impacts_table, rise_in_epc) {
    # Return zero if rise_in_epc is lower than zero
    # if (rise_in_epc <= 0) {
    #   return(list(total_impact = 0, count = 0))
    # }
    
    impacts <- impacts_table[IMPROVEMENT_ID %in% ids, .(IMPACT_EPC = as.numeric(IMPACT_EPC))]
    impacts <- impacts[!is.na(IMPACT_EPC)]
    
    total_impact <- 0
    count <- 0
    
    for (impact in impacts$IMPACT_EPC) {
      total_impact <- total_impact + impact
      count <- count + 1
      if (total_impact >= rise_in_epc) {
        break
      }
    }
    
    return(list(total_impact = total_impact, count = count))
  }
  
  # Apply the function to each row and store the first value in ESTIMATED_IMPACT2 and the second value in N_RETROFITS
  results <- mapply(function(ids, rise_in_epc) sum_EPC_until_cap(ids, improvements_IDs, rise_in_epc), split_ids_list, master_dataset$RISE_IN_EPC)
  
  
  # Store the results in master_dataset
  N_retrofits <- paste0("N_RETROFITS_",EPC_cap)
  master_dataset[, (paste0("REACH_EPC_",EPC_cap)) := results[1, ]]
  master_dataset[, (N_retrofits) := results[2, ]]
  
  # # Function to sum costs based on preprocessed IDs for first N_retrofits
  # sum_impacts_cap <- function(ids_split, impact_column, n_retrofits, impacts_table) {
  #   # Return 0 if n_retrofits is zero
  #   # print(paste("n_retrofits:", n_retrofits))
  #   # if (is.na(n_retrofits) || is.nan(n_retrofits) || n_retrofits == 0) {
  #   #   return(0)
  #   # }
  #   
  #   sum(sapply(ids_split[1:n_retrofits], function(ids) {
  #     # Subset the table based on the improvement IDs
  #     subset_table <- impacts_table[impacts_table$IMPROVEMENT_ID %in% ids, ]
  #     
  #     # Convert the impact column to numeric
  #     impact_values <- as.numeric(subset_table[[impact_column]])
  #     
  #     # Sum the impact values with na.rm = TRUE
  #     sum(impact_values, na.rm = TRUE)
  #   }))
  # }
  
  # Function to sum costs based on preprocessed IDs for first N_retrofits
  sum_impacts_cap <- function(ids_split, impact_column, n_retrofits, impacts_table) {
    # Convert the impacts_table to a data.table if it is not already
    # if (!is.data.table(impacts_table)) {
    #   impacts_table <- as.data.table(impacts_table)
    # }
    
    # Concatenate the first n_retrofits IDs into a single vector
    all_ids <- unlist(ids_split[1:n_retrofits])
    
    # Subset the impacts_table based on all_ids
    subset_table <- impacts_table[IMPROVEMENT_ID %in% all_ids]
    
    # Sum the impact values, ensuring conversion to numeric and handling NAs
    total_sum <- sum(as.numeric(subset_table[[impact_column]]), na.rm = TRUE)
    
    return(total_sum)
  }
  
  # Select impacts to be computed
  impact_names_2 <- c(impact_names, "RETROFIT_COST")
  
  # Precompute the number of retrofits
  N_retrofits_values <- master_dataset[[N_retrofits]]
  
  # Sum all remaining impacts until EPC cap is met
  for (columns in impact_names_2) {
    print(paste("Computing:",columns))
    columns_d <- paste(columns, EPC_cap, sep = "_")
    
    # Compute the new column values in a vectorized way
    new_column_values <- mapply(function(ids, n_retrofits) {
      sum_impacts_cap(ids, columns, n_retrofits, improvements_IDs)
    }, split_ids_list, N_retrofits_values)
    
    # Assign the new column values to the master_dataset
    master_dataset[, (columns_d) := new_column_values]
    #master_dataset[, (columns_d) := mapply(function(ids,impact_column,n_retrofits ) sum_impacts_cap(ids, impact_column, n_retrofits,improvements_IDs), split_ids_list, columns, get(N_retrofits))]
  }
  
  fwrite(master_dataset,paste0("data/cleaned/master_cap_",EPC_cap,".csv")) # Output file
  
  # second algorithm can prioritise improvement a) most impactful and/or b) least costly (assuming the list provided is suboptimal according to regressions)
  # RQ: order of improvement on your EPC record!!
  

} # end if on cost_of_retrofit


# DESCRIBE DATA ----------------------------------------------------------------
if (descriptive == 1 ) {
  
  # MASTER DATASET -------------------------------------------------------------
  # Load dataset
  master_dataset <- as.data.table(fread("data/cleaned/master_dataset_all.csv"))
  
  # Add EPC dummy variables initialized to 0
  print(paste("max EPC score: ",max(master_dataset$CURRENT_ENERGY_EFFICIENCY),sep=""))
  total_EPC_d <- 100 # cutoff at 100
  EPC_BIN_NAMES <- paste0("EPC_", 1:total_EPC_d) 
  master_dataset[, (EPC_BIN_NAMES) := 0]
  
  # Efficient update: Use a binary join to set dummies
  for (epc_d in 1:total_EPC_d) {
    dummy_col <- paste0("EPC_", epc_d)
    master_dataset[CURRENT_ENERGY_EFFICIENCY == epc_d, (dummy_col) := 1]
  }
  master_dataset[CURRENT_ENERGY_EFFICIENCY >= total_EPC_d, (dummy_col) := 1] # force max EPC_d to 1 from cutoff
  
  # Count the number of observations for each EPC bin
  obs_counts <- master_dataset %>%
    group_by(CURRENT_ENERGY_EFFICIENCY) %>%
    summarise(n_obs = n())
  
  ## Plot all EPC scores -------------------------------------------------------
  formula <- as.formula(paste("PRICE ~ 0", paste(EPC_BIN_NAMES, collapse = " + "), sep="+"))
  
  # Extract coefficients and standard errors
  coefficients <- summary(lm(formula, data = master_dataset))$coefficients
  coeffs_std <- data.frame(
    EPC_BIN_NAMES = rownames(coefficients),
    Estimate = coefficients[, "Estimate"],
    Std.Error = coefficients[, "Std. Error"],
    cluster = cluster_se
  )
  
  # Compute CI boundaries
  coeffs_std <- coeffs_std %>%
    mutate(se_low = Estimate - CI_level*Std.Error,
           se_high = Estimate + CI_level*Std.Error)
  
  # Exclude rows matching pattern_controls
  coeffs_std[!grepl(pattern = pattern_controls, rownames(coeffs_std)), ]
  
  # Filter out only coefficients for predictor variables starting with "EPC_"
  coeffs_std <- coeffs_std[grep("^EPC_", rownames(coeffs_std)), ]
  
  # # Add number of observations to coeffs_std data frame
  # coeffs_std <- coeffs_std %>%
  #   left_join(obs_counts, by = c("EPC_BIN_NAMES" = "CURRENT_ENERGY_EFFICIENCY"))
  
  # Plot regression coefficients with standard errors
  epc_plot <- ggplot(coeffs_std, aes(x = 1:nrow(coeffs_std), y = Estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = se_low, ymax = se_high), width = 0.2) +
    labs(title = "Regression Coefficients with Standard Errors",
         x = "EPC scores",
         y = "Property Prices") +
    theme_minimal()
  plot(epc_plot) # print plot
  
  # Save the plot
  ggsave("output/plots/epc_plot_1.png", plot = epc_plot, width = 8, height = 6, dpi = 300)
  
  # Overlay the density plot on the regression plot
  final_plot <- epc_plot +
    geom_density(data = master_dataset[master_dataset$CURRENT_ENERGY_EFFICIENCY <= 100, ], 
                 aes(x = as.numeric(CURRENT_ENERGY_EFFICIENCY), y = ..count.. / max(..count..) * max(coeffs_std$Estimate)), 
                 fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "Density (scaled)"))
  print(final_plot) # print plot
  
  # Save the plot
  ggsave("output/plots/epc_plot_2.png", plot = final_plot, width = 8, height = 6, dpi = 300)
  rm(epc_plot,final_plot,oefficients,coeffs_std,obs_counts) # free memory
  
  ### SOME MORE STATS
  
  table(master_dataset$CURRENT_ENERGY_RATING)
  
  master_dataset[, .(count = .N), by = CURRENT_ENERGY_RATING]
  
  descriptives <- master_dataset[, .(name = mean(PRICE)), by = .(CURRENT_ENERGY_EFFICIENCY, PROPERTY_TYPE)]
  
  print(descriptives, n = 400)
  
  # Descriptive Stats Quick Look
  table(master_dataset$PROPERTY_TYPE)
  table(master_dataset$TRANSACTION_TYPE)
  table(master_dataset$CONSTRUCTION_AGE_BAND)
  table(master_dataset$MAIN_FUEL)
  uniqueN(master_dataset$BUILDING_REFERENCE_NUMBER)
  
  # Density plot
  ggplot(master_dataset, aes(x = CURRENT_ENERGY_EFFICIENCY)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = "Density Plot of Current Energy Efficiency",
         x = "Current Energy Efficiency",
         y = "Density") +
    theme_minimal()
  
} # end descriptive stats
