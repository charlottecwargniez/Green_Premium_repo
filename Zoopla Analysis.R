
# FOLDER ARCHITECTURE ----------------------------------------------------------
 
  # -FOLDER/
  #   |- Green Premium.r
  #   |- data/
  #   |     |- rent/      :: folder containing raw rent data
  #   |     |- sale/      :: folder containing raw sale data  
  #   |     |- cleaned/
  #   |       |- rent/      :: folder containing processed data
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
library(rlang)
library(fixest)

# Clear the environment
rm(list = ls())

# Set path
setwd('/Users/charlottewargniez/Desktop/GreenPremium')

# --------------------------- CONFIGURATION ------------------------------------


# Set flags to execute sections of code (0 no, 1 yes)
clean_zoopla <- FALSE
gen_zoopla_all <- FALSE
compute_active <- FALSE
predictive <- FALSE
elasticities <- TRUE

# ------------------------ GLOBAL VARIABLES ------------------------------------

# Analysis parameter
rolling_window <- 30
grouping_var <- "pcu_area"

# File directions
rent_files <- length(list.files("data/rent",full.names=TRUE)) # pattern="*.csv"
numCores <- detectCores()  # example: mclapply(1:10, function(x) x^2, mc.cores = numCores)
desktop_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop") # Get the desktop path

# REGRESSION PARAMETERS
controls <- "property_type + num_bed_max + num_floors_max + num_bathrooms_max + num_receptions_max + pcu_area"
controls_EPC <- "PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + num_floors_max + pcu_area"

time_fe <- "zoopla_year_f"
cluster_se <- c("pcu_district") # Clustered Standard Errors
pattern_controls <- "zoopla_year|pcu_area|property_type|num_bed_max|num_receptions_max|num_bathrooms_max|num_floors_max"
pattern_controls_EPC <- "zoopla_year|pcu_area|PROPERTY_TYPE|NUMBER_HABITABLE_ROOMS|num_floors_max"

bandwidth <- 5 # Bandwidth Selection for Regression Discontinuity Design
CI_level <- 1.96 # Confidence Interval level


# ------------------------ Helper Functions ------------------------------------

# Cleans a single address string by trimming, removing special characters, and converting to uppercase
clean_address <- function(address) {
  address %>%
    str_trim() %>% # Remove leading and trailing whitespace
    str_remove_all("[^a-zA-Z0-9, ]") %>% # Remove all special characters except commas and spaces
    str_to_upper() # Convert to uppercase
}

# Create a function to generate a sequence of years
generate_years <- function(min_date, max_date) {
  seq(from = as.numeric(min_date),
      to = as.numeric(max_date))
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

# -------------------------- Main Execution ------------------------------------

# CLEAN RENT AND SALE -------------------------------------------------------
if (clean_zoopla == 1) {
  
  for (folder in c("rent","sale")) { 
    print(folder)
    filenames <- list.files(paste("data/",folder,sep=""),full.names = FALSE) #pattern="*.csv" # find all folders
    
    # Loop through filenames and read the data
    for (i in seq_along(filenames)) {
      files <- filenames[i]
      print(paste(i, files))
      temp_df <- fread(paste0("data/", folder, "/", files)) # Load each file
      
      # Keep essential
      temp_df <- temp_df[, .(property_id,property_type,property_number,street_name, address, start_date, end_date, price_unique, price_first,price_last,price_min,price_max,price_flag,num_bed_max,num_floors_max,num_bathrooms_max,num_receptions_max,pcu,pcu_outcode,pcu_incode,pcu_district,pcu_sector,pcu_area,latitude,longitude,category,duration,zoopla_year,lad20cd,lad20nm)]
      
      # Clean columns
      temp_df <- temp_df |>
        mutate(
          property_number = clean_address(property_number),
          street_name = clean_address(street_name),
          address = clean_address(address),
          price_unique = clean_address(price_unique),
        )
      
      fwrite(temp_df,paste0("data/cleaned/",folder,"/",files) )
      rm(temp_df)
    } # end for on i
    
  } # end for folder
  
} # end if clean_zoopla


# GEN ALL ZOOPLA RENT AND SALE -------------------------------------------------
if (gen_zoopla_all) {

   for (folder in c("rent","sale")) { 
     print(folder)
     filenames <- list.files(paste("data/cleaned/",folder,sep=""),full.names = FALSE) #pattern="*.csv" # find all folders
     count <- 1 # Init  
     
     # Initialize list to store individual data frames
     list_of_dfs <- vector("list", length(filenames))
     
     # Loop through filenames and read the data
     for (i in seq_along(filenames)) {
       files <- filenames[i]
       print(paste(i, files))
       temp_df <- fread(paste0("data/cleaned/", folder, "/", files)) # Load each file
       list_of_dfs[[i]] <- temp_df
       rm(temp_df)
     }
     
     # Combine all data frames at once
     full_df <- rbindlist(list_of_dfs, use.names = TRUE, fill = TRUE)
     rm(list_of_dfs) # free memory
     
     # Keep the first observation by property_id and start_date
     full_df <- full_df[, .SD[1], by = .(property_id, start_date)]
     
     # Create a unique ID per property
     full_df[, row_index := sequence(.N), by = property_id]
     full_df[, zoopla_ID := paste0(property_id,"_",row_index)]
     full_df[, row_index := NULL]
     
     # Organise dataset
     full_df <- full_df[order(property_id,start_date,end_date),]
     
     # Create date to merge with actives
     #full_df[, date_to_merge := as.IDate(format(end_date, "%Y-%m-01"))]
     full_df[, date_to_merge := as.character(end_date)]
     full_df[, date_to_merge := substr(date_to_merge, 1, nchar(date_to_merge) - 3)]
     
     # Output file
     fwrite(full_df,paste("data/cleaned/zoopla_all_",folder,".csv",sep="") )
     rm(full_df) # free memory
     
   } # end for folder
  
} # end if gen_zoopla_all


# COMPUTE ACTIVE ---------------------------------------------------------------
if (compute_active) {
  
  for (folder in c("rent","sale")) { 
    print(folder)
    
    # Load data
    full_df <- fread(paste0("data/cleaned/zoopla_all_",folder,".csv"))
    
    # Select necessary for calculating monthly supply
    full_df <- full_df[, .(property_id,property_type, start_date, end_date,pcu_outcode,pcu_incode,pcu_district,pcu_sector,pcu_area,lad20cd)]
    
    # Define the start and end dates for the given time period
    period_start <- as.Date(min(full_df$start_date))
    period_end <- as.Date(max(full_df$end_date))
    
    # Create a sequence of dates within the time period
    date_sequence <- seq.Date(from = period_start, to = period_end, by = "day")
    
    # Initialize an empty data frame to store results
    results <- data.frame()
    
    # Loop through each date in the sequence
    for (current_date in date_sequence) {
      # Filter active listings at the current date
      active_listings <- full_df %>%
        filter(start_date <= current_date & end_date >= (current_date - rolling_window))
      
      # Count active listings by PCU_ZONE
      active_listings_count <- active_listings %>%
        group_by("pcu_area") %>%
        summarise(active_count = n()) %>%
        mutate(date = current_date)
      
      # Append the results to the final data frame
      results <- bind_rows(results, active_listings_count)
    }
    
    # Change format to date
    results$date <- as.Date(results$date)
    
    # Remove days
    results$date_to_merge <- as.character(results$date)
    results$date_to_merge <- substr(results$date_to_merg,1,nchar(results$date_to_merge) - 3)
    #results$date_to_merge <- gsub("-01","",results$date)
    
    # Compute lag by 1 month and group by !!sym(grouping_var)
    results <- results %>%
      group_by("pcu_area") %>%            
      mutate(active_L1 = shift(active_count, n = 1)) %>%  # Create a lagged column
      ungroup()  # Ungroup after operation
    
    setnames(results, "active_count", paste0("active_",folder))
    setnames(results, "active_L1", paste0("active_L1_",folder))
  
    # Output file
    fwrite(results,paste("data/cleaned/zoopla_active_",folder,".csv",sep="") )
    rm(results,active_listings_count,active_listings) # free memory
    
  } # end for folder
  
  # Clean addresses
  # convert lad20mm to upper case, keep first set of characters before space
  # cut property_number using comma delimter, keep first part as number, merge second part with street
  # cut address by comma delimiter, keep first part and remove town from street
  
} # end if compute_active

# PRICE PREDICTOR --------------------------------------------------------------
if (predictive) {
  
  for (folder in c("rent","sale")) { 
    print(folder)
    
    # Load data
    full_df <- fread(paste0("data/cleaned/zoopla_all_",folder,".csv"))
    print(paste(nrow(full_df), "observations"))
    table(full_df$property_type)
    
    # Keep essential
    full_df <- full_df[, .(property_id,property_type,price_last,num_bed_max,num_floors_max,num_bathrooms_max,num_receptions_max,pcu,pcu_outcode,pcu_incode,pcu_district,pcu_sector,pcu_area,zoopla_year,lad20cd,lad20nm)]
    
    # Create and apply filter on property types
    property_categories <- c("Flat", "Bungalow", "Detached house", "End terrace house", "Semi-detached house", "Semi-detached bungalow","Studio", "Terraced house", "Town house", "Maisonette")
    full_df <- full_df[property_type %in% property_categories]
    print(paste(nrow(full_df), "observations after property type filter"))
    
    # Recode PROPERTY_TYPE in line with master_dataset
    full_df[, PROPERTY_TYPE := ""]
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Detached house"| property_type == "Town house", "D", PROPERTY_TYPE)] # town house is similar for rentals
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Flat"| property_type == "Maisonette"| property_type == "Studio", "F", PROPERTY_TYPE)]
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Bungalow"| property_type == "Semi-detached bungalow", "O", PROPERTY_TYPE)]
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Semi-detached house"| property_type == "End terrace house", "S", PROPERTY_TYPE)]
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Terraced house", "T", PROPERTY_TYPE)]
    
    # compute per habitable rooms too and fill missing with linear number?
    
    # Drop observations where num_bathrooms_max is zero
    full_df <- full_df[num_bathrooms_max > 0]
    print(paste(nrow(full_df), "observations after non zero bathrooms filter"))
    
    # Compute total number of habitable rooms (+1 to include kitchen)
    full_df[, habitable_rooms := num_bed_max + num_receptions_max + 1]
    
    # Compute the 99th percentile
    percentile <- quantile(full_df$habitable_rooms, 0.99, na.rm = TRUE)
    
    # Remove observations where price_last is above the percentile
    full_df <- full_df[habitable_rooms <= percentile]
    print(paste(nrow(full_df), "observations after percentile"))
    
    # Compute total number of habitable rooms
    full_df[, price_over_size := 10*price_last/habitable_rooms]
    
    # Compute the average price_last by grouping by the specified columns
    # average_price_dt <- full_df[, .(average_price_over_size = mean(price_over_size, na.rm = TRUE)), 
    #                        by = .(pcu_area, PROPERTY_TYPE, zoopla_year)]
    
    #Calculate the average price_over_size and count the number of observations
    average_price_dt <- full_df[, .(
      average_price_over_size = round(mean(price_over_size, na.rm = TRUE)),
      count = .N
    ), by = .(pcu_area, PROPERTY_TYPE, zoopla_year)]
    
    # Output file
    fwrite(average_price_dt,paste0("data/cleaned/zoopla_",folder,"_predict.csv") )
    rm(full_df,average_price_dt) # free memory
    
  } # end for folder
  
} # end if predictive

# ELASTICITIES OF RENTS AND SALES -----------------------------------------------
if (elasticities) {
  
  # Load interest rates
  interest_rates <- fread("data/IRLTLT01GBM156N.csv")
  setnames(interest_rates,"IRLTLT01GBM156N","base_rate")
  interest_rates[, date_to_merge := as.character(DATE)]
  interest_rates[, date_to_merge := substr(date_to_merge, 1, nchar(date_to_merge) - 3)]
  interest_rates[, DATE := NULL]
  
  # Lag by 3-6months
  interest_rates <- interest_rates %>%         
    mutate(
      base_rate_L6 = shift(base_rate, n = 6),
      base_rate_L3 = shift(base_rate, n = 3)
      ) %>%  # Create a lagged column
    ungroup()  # Ungroup after operation
  
  for (folder in c("sale")) { 
    print(folder)
    
    # Load folder data
    full_df <- fread(paste0("data/cleaned/zoopla_all_",folder,".csv"))
    print(paste(nrow(full_df), "observations"))
    
    # Load main explanatory variables
    active_rents <- fread("data/cleaned/zoopla_active_rent.csv")
    active_sales <- fread("data/cleaned/zoopla_active_sale.csv")
    
    # Remove redundant dates
    active_rents[, date_to_merge := NULL]
    active_sales[, date_to_merge := NULL]

    # Merge with both actives
    full_df <- merge (full_df,active_rents, by.x = c("end_date", grouping_var),by.y = c("date",grouping_var),all.x = TRUE)
    full_df <- merge (full_df,active_sales, by.x = c("end_date", grouping_var),by.y = c("date",grouping_var),all.x = TRUE)
    
    # Merge with interest rates
    full_df <- merge (full_df,interest_rates, by = c("date_to_merge"),all.x = TRUE)
    
    # Remove not needed
    #full_df[, date_to_merge := NULL]
    rm(active_rents,active_sales)
    
    # Regression variables
    full_df[,price_last := price_last/100] # price in £k
    full_df[,price_first := price_first/100] # price in £k
    
    # Create logs
    full_df[, ln_price_last := log(price_last)]
    full_df[, ln_price_first := log(price_first)]
    full_df[, ln_price_diff := ln_price_last-ln_price_first]
    full_df[, ln_active_rent := log(active_rent)]
    full_df[, ln_active_sale := log(active_sale)]
    full_df[, ln_base_rate := log(base_rate)]
    full_df[, ln_duration := log(duration)]
    # Create factor variables
    full_df[,zoopla_year_f := factor(zoopla_year)]
    full_df[,month := substr(as.character(end_date),6,7)]
    #full_df[,month_f := factor(as.numeric(month))]
    
    # Compute total number of habitable rooms (+1 to include kitchen)
    full_df[, NUMBER_HABITABLE_ROOMS := num_bed_max + num_receptions_max + 1]
    
    # Cleaning
    full_df <- full_df[!is.na(ln_price_last)]
    full_df <- full_df[ln_price_last != -Inf]
    full_df <- full_df[!is.na(ln_duration)]
    full_df <- full_df[ln_duration != -Inf]
    
    # # Perform OLS (estimate all coeffs)
    # formula <- as.formula(paste("ln_price_last ~ base_rate + ln_active_rent + ln_active_sale + ln_duration", controls,time_fe,sep="+"))
    # reg_output <- lm(formula, data = full_df)
    # print(summary(reg_output))
    
    model <- feols(ln_price_last ~ base_rate + base_rate_L3 + base_rate_L3:duration + ln_active_sale + ln_duration | NUMBER_HABITABLE_ROOMS + property_type + pcu_area + zoopla_year + month, data = full_df)
    print(summary(model))
    
    # Create and apply filter on property types
    property_categories <- c("Flat", "Bungalow", "Detached house", "End terrace house", "Semi-detached house", "Semi-detached bungalow","Studio", "Terraced house", "Town house", "Maisonette")
    full_df <- full_df[property_type %in% property_categories]
    print(paste(nrow(full_df), "observations after property type filter"))
    
    # Recode PROPERTY_TYPE in line with master_dataset
    full_df[, PROPERTY_TYPE := ""]
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Detached house"| property_type == "Town house", "D", PROPERTY_TYPE)] # town house is similar for rentals
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Flat"| property_type == "Maisonette"| property_type == "Studio", "F", PROPERTY_TYPE)]
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Bungalow"| property_type == "Semi-detached bungalow", "O", PROPERTY_TYPE)]
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Semi-detached house"| property_type == "End terrace house", "S", PROPERTY_TYPE)]
    full_df[, PROPERTY_TYPE := ifelse(property_type == "Terraced house", "T", PROPERTY_TYPE)]
    
    # Perform OLS (estimate all coeffs)
    # formula <- as.formula(paste("ln_price_last ~ base_rate + ln_active_rent + ln_active_sale + ln_duration", controls_EPC,time_fe,sep="+"))
    # reg_output <- lm(formula, data = full_df)
    # print(summary(reg_output))
    
    model <- feols(ln_price_last ~ base_rate_L3 + ln_active_sale + ln_duration | NUMBER_HABITABLE_ROOMS + PROPERTY_TYPE + pcu_area + zoopla_year + month, data = full_df)
    print(summary(model))
    
    model <- feols(ln_price_last ~ base_rate_L3 + base_rate + ln_active_sale + ln_duration | NUMBER_HABITABLE_ROOMS + PROPERTY_TYPE + pcu_area + zoopla_year + month, data = full_df)
    print(summary(model))

    model <- feols(ln_price_last ~ base_rate + base_rate_L3 + base_rate_L3:duration + ln_active_sale + ln_duration | NUMBER_HABITABLE_ROOMS + PROPERTY_TYPE + pcu_area + zoopla_year + month, data = full_df)
    print(summary(model))
    
    model <- feols(ln_price_diff ~ base_rate + base_rate_L3 + base_rate_L3:duration + ln_active_sale + ln_duration + NUMBER_HABITABLE_ROOMS | PROPERTY_TYPE + pcu_area + zoopla_year + month, data = full_df)
    print(summary(model))
    
    # Look at price growth
    repeat_df <- keep_occurrences(full_df,full_df$property_id,1)
    repeat_df[, lag_ln_price := shift(ln_price_last, type = "lag", fill = NA), by = property_id]
    repeat_df[, ln_growth_price := ln_price_last - lag_ln_price]
    
    model <- feols(ln_growth_price ~ base_rate + base_rate_L3 + base_rate_L3:duration + ln_active_sale + ln_duration | NUMBER_HABITABLE_ROOMS + PROPERTY_TYPE + pcu_area + zoopla_year + month, data = repeat_df)
    print(summary(model))
    
    model <- feols(ln_growth_price ~ base_rate + base_rate_L3 + base_rate_L3:duration + ln_active_sale + ln_duration + NUMBER_HABITABLE_ROOMS| PROPERTY_TYPE + pcu_area + zoopla_year + month, data = repeat_df)
    print(summary(model))
    
    # immigration -> pas un GEM...
    
  } # end for folder
  
} # end if elasticities

# Output as LaTeX table
model_summary <- summary(model)
latex_table <-kable(model, format = "latex", booktabs = TRUE)
writeLines(latex_table, paste0("output/tables/elasticities_results.tex"))