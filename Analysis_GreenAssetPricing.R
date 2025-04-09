
# ---------------------------- SET UP ---------------------------------------
# Import Libraries -------------------------------------------------------------
library(conflicted)
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
library(sgd)
library(zoo)
library(did)
library(lubridate) #to work with dates 
library(ggplot2)
library(MatchIt)
library(Matching)
library(rbounds)
library(fixest)

library(AER)
library(caret)
library(splines)
library(spdep)

library(e1071) # to save in latex tables 
library(xtable)
library(stargazer)
library(kableExtra)

library(car)

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

# # Function to keep occurrences larger than 1
# keep_occurrences <- function(data, var1, occurrence) {
#   setDT(data)
#   n_occur <- data[, .N, by = var1][N > occurrence]
#   data <- data[var1 %in% n_occur$var1]
#   rm(n_occur)
#   return(data)
# }

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
  if (id %in% c(2,3)) return(1)
  if (id ==9) return(10)
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

# Load master data --------------------------------------------------------------------
setwd('/Users/charlottewargniez/Desktop/GreenPremium')
master_dataset <- as.data.table(fread('data/cleaned/master_dataset_all.csv'))

# Cleaning: Identify columns starting with "EPC_"
cols_to_remove <- grep("^EPC_", names(master_dataset), value = TRUE)

# Remove the identified columns
master_dataset[, (cols_to_remove) := NULL]
rm(cols_to_remove)

# Drop transactions without Inspection Date or UPRN
master_dataset  <- master_dataset [!is.na(UPRN)]
master_dataset  <- master_dataset [!is.na(INSPECTION_DATE)]
print(paste(length(unique(master_dataset$UPRN)), "unique UPRNs found"))

# # keep repeat transactions (i.e. houses which transacted several times)
# master_dataset <- keep_occurrences(master_dataset,master_dataset$UPRN,1)
# print(paste(length(unique(master_dataset$UPRN)), "unique UPRNs eith at least 2 transactions kept"))

master_dataset[,XT_SET2 :=YEAR*100+MONTH]

# Load Deflator 
deflator <- as.data.table(fread('data/RPI_deflator.csv'))
deflator <- deflator[, .(YEAR, MONTH, `From 2019-Jan`)]
deflator[,XT_SET2 :=YEAR*100+MONTH]

# Merge Cumul with master dataset
master_dataset <- merge(x = master_dataset, y = deflator, by = "XT_SET2", all.x = T)

master_dataset[, REAL_PRICE := PRICE/(`From 2019-Jan`+1)] #estimate inflated controlled prices 

# Remove houses in top 1% of price
initial_count <- nrow(master_dataset)
price_threshold <- quantile(master_dataset$REAL_PRICE, 0.99) ## Calculate the threshold for the top 1% of PRICE
master_dataset <- master_dataset[REAL_PRICE <= price_threshold] # Filter out rows
final_count <- nrow(master_dataset) # Calculate the number of observations after filtering
removed_count <- initial_count - final_count # Calculate the number of observations removed

# Sort master_dataset
master_dataset <- master_dataset[order(UPRN,TRANSACTION_DATE,INSPECTION_DATE),]




# Global Variables --------------------------------------------
master_dataset[, price_diff := REAL_PRICE - shift(REAL_PRICE), by = UPRN]
master_dataset[, EPC_diff := CURRENT_ENERGY_EFFICIENCY - shift(CURRENT_ENERGY_EFFICIENCY), by = UPRN]


# Break up postcode in master-dataset to extract district code
master_dataset[, pcu_district := sub(" .*", "", POSTCODE_EPC)]
master_dataset[, pcu_district := as.factor(pcu_district)]

# REGRESSION PARAMETERS
controls <- "EXTENSION_COUNT + PROPERTY_TYPE + TOTAL_FLOOR_AREA + CONSTRUCTION_AGE_BAND + MAIN_FUEL_f + NUMBER_HABITABLE_ROOMS + ESTATE "
time_fe <- "YEAR_f + MONTH_f"
cluster_se <- c("BOROUGH") # Clustered Standard Errors: choose among TOWN, BOROUGH, COUNTY (GREATER LONDON, includes all E09)
pattern_controls <- "MAIN_FUEL|YEAR|MONTH|PROPERTY|CONSTRUCTION|ESTATE|EXTENSION|NUMBER_HABITABLE|TOTAL_FLOOR"
bandwidth <- 5 # Bandwidth Selection for Regression Discontinuity Design
CI_level <- 1.96 # Confidence Interval level




# Additional Controls --------------------------------------------
  ## Energy Efficiency ------------------------------------------
  
  # Recode the values and assign to the new column
  SELECT_ENERGY <- c("HOT_WATER_ENERGY_EFF","WINDOWS_ENERGY_EFF","WALLS_ENERGY_EFF","ROOF_ENERGY_EFF","MAINHEAT_ENERGY_EFF","MAINHEATC_ENERGY_EFF","LIGHTING_ENERGY_EFF")  # empty: "SHEATING_ENERGY_EFF"
  
  for (columns in SELECT_ENERGY) {
    master_dataset[, (columns) := factor(na.exclude(get(columns)), levels = c("Very Poor", "Poor", "Average", "Good", "Very Good"))]
  }
  
#Create numerical representation of efficiency 
  for (columns in SELECT_ENERGY) {
    columns_d <- paste(columns, "_f", sep = "")
    master_dataset[, (columns_d) := ifelse(get(columns) == "Very Poor", -2,
                                           ifelse(get(columns) == "Poor", -1,
                                                  ifelse(get(columns) == "Average", 0,
                                                         ifelse(get(columns) == "Good", 1,
                                                                ifelse(get(columns) == "Very Good", 2, NA)))))]
    master_dataset[, (columns_d) := as.numeric(as.character(get(columns_d)))] # Ensure the new column is numeric
  }
  
  SELECT_ENERGY_f <- paste(SELECT_ENERGY,"_f",sep="") # keep track of extra-columns
  
  for (columns in SELECT_ENERGY_f) {
    print(paste("cleaning", columns))
    columns_d <- paste(columns, "_diff", sep = "")
    master_dataset[, (columns_d) := get(columns) - shift(get(columns)), by = UPRN]
    master_dataset[, (columns_d) := ifelse(get(columns_d) != 0, 1, 0)] # Assign 1 if the difference is anything but 0
  }
  
  # Find all covariates that end with "_EFF_f_diff"
  covariates_diff <- grep("_EFF_f_diff$", names(master_dataset), value = TRUE)
  covariates_diff <- paste(covariates_diff, collapse = "+")
  
  # Find all covariates that end with "_EFF_f" #So only the actual vlues 
  covariates <- grep("_EFF_f$", names(master_dataset), value = TRUE)
  covariates <- paste(covariates, collapse = "+")
  
  ## Improvement IDs --------------------------------------------
  
  # Separate improvements with comma ","
  master_dataset <- master_dataset %>%
    mutate(IMPROVEMENTS_IDs = gsub("\\|", ",", IMPROVEMENTS_IDs))
  master_dataset[, IMPROVEMENTS_IDs_list := lapply(strsplit(IMPROVEMENTS_IDs, ","), as.numeric)] # Split IMPROVEMENTS_IDs into a list of numeric vectors

  # Replace Improvement IDs based on duplicates
  master_dataset <- master_dataset %>%
    mutate(IMPROVEMENTS_IDs_list = lapply(IMPROVEMENTS_IDs_list, function(x) sapply(x, replace_improvement_id)))

  # Create dummy variables for all unique IDs at once
  unique_ids <- unique(unlist(master_dataset$IMPROVEMENTS_IDs_list)) # Find all unique IDs
  master_dataset[, paste0("ID_", unique_ids) := lapply(unique_ids, function(id) {
    sapply(IMPROVEMENTS_IDs_list, function(x) as.integer(id %in% x))
  })]
  
  # Identify missing IDs 
  dummy_columns <- paste0("ID_", unique_ids)
  gc()
  master_dataset[, (dummy_columns) := lapply(.SD, function(col) {
    # Calculate the difference between the current and previous row
    diff_col <- col - shift(col, type = "lag", fill = 0)
    # Multiply by -1 and convert -1 to 1 (indicating a missing ID)
    col <- diff_col * -1
    as.integer(col == 1)
  }), by = UPRN, .SDcols = dummy_columns]
  
  
  # Save Dummy IDs columns
  covariates2 <- grep("ID_", names(master_dataset), value = TRUE)
  covariates2 <- paste(covariates2, collapse = " + ")
  
  
  # # Create a column for each unique ID and assign dummy 
  # unique_ids <- unique(unlist(master_dataset$IMPROVEMENTS_IDs_list)) # Find all unique IDs
  # for (id in unique_ids) {
  #   master_dataset[, paste0("ID_", id) := sapply(IMPROVEMENTS_IDs_list, function(x) as.integer(id %in% x))]
  # }
  # 
  # # Identify missing IDs
  # for (id in unique_ids) {
  #   column_name <- paste0("ID_", id)
  #   
  #   # Create the dummy variable for each ID
  #   master_dataset[, (column_name) := sapply(IMPROVEMENTS_IDs_list, function(x) as.integer(id %in% x))]
  #   
  #   # Identify missing IDs by checking the lagged difference within each UPRN
  #   master_dataset[, (column_name) := {
  #     diff_col <- shift(get(column_name), type = "lag", fill = 0)
  #     as.integer(diff_col == 1 & get(column_name) == 0)
  #   }, by = UPRN]
  # }
    
  # # Create the new data.table including all controls and the improvement IDs
  # keep_columns <- c("CURRENT_ENERGY_EFFICIENCY", "price_diff", "REAL_PRICE", "EPC_diff", "EXTENSION_COUNT", "PROPERTY_TYPE", "POSTCODE", "TOTAL_FLOOR_AREA", 
  #               "CONSTRUCTION_AGE_BAND", "MAIN_FUEL", "NUMBER_HABITABLE_ROOMS")
  # 
  # separated_improvements <- master_dataset[, 
  #                                          c(.SD, .(IMPROVEMENT_ID = unlist(strsplit(IMPROVEMENTS_IDs, ",")))), 
  #                                          by = .(UPRN, TRANSACTION_DATE), 
  #                                          .SDcols = keep_columns
  # ]
  
              # # Compute values only for the necessary rows
              # filtered_indices <- which(master_dataset$change_EPC == 1)
              # master_dataset[filtered_indices, MISSING_IDs := {
              #   prev_ids <- master_dataset$IMPROVEMENTS_IDs[.I-1]
              #   curr_ids <- master_dataset$IMPROVEMENTS_IDs[.I]
              #   unmatches <- mapply(find_unmatched_positions, prev_ids, curr_ids)
              #   unmatched <- unlist(lapply(unmatches, function(x) prev_ids[x]))
              #   unmatched
              # }, by = .I]
              # 
              # # Initialize columns for IDs from 1 to 63
              # for (id in 1:63) {
              #   col_name <- paste0("ID_", id)
              #   master_dataset[, (col_name) := 0]  # Initialize the column with 0
              # }
              # cols_nonexistent<- paste0("ID_", 51:55)
              # existing_cols <- names(master_dataset)
              # cols_to_remove_existing <- intersect(existing_cols, cols_nonexistent)
              # master_dataset[, (cols_to_remove_existing ) := NULL]
              # 
              # # Update columns based on MISSING_IDs
              # for (i in filtered_indices) {
              #   missing_ids <- unlist(strsplit(master_dataset[i, MISSING_IDs], ","))
              #   for (id in missing_ids) {
              #     if (!is.na(id) && id != "") {  # Check if the id is not NA and not an empty string
              #       col_name <- paste0("ID_", id)
              #       master_dataset[i, (col_name) := 1]
              #     }
              #   }
              # }
              # 
              # # Define relationships between dummies
              # relationships <- list(
              #   c(from_id = "ID_11", to_ids = c("ID_12","ID_13","ID_14","ID_15","ID_17","ID_18")),
              #   c(from_id = "ID_20", to_ids = c("ID_21")),
              #   c(from_id = "ID_30", to_ids = c("ID_24")),
              #   c(from_id = "ID_25", to_ids = c("ID_31")),
              #   c(from_id = "ID_27", to_ids = c("ID_29", "ID_32")),
              #   c(from_id = "ID_37", to_ids = c("ID_38")),
              #   c(from_id = "ID_40", to_ids = c("ID_41")),
              #   c(from_id = "ID_59", to_ids = c("ID_61")),
              #   c(from_id = "ID_60", to_ids = c("ID_62"))
              # )
              # 
              # # Apply the function to propagate values
              # propagate_values(master_dataset, relationships)
              # 
  
  
  ## Save as new file! (Avoid loosing time) ----------------------
  master_dataset[, IMPROVEMENTS_IDs_list := NULL]
  fwrite(master_dataset, "data/cleaned/master_dataset_regression.csv") #Output File
  
# --------------------  DESCRIPTIVE STATISTICS -------------------------------
  
  # Filter the dataset to exclude NA and INVALID EPC ratings
  filtered_dataset <- master_dataset %>%
    dplyr::filter(!is.na(CURRENT_ENERGY_RATING) & CURRENT_ENERGY_RATING != "INVALID")
  
# Figure 1 a- Price Distribution Analysis -------------------------------------
  
  # Filter the dataset to exclude NA and INVALID EPC ratings
  library(dplyr)
  filtered_dataset <- master_dataset %>%
    filter(!is.na(CURRENT_ENERGY_RATING) & CURRENT_ENERGY_RATING != "INVALID!")
  
  # Calculate average price by EPC rating and property type
  summary_stats <- master_dataset %>%
    group_by(CURRENT_ENERGY_RATING, PROPERTY_TYPE_EPC) %>%
    summarize(avg_price = mean(REAL_PRICE, na.rm = TRUE), .groups = 'drop')
  
  # Pivot the data to wide format
  summary_stats_wide <- summary_stats %>%
    pivot_wider(
      names_from = PROPERTY_TYPE_EPC,
      values_from = avg_price,
      names_prefix = "PropertyType_"
    )
  property_types <- unique(summary_stats$PROPERTY_TYPE_EPC)
  
  # Save the table to LaTeX
  kable(summary_stats_wide, format = "latex", booktabs = TRUE,
        col.names = c("EPC Rating", property_types)) %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    save_kable("output/tables/price_by_epc&property_type.tex")
  
# Figures 2a-c UPRN Transaction Histogram --------------------------------------

# Group by UPRN and count the number of transactions per UPRN
transaction_counts <- master_dataset[, .(transaction_count = .N), by = UPRN]

# Count the occurrences of each transaction count
occurrence_counts <- transaction_counts[, .(N = .N), by = transaction_count]

# 2.a. Number of Occurrences (net UPRN)
      # Plot the histogram
      plot_2a <- ggplot(occurrence_counts, aes(x = factor(transaction_count), y = N)) +
        geom_bar(stat = "identity") +
        xlab('Number of Transactions') +
        ylab('Number of UPRNs') +
        ggtitle('Histogram of Observations per Transaction Count (UPRNs)') +
        theme_minimal()
      # Save output graph (Grpah 2.b)
      ggsave(paste0("output/plots/Figure_2.a",".png"), plot_2a)

# 2.b . Number of Occurrences (Percentage)
      # Calculate the total number of UPRNs
      total_uprns <- sum(occurrence_counts$N)
      # Calculate the percentage of each occurrence
      occurrence_counts[, percentage := (N / total_uprns) * 100]
      # Plot the histogram
      plot_2b <- ggplot(occurrence_counts, aes(x = factor(transaction_count), y = percentage)) +
        geom_bar(stat = "identity") +
        xlab('Number of Transactions') +
        ylab('Percentage of UPRNs') +
        ggtitle('Histogram of Observations per Transaction Count (Percentage)') +
        theme_minimal()
      # Save output graph (Grpah 2.b)
      ggsave(paste0("output/plots/Figure_2.b",".png"), plot_2b)

# 2.c. Number of Occurrences (Log Transformation)
    #Add 1 to avoid log(0) 
    occurrence_counts[, N_log := log(N + 1)]
    # Plot the histogram
    plot_2c <- ggplot(occurrence_counts, aes(x = transaction_count, y = N_log)) +
      geom_col() +
      xlab('Number of Transactions') +
      ylab('Number of UPRNs (log scale)') +
      ggtitle('Histogram of Transaction Counts per UPRN') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # Save output graph (Grpah 2.b)
    ggsave(paste0("output/plots/Figure_2.c",".png"), plot_2c)

# Table. 2.a. Frequency Table 
    # Descriptive statistics
    mean_transactions <- mean(transaction_counts$transaction_count)
    median_transactions <- median(transaction_counts$transaction_count)
    mode_transactions <- as.numeric(names(sort(table(transaction_counts$transaction_count), decreasing=TRUE)[1]))
    range_transactions <- range(transaction_counts$transaction_count)
    variance_transactions <- var(transaction_counts$transaction_count)
    sd_transactions <- sd(transaction_counts$transaction_count)
    skewness_transactions <- e1071::skewness(transaction_counts$transaction_count)
    kurtosis_transactions <- e1071::kurtosis(transaction_counts$transaction_count)
    quantiles_transactions <- quantile(transaction_counts$transaction_count, probs = c(0.25, 0.5, 0.75))
    iqr_transactions <- IQR(transaction_counts$transaction_count)
    # Display results
    list(
      mean = mean_transactions,
      median = median_transactions,
      mode = mode_transactions,
      range = range_transactions,
      variance = variance_transactions,
      sd = sd_transactions,
      skewness = skewness_transactions,
      kurtosis = kurtosis_transactions,
      quantiles = quantiles_transactions,
      iqr = iqr_transactions,
      total_properties = total_uprns
    )
    # Organize results into a data frame
    statistics_df <- data.frame(
      Statistic = c("Mean", "Median", "Mode", "Range", "Variance", "Standard Deviation", 
                    "Skewness", "Kurtosis", "25th Percentile", "50th Percentile (Median)", 
                    "75th Percentile", "Interquartile Range", "Total Properties"),
      Value = c(mean_transactions, median_transactions, mode_transactions, 
                paste(range_transactions, collapse = " - "), variance_transactions, 
                sd_transactions, skewness_transactions, kurtosis_transactions, 
                quantiles_transactions[1], quantiles_transactions[2], 
                quantiles_transactions[3], iqr_transactions, total_uprns)
    )
    # Convert the data frame to a LaTeX table
    latex_table <- xtable(statistics_df, caption = "Descriptive Statistics of Property Transactions", label = "tab:descriptive_stats")
    # Save the LaTeX table to a file
    print(latex_table, file = "output/tables/Table2a_descriptive_statistics.tex", include.rownames = FALSE)
    
    
    
    
    
# Figures 3-a-c Frequency and Timing of Transactions ------------------------
    ## Time between transactions --------------------
    master_dataset[, time_between_transactions := difftime(TRANSACTION_DATE, shift(TRANSACTION_DATE), units = "days"), by = UPRN]
    
    # Clean for all transaction time diff = 0 (expect for first observation for each UPRN)
    master_dataset <- master_dataset[time_between_transactions != 0 | is.na(time_between_transactions)]
    
     # Calculate average and median time between transactions
    avg_time_between_transactions <- master_dataset[, mean(time_between_transactions, na.rm = TRUE)]
    median_time_between_transactions <- master_dataset[, median(time_between_transactions, na.rm = TRUE)]
    
    # Histogram of Time Between Transactions
    library(ggplot2)
    plot_3a <- ggplot(master_dataset, aes(x = time_between_transactions)) +
      geom_histogram(binwidth = 30, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(title = "Distribution of Time Between Transactions",
           x = "Time Between Transactions (Days)", y = "Frequency") +
      theme_minimal()
    ggsave("output/plots/Figure3a_timebtwtransactions.png", plot = plot_3a)
    
    # Calculate mean and standard deviation of time between transactions by Property Type
    stats <- master_dataset[, .(avg_time = mean(time_between_transactions, na.rm = TRUE),
                                sd_time = sd(time_between_transactions, na.rm = TRUE)),
                            by = PROPERTY_TYPE_EPC]
    
    # Plotting average time and standard deviation by Property Type
    plot_3d <- ggplot(stats, aes(x = PROPERTY_TYPE_EPC, y = avg_time)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.7) +
      geom_errorbar(aes(ymin = avg_time - sd_time, ymax = avg_time + sd_time), width = 0.2, color = "orange") +
      labs(title = "Time Between Transactions by Property Type",
           x = "Property Type", y = "Average Time Between Transactions (Days)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save the plot
    ggsave("output/plots/Figure3d_timebtwtransactions_by_propertytype.png", plot = plot_3d)
    
    # Boxplot of Time Between Transactions by EPC Change
    filtered_dataset <- master_dataset[!is.na(time_between_transactions)] # remove first transaction (since no time and don;t know if EPC change)
    filtered_dataset <- filtered_dataset[TOWN != "LONDON"] # remove london 
    boxplot3a <- ggplot( filtered_dataset, aes(x = factor(change_EPC), y = time_between_transactions)) +
      geom_boxplot(fill = "lightgreen", color = "darkgreen") +
      labs(title = "Time Between Transactions by EPC Change",
           x = "EPC Improvement (1 = Yes, 0 = No)", y = "Time Between Transactions (Days)") +
      theme_minimal()
    ggsave("output/plots/Boxplot_transactions.png", plot = boxplot3a)
    
    # Time between transactions by price range 
    master_dataset[, PRICE_RANGE := cut(REAL_PRICE, 
                                        breaks = c(0, 100000, 200000, 300000, 400000, 500000, Inf),
                                        labels = c("0-100k", "100k-200k", "200k-300k", "300k-400k", "400k-500k", "500k+"))]
    
    # Step 2: Calculate mean and standard deviation for REAL_PRICE and time_between_transactions by PRICE_RANGE
    summary_stats <- master_dataset[, .(
      avg_price = mean(REAL_PRICE, na.rm = TRUE),
      sd_price = sd(REAL_PRICE, na.rm = TRUE),
      avg_time = as.numeric(mean(time_between_transactions, na.rm = TRUE)),
      sd_time = as.numeric(sd(time_between_transactions, na.rm = TRUE))
    ), by = PRICE_RANGE]
    
    # Step 3: Print or save the summary table
    summary_stats_long <- summary_stats %>%
      pivot_longer(
        cols = c(avg_price, sd_price, avg_time, sd_time),
        names_to = "Statistic",
        values_to = "Value"
      )
    
    # Create a wide format data frame for LaTeX output
    summary_stats_wide <- summary_stats_long %>%
      pivot_wider(
        names_from = c(Statistic),
        values_from = Value
      )
    
    # Optionally, save the table to a Latex 
    kable(summary_stats_wide, format = "latex", booktabs = TRUE,
          col.names = c("Price Range", "Average Price", "Price Std Dev", 
                        "Average Time Between Transactions", "Time Std Dev")) %>%
      kable_styling(latex_options = c("striped", "hold_position")) %>%
      save_kable("output/tables/summary_time_by_price.tex")
    
    ## Time between transaction by property type and EPC ---------
    
    # Calculate average time between transactions by EPC rating and property type
    summary_stats <- filtered_dataset %>%
      group_by(CURRENT_ENERGY_RATING, PROPERTY_TYPE_EPC) %>%
      summarize(avg_time = mean(time_between_transactions, na.rm = TRUE), .groups = 'drop')
    
    # Get and sort unique property types
    property_types <- sort(unique(summary_stats$PROPERTY_TYPE_EPC))
    
    # Pivot the data to wide format
    summary_stats_wide <- summary_stats %>%
      pivot_wider(
        names_from = PROPERTY_TYPE_EPC,
        values_from = avg_time
      )
    
    # Rename columns to ensure proper order
    colnames(summary_stats_wide) <- c("EPC Rating", property_types)
    
    # Save the table to LaTeX
    kable(summary_stats_wide, format = "latex", booktabs = TRUE) %>%
      kable_styling(latex_options = c("striped", "hold_position")) %>%
      save_kable("output/tables/time_by_epc&property.tex")

    ## Transaction Counts by Year/Quarter --------------------
    
    # Extract year and quarter from transaction date
    master_dataset[, `:=`(year = year(TRANSACTION_DATE), 
                          quarter = quarter(TRANSACTION_DATE))]
    
    # Count transactions by year
    transactions_by_year <- master_dataset[, .N, by = year]
    
    # Count transactions by year and quarter
    transactions_by_quarter <- master_dataset[, .N, by = .(year, quarter)]
    
    # Time Series Plot of Transaction Counts by Year/Quarter
    plot_3b <- ggplot(transactions_by_quarter, aes(x = interaction(year, quarter), y = N)) +
      geom_line(group = 1, color = "blue") +
      geom_point(color = "red") +
      labs(title = "Transaction Counts by Year and Quarter",
           x = "Year-Quarter", y = "Number of Transactions") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave("output/plots/Figure3b_transactionfreq.png", plot = plot_3b)
    
    ## Time between Transactions and Price Change ------------------
    plot_3c <- ggplot(filtered_dataset, aes(x = time_between_transactions, y = price_diff)) +
      geom_point(alpha = 0.5, color = "blue") +
      labs(title = "Time Between Transactions vs. Price Change",
           x = "Time Between Transactions (Days)", y = "Price Change (£)") +
      theme_minimal()
    ggsave("output/plots/Figure3c_timingNprice.png", plot = plot_3c)
    
    # Calculate the mean and standard deviation of price_diff for each time_between_transactions
    aggregated_data <- filtered_dataset %>%
      group_by(time_between_transactions) %>%
      summarise(mean_price_diff = mean(price_diff, na.rm = TRUE),
                sd_price_diff = sd(price_diff, na.rm = TRUE))
    # Plotting mean and standard deviation
    plot_3d <- ggplot(aggregated_data, aes(x = time_between_transactions)) +
      geom_line(aes(y = mean_price_diff), color = "blue") +
      geom_ribbon(aes(ymin = mean_price_diff - sd_price_diff, ymax = mean_price_diff + sd_price_diff), 
                  fill = "blue", alpha = 0.2) +
      labs(title = "Time Between Transactions vs. Price Change",
           x = "Time Between Transactions (Days)", 
           y = "Mean Price Change (£) ± SD") +
      theme_minimal()
    ggsave("output/plots/Figure3d_SDtimingNprice.png", plot = plot_3d)
    
    
# -------------------------- REGRESSIONS -------------------------------------
    ## Clean Main Fuels -------
    fuel_mapping <- as.data.table(fread('data/cleaned/fuel_mapping.csv')) #load map 
    setnames(fuel_mapping, "ORIGINAL", "MAIN_FUEL")
    master_dataset <- merge(x= master_dataset, y = fuel_mapping, by = "MAIN_FUEL", all.x = TRUE)
    setnames(master_dataset, "REPLACEMENT", "MAIN_FUEL_f")
    rm(fuel_mapping)
    
    ## Clean Construction_AGE_BAND -------
    
    # Regular expression pattern for valid year ranges
    year_range_pattern <- "England and Wales: \\d{4}-\\d{4}"
    
    # Regular expression pattern for single years
    single_year_pattern <- "^\\d{4}$"
    
    # Regular expression pattern for 'onwards'
    onwards_pattern <- "England and Wales: \\d{4} onwards"
    
    # Define year ranges for single years
    year_ranges <- list(
      "2021" = "2021-2022",
      "2020" = "2019-2020",
      "2019" = "2019-2020",
      "2018" = "2017-2018",
      "2017" = "2017-2018",
      "2016" = "2015-2016",
      "2015" = "2015-2016",
      "2014" = "2013-2014",
      "2013" = "2013-2014",
      "2012" = "2012-2022",
      "2011" = "2007-2011",
      "2010" = "2007-2010",
      "2007" = "2007-2022",
      "2004" = "2003-2006",
      "2002" = "2001-2002",
      "1876" = "1876-1880"  # Adjust as necessary
    )
    
    # Clean the CONSTRUCTION_AGE_BAND column
    year_range_pattern <- "^England and Wales: \\d{4}-\\d{4}$"
    onwards_pattern <- "^England and Wales: \\d{4} onwards$"
    
    # Clean the CONSTRUCTION_AGE_BAND column
    master_dataset[, CONSTRUCTION_AGE_BAND := ifelse(
      grepl(year_range_pattern, CONSTRUCTION_AGE_BAND), 
      sub("England and Wales: ", "", CONSTRUCTION_AGE_BAND), 
      ifelse(
        grepl(onwards_pattern, CONSTRUCTION_AGE_BAND), 
        paste0(sub("England and Wales: ", "", gsub(" onwards", "", CONSTRUCTION_AGE_BAND)), "-2022"), 
        ifelse(
          CONSTRUCTION_AGE_BAND %in% names(year_ranges),
          year_ranges[CONSTRUCTION_AGE_BAND],
          NA
        )
      )
    )]
    
    #Convert to factor 
    master_dataset[, CONSTRUCTION_AGE_BAND := as.factor(CONSTRUCTION_AGE_BAND)]
    
    # Sort master_dataset
    master_dataset <- master_dataset[order(UPRN,TRANSACTION_DATE)]
    
    # # Create interaction terms 
    # cov_list <- c("EXTENSION_COUNT", "PROPERTY_TYPE", "TOTAL_FLOOR_AREA", "CONSTRUCTION_AGE_BAND", "MAIN_FUEL", "NUMBER_HABITABLE_ROOMS", "ESTATE", "rooms_diff") #change a appropriate 
    # for (cov in cov_list) { 
    #   master_dataset[, paste0(cov, "_EPC_diff") := as.numeric(EPC_diff) * as.numeric(get(cov))]
    # }
    # interactions <- grep("_EPC_diff", names(master_dataset), value = TRUE)
    # interactions <- paste(interactions, collapse = " + ")
    
    
    
    
    
   
    
    
# Set up ---- 
setwd('/Users/charlottewargniez/Desktop/GreenPremium')
master_dataset <- as.data.table(fread('data/cleaned/master_dataset_regression.csv'))

master_dataset_unique <- master_dataset[!duplicated(master_dataset[, c("UPRN", "XT_SET2")]), ]
master_dataset_unique <- as.data.table(master_dataset_unique)

master_dataset_unique[ ,CONSTRUCTION_AGE_BAND := as.character(CONSTRUCTION_AGE_BAND)] #comment if computer poweful enough to process
master_dataset_unique[ ,MAIN_FUEL_fD := as.character(MAIN_FUEL_f)]
master_dataset_unique[, TOTAL_FLOOR_AREA := as.numeric(TOTAL_FLOOR_AREA)]

# Convert the data to a pdata.frame
pdata <- pdata.frame(master_dataset_unique, c("UPRN", "XT_SET2"))



# Table 3. First Difference Regressions -----------------------------

# Calculate First Difference Parameters 
master_dataset[, rooms_diff := NUMBER_HABITABLE_ROOMS - shift(NUMBER_HABITABLE_ROOMS), by = UPRN]
master_dataset[, area_diff := TOTAL_FLOOR_AREA - shift(TOTAL_FLOOR_AREA), by = UPRN]
master_dataset[, date_diff := (as.numeric(TRANSACTION_DATE) - as.numeric(shift(TRANSACTION_DATE))) / 365.25, by = UPRN]

# Find all covariates that end with "_EFF_f_diff"
covariates_diff <- grep("_EFF_f_diff$", names(master_dataset), value = TRUE)
covariates_diff <- paste(covariates_diff, collapse = "+")

# FInd all Improvement IDs changes 
covariates2 <- grep("ID_", names(master_dataset), value = TRUE)
covariates2 <- paste(covariates2, collapse = " + ")

# Formulas of first difference regressions to Run 
diff_EPC <- lm(price_diff ~ EPC_diff + date_diff + area_diff, 
                  data = master_dataset, 
                  panel.id = c("UPRN", "XT_SET2"))

diff_Eff <- lm(as.formula(paste("price_diff ~ date_diff + area_diff +", covariates_diff)), 
                  data = master_dataset, 
                  panel.id = c("UPRN", "XT_SET2"))

diff_ID <- lm(as.formula(paste("price_diff ~ YEAR_f + area_diff +", covariates2)), 
                 data = master_dataset, 
                 panel.id = c("UPRN", "XT_SET2"))


# Upload Results 
formula_count <- 1 
for (results in reg_output_list_1) {
  row_names <- row.names(reg_output_list_1[[formula_count]])
  coefficients <- reg_output_list_1[[formula_count]]['Estimate']
  p_values <- reg_output_list_1[[formula_count]][4]
  
  results_frame <- cbind(row_names, coefficients, p_values)
  fwrite(results_frame,paste0("output/tables/reg_output_forumla_",formula_count,".csv")) # Output file
  
  formula_count <- formula_count +1
}

# Merge Regressions Results of Improvements with Costs 
# !! NEED TO ADJUST !!
improvements_costs <- read.csv("output/tables/improvements_complete.csv")
coefficients <- reg_output_list_1[[1]]['Estimate'] #Adjust [[i]] depending on the formulas out of comment 
p_values <- reg_output_list_1[[1]][4]
combine_dataset <- cbind(improvements_costs, coefficients, p_values)

fwrite(combine_dataset,"output/tables/improvements_reg_output.csv") # Output file


# Run the TSLS 
tsls_model <- ivreg(price_diff~ EPC_diff | HOT_WATER_ENERGY_EFF_f_diff+WINDOWS_ENERGY_EFF_f_diff+WALLS_ENERGY_EFF_f_diff+ROOF_ENERGY_EFF_f_diff+MAINHEAT_ENERGY_EFF_f_diff+MAINHEATC_ENERGY_EFF_f_diff+LIGHTING_ENERGY_EFF_f_diff, data = master_dataset)
# Display the summary of the TSLS 
summary(tsls_model)







# Table 4. Plm Regressions --------------------------------

#Controls (independent variables)
    controls <- "TOTAL_FLOOR_AREA + PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + BUILT_FORM + CONSTRUCTION_AGE_BAND + MAIN_FUEL_f"

# # Run Simple Models (Price ~ EPC) 
# model_1a <- plm(REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY, data = master_dataset, index = c("UPRN", "XT_SET2"), model = "pooling") #OLS
# model_2a <- plm(REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY, data = master_dataset, index = c("UPRN", "XT_SET2"), model = "within") #Fixed Effects Model
# model_3a <- plm(REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY, data = master_dataset, index = c("UPRN", "XT_SET2"), model = "random") #Random Effects Model

  ## Simple Models (no time fixed effects) -------------------
    # Run simple model with controls (Price ~ EPC + controls)
    # formula_b <- as.formula(paste("REAL_PRICE ~ 0 + CURRENT_ENERGY_EFFICIENCY + ", controls))
    # #model_1b <- plm(formula_b, data = pdata, index = c("UPRN", "XT_SET2"), model = "pooling")
    # model_2b <- plm(formula_b, data = pdata, model = "within")
    # #model_3b <- plm(formula_b, data = pdata, index = c("UPRN", "XT_SET2"), model = "random")

# Load interest rates
interest_rates <- fread("data/IRLTLT01GBM156N.csv")
setnames(interest_rates,"IRLTLT01GBM156N","base_rate")
interest_rates[, date_to_merge := as.character(DATE)]
interest_rates[, date_to_merge := substr(date_to_merge, 1, nchar(date_to_merge) - 3)]
interest_rates[, DATE := NULL]

#adjust date to merge with interest rates
master_dataset$date_to_merge <- format(as.Date(master_dataset$TRANSACTION_DATE), "%Y-%m")

#merge with interest rates 
master_dataset <- merge(x = master_dataset, y = interest_rates, by = "date_to_merge", all.x = TRUE)

    model_2b <- feols(REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY | TOTAL_FLOOR_AREA + PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + BUILT_FORM + CONSTRUCTION_AGE_BAND , data = master_dataset, panel.id=c("UPRN", "XT_SET2"))
    model_2b_fixed <- feols(REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY | TOTAL_FLOOR_AREA + PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + BUILT_FORM + CONSTRUCTION_AGE_BAND + YEAR_f + MONTH_f, data = master_dataset, panel.id=c("UPRN", "XT_SET2"))
    model_2b_interest <- feols(REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY | TOTAL_FLOOR_AREA + PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + BUILT_FORM + CONSTRUCTION_AGE_BAND  + base_rate, data = master_dataset, panel.id=c("UPRN", "XT_SET2"))
    
    # # Prepare data for kable
    # model_2b_summary <- summary(model_2b)
    # 
    # # Save the table (Fixed Effects Model) to a LaTeX file
    # latex_table <- model_2b_summary %>%
    #   kbl(format = "latex", booktabs = TRUE, 
    #       caption = paste("Regression Results: Simple Fixed Effects Model"),
    #       row.names = FALSE) %>%
    #   kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
    #   add_header_above(c("Variable" = 1, "Coefficient" = 1, "Standard Error" = 1, "t Value" = 1, "p Value" = 1))
    # writeLines(latex_table, "output/tables/hedonic_m1_results.tex")
    # 
    # # Hausman test between fixed and random effects 
    #  phtest(model_2b, model_3b)
    # 
    # # F-test 
    # pFtest(model_2b, model_1b) #investigates significance of fixed effects 

    texreg(
      list(model_2b, model_2b_fixed),  # List of models
      custom.model.names = c("Model 1", "Model2"),  # Custom label for the model
      caption = "Regression Results: Hedonic Price Model with and without Fixed Effects",
      label = "tab:hedonic_model",
      booktabs = TRUE,
      use.packages = FALSE
    )

  ## Model with Time Fixed Effects ----------------
    time_fe <- "YEAR_f + MONTH_f"
    
    formula_fixed <- as.formula(paste("REAL_PRICE ~ CURRENT_ENERGY_EFFICIENCY + ", controls, "+", time_fe))
    #model_1fixed <- plm(formula_fixed, data = pdata, index = c("UPRN", "XT_SET2"), model = "pooling") #OLS
    model_2fixed <- plm(formula_fixed, data = pdata, model = "within") #Fixed Effects
    #model_3fixed <- plm(formula_fixed, data = pdata, index = c("UPRN", "XT_SET2"), model = "random") #Random 
    
    # Prepare data for kable
    model_2fixed_summary <- tidy(model_2fixed)

    # Format the p-value column for precision
    model_2fixed_summary$p.value <- format.pval(model_2fixed_summary$p.value, scientific = TRUE)
    
    # Save the table (Fixed Effects Model) to a LaTeX file
    latex_table <- model_2fixed_summary %>%
      kbl(format = "latex", booktabs = TRUE, 
          caption = paste("Regression Results: Hedonic price Model (ref) with Yearly and Monthly Fixed Effects"),
          row.names = FALSE) %>%
      kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
    add_header_above(c("Variable" = 1, "Coefficient" = 1, "Standard Error" = 1, "t Value" = 1, "p Value" = 1))
    writeLines(latex_table, "output/tables/hedonic_m2_results.tex")

    
  ## Model with Energy Efficiency ----------------
    
    #Formula with and without time fixed effects 
    SELECT_ENERGY <- c("HOT_WATER_ENERGY_EFF","WINDOWS_ENERGY_EFF","WALLS_ENERGY_EFF","ROOF_ENERGY_EFF","MAINHEAT_ENERGY_EFF","MAINHEATC_ENERGY_EFF","LIGHTING_ENERGY_EFF")  # empty: "SHEATING_ENERGY_EFF"
    SELECT_ENERGY_f <- paste(SELECT_ENERGY,"_f",sep="") # keep track of extra-columns
    
    # Assuming 0 is the middle factor level (reference is average)
    master_dataset[, (SELECT_ENERGY_f) := lapply(.SD, relevel, ref = "0"), .SDcols = SELECT_ENERGY_f]
    
    # Create the dynamic part of the formula
    energy_vars <- paste(SELECT_ENERGY_f, collapse = " + ")
    
    # Combine with the rest of the formula
    full_formula <- paste("price_diff ~", energy_vars, "| TOTAL_FLOOR_AREA + PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + BUILT_FORM + CONSTRUCTION_AGE_BAND + YEAR_f + MONTH_f")
    
    # Convert to formula
    model_formula <- as.formula(full_formula)
    
    # Run the fixed effects model
    model_EFF_fixed <- feols(model_formula, data = master_dataset, panel.id = c("UPRN", "XT_SET2"))
    
    model_EFF <- feols(REAL_PRICE ~ energy_vars| TOTAL_FLOOR_AREA + PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + BUILT_FORM + CONSTRUCTION_AGE_BAND , data = master_dataset, panel.id=c("UPRN", "XT_SET2"))
    
    ## factor for different types of change ----------------
    
    # Assuming the dataset is ordered by UPRN and time (XT_SET2)
    setorder(master_dataset, UPRN, TRANSACTION_DATE)
    
    # Loop through each SELECT_ENERGY_f variable and create a new column to track changes
    for (var in SELECT_ENERGY_f) {
      change_var <- paste0(var, "_change")
      
      # Calculate the change between the current and previous row
      master_dataset[, (change_var) := shift(.SD, type = "lag", fill = NA), by = UPRN, .SDcols = var]
      
      # Flag the specific changes
      master_dataset[, (change_var) := paste(shift(get(var), type = "lag"), "to", get(var)), by = UPRN]
    }
    
    # Define the transition labels as a named vector
    transition_labels <- c(
      "-2 to -1" = "very poor to poor",
      "-1 to 0" = "poor to average",
      "0 to 1" = "average to good",
      "1 to 2" = "good to very good",
      "2 to 1" = "very good to good",
      "1 to 0" = "good to average",
      "0 to -1" = "average to poor",
      "-1 to -2" = "poor to very poor"
    )
    
    # Loop through each SELECT_ENERGY_f variable and create a new column to track changes
    for (var in SELECT_ENERGY_f) {
      change_var <- paste0(var, "_change")
      
      # Calculate the change and label the transition in one step
      master_dataset[, (change_var) := factor(
        transition_labels[paste(shift(get(var), type = "lag", fill = NA), "to", get(var))],
        levels = transition_labels
      ), by = UPRN]
    }
    
    # Optionally, set levels explicitly if needed
    for (var in SELECT_ENERGY_f) {
      change_var <- paste0(var, "_change")
      master_dataset[, (change_var) := factor(get(change_var), levels = names(transition_labels))]
    }
    
    # CREATIVE WAY OF ACHEIVING SAME THING 
    
    # Define the mapping from factor levels to the new values
    factor_mapping <- c("-2" = 3, "-1" = 8, "0" = 15, "1" = 24, "2" = 35)
    
    # Loop through each SELECT_ENERGY_f variable and create a corresponding change column
    for (var in SELECT_ENERGY_f) {
      change_var <- paste0(var, "_change")
      
      # Apply the mapping and assign to the new column
      master_dataset[, (change_var) := factor_mapping[as.character(get(var))]]
    }
    
    # Identify all columns that end with "_change"
    change_columns <- grep("_change$", names(master_dataset), value = TRUE)
    
    # Loop through each _change column and calculate the difference between rows
    for (col in change_columns) {
      diff_var <- paste0(col, "_diff")  # Create a new column name for the difference
      
      # Calculate the difference between current and previous row
      master_dataset[, (diff_var) := get(col) - shift(get(col), type = "lag", fill = NA), by = UPRN]
      print("finished")
    }
    
    # # Define the mapping from numeric values to labels
    # diff_label_mapping <- c(
    #   "5" = "Very poor to Poor",
    #   "7" = "Poor to Average",
    #   "9" = "Average to Good",
    #   "11" = "Good to Very Good"
    # )
    # 
    # # Loop through each _diff column and apply the label mapping
    # for (col in change_columns) {
    #   diff_var <- paste0(col, "_diff")  # The existing _diff column
    #   
    #   # Apply the mapping: if the value is matched, replace it with the label; otherwise, set to NA
    #   master_dataset[, (diff_var) := factor(diff_label_mapping[as.character(get(diff_var))], 
    #                                         levels = c("Very poor to Poor", "Poor to Average", "Average to Good", "Good to Very Good"))]
    #   print("finished", diff_var)
    # }
    
    # Identify all columns that end with "_diff"
    diff_columns <- grep("change_diff$", names(master_dataset), value = TRUE)
    
    #set as a factor 
    for (col in diff_columns) {
      master_dataset[, (col) := factor(get(col))]
    }
    
    # Create the dynamic part of the formula
    diff_vars <- paste(diff_columns, collapse = " + ")
    
    # Combine with the rest of the formula
    full_formula <- paste("price_diff ~", diff_vars, "| TOTAL_FLOOR_AREA + PROPERTY_TYPE + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + BUILT_FORM + CONSTRUCTION_AGE_BAND + YEAR_f + MONTH_f")
    
    # Convert to formula
    model_formula <- as.formula(full_formula)
    
    # Run the fixed effects model
    model_EFF_fixed <- feols(model_formula, data = master_dataset, panel.id = c("UPRN", "XT_SET2"))
    
# Table 5. Diff in Diff Regressions -------------------------------
    ## Simple Models (no time fixed effects) -------------------
   
     #Convert to factors
    master_dataset[, post_treat := as.factor(post_treat)]
    master_dataset[, treat := as.factor(treat)]
    master_dataset[, post := as.factor(post)]
    master_dataset[, PROPERTY_TYPE := as.factor(PROPERTY_TYPE)]
    
    # OLS with Diff-in-diff and interactions: PRICE ~ post_treat:PROPERTY_TYPE + treat:PROPERTY_TYPE + post:PROPERTY_TYPE + post_treat + treat + post + controls
    formula <- as.formula(paste("REAL_PRICE ~ post_treat:PROPERTY_TYPE + treat:PROPERTY_TYPE + post:PROPERTY_TYPE + post_treat + treat + post", time_fe, sep="+"))
    model_DID <- lm(formula, data = master_dataset)
    
    #summary of model 
    model_summary <- tidy(model_DID)

    # Save the table (time fe) to a LaTeX file
    latex_table <-  model_summary %>%
      kbl(format = "latex", booktabs = TRUE, 
          caption = paste("DID Regression: post_treat and post"),
          row.names = FALSE) %>%
      kable_styling(latex_options = c("striped", "hold_position", "scale_down"))
    add_header_above(c("Variable" = 1, "Coefficient" = 1, "Standard Error" = 1, "t Value" = 1, "p Value" = 1))
    writeLines(latex_table, "output/tables/Table5_DID_model1.tex")
    
    
    
    
# Table 6.Regression Discontinuity Design -------------------------------
    
    # RDD with EPC cutoff values corresponding to bands
    # A: >= 92 ; B: 81-91 ; C: 69-80 ; D: 55-68 ; E: 39-54 ; F: 21-38 ; G: 01-20
    # ESTATE: "F", "L"  OR PROPERTY_TYPE: "D","F","O","S","T
    library(rddtools)
    library(rdmulti)
    library(rdrobust)
    
    controls <- "EXTENSION_COUNT + PROPERTY_TYPE + TOTAL_FLOOR_AREA + CONSTRUCTION_AGE_BAND + MAIN_FUEL_f + NUMBER_HABITABLE_ROOMS + ESTATE"
    
    # Specify Variable 
    price_y <- as.numeric(master_dataset$REAL_PRICE)
    EPC_x <- as.numeric(master_dataset$CURRENT_ENERGY_EFFICIENCY)
    cut_off <- c(21,39,55,69,81,92)
    Z <- cbind(master_dataset$PROPERTY_TYPE, master_dataset$TOTAL_FLOOR_AREA)
      
    # Estimate the RDD model with the specified bandwidth
    rdd_model_1 <- rdms(price_y,EPC_x,cut_off, h=c(10,10,10,10,10,10))
    gc()
    rdd_model_2 <- rdms(price_y,EPC_x,cut_off, h=c(5,5,5,5,5,5))
    # Plot result 
    rdd_plot <- rdmcplot(price_y,EPC_x,cut_off, h=c(5,5,5,5,5,5,5))
    
    master_dataset <- master_dataset[CURRENT_ENERGY_EFFICIENCY <= 100]
    bandwidth <- c(10, 5) # You can adjust bandwidth as needed
    
    # Ensure the dataset is correctly formatted
    master_dataset <- master_dataset[CURRENT_ENERGY_EFFICIENCY <= 100]
    cut_off <- c(21, 39, 55, 69, 81, 92)
    bandwidth <- c(10, 5) # You can adjust bandwidth as needed
    
    # Ensure the dataset is correctly formatted
    master_dataset <- master_dataset[CURRENT_ENERGY_EFFICIENCY <= 100]
    cut_off <- c(21, 39, 55, 69, 81, 92)
    bandwidth <- c(10, 5) # You can adjust bandwidth as needed
    
    # Initialize a list to store models
    rdd_models_interaction <- list()
    
    # Loop through each cutoff and apply the regression model with interaction term
    for (i in seq_along(cut_off)) {
      for (bw in bandwidth) {
        # Subset the data within the bandwidth around the current cutoff
        subset_data <- master_dataset[abs(CURRENT_ENERGY_EFFICIENCY - cut_off[i]) <= bw, ]
        
        # Define the treatment indicator (1 if above cutoff, 0 if below)
        subset_data$treatment <- ifelse(subset_data$CURRENT_ENERGY_EFFICIENCY >= cut_off[i], 1, 0)
        
        # Check the data structure to ensure all required columns are present
        if (!all(c("REAL_PRICE", "CURRENT_ENERGY_EFFICIENCY", "treatment",
                   "EXTENSION_COUNT", "PROPERTY_TYPE", "TOTAL_FLOOR_AREA", 
                   "CONSTRUCTION_AGE_BAND", "MAIN_FUEL_f", 
                   "NUMBER_HABITABLE_ROOMS", "ESTATE") %in% names(subset_data))) {
          stop("Required columns missing from the subset data")
        }
        
        # Create the formula with interaction term
        formula <- as.formula("REAL_PRICE ~ treatment * CURRENT_ENERGY_EFFICIENCY + EXTENSION_COUNT + 
                           PROPERTY_TYPE + TOTAL_FLOOR_AREA + CONSTRUCTION_AGE_BAND + 
                           MAIN_FUEL_f + NUMBER_HABITABLE_ROOMS + ESTATE")
        
        # Try to fit the model and catch potential errors
        tryCatch({
          rdd_model <- lm(formula, data = subset_data)
          
          # Store the model in the list
          model_name <- paste0("rdd_model_interaction_cutoff_", cut_off[i], "_bw_", bw)
          rdd_models_interaction[[model_name]] <- rdd_model
        }, error = function(e) {
          message("Error fitting model: ", e)
        })
      }
    }
    
    # Example of how to access a specific model
    if ("rdd_model_interaction_cutoff_21_bw_10" %in% names(rdd_models_interaction)) {
      summary(rdd_models_interaction[["rdd_model_interaction_cutoff_21_bw_10"]])
    } else {
      message("Model rdd_model_interaction_cutoff_21_bw_10 not found.")
    }
    
    
    
    
    # Optional: Plot results for each model (this is just an example for one cutoff)
    rdd_plot <- rdplot(price_y, EPC_x, c=cut_off[1], bw=bandwidth[1])
    
    # Initialize an empty list to store the results
    results_list <- list()
    
    # Loop through each model and extract relevant statistics
    for (model_name in names(rdd_models)) {
      model <- rdd_models[[model_name]]
      
      # Extract the coefficients, standard errors, t-values, and p-values
      coefs <- coef(summary(model))
      
      # Only keep the coefficient of interest (EPC_x_subset)
      epc_coef <- coefs["EPC_x_subset", ]
      
      # Extract cutoff and bandwidth from the model name
      cutoff_info <- strsplit(model_name, "_")[[1]]
      cutoff <- cutoff_info[4]
      bandwidth <- cutoff_info[6]
      
      # Store the extracted information in a list
      results_list[[model_name]] <- c(Cutoff = cutoff,
                                      Bandwidth = bandwidth,
                                      Estimate = epc_coef["Estimate"],
                                      StdError = epc_coef["Std. Error"],
                                      tValue = epc_coef["t value"],
                                      pValue = epc_coef["Pr(>|t|)"])
    }
    
    # Convert the list to a data frame
    results_df <- do.call(rbind, results_list)
    
    # Transpose the data frame to have cutoffs as columns
    results_df <- as.data.frame(t(results_df))
    
    # Set the row names as the first column for better readability
    results_df <- cbind(Statistic = rownames(results_df), results_df)
    
    # Display the table
    print(results_df)
    
    
# Table 7. Exploration Controls -------------------------------
    
# Table 8. Log Price Regressions -------------------------------
list_of_formulas_3 <- list(
  formula_1 <- as.formula("log(REAL_PRICE) ~ 0 + log(CURRENT_ENERGY_EFFICIENCY)"),
  formula_2 <- as.formula("REAL_PRICE ~ 0 + CURRENT_ENERGY_EFFICIENCY + TRANSACTION_DATE + TOTAL_FLOOR_AREA"), 
  formula_3 <- as.formula("REAL_PRICE ~ 0 + change_EPC + TRANSACTION_DATE + TOTAL_FLOOR_AREA"),
  formula_4 <- as.formula(paste("REAL_PRICE ~ 0 + TRANSACTION_DATE + TOTAL_FLOOR_AREA", covariates, sep="+")),
  formula_6 <- as.formula(paste("REAL_PRICE ~ 0 + TRANSACTION_DATE + TOTAL_FLOOR_AREA", covariates2, sep="+"))
)

# Run the extended series of regressions
reg_output_list_2 <- lapply(list_of_formulas_2, function(formula) {
  run_regression(formula, master_dataset, cluster_se, pattern_controls)
})
# price vs x
# 
# log price vs x
# 
# log price vs log(EPC_score) # 





library(dplyr)

list_of_formulas_4 <- list(
  as.formula("log(REAL_PRICE) ~ 0 + log(CURRENT_ENERGY_EFFICIENCY)")
  #as.formula("log(REAL_PRICE) ~ 0 + log(CURRENT_ENERGY_EFFICIENCY) + TRANSACTION_DATE + ln(TOTAL_FLOOR_AREA)")
  #as.formula("log(REAL_PRICE) ~ 0 + log(change_EPC) + TRANSACTION_DATE + log(TOTAL_FLOOR_AREA)"),
  #as.formula(paste("log(REAL_PRICE) ~ 0 + TRANSACTION_DATE + log(TOTAL_FLOOR_AREA) +", EFF_covariates)),
  #as.formula(paste("log(REAL_PRICE) ~ 0 + TRANSACTION_DATE + log(TOTAL_FLOOR_AREA) +", covariates2))
)

# Run the extended series of regressions
reg_output_list_4 <- lapply(list_of_formulas_4, function(formula) {
  run_regression(formula, master_dataset, cluster_se, pattern_controls)
})

print(reg_output_list_4)


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

















