
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

# Load Deflator 
deflator <- as.data.table(fread('data/RPI_deflator.csv'))
deflator <- deflator[, .(YEAR, MONTH, `From 2019-Jan`)]
deflator[,XT_SET2 :=YEAR*100+MONTH]

# Merge Cumul with master dataset
master_dataset[,XT_SET2 :=YEAR*100+MONTH]
master_dataset <- merge(x = master_dataset, y = deflator, by = "XT_SET2", all.x = T)

master_dataset[, REAL_PRICE := PRICE/(`From 2019-Jan`+1)] #estimate inflated controlled prices 

# Remove houses in top 1% of price
initial_count <- nrow(master_dataset)
price_threshold <- quantile(master_dataset$REAL_PRICE, 0.99) ## Calculate the threshold for the top 1% of PRICE
master_dataset <- master_dataset[REAL_PRICE <= price_threshold] # Filter out rows
final_count <- nrow(master_dataset) # Calculate the number of observations after filtering
removed_count <- initial_count - final_count # Calculate the number of observations removed

# Sort master_dataset
master_dataset <- master_dataset[order(UPRN,TRANSACTION_DATE,INSPECTION_DATE, ),]

#------------------------- CLEAN master_dataset ---------------------------------------
summary_table <- data.table()


# Get a list of unique EPC ratings
epc_ratings <- unique(master_dataset$CURRENT_ENERGY_RATING)

# Create an empty list to store the data tables
epc_tables <- list()

# Loop through each unique EPC rating and create a separate data table
for (rating in epc_ratings) {
  # Create a data table for each EPC rating
  epc_tables[[rating]] <- master_dataset[CURRENT_ENERGY_RATING == rating]
}

# Calculate metrics for each EPC rating
epc_ratings <- unique(master_dataset$CURRENT_ENERGY_RATING)
metrics_list <- list()


# Assuming master_dataset is already a data.table
# If it's not, convert it first:
# master_dataset <- data.table(master_dataset)

# Function to calculate the required metrics for each EPC rating
calculate_metrics <- function(dt) {
  total_homes <- uniqueN(dt$UPRN)  # Calculate the number of unique UPRNs
  
  # Property type distribution normalized
  property_type_distribution <- dt[, .N, by = PROPERTY_TYPE_EPC]
  property_type_distribution[, density := N / sum(N)]
  
  # Tenure distribution normalized
  tenure_distribution <- dt[, .N, by = TENURE]
  tenure_distribution[, density := N / sum(N)]
  
  list(
    avg_price = mean(dt$PRICE, na.rm = TRUE), # Calculate the average price
    total_homes = total_homes,                # Number of unique UPRNs
    avg_floor_area = mean(dt$TOTAL_FLOOR_AREA, na.rm = TRUE), # Calculate the average floor area
    property_type_distribution = property_type_distribution,  # Normalized distribution of property types
    tenure_distribution = tenure_distribution  # Normalized distribution of tenures
  )
}

# Calculate metrics for each EPC rating
epc_ratings <- unique(master_dataset$CURRENT_ENERGY_RATING)
metrics_list <- list()

for (rating in c("ALL", epc_ratings)) {
  if (rating == "ALL") {
    dt <- master_dataset
  } else {
    dt <- master_dataset[CURRENT_ENERGY_RATING == rating]
  }
  metrics <- calculate_metrics(dt)
  metrics_list[[rating]] <- metrics
}

# Create the initial result table for basic metrics
result_table <- data.table(
  Metric = c("Average Price", "Total Homes", "Average Floor Area")
)

for (rating in c("ALL", epc_ratings)) {
  result_table[[rating]] <- c(
    metrics_list[[rating]]$avg_price,
    metrics_list[[rating]]$total_homes,
    metrics_list[[rating]]$avg_floor_area
  )
}

# Add summarized property type distribution as normalized density
property_type_row <- c("Property Type Distribution (Density)")
for (rating in c("ALL", epc_ratings)) {
  distribution <- metrics_list[[rating]]$property_type_distribution
  summarized_distribution <- paste(distribution$PROPERTY_TYPE_EPC, round(distribution$density, 4), sep=": ", collapse=", ")
  property_type_row <- c(property_type_row, summarized_distribution)
}
result_table <- rbind(result_table, property_type_row, fill = TRUE)

# Add summarized tenure distribution as normalized density
tenure_row <- c("Tenure Distribution (Density)")
for (rating in c("ALL", epc_ratings)) {
  distribution <- metrics_list[[rating]]$tenure_distribution
  summarized_distribution <- paste(distribution$TENURE, round(distribution$density, 4), sep=": ", collapse=", ")
  tenure_row <- c(tenure_row, summarized_distribution)
}
result_table <- rbind(result_table, tenure_row, fill = TRUE)

# Save the result_table as a LaTeX file
library(xtable)
output_file <- "output/tables/summary_stats.tex"
latex_table <- xtable(result_table)
print(latex_table, type = "latex", file = output_file)

cat("LaTeX table has been saved to:", output_file, "\n")


# --------------- DISTRICT MAP -----------------------------------------------
library(sf) 

#load costs data 
setwd('/Users/charlottewargniez/Desktop/GreenPremium')
costs_per_UPRN <- as.data.table(fread('data/cleaned/costs_per_uprn.csv'))
setnames(costs_per_UPRN, 'POSTCODE_EPC', 'pcd')

# Load the ONSPD data
onspd_df <- as.data.table(fread('data/ONSPD_AUG_2024_UK.csv'))
#'oslaua' is the column with LAD codes
# 'districts_df' is the dataframe with LAD22CD (from the shapefile or boundary data)
onspd_df <- onspd_df[, .(pcd, oslaua)]
# onspd_df[, pcu_area := toupper(gsub("[^A-Za-z]", "", pcd))]
# onspd_df[, pcu_area := substr(pcu_area, 1, 2)]
# onspd_df <- onspd_df %>%
#   distinct(pcu_area, .keep_all = TRUE)

# Merge based on pcu_area
costs_per_UPRN <- merge(x = costs_per_UPRN, y = onspd_df, by = "pcd", all.x = TRUE)

#load local authority distrincts data (from ONS)
authority_districts <- as.data.table(fread('data/local_districts_ONS.csv'))
districts_sf <- st_read('data/LAD_MAY_2022_UK_BFE_V3.shp')

#merge based on LAD22CD and oslaua
cost_district <- costs_per_UPRN %>%
  left_join(authority_districts, by = c("oslaua" = "LAD22CD"))

# Average costs by district 
cost_district <- cost_district %>%
  group_by(oslaua) %>%
  summarize(avg_cost = mean(BY_EPC, na.rm = TRUE))
setnames(cost_district, "oslaua", "LAD22CD")

# Merge average costs with the spatial data
map_data <- districts_sf %>%
  left_join(cost_district, by = "LAD22CD")

# Plot the map
map_figure <- ggplot(map_data) +
  geom_sf(aes(fill = avg_cost)) +
  scale_fill_viridis_c(option = "plasma", name = "Average Cost", na.value = "grey") +  # Handle NA values
  theme_minimal() +
  labs(caption = "Source: Author (2024)")
# Save the last created plot to a file (e.g., PNG format)
ggsave("average_costs_per_district.png", plot = map_figure, width = 10, height = 8, dpi = 300)


# ----- Count propertes at risk ------------


# in highest cost districts 
property_count <- costs_per_UPRN %>%
  dplyr::filter(oslaua %in% c("S12000026", "E06000057", "E06000054", "E07000047")) %>%
  summarize(count = n())  # Count the number of rows (properties)
print(print(property_count))

# with construction age band below 1949 
# Filter for the specific CONSTRUCTION_AGE_BAND categories and count unique UPRNs
age_bands <- c(
  "England and Wales: 1900-1929",
  "England and Wales: 1930-1949",
  "England and Wales: before 1900"
)
unique_uprn_count <- master_dataset %>%
  dplyr::filter(CONSTRUCTION_AGE_BAND %in% age_bands) %>%
  summarize(unique_uprn_count = n_distinct(UPRN, na.rm = TRUE))
print(unique_uprn_count)

