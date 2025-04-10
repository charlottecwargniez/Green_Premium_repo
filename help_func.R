
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


# Replace IMPROVEMENT ID duplicates
replace_improvement_id <- function(id) {
  if (id %in% c(2,3)) return(1)
  if (id %in% c(12, 13, 14, 15, 17, 18)) return(11)
  if (id == 21) return(20)
  if (id == 24) return(30)
  if (id == 31) return(25)
  if (id == 28) return(43)
  if (id %in% c(29, 32)) return(27)
  if (id == 38) return(37)
  if (id == 39) return(23)
  if (id == 41) return(40)
  if (id == 61) return(59)
  if (id == 62) return(60)
  return(id)
}


# Group Improvements by category 
group_improvents <- function(id) {
  if (id %in% c(2,3)) return(1)
  if (id %in% c(7,63)) return(6)
  if (id == 46) return(45)
  if (id %in% c(11,12,13,14,15,17,18)) return(16)
  if (id %in% c(21,22,27,28,29,43,36,37,38,40,41)) return(20)
  if (id %in% c(39,24,25,26,49,50,59,61,60,62)) return(23)
  if (id %in% c(9,10,56)) return(8)
  if (id == 19) return(34)
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
