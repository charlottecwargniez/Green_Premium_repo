
# Energy Efficiency ------------------------------------------------------------
# this function takes in 

group_energyeff <- function(dataset, select_energy){
  
  #Create numerical representation of efficiency 
  for (columns in select_energy) {
    columns_d <- paste(columns, "_f", sep = "")
    dataset[, (columns_d) := ifelse(get(columns) == "Very Poor", -2,
                                           ifelse(get(columns) == "Poor", -1,
                                                  ifelse(get(columns) == "Average", 0,
                                                         ifelse(get(columns) == "Good", 1,
                                                                ifelse(get(columns) == "Very Good", 2, NA)))))]
    dataset[, (columns_d) := as.numeric(as.character(get(columns_d)))] # Ensure the new column is numeric
  }
  
  for (columns in select_energy) {
    dataset[, (columns) := factor(na.exclude(get(columns)), levels = c("Very Poor", "Poor", "Average", "Good", "Very Good"))]
  }
  
  SELECT_ENERGY_f <- paste(select_energy,"_f",sep="") # keep track of extra-columns
  
  for (columns in SELECT_ENERGY_f) {
    print(paste("cleaning", columns))
    columns_d <- paste(columns, "_diff", sep = "")
    dataset[, (columns_d) := get(columns) - shift(get(columns)), by = UPRN]
    dataset[, (columns_d) := ifelse(get(columns_d) != 0, 1, 0)] # Assign 1 if the difference is anything but 0
  }
  
}


