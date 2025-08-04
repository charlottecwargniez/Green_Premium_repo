
  # # Load EPC dataset
  # master_epc <- as.data.table(fread("data/cleaned/master_epcs.csv"))

  source("functions/help_func.R")
  source("clean_masterEPC.R")
  
  # rename EPC variables
  for (vars in c("PROPERTY_TYPE","ADDRESS","POSTCODE")) {
    setnames(master_epc, vars, paste(vars,"_EPC",sep=""),skip_absent=TRUE)
  }
  
  collapse_ids_safe <- function(dt, collapse_map) {
    for (target in unique(unlist(collapse_map))) {
      sources <- names(collapse_map)[collapse_map == target]
      source_cols <- paste0("ID_", sources)
      target_col <- paste0("ID_", target)
      
      # Only use existing source cols
      existing_sources <- source_cols[source_cols %in% names(dt)]
      
      # Skip if no source cols exist
      if (length(existing_sources) == 0) next
      
      # If target column doesn't exist, treat it as 0
      if (!(target_col %in% names(dt))) {
        dt[, (target_col) := 0]
      }
      
      # Collapse: target = target | any(source_cols)
      dt[, (target_col) := as.numeric(Reduce(`|`, c(.SD, list(get(target_col))))), .SDcols = existing_sources]
      
      # Remove source columns only
      dt[, (existing_sources) := NULL]
    }
  }
  
  
  collapse_map <- c(
    "2" = 1, "3" = 1,
    "12" = 11, "13" = 11, "14" = 11, "15" = 11, "17" = 11, "18" = 11,
    "21" = 20,
    "30" = 24,
    "31" = 25,
    "29" = 27, "32" = 27,
    "38" = 37,
    "41" = 40,
    "28" = 43,
    "39" = 23,
    "61" = 59,
    "62" = 60
  )
  
  collapse_ids_safe(master_epc, collapse_map)

  
  fwrite(master_epc,"data/cleaned/master_epc_ID.csv") # Output file

