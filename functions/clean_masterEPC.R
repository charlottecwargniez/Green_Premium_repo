# since master_epc is so large, we load it in chuncks and also clean for the following: 

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

library(data.table)
# Remove rows with UPRN is NA 
master_epc <- master_epc [!is.na(UPRN)]
master_epc[, UPRN := as.numeric(UPRN)]

# Remove rows with EPC score > 100
master_epc <- master_epc[CURRENT_ENERGY_EFFICIENCY <100]

gc()