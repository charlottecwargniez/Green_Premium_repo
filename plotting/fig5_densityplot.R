"""
This plots Figure 3 in the paper
"""
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

# ---------------------------
""" 
Load Data
"""


# Merge UPRNs from master_epc with indidual costs (Costs_per_UPRN) with master_dataset 
master_dataset <- as.data.table(fread("data/cleaned/master_dataset_all.csv"))  #load master_dataset
master_dataset[, UPRN := as.numeric(UPRN)]  
master_dataset <- master_dataset[master_dataset[, .I[which.max(INSPECTION_DATE)], by = .(UPRN)]$V1]  #keep observations with latest inspection date
master_dataset <- master_dataset[, .(UPRN, PROPERTY_TYPE, PRICE)] #keep only columns of interest 
=======
#_____ Load Data ___________________________________________

#Read costs of UPRN (as calculated by the following EPC recommendations method - see improvements_costs.R for more detail)
costs_per_UPRN <- as.data.table(fread("data/cleaned/costs_uprn_epc.csv"))
setnames(costs_per_UPRN, "BY_EPC", "Cost") # replace column name 

# Merge UPRNs from master_epc with individual costs (Costs_per_UPRN) with master_dataset 
master_dataset <- as.data.table(fread("data/cleaned/master_dataset_all.csv"))  #load master_dataset
master_dataset[, UPRN := as.numeric(UPRN)]  
master_dataset <- master_dataset[master_dataset[, .I[which.max(INSPECTION_DATE)], by = .(UPRN)]$V1]  #keep observations with latest inspection date
master_dataset <- master_dataset[, .(UPRN, PROPERTY_TYPE, PRICE, CONSTRUCTION_AGE_BAND )] #keep only columns of interest 
>>>>>>> 9dd7829f87b16b017d4de062de8226e42e2b6ac6
costs_per_UPRN <- merge(x = costs_per_UPRN, y = master_dataset, by = "UPRN", all.x = TRUE, allow.cartesian = FALSE)
setorder(costs_per_UPRN, UPRN)

# remove NAs
costs_per_UPRN <- costs_per_UPRN[!is.na(PRICE)]

# ------ 1. Density Plot per Price --------------------------------------------------------
# Create price range categories (adjust the breaks as per your dataset)
costs_per_UPRN[, price_range := cut(PRICE, 
                                    breaks = c(0, 100000, 200000, 300000, 400000, 500000, Inf), 
                                    labels = c("0-100k", "100-200k", "200-300k", "300-400k", "400-500k", "500k+"),
                                    right = FALSE)]

# Create a density plot with different shading for different price ranges
density_price <- ggplot(costs_per_UPRN, aes(x = Cost, fill = price_range)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 8945, color = "red", linetype = "dashed", size = 0.5) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 8945, color = "red", linetype = "dashed", linewidth = 0.5) +
>>>>>>> 9dd7829f87b16b017d4de062de8226e42e2b6ac6
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
ggplot(costs_per_UPRN, aes(x = Cost, fill = PROPERTY_TYPE)) +
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
=======


# create a density plot with relative cost of retrofit to price of house
costs_per_UPRN[ , per_cost := Cost/PRICE*100]
ggplot(costs_per_UPRN, aes(x = per_cost, fill = price_range)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Average Retrofitting Costs by Property Type",
    x = "Relative Cost of Retrofiting (%)",
    y = "Density",
    fill = "Property Type"
  ) +
  xlim(c(0,15))
theme_minimal()



# ------ 2. Density Plot per Age Band --------------------------------------------------------
# To DO: Make this a separate function 
## Clean Construction_AGE_BAND -------

# Define mappings for single years and special strings (including the original weird strings)
year_to_bucket <- list(
  # single years mapped to buckets
  "1876" = "before 1900",
  "1890" = "before 1900",
  "1900" = "1900-2000",
  "1930" = "1900-2000",
  "1950" = "1900-2000",
  "1967" = "1900-2000",
  "1976" = "1900-2000",
  "1983" = "1900-2000",
  "1991" = "1900-2000",
  "1996" = "1900-2000",
  "2000" = "1900-2000",
  "2002" = "after 2000",
  "2003" = "after 2000",
  "2004" = "after 2000",
  "2006" = "after 2000",
  "2007" = "after 2000",
  "2009" = "after 2000",
  "2010" = "after 2000",
  "2011" = "after 2000",
  "2012" = "after 2000",
  "2013" = "after 2000",
  "2014" = "after 2000",
  "2015" = "after 2000",
  "2016" = "after 2000",
  "2017" = "after 2000",
  "2018" = "after 2000",
  "2019" = "after 2000",
  "2020" = "after 2000",
  "2021" = "after 2000",
  "2022" = "after 2000",
  "2023" = "after 2000",
  
  # special entries mapped to buckets
  "England and Wales: before 1900" = "before 1900",
  "England and Wales: 1900-1929" = "1900-2000",
  "England and Wales: 1930-1949" = "1900-2000",
  "England and Wales: 1950-1966" = "1900-2000",
  "England and Wales: 1967-1975" = "1900-2000",
  "England and Wales: 1976-1982" = "1900-2000",
  "England and Wales: 1983-1990" = "1900-2000",
  "England and Wales: 1991-1995" = "1900-2000",
  "England and Wales: 1996-2002" = "after 2000",
  "England and Wales: 2003-2006" = "after 2000",
  "England and Wales: 2007-2011" = "after 2000",
  "England and Wales: 2007 onwards" = "after 2000",
  "England and Wales: 2012 onwards" = "after 2000"
)

# Now apply the logic
costs_per_UPRN[, CONSTRUCTION_AGE_BAND := sapply(CONSTRUCTION_AGE_BAND, function(x) year_to_bucket[[x]])]

# remove NAs and NULLS 
costs_per_UPRN <- costs_per_UPRN[!is.na(CONSTRUCTION_AGE_BAND)]
costs_per_UPRN <- costs_per_UPRN[!costs_per_UPRN$CONSTRUCTION_AGE_BAND=="NULL"]

# Make factor (categorical) values
costs_per_UPRN$CONSTRUCTION_AGE_BAND <- unlist(costs_per_UPRN$CONSTRUCTION_AGE_BAND) #convert column from list to characters 
costs_per_UPRN[, CONSTRUCTION_AGE_BAND := as.factor(CONSTRUCTION_AGE_BAND)]  

# create a density plot with relative cost of retrofit to construction age 
costs_per_UPRN[ , per_cost := Cost/PRICE*100]
ggplot(costs_per_UPRN, aes(x = per_cost, fill = CONSTRUCTION_AGE_BAND)) +
  geom_density(alpha = 0.5) +
  labs(
    x = "Relative Retrofiting Cost (%)",
    y = "Density",
    fill = "Construction Age Band"
  ) +
  xlim(c(0,15))
  theme_minimal()
ggsave("output/plots/Density_retrofittingcosts_byAge.png", density_price, width = 8, height = 6)

# Separate plots per construction age 
ggplot(costs_per_UPRN, aes(x = Cost)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~ CONSTRUCTION_AGE_BAND, scales = "free_y", ncol = 4) +
  theme_minimal()
ggsave("output/plots/Density_Costs_seperatebyAge.png", density_price, width = 12, height = 6)

# To DO: instead of simple density plot, instead compare distributions (normalized) of prices per age band 


# ------- 3. Test Difference between Distributions by Age ------------- 
