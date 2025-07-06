"""
This plots Figure 3 in retrofitting article 

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

#_____ Plot ___________________________________________

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
