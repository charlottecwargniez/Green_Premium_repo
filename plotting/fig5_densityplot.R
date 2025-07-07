#"""
#This plots Figure 3 in retrofitting article 

#"""

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

#_____ Load Data ___________________________________________

#Read costs of UPRN (as calculated by the following EPC recommendations method - see improvements_costs.R for more detail)
costs_per_UPRN <- as.data.table(fread("data/cleaned/costs_uprn_epc.csv"))
setnames(costs_per_UPRN, "BY_EPC", "Cost") # replace column name 

# Merge UPRNs from master_epc with individual costs (Costs_per_UPRN) with master_dataset 
master_dataset <- as.data.table(fread("data/cleaned/master_dataset_all.csv"))  #load master_dataset
master_dataset[, UPRN := as.numeric(UPRN)]  
master_dataset <- master_dataset[master_dataset[, .I[which.max(INSPECTION_DATE)], by = .(UPRN)]$V1]  #keep observations with latest inspection date
master_dataset <- master_dataset[, .(UPRN, PROPERTY_TYPE, PRICE, CONSTRUCTION_AGE_BAND )] #keep only columns of interest 
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
  geom_vline(xintercept = 8945, color = "red", linetype = "dashed", linewidth = 0.5) +
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



# -------- Clean construction band Column ----- 
# To DO: Make this a separate function 
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
costs_per_UPRN[, CONSTRUCTION_AGE_BAND := ifelse(
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

# remove NAs
costs_per_UPRN <- costs_per_UPRN[!is.na(CONSTRUCTION_AGE_BAND)]

# Make factor (categorical) values
costs_per_UPRN[, Age_band := as.factor(CONSTRUCTION_AGE_BAND)]
costs_per_UPRN[, CONSTRUCTION_AGE_BAND := as.factor(CONSTRUCTION_AGE_BAND)]  

# create a density plot with relative cost of retrofit to construction age 
costs_per_UPRN[ , per_cost := Cost/PRICE*100]
ggplot(costs_per_UPRN, aes(x = per_cost, fill = Age_band)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Average Retrofitting Costs by Property Type",
    x = "Relative Cost of Retrofiting (%)",
    y = "Density",
    fill = "Property Type"
  ) +
  xlim(c(0,15))
  theme_minimal()


#normalise the distribution of price for each age band 
costs_per_UPRN[, cost_mean_age := mean(Cost), by = Age_band]

# To DO: instead of simple density plot, instead compare distributions (normalized) of prices per age band 


