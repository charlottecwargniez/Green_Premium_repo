
# ---- 
# """ 
# This script runs Hedonic regressions (Table 6 in EPC Retrofitting Paper)
# """""
# ------ 

# Import libraries
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
library(magrittr)  # For the pipe operator
library(viridis)  # For viridis color palette

library(e1071) # to save in latex tables 
library(xtable)
library(stargazer)
library(kableExtra)

library(sf) # spatial visualization 
library(ggmap)
library(osmdata)
library(mapproj)

library(rlang)

# Load Data
master_dataset <- as.data.frame(fread("data/cleaned/master_dataset_regression.csv"))
zoopla_active <- as.data.frame(fread("data/cleaned/zoopla_active_sale.csv"))

# Extract PCU data (for zoopla active sales)
# to do!!: fix this (there is a miss-use of the := operator)
conflicts_prefer(data.table::':=')
master_dataset[, pcu_area := toupper(gsub("[^A-Za-z]", "", POSTCODE_EPC))]
master_dataset[, pcu_area := substr(pcu_area, 1, 2)]

#merge datasets on pcu_area
zoopla_active <- merge(x=zoopla_active, y=master_dataset, by="pcu_area", all.x = T)


