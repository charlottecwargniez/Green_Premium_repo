"""
This plots Figure 3 in 
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