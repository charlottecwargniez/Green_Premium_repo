
# ########
# # AGM 
# ###########
# # Extended OLS regression formulas with interactions and non-linear terms
# list_of_formulas_4 <- list(
#   formula_1 <- as.formula(paste("PRICE ~ CURRENT_ENERGY_EFFICIENCY + I(CURRENT_ENERGY_EFFICIENCY^2) + I(CURRENT_ENERGY_EFFICIENCY^3)", controls, time_fe, sep="+")),
#   formula_2 <- as.formula(paste("PRICE ~ change_EPC*BOROUGH + change_EPC*CONSTRUCTION_AGE_BAND", controls, time_fe, sep="+")),
#   formula_3 <- as.formula(paste("PRICE ~ bs(CURRENT_ENERGY_EFFICIENCY, df=4)", controls, time_fe, sep="+")),
#   formula_4 <- as.formula(paste("PRICE ~ change_EPC + change_EPC:after_2022 + after_2022 + I(change_EPC^2)", controls, time_fe, sep="+")),
#   formula_5 <- as.formula(paste("PRICE ~ change_EPC + change_EPC:after_2022 + after_2022", controls, time_fe, sep="+"))
# )
# 
# # Run the extended series of regressions
# reg_output_list_4 <- lapply(list_of_formulas_4, function(formula) {
#   run_regression(formula, master_dataset, cluster_se, pattern_controls)
# })
# #custom_write_csv(reg_output_list_4,"output/tables/reg_Price_4.csv")
# print(reg_output_list_4)
# 

# ########### 
# 
# ######################################################################
# # Staggered Diff-in-Diff Regression 
# #####################################################################
# 
# 
# 
# 
# ######################################################################
# # Propensity Scores 
# # --------------------------------------------------------
#       # Estimate treatment effect by comparing hoursses with similar covariates (house characteristics) 
#       # and different EPC scores. 
# #####################################################################
# 
# # Covariates
# matching_characteristics <- "PROPERTY_TYPE + NEWBUILD + ESTATE + TOTAL_FLOOR_AREA + NUMBER_HABITABLE_ROOMS + EXTENSION_COUNT + CONSTRUCTION_AGE_BAND + POSTTOWN + MAIN_FUEL"
# covariates_names <- strsplit(covariates, "\\s*\\+\\s*")[[1]]
# cov_data <- cbind(master_dataset[1:10000,..covariates_names])
# 
# # Loop through the covariates and create dummy variables
# for (cov in covariates_trim) {
#     dummy_name <- paste0(cov, "_d")
#     master_dataset <- master_dataset %>%
#       mutate(!!dummy_name := as.factor(!!sym(cov)))
#     }
# 
# # Select for EPC scores above C as treatment effect
# master_dataset <- master_dataset %>%
#   mutate(binary_EPC = ifelse(CURRENT_ENERGY_RATING %in% c("A", "B","C"), 1, 0))
# # save new EPC dummy
# EPC_abovegood <- master_dataset$binary_EPC[1:10000]
# 
# price <- master_dataset$PRICE[1:10000]
# # 
# # ---------------------------------------------------
# # 1:1 Nearest Neighboour PS matching w/o replacement
# # -------------------------------------------------
#   # One by one, each treated unit is paired with an available control unit that has 
#   # the closest propensity score to it. Any remaining control units are left unmatched 
#   # and excluded from further analysis. Due to the theoretical balancing properties of the propensity score described by Rosenbaum and Rubin (1983), 
#   # propensity score matching can be an effective way to achieve covariate balance in the treatment groups. 
# #m.out1 <- matchit(binary_EPC ~ PROPERTY_TYPE_d + NEWBUILD_d + ESTATE_d + TOTAL_FLOOR_AREA_d + CONSTRUCTION_AGE_BAND_d + POSTTOWN_d + MAIN_FUEL_d, data = master_dataset, method = "nearest", distance = "glm")
# #results_out1 <- summary(m.out1, un = FALSE)
# 
# glm1 <- glm(EPC_abovegood ~., family=binomial, data=cov_data)
# propensity_scores <- glm1$fitted.values
# summary(glm1)
# 
# # average treatment on treated effect 
# rrl <- Match(Y = price, Tr = EPC_abovegood, X = propensity_scores)
# summary(rrl)
# 
# # ----------------------------------------------
# # 1:2 Full Matching on a probit Propensity score 
# # ----------------------------------------------
#   # Below, we’ll try full matching, which matches every treated unit to at least one control and every control 
#   # to at least one treated unit (Hansen 2004; Stuart and Green 2008).
# m.out2 <- matchit(binary_EPC ~ covariates, data = master_dataset, method = "full", distance = "glm", link="probit")
# results_out2 <- summary(m.out2, un = FALSE)
