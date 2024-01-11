###################################
####### Sensitivity analysis ######
###  Only admissions after wave ###
###### Part 2. Cox Regression #####
###################################

# Note: requires sensitivity_analyses.R to be run first to generate data file

library(survival)

## Part 1: Descriptive tables ##

# Unweighted tables #

# Create overall outcome for ambulatory care
sensitivity_analyses$time_to_ambulatory_all <- with(sensitivity_analyses, pmin(sensitivity_analyses$`Time to Ambulatory Care Sensitive Acute`, sensitivity_analyses$`Time to Ambulatory Care Sensitive Chronic`, sensitivity_analyses$`Time to Ambulatory Care Sensitive Vaccine-preventable`, na.rm = TRUE))

# Create table
table1 <- data.frame(mean(sensitivity_analyses$time_to_ambulatory_all, na.rm=T)) # Save frequency mean

hold <- data.frame(mean(sensitivity_analyses$`Time to Ambulatory Care Sensitive Acute`, na.rm=T)) # Repeat for each outcome
table1 <- cbind(table1, hold)

hold <- data.frame(mean(sensitivity_analyses$`Time to Ambulatory Care Sensitive Chronic`, na.rm=T))
table1 <- cbind(table1, hold) 

hold <- data.frame(mean(sensitivity_analyses$`Time to Ambulatory Care Sensitive Vaccine-preventable`, na.rm=T))
table1 <- cbind(table1, hold) 

hold <- data.frame(mean(sensitivity_analyses$`Time to Emergency Urgent Care Sensitive `, na.rm=T))
table1 <- cbind(table1, hold)

hold <- data.frame(mean(sensitivity_analyses$time_to_total_admissions, na.rm=T))
table1 <- cbind(table1, hold) 

# Save
write.csv(table1, "../../outputs/summary_table_unweighted_survival.csv") # Save
rm(hold, table1) # Tidy

# Weighted tables #

# Define survey design
wgt3 <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = sensitivity_analyses, variables = NULL)

# Create table
table1 <- data.frame(svymean(sensitivity_analyses$time_to_ambulatory_all, na.rm=T, design = wgt3)) # Save frequency mean

hold <- data.frame(svymean(sensitivity_analyses$`Time to Ambulatory Care Sensitive Acute`, na.rm=T, design = wgt3)) # Repeat for each outcome
table1 <- cbind(table1, hold)

hold <- data.frame(svymean(sensitivity_analyses$`Time to Ambulatory Care Sensitive Chronic`, na.rm=T, design = wgt3))
table1 <- cbind(table1, hold) 

hold <- data.frame(svymean(sensitivity_analyses$`Time to Ambulatory Care Sensitive Vaccine-preventable`, na.rm=T, design = wgt3))
table1 <- cbind(table1, hold) 

hold <- data.frame(svymean(sensitivity_analyses$`Time to Emergency Urgent Care Sensitive `, na.rm=T, design = wgt3))
table1 <- cbind(table1, hold)

hold <- data.frame(svymean(sensitivity_analyses$time_to_total_admissions, na.rm=T, design = wgt3))
table1 <- cbind(table1, hold) 

# Save
write.csv(table1, "../../outputs/summary_table_weighted_survival.csv") # Save
rm(hold, table1) # Tidy


# Part 2: Any type of disruption #

# Model a: Any ambulatory care sensitive conditions
sensitivity_analyses$time_to_ambulatory_all[is.na(sensitivity_analyses$time_to_ambulatory_all)] <- 573 # set missing value to max time
sensitivity_analyses$censor <- 1 # Have an event for censoring purposes
sensitivity_analyses$censor[sensitivity_analyses$time_to_ambulatory_all == 573] <- 0
model_surv_any_a_u <- svycoxph(Surv(time_to_ambulatory_all, censor) ~ imp_disruption_any, data = sensitivity_analyses, design = wgt3, rescale = TRUE) # Unadjusted
table2 <- tidy(model_surv_any_a_u, conf.int = TRUE) # Save key results
table2 <- table2[2,] # Extract only healthcare disruption variable
table2$model <- "All ACS - unadj" # Note model name

model_surv_any_a_a <- svycoxph(Surv(time_to_ambulatory_all, censor) ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_any_a_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model b: Any ambulatory care sensitive acute conditions
sensitivity_analyses$`Time to Ambulatory Care Sensitive Acute`[is.na(sensitivity_analyses$`Time to Ambulatory Care Sensitive Acute`)] <- 573 # set missing value to max time
sensitivity_analyses$censor <- 1 # Have an event for censoring purposes
sensitivity_analyses$censor[sensitivity_analyses$`Time to Ambulatory Care Sensitive Acute` == 573] <- 0
model_surv_any_b_u <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Acute`, censor) ~ imp_disruption_any, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_any_b_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_surv_any_b_a <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Acute`, censor) ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_any_b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model c: Any ambulatory care sensitive chronic conditions
sensitivity_analyses$`Time to Ambulatory Care Sensitive Chronic`[is.na(sensitivity_analyses$`Time to Ambulatory Care Sensitive Chronic`)] <- 573 # set missing value to max time
sensitivity_analyses$censor <- 1 # Have an event for censoring purposes
sensitivity_analyses$censor[sensitivity_analyses$`Time to Ambulatory Care Sensitive Chronic` == 573] <- 0
model_surv_any_c_u <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Chronic`, censor) ~ imp_disruption_any, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_any_c_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_surv_any_c_a <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Chronic`, censor) ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_any_c_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model d: Any vaccine preventable ambulatory care sensitive conditions
sensitivity_analyses$`Time to Ambulatory Care Sensitive Vaccine-preventable`[is.na(sensitivity_analyses$`Time to Ambulatory Care Sensitive Vaccine-preventable`)] <- 573 # set missing value to max time
sensitivity_analyses$censor <- 1 # Have an event for censoring purposes
sensitivity_analyses$censor[sensitivity_analyses$`Time to Ambulatory Care Sensitive Vaccine-preventable` == 573] <- 0
model_surv_any_d_u <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Vaccine-preventable`, censor) ~ imp_disruption_any, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_any_d_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_surv_any_d_a <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Vaccine-preventable`, censor) ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_any_d_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
sensitivity_analyses$`Time to Emergency Urgent Care Sensitive `[is.na(sensitivity_analyses$`Time to Emergency Urgent Care Sensitive `)] <- 573 # set missing value to max time
sensitivity_analyses$censor <- 1 # Have an event for censoring purposes
sensitivity_analyses$censor[sensitivity_analyses$`Time to Emergency Urgent Care Sensitive ` == 573] <- 0
model_surv_any_e_u <- svycoxph(Surv(`Time to Emergency Urgent Care Sensitive `, censor) ~ imp_disruption_any, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_any_e_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_surv_any_e_a <- svycoxph(Surv(`Time to Emergency Urgent Care Sensitive `, censor) ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_any_e_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model f: Any hospital admission
sensitivity_analyses$time_to_total_admissions[is.na(sensitivity_analyses$time_to_total_admissions)] <- 573 # set missing value to max time
sensitivity_analyses$censor <- 1 # Have an event for censoring purposes
sensitivity_analyses$censor[sensitivity_analyses$time_to_total_admissions == 573] <- 0
model_surv_any_f_u <- svycoxph(Surv(time_to_total_admissions, censor) ~ imp_disruption_any, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_any_f_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_surv_any_f_a <- svycoxph(Surv(time_to_total_admissions, censor) ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_any_f_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

write.csv(table2, file = "S:/LLC_0009/outputs/regression_model1_survival.csv") # Save



# Part 3: Type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_surv_type_a_u <- svycoxph(Surv(time_to_ambulatory_all, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, design = wgt3) # Unadjusted
table3 <- tidy(model_surv_type_a_u, conf.int = TRUE) # Save key results
table3 <- table3[2:4,] # Extract only healthcare disruption variable
table3$model <- "All ACS - unadj" # Note model name

model_surv_type_a_a <- svycoxph(Surv(time_to_ambulatory_all, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_type_a_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model b: Acute ambulatory care sensitive acute conditions
model_surv_type_b_u <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Acute`, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_type_b_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_surv_type_b_a <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Acute`, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_type_b_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model c: Chronic ambulatory care sensitive chronic conditions
model_surv_type_c_u <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Chronic`, censor)  ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_type_c_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_surv_type_c_a <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Chronic`, censor)  ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_type_c_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model d: Vaccinhe preventable ambulatory care sensitive conditions
model_surv_type_d_u <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Vaccine-preventable`, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_type_d_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_surv_type_d_a <- svycoxph(Surv(`Time to Ambulatory Care Sensitive Vaccine-preventable`, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_type_d_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_surv_type_e_u <- svycoxph(Surv(`Time to Emergency Urgent Care Sensitive `, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_type_e_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_surv_type_e_a <- svycoxph(Surv(`Time to Emergency Urgent Care Sensitive `, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_type_e_a, conf.int = TRUE) # Save key results
# If unhappy, then can do manually with confint(model_surv_type_e_a, parm = "disruption_appointments")
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model f: Any hospital admission
model_surv_type_f_u <- svycoxph(Surv(time_to_total_admissions, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, design = wgt3) # Unadjusted
hold <- tidy(model_surv_type_f_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_surv_type_f_a <- svycoxph(Surv(time_to_total_admissions, censor) ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) +factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, design = wgt3) # Adjusted
hold <- tidy(model_surv_type_f_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

write.csv(table3, file = "S:/LLC_0009/outputs/regression_model2_survival.csv") # Save
gc()






