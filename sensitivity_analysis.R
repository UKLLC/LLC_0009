###################################
####### Sensitivity analysis ######
###  Only admissions after wave ###
### Part 1. Logistic Regression ###
###################################

# Note: Need to run analyses.R first

## Tidy data ##

# Tidy data and add in updated outcomes
# load("S:/LLC_0009/data/Cleaned Cohorts/genscot_admissions_post_wave.RData") # Genscot data
load("S:/LLC_0009/data/Cleaned Cohorts/nhs_hes_post_wave.RData") # HES records
# names(hes)[names(hes) == "llc_0009_stud_id"] <- "LLC_0009_stud_id" # Rename (V001)
names(hes)[names(hes) == "llc_0009_stud_id"] <- "llc_0009_stud_id15" # Rename (V002)
# outcomes <- rbind(hes, genscot_tot) # Join together all outcome variables
# sensitivity_analyses <- all_data[,c(1:24, 31:69)] # Drop outcome variables (V001)
sensitivity_analyses <- all_data[,c(1:64)] # Drop outcome variables (V002)
# sensitivity_analyses <- merge(sensitivity_analyses, hes, by = "LLC_0009_stud_id", all.x = TRUE) # Join outcomes onto main dataset (V001)
sensitivity_analyses <- merge(sensitivity_analyses, hes, by = "llc_0009_stud_id15", all.x = TRUE) # Join outcomes onto main dataset (V002)
rm(hes) # Tidy

# Where missing data for outcomes, these should be 0s
sensitivity_analyses$`Ambulatory Care Sensitive Acute`[is.na(sensitivity_analyses$`Ambulatory Care Sensitive Acute`)] <- 0 # Fill in NAs with 0s for joined on outcome measures (since if no match then they did not have a hospital admission)
sensitivity_analyses$`Ambulatory Care Sensitive Chronic`[is.na(sensitivity_analyses$`Ambulatory Care Sensitive Chronic`)] <- 0
sensitivity_analyses$`Ambulatory Care Sensitive Vaccine-preventable`[is.na(sensitivity_analyses$`Ambulatory Care Sensitive Vaccine-preventable`)] <- 0
sensitivity_analyses$`Emergency Urgent Care Sensitive `[is.na(sensitivity_analyses$`Emergency Urgent Care Sensitive `)] <- 0
sensitivity_analyses$total_admissions[is.na(sensitivity_analyses$total_admissions)] <- 0
sensitivity_analyses$`Ambulatory Care Sensitive All` <- sensitivity_analyses$`Ambulatory Care Sensitive Acute` + sensitivity_analyses$`Ambulatory Care Sensitive Chronic` + sensitivity_analyses$`Ambulatory Care Sensitive Vaccine-preventable` # Create overall ambulatory care sensitive measure

# Convert outcome variables to binary variables
sensitivity_analyses$`Ambulatory Care Sensitive All`[sensitivity_analyses$`Ambulatory Care Sensitive All` > 1] <- 1
sensitivity_analyses$`Ambulatory Care Sensitive Acute`[sensitivity_analyses$`Ambulatory Care Sensitive Acute` > 1] <- 1
sensitivity_analyses$`Ambulatory Care Sensitive Chronic`[sensitivity_analyses$`Ambulatory Care Sensitive Chronic` > 1] <- 1
sensitivity_analyses$`Ambulatory Care Sensitive Vaccine-preventable`[sensitivity_analyses$`Ambulatory Care Sensitive Vaccine-preventable` > 1] <- 1
sensitivity_analyses$`Emergency Urgent Care Sensitive `[sensitivity_analyses$`Emergency Urgent Care Sensitive ` > 1] <- 1
sensitivity_analyses$total_admissions[sensitivity_analyses$total_admissions > 1] <- 1

## Re-run analyses under new data ##

## Part 1: Descriptive tables ##

# Unweighted tables #

# Population size of each cohort
table1 <- data.frame(table(sensitivity_analyses$cohort)) # Save table
table1$measure <- "Cohort" # Note measure

# Outcome variables
hold <- data.frame(table(sensitivity_analyses$`Ambulatory Care Sensitive All`)) # Save frequency table
hold$measure <- "Ambulatory Care Sensitive All" # Note outcome measure
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(sensitivity_analyses$`Ambulatory Care Sensitive Acute`)) # Repeat for each outcome
hold$measure <- "Ambulatory Care Sensitive Acute"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(sensitivity_analyses$`Ambulatory Care Sensitive Chronic`))
hold$measure <- "Ambulatory Care Sensitive Chronic"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(sensitivity_analyses$`Ambulatory Care Sensitive Vaccine-preventable`))
hold$measure <- "Ambulatory Care Sensitive Vaccine-preventable"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(sensitivity_analyses$`Emergency Urgent Care Sensitive `))
hold$measure <- "Emergency Urgent Care Sensitive"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(sensitivity_analyses$total_admissions))
hold$measure <- "Total admissions"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(sensitivity_analyses$vaccinated)) 
hold$measure <- "Vaccinated"
table1 <- rbind(table1, hold)

# Explanatory variables
hold <- data.frame(table(sensitivity_analyses$imp_disruption_any)) # Repeat same process for explanatory variables
hold$measure <- "Disruption any"
table1 <- rbind(table1, hold)

hold <- data.frame(table(sensitivity_analyses$imp_disruption_appointments)) 
hold$measure <- "Disruption appointments"
table1 <- rbind(table1, hold)

hold <- data.frame(table(sensitivity_analyses$imp_disruption_medications))
hold$measure <- "Disruption medications"
table1 <- rbind(table1, hold)

hold <- data.frame(table(sensitivity_analyses$imp_disruption_procedures)) 
hold$measure <- "Disruption procedures"
table1 <- rbind(table1, hold)

# Save
write.csv(table1, "../../outputs/summary_table_unweighted_sa.csv") # Save
rm(hold, table1) # Tidy


# Weighted tables #

# Define survey design
wgt2 <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = sensitivity_analyses)

# Outcome variables
table1w <- data.frame(svymean(sensitivity_analyses$total_admissions, wgt2)) # Generate statistics (used mean here but is basically the proportion since binary)
table1w$measure <- "Total admissions" # Note outcome measure

hold <- data.frame(svymean(sensitivity_analyses$`Ambulatory Care Sensitive All`, wgt2)) # Repeat process
hold$measure <- "Ambulatory care sensitive any"
table1w <- rbind(table1w, hold) # Join onto main table

hold <- data.frame(svymean(sensitivity_analyses$`Ambulatory Care Sensitive Acute`, wgt2))
hold$measure <- "Ambulatory care sensitive acute"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(sensitivity_analyses$`Ambulatory Care Sensitive Chronic`, wgt2))
hold$measure <- "Ambulatory care sensitive chronic"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(sensitivity_analyses$`Ambulatory Care Sensitive Vaccine-preventable`, wgt2))
hold$measure <- "Ambulatory care sensitive vaccine preventable"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(sensitivity_analyses$`Emergency Urgent Care Sensitive `, wgt2))
hold$measure <- "Emergency urgent care sensitive"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(sensitivity_analyses$vaccinated, wgt2))
hold$measure <- "Vaccinated"
table1w <- rbind(table1w, hold)

# Explanatory variables
hold <- data.frame(svymean(sensitivity_analyses$imp_disruption_any, wgt2))
hold$measure <- "Disruption any"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(sensitivity_analyses$imp_disruption_appointments, wgt2))
hold$measure <- "Disruption appointments"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(sensitivity_analyses$imp_disruption_medications, wgt2))
hold$measure <- "Disruption medications"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(sensitivity_analyses$imp_disruption_procedures, wgt2))
hold$measure <- "Disruption procedures"
table1w <- rbind(table1w, hold)

# Save
write.csv(table1w, "../../outputs/summary_table_weighted_sa.csv") # Save
rm(hold, table1w) # Tidy


# Part 2: Any type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_sa_any_a_u <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_any, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
table2 <- tidy(model_sa_any_a_u, conf.int = TRUE) # Save key results
table2 <- table2[2,] # Extract only healthcare disruption variable
table2$model <- "All ACS - unadj" # Note model name

model_sa_any_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_any_a_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model b: Any ambulatory care sensitive acute conditions
model_sa_any_b_u <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_any, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_any_b_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_sa_any_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_any_b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model c: Any ambulatory care sensitive chronic conditions
model_sa_any_c_u <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_any, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_any_c_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_sa_any_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_any_c_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model d: Any vaccine preventable ambulatory care sensitive conditions
model_sa_any_d_u <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_any, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_any_d_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_sa_any_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_any_d_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_sa_any_e_u <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_any, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_any_e_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_sa_any_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_any_e_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model f: Any hospital admission
model_sa_any_f_u <- svyglm(total_admissions ~ imp_disruption_any, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_any_f_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_sa_any_f_a <- svyglm(total_admissions ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_any_f_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

write.csv(table2, "../../outputs/regression_model1_sa.csv") # Save
rm(table2) # Tidy

# Part 3: Type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_sa_type_a_u <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
table3 <- tidy(model_sa_type_a_u, conf.int = TRUE) # Save key results
table3 <- table3[2:4,] # Extract only healthcare disruption variable
table3$model <- "All ACS - unadj" # Note model name

model_sa_type_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_type_a_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model b: Acute ambulatory care sensitive acute conditions
model_sa_type_b_u <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_type_b_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_sa_type_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_type_b_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model c: Chronic ambulatory care sensitive chronic conditions
model_sa_type_c_u <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_type_c_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_sa_type_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_type_c_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model d: Vaccinhe preventable ambulatory care sensitive conditions
model_sa_type_d_u <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_type_d_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_sa_type_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_type_d_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_sa_type_e_u <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_type_e_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_sa_type_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_type_e_a, conf.int = TRUE) # Save key results
# If unhappy, then can do manually with confint(model_sa_type_e_a, parm = "disruption_appointments")
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model f: Any hospital admission
model_sa_type_f_u <- svyglm(total_admissions ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = sensitivity_analyses, family = "binomial", design = wgt2) # Unadjusted
hold <- tidy(model_sa_type_f_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_sa_type_f_a <- svyglm(total_admissions ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = sensitivity_analyses, family = "binomial", design = wgt2) # Adjusted
hold <- tidy(model_sa_type_f_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

write.csv(table3, file = "S:/LLC_0009/outputs/regression_model2_sa.csv") # Save


gc()
