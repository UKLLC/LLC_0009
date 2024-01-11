########################################
### Compare V0001 and V0002 datasets ###
########################################

# Aim: To compare how the missing data issues affected the variables we used in our analyses.

# Libraries
library(survey)
library(broom)
set.seed(19610704) # So can reproduce results

### Set up data in R ###

# Load imputed data and tidy
load(file = "S:/LLC_0009/data/Cleaned Cohorts/imputed_data.RData")

# Calculate any disruption from the imputed dataset
all_data$imp_disruption_any <- all_data$imp_disruption_appointments + all_data$imp_disruption_procedures + all_data$imp_disruption_medications # Add together
all_data$imp_disruption_any[all_data$imp_disruption_any > 1] <- 1 # Recode to binary yes/no option

# Calculate age-squared
all_data$imp_age2 <- all_data$imp_age^2

# Load in ID lookup table
lkup <- read.csv(file = "S:/LLC_0009/llc_guidance/stud_id15_18_lookup_0009.csv", fileEncoding = "UTF-8-BOM") # So can match the old and new IDs
lkup$cohort <- NULL #  Drop as not needed
all_data <- merge(all_data, lkup, by.x = "LLC_0009_stud_id", by.y = "llc_0009_stud_id18", all.x = TRUE) # Join on lookup table to imputed data file
rm(lkup)

# Set up R
options(survey.adjust.domain.lonely = TRUE) # Let adjustments be made
options(survey.lonely.psu = "adjust") # Rather than the code fail, we set it to handle single PSUs by centering on the sample grand mean rather than strata mean - this is quite conservative


## Vaccination records ##

# Drop affected variables
all_data$vax_doses <- NULL

# Load on V001 data
load("S:/LLC_0009/data/Cleaned Cohorts/V0001/nhs_vaccinated.RData") # COVID-19 vaccination records for England and Wales (v001)
names(vax)[names(vax) == "vax_doses"] <- "vax_doses_v0001" # Rename variable
all_data <- merge(all_data, vax, by.x = "LLC_0009_stud_id", by.y = "llc_0009_stud_id", all.x = TRUE) # Join data together
rm(vax) # Tidy

# Fully vaccinated or not
all_data$vaccinated_v001 <- NA # Create new variable
all_data$vaccinated_v001[is.na(all_data$vax_doses_v0001) | all_data$vax_doses_v0001 == 1] <- 0 # If missing, then is 0
all_data$vaccinated_v001[all_data$vax_doses_v0001 > 1] <- 1

# Load V002 data
load("S:/LLC_0009/data/Cleaned Cohorts/nhs_vaccinated.RData") # England and Wales (v002)
names(vax)[names(vax) == "vax_doses"] <- "vax_doses_v0002" # Rename variable
vax$llc_0009_stud_id <- as.numeric(vax$llc_0009_stud_id) # To match main data class type
all_data <- merge(all_data, vax, by.x = "llc_0009_stud_id15", by.y = "llc_0009_stud_id", all.x = TRUE) # Join data together
rm(vax) # Tidy

# Fully vaccinated or not
all_data$vaccinated_v002 <- NA # Create new variable
all_data$vaccinated_v002[is.na(all_data$vax_doses_v0002) | all_data$vax_doses_v0002 == 1] <- 0 # If missing, then is 0
all_data$vaccinated_v002[all_data$vax_doses_v0002 > 1] <- 1

# Vaccination record incorrect
all_data$vax_wrong <- 0 # Create blank variable (i.e., no misclassification)
all_data$vax_wrong[all_data$vaccinated_v001 == 0 & all_data$vaccinated_v002 == 1] <- 1



### Compare outcome variables ###

# Define survey design
wgt <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = all_data)

## Vaccination status ##

# Descriptive statistics - overall
table(all_data$vaccinated_v001, all_data$vaccinated_v002, exclude = NULL) # Variable used - unweighted (note the rows are v0001 and the columns are v002)
svymean(all_data$vaccinated_v001, wgt) # weighted - v001 value
svymean(all_data$vaccinated_v002, wgt) # weighted - v002 value

# Descriptive statistics - by overall disruption
table(all_data$vaccinated_v001, all_data$vaccinated_v002, all_data$imp_disruption_any, exclude = NULL) # Give frequency counts by if experienced disruption or not


# Regression model

# Model a: Any disruption
model8a_u <- svyglm(vaccinated_v001 ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
table6 <- tidy(model8a_u, conf.int = TRUE) # Save key results
table6 <- table6[2,] # Extract only healthcare disruption variable
table6$model <- "Any - unadj - v001" # Note model name

model8b_a <- svyglm(vaccinated_v002 ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model8b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any - unadj - v002" # Note model name
table6 <- rbind(table6, hold) # Join to main table

model8a_a <- svyglm(vaccinated_v001 ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model8a_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any - adj - v001" # Note model name
table6 <- rbind(table6, hold) # Join to main table

model8b_a <- svyglm(vaccinated_v002 ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model8b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any - adj - v002" # Note model name
table6 <- rbind(table6, hold) # Join to main table

# Model b: Disruption by type
model8c_u <- svyglm(vaccinated_v001 ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model8c_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - unadj - v001" # Note model name
table6 <- rbind(table6, hold) # Join to main table

model8d_u <- svyglm(vaccinated_v002 ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model8d_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - unadj - v002" # Note model name
table6 <- rbind(table6, hold) # Join to main table

model8c_a <- svyglm(vaccinated_v001 ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model8c_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - adj - v001" # Note model name
table6 <- rbind(table6, hold) # Join to main table

model8d_a <- svyglm(vaccinated_v002 ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model8d_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - adj - v002" # Note model name
table6 <- rbind(table6, hold) # Join to main table

table6
write.csv(table6, "../../outputs/falsification_compare_v1_v2.csv") # Save
gc()

# Which factors were predictive of misclassification

model9a_u <- svyglm(vax_wrong ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
table7 <- tidy(model9a_u, conf.int = TRUE) # Save key results
table7 <- table7[2,] # Extract only healthcare disruption variable
table7$model <- "Any - unadj" # Note model name

model9a_a <- svyglm(vax_wrong ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model9a_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any - adj" # Note model name
table7 <- rbind(table7, hold) # Join to main table

model9b_u <- svyglm(vax_wrong ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model9b_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - unadj" # Note model name
table7 <- rbind(table7, hold) # Join to main table

model9b_a <- svyglm(vax_wrong ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model9b_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - adj" # Note model name
table7 <- rbind(table7, hold) # Join to main table

table7
write.csv(table7, "../../outputs/vax_misclass_analysis.csv") # Save
gc()


## Hospital admissions ##

# Drop affected variables
all_data$total_admissions <- NULL
all_data$`Ambulatory Care Sensitive All` <- NULL
all_data$`Ambulatory Care Sensitive Acute` <- NULL
all_data$`Ambulatory Care Sensitive Chronic` <- NULL
all_data$`Ambulatory Care Sensitive Vaccine-preventable` <- NULL
all_data$`Emergency Urgent Care Sensitive ` <- NULL

# Load in V001 measures 
load("S:/LLC_0009/data/Cleaned Cohorts/V0001/nhs_hes.RData") # Load HES outcome vars

# Join on HES data
all_data <- merge(all_data, hes, by.x = "LLC_0009_stud_id", by.y = "llc_0009_stud_id", all.x = TRUE) # Join data together
all_data$`Ambulatory Care Sensitive Acute`[is.na(all_data$`Ambulatory Care Sensitive Acute`)] <- 0 # Fill in NAs with 0s for joined on outcome measures (since if no match then they did not have a hospital admission)
all_data$`Ambulatory Care Sensitive Chronic`[is.na(all_data$`Ambulatory Care Sensitive Chronic`)] <- 0
all_data$`Ambulatory Care Sensitive Vaccine-preventable`[is.na(all_data$`Ambulatory Care Sensitive Vaccine-preventable`)] <- 0
all_data$`Emergency Urgent Care Sensitive `[is.na(all_data$`Emergency Urgent Care Sensitive `)] <- 0
all_data$total_admissions[is.na(all_data$total_admissions)] <- 0
all_data$`Ambulatory Care Sensitive All` <- all_data$`Ambulatory Care Sensitive Acute` + all_data$`Ambulatory Care Sensitive Chronic` + all_data$`Ambulatory Care Sensitive Vaccine-preventable` # Create overall ambulatory care sensitive measure

# Convert outcome variables to binary variables
all_data$`Ambulatory Care Sensitive All`[all_data$`Ambulatory Care Sensitive All` > 1] <- 1
all_data$`Ambulatory Care Sensitive Acute`[all_data$`Ambulatory Care Sensitive Acute` > 1] <- 1
all_data$`Ambulatory Care Sensitive Chronic`[all_data$`Ambulatory Care Sensitive Chronic` > 1] <- 1
all_data$`Ambulatory Care Sensitive Vaccine-preventable`[all_data$`Ambulatory Care Sensitive Vaccine-preventable` > 1] <- 1
all_data$`Emergency Urgent Care Sensitive `[all_data$`Emergency Urgent Care Sensitive ` > 1] <- 1
all_data$total_admissions[all_data$total_admissions > 1] <- 1

# Rename variables
names(all_data)[names(all_data) == "Ambulatory Care Sensitive All"] <- "Ambulatory Care Sensitive All V001" 
names(all_data)[names(all_data) == "Ambulatory Care Sensitive Acute"] <- "Ambulatory Care Sensitive Acute V001" 
names(all_data)[names(all_data) == "Ambulatory Care Sensitive Chronic"] <- "Ambulatory Care Sensitive Chronic V001" 
names(all_data)[names(all_data) == "Ambulatory Care Sensitive Vaccine-preventable"] <- "Ambulatory Care Sensitive Vaccine-preventable V001" 
names(all_data)[names(all_data) == "Emergency Urgent Care Sensitive "] <- "Emergency Urgent Care Sensitive V001" 
names(all_data)[names(all_data) == "total_admissions"] <- "total_admissions_V001"

rm(hes)


# Load in V002 measures 
load("S:/LLC_0009/data/Cleaned Cohorts/nhs_hes.RData") # Load HES outcome vars

# Join on HES data
all_data <- merge(all_data, hes, by.x = "llc_0009_stud_id15", by.y = "llc_0009_stud_id", all.x = TRUE) # Join data together
all_data$`Ambulatory Care Sensitive Acute`[is.na(all_data$`Ambulatory Care Sensitive Acute`)] <- 0 # Fill in NAs with 0s for joined on outcome measures (since if no match then they did not have a hospital admission)
all_data$`Ambulatory Care Sensitive Chronic`[is.na(all_data$`Ambulatory Care Sensitive Chronic`)] <- 0
all_data$`Ambulatory Care Sensitive Vaccine-preventable`[is.na(all_data$`Ambulatory Care Sensitive Vaccine-preventable`)] <- 0
all_data$`Emergency Urgent Care Sensitive `[is.na(all_data$`Emergency Urgent Care Sensitive `)] <- 0
all_data$total_admissions[is.na(all_data$total_admissions)] <- 0
all_data$`Ambulatory Care Sensitive All` <- all_data$`Ambulatory Care Sensitive Acute` + all_data$`Ambulatory Care Sensitive Chronic` + all_data$`Ambulatory Care Sensitive Vaccine-preventable` # Create overall ambulatory care sensitive measure

# Convert outcome variables to binary variables
all_data$`Ambulatory Care Sensitive All`[all_data$`Ambulatory Care Sensitive All` > 1] <- 1
all_data$`Ambulatory Care Sensitive Acute`[all_data$`Ambulatory Care Sensitive Acute` > 1] <- 1
all_data$`Ambulatory Care Sensitive Chronic`[all_data$`Ambulatory Care Sensitive Chronic` > 1] <- 1
all_data$`Ambulatory Care Sensitive Vaccine-preventable`[all_data$`Ambulatory Care Sensitive Vaccine-preventable` > 1] <- 1
all_data$`Emergency Urgent Care Sensitive `[all_data$`Emergency Urgent Care Sensitive ` > 1] <- 1
all_data$total_admissions[all_data$total_admissions > 1] <- 1

# Rename variables
names(all_data)[names(all_data) == "Ambulatory Care Sensitive All"] <- "Ambulatory Care Sensitive All V002" 
names(all_data)[names(all_data) == "Ambulatory Care Sensitive Acute"] <- "Ambulatory Care Sensitive Acute V002" 
names(all_data)[names(all_data) == "Ambulatory Care Sensitive Chronic"] <- "Ambulatory Care Sensitive Chronic V002" 
names(all_data)[names(all_data) == "Ambulatory Care Sensitive Vaccine-preventable"] <- "Ambulatory Care Sensitive Vaccine-preventable V002" 
names(all_data)[names(all_data) == "Emergency Urgent Care Sensitive "] <- "Emergency Urgent Care Sensitive V002" 
names(all_data)[names(all_data) == "total_admissions"] <- "total_admissions_V002"

rm(hes)


# Compare measures

# Overall
table(all_data$`Ambulatory Care Sensitive All V001`, all_data$`Ambulatory Care Sensitive All V002`)
table(all_data$`Ambulatory Care Sensitive Acute V001`, all_data$`Ambulatory Care Sensitive Acute V002`)
table(all_data$`Ambulatory Care Sensitive Chronic V001`, all_data$`Ambulatory Care Sensitive Chronic V002`)
table(all_data$`Ambulatory Care Sensitive Vaccine-preventable V001`, all_data$`Ambulatory Care Sensitive Vaccine-preventable V002`)
table(all_data$`Emergency Urgent Care Sensitive V001`, all_data$`Emergency Urgent Care Sensitive V002`)
table(all_data$total_admissions_V001, all_data$`total_admissions_V002`)

# By level of disruption
table(all_data$`Ambulatory Care Sensitive All V001`, all_data$`Ambulatory Care Sensitive All V002`, all_data$imp_disruption_any)
table(all_data$`Ambulatory Care Sensitive Acute V001`, all_data$`Ambulatory Care Sensitive Acute V002`, all_data$imp_disruption_any)
table(all_data$`Ambulatory Care Sensitive Chronic V001`, all_data$`Ambulatory Care Sensitive Chronic V002`, all_data$imp_disruption_any)
table(all_data$`Ambulatory Care Sensitive Vaccine-preventable V001`, all_data$`Ambulatory Care Sensitive Vaccine-preventable V002`, all_data$imp_disruption_any)
table(all_data$`Emergency Urgent Care Sensitive V001`, all_data$`Emergency Urgent Care Sensitive V002`, all_data$imp_disruption_any)
table(all_data$total_admissions_V001, all_data$`total_admissions_V002`, all_data$imp_disruption_any)

# Create misclassified variables
all_data$wrong_acs_all <- 0
all_data$wrong_acs_all[all_data$`Ambulatory Care Sensitive All V001` == 0 & all_data$`Ambulatory Care Sensitive All V002` == 1] <- 1

all_data$wrong_acs_acute <- 0
all_data$wrong_acs_acute[all_data$`Ambulatory Care Sensitive Acute V001` == 0 & all_data$`Ambulatory Care Sensitive Acute V002` == 1] <- 1

all_data$wrong_acs_chr <- 0
all_data$wrong_acs_chr[all_data$`Ambulatory Care Sensitive Chronic V001` == 0 & all_data$`Ambulatory Care Sensitive Chronic V002` == 1] <- 1

all_data$wrong_acs_vp <- 0
all_data$wrong_acs_vp[all_data$`Ambulatory Care Sensitive Vaccine-preventable V001` == 0 & all_data$`Ambulatory Care Sensitive Vaccine-preventable V002` == 1] <- 1

all_data$wrong_eucs <- 0
all_data$wrong_eucs[all_data$`Emergency Urgent Care Sensitive V001` == 0 & all_data$`Emergency Urgent Care Sensitive V002` == 1] <- 1

all_data$wrong_tot_adm <- 0
all_data$wrong_tot_adm[all_data$total_admissions_V001 == 0 & all_data$total_admissions_V002 == 1] <- 1

# Define survey design
wgt <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = all_data)

# Run regression models
# Part 1: Any type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_any_a_u <- svyglm(wrong_acs_all ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
table2 <- tidy(model_any_a_u, conf.int = TRUE) # Save key results
table2 <- table2[2,] # Extract only healthcare disruption variable
table2$model <- "All ACS - unadj" # Note model name

model_any_a_a <- svyglm(wrong_acs_all ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_a_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model b: Any ambulatory care sensitive acute conditions
model_any_b_u <- svyglm(wrong_acs_acute ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_b_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_b_a <- svyglm(wrong_acs_acute ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model c: Any ambulatory care sensitive chronic conditions
model_any_c_u <- svyglm(wrong_acs_chr ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_c_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_c_a <- svyglm(wrong_acs_chr ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_c_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model d: Any ambulatory care sensitive conditions vaccine-prev
model_any_d_u <- svyglm(wrong_acs_vp ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_d_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_d_a <- svyglm(wrong_acs_vp ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_d_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_any_e_u <- svyglm(wrong_eucs ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_e_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_e_a <- svyglm(wrong_eucs ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_e_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model f: Total admissions
model_any_f_u <- svyglm(wrong_tot_adm ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_f_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_f_a <- svyglm(wrong_tot_adm ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_f_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

table2
write.csv(table2, "../../outputs/regression_model1_misclass.csv") # Save
rm(table2) # Tidy


# Part 2: Type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_type_a_u <- svyglm(wrong_acs_all ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
table3 <- tidy(model_type_a_u, conf.int = TRUE) # Save key results
table3 <- table3[2:4,] # Extract only healthcare disruption variable
table3$model <- "All ACS - unadj" # Note model name

model_type_a_a <- svyglm(wrong_acs_all ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_a_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model b: Acute ambulatory care sensitive acute conditions
model_type_b_u <- svyglm(wrong_acs_acute ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_b_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_b_a <- svyglm(wrong_acs_acute ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_b_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model c: Chronic ambulatory care sensitive chronic conditions
model_type_c_u <- svyglm(wrong_acs_chr ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_c_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_c_a <- svyglm(wrong_acs_chr ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_c_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model d: Vaccinhe preventable ambulatory care sensitive conditions
model_type_d_u <- svyglm(wrong_acs_vp ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_d_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_d_a <- svyglm(wrong_acs_vp ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_d_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_type_e_u <- svyglm(wrong_eucs ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_e_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_e_a <- svyglm(wrong_eucs ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_e_a, conf.int = TRUE) # Save key results
# If unhappy, then can do manually with confint(model_type_e_a, parm = "disruption_appointments")
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model f: Any hospital admission
model_type_f_u <- svyglm(wrong_tot_adm ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_f_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_f_a <- svyglm(wrong_tot_adm ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_f_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

write.csv(table3, file = "S:/LLC_0009/outputs/regression_model2_misclass.csv") # Save



