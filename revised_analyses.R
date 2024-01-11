#######################
### Paper revisions ###
#######################

# All additional analyses as part of the paper review process where I have not edited the main files themselves.

# Libraries
library(data.table)
library(survey)
library(broom)
library(mice)
set.seed(19610704) # So can reproduce results


### 1. Load data ###

# Load data and tidy
load(file = "S:/LLC_0009/data/Cleaned Cohorts/imputed_data.RData")

# Calculate any disruption from the imputed dataset
all_data$imp_disruption_any <- all_data$imp_disruption_appointments + all_data$imp_disruption_procedures + all_data$imp_disruption_medications # Add together
all_data$imp_disruption_any[all_data$imp_disruption_any > 1] <- 1 # Recode to binary yes/no option

# Dealing with weights
options(survey.adjust.domain.lonely = TRUE) # Let adjustments be made
options(survey.lonely.psu = "adjust") # Rather than the code fail, we set it to handle single PSUs by centering on the sample grand mean rather than strata mean - this is quite conservative

# Create measure for age-squared
all_data$imp_age2 <- all_data$imp_age^2

# Define survey design
wgt <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = all_data)


### 2. Descriptive table by disruption ###

# Unweighted tables

# Easier to add together the totals manually rather than me code them yes I am being lazy, but stressed getting these revisions done

# Sex
table1 <- data.frame(table(all_data$imp_sex, all_data$imp_disruption_any)) # Save frequency table
table1$measure <- "Sex" # Note measure

# Ethnicity
hold <- data.frame(table(all_data$imp_not_white, all_data$imp_disruption_any)) # Save frequency table
hold$measure <- "Ethnicity" # Note measure
table1 <- rbind(table1, hold) # Join onto main table

# Tenure
hold <- data.frame(table(all_data$imp_own_home, all_data$imp_disruption_any)) # Save frequency table
hold$measure <- "Tenure" # Note measure
table1 <- rbind(table1, hold) # Join onto main table

# Self-rated health
hold <- data.frame(table(all_data$imp_general_health, all_data$imp_disruption_any)) # Save frequency table
hold$measure <- "Self-rated health" # Note measure
table1 <- rbind(table1, hold) # Join onto main table

# IMD
hold <- data.frame(table(all_data$imp_imd2019_income_q5, all_data$imp_disruption_any)) # Save frequency table
hold$measure <- "IMD" # Note measure
table1 <- rbind(table1, hold) # Join onto main table

# Age
table1 <- rbind(table1, data.frame(Var1 = "age", Var2 = 0, Freq = mean(all_data$imp_age[all_data$imp_disruption_any == 0]), measure = "age")) # No disruption
table1 <- rbind(table1, data.frame(Var1 = "age", Var2 = 1, Freq = mean(all_data$imp_age[all_data$imp_disruption_any == 1]), measure = "age")) # Disruption
table1 <- rbind(table1, data.frame(Var1 = "age", Var2 = 1, Freq = mean(all_data$imp_age), measure = "age")) # Total

# Charlson index
table1 <- rbind(table1, data.frame(Var1 = "Charlson", Var2 = 0, Freq = mean(all_data$imp_quan_cci[all_data$imp_disruption_any == 0]), measure = "age")) # No disruption
table1 <- rbind(table1, data.frame(Var1 = "Charlson", Var2 = 1, Freq = mean(all_data$imp_quan_cci[all_data$imp_disruption_any == 1]), measure = "age")) # Disruption
table1 <- rbind(table1, data.frame(Var1 = "Charlson", Var2 = 1, Freq = mean(all_data$imp_quan_cci), measure = "age")) # Total

# Elixhauser index
table1 <- rbind(table1, data.frame(Var1 = "Elixhauser", Var2 = 0, Freq = mean(all_data$imp_unw_eci[all_data$imp_disruption_any == 0]), measure = "age")) # No disruption
table1 <- rbind(table1, data.frame(Var1 = "Elixhauser", Var2 = 1, Freq = mean(all_data$imp_unw_eci[all_data$imp_disruption_any == 1]), measure = "age")) # Disruption
table1 <- rbind(table1, data.frame(Var1 = "Elixhauser", Var2 = 1, Freq = mean(all_data$imp_unw_eci), measure = "age")) # Total

# Asthma
hold <- data.frame(table(all_data$imp_asthma, all_data$imp_disruption_any)) # Save frequency table
hold$measure <- "Asthma" # Note measure
table1 <- rbind(table1, hold) # Join onto main table

# Cancer
hold <- data.frame(table(all_data$imp_cancer, all_data$imp_disruption_any)) # Save frequency table
hold$measure <- "Cancer" # Note measure
table1 <- rbind(table1, hold) # Join onto main table

# Diabetes
hold <- data.frame(table(all_data$imp_diabetes, all_data$imp_disruption_any)) # Save frequency table
hold$measure <- "Diabetes" # Note measure
table1 <- rbind(table1, hold) # Join onto main table

# Hypertension
hold <- data.frame(table(all_data$imp_hypertension, all_data$imp_disruption_any)) # Save frequency table
hold$measure <- "Hypertension" # Note measure
table1 <- rbind(table1, hold) # Join onto main table

# Save
write.csv(table1, "../../outputs/revised_summary_table_unweighted.csv") # Save
rm(hold, table1) # Tidy


# Weighted tables

# Define survey design
disrup <- all_data[all_data$imp_disruption_any == 1,] # Subset into people who were disrupted
wgt_disrp <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = disrup) # Do survey design for above
nodisrup <- all_data[all_data$imp_disruption_any == 0,] # Subset into people who were not disrupted
wgt_nodisrp <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = nodisrup)


# Age
table1w <- data.frame(svymean(disrup$imp_age, wgt_disrp)) # Generate statistics - do for disruption first, then no disruption
table1w$measure <- "Age" # Note measure
table1w$exposure <- "Disruption" # Note exposure (disruption)
hold <- data.frame(svymean(nodisrup$imp_age, wgt_nodisrp)) # Generate statistics - do for disruption first, then no disruption
hold$measure <- "Age" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(all_data$imp_age, wgt)) # Generate statistics - overall
hold$measure <- "Age" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# Sex
hold <- data.frame(svymean(~factor(disrup$imp_sex), wgt_disrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Sex" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(nodisrup$imp_sex), wgt_nodisrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Sex" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(all_data$imp_sex), wgt)) # Generate statistics (used mean here but is basically the proportion since binary) - all cases
hold$measure <- "Sex" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# Ethnicity
hold <- data.frame(svymean(disrup$imp_not_white, wgt_disrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Not white" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(nodisrup$imp_not_white, wgt_nodisrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Not white" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(all_data$imp_not_white), wgt)) # Generate statistics (used mean here but is basically the proportion since binary) - all cases
hold$measure <- "Not white" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# Tenure
hold <- data.frame(svymean(disrup$imp_own_home, wgt_disrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Tenure" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(nodisrup$imp_own_home, wgt_nodisrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Tenure" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(all_data$imp_own_home), wgt)) # Generate statistics (used mean here but is basically the proportion since binary) - all cases
hold$measure <- "Tenure" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# Self-rated health
hold <- data.frame(svymean(~factor(disrup$imp_general_health), wgt_disrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Self-rated health" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(nodisrup$imp_general_health), wgt_nodisrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Self-rated health" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(all_data$imp_general_health), wgt)) # Generate statistics (used mean here but is basically the proportion since binary) - all cases
hold$measure <- "Self-rated health" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# IMD
hold <- data.frame(svymean(~factor(disrup$imp_imd2019_income_q5), wgt_disrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "IMD" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(nodisrup$imp_imd2019_income_q5), wgt_nodisrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "IMD" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(all_data$imp_imd2019_income_q5), wgt)) # Generate statistics (used mean here but is basically the proportion since binary) - all cases
hold$measure <- "IMD" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# Charlson
hold <- data.frame(svymean(disrup$imp_quan_cci, wgt_disrp)) # Generate statistics - do for disruption first, then no disruption
hold$measure <- "Charlson" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(nodisrup$imp_quan_cci, wgt_nodisrp)) # Generate statistics - do for disruption first, then no disruption
hold$measure <- "Charlson" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(all_data$imp_quan_cci, wgt)) # Generate statistics - overall
hold$measure <- "Charlson" # Note measure
hold$exposure <- "Total" # Note exposure (all cases - total)
table1w <- rbind(table1w, hold) # Join onto main table

# Elixhauser (unweighted)
hold <- data.frame(svymean(disrup$imp_unw_eci, wgt_disrp)) # Generate statistics - do for disruption first, then no disruption
hold$measure <- "Elixhauser" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(nodisrup$imp_unw_eci, wgt_nodisrp)) # Generate statistics - do for disruption first, then no disruption
hold$measure <- "Elixhauser" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(all_data$imp_unw_eci, wgt)) # Generate statistics - overall
hold$measure <- "Elixhauser" # Note measure
hold$exposure <- "Total" # Note exposure (all cases - total)
table1w <- rbind(table1w, hold) # Join onto main table

# Asthma
hold <- data.frame(svymean(~factor(disrup$imp_asthma), wgt_disrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Asthma" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(nodisrup$imp_asthma), wgt_nodisrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Asthma" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(all_data$imp_asthma), wgt)) # Generate statistics (used mean here but is basically the proportion since binary) - all cases
hold$measure <- "Asthma" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# Cancer
hold <- data.frame(svymean(~factor(disrup$imp_cancer), wgt_disrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Cancer" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(nodisrup$imp_cancer), wgt_nodisrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Cancer" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(all_data$imp_cancer), wgt)) # Generate statistics (used mean here but is basically the proportion since binary) - all cases
hold$measure <- "Cancer" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# Diabetes
hold <- data.frame(svymean(~factor(disrup$imp_diabetes), wgt_disrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Diabetes" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(nodisrup$imp_diabetes), wgt_nodisrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Diabetes" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(all_data$imp_diabetes), wgt)) # Generate statistics (used mean here but is basically the proportion since binary) - all cases
hold$measure <- "Diabetes" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# Hypertension
hold <- data.frame(svymean(~factor(disrup$imp_hypertension), wgt_disrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Hypertension" # Note measure
hold$exposure <- "Disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(nodisrup$imp_hypertension), wgt_nodisrp)) # Generate statistics (used mean here but is basically the proportion since binary) - do for disruption first, then no disruption
hold$measure <- "Hypertension" # Note measure
hold$exposure <- "No disruption" # Note exposure (disruption)
table1w <- rbind(table1w, hold) # Join onto main table
hold <- data.frame(svymean(~factor(all_data$imp_hypertension), wgt)) # Generate statistics (used mean here but is basically the proportion since binary) - all cases
hold$measure <- "Hypertension" # Note measure
hold$exposure <- "Total" # Note exposure (total - all cases)
table1w <- rbind(table1w, hold) # Join onto main table

# Save
write.csv(table1w, "../../outputs/revised_summary_table_weighted.csv") # Save
rm(hold, table1w) # Tidy


### 3. Checking impact of including COVID-19 into the model ###

# Note: I am not sure there is a conceptual justification for the inclusion of COVID-19 in the model (i.e., why having COVID-19 would lead to an avoidable hospitalisation for conditions that do not include COVID-19)

# I am checking only disruption overall since if there is noticeable overall impact, there is justification to go deeper

# All ambulatory care sensitive condition
model <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_covid), data = all_data, family = "binomial", design = wgt)
summary(model) # Print summary of results 
# Here it has no impact on the model really - COVID-19 infection has no association or an imprecise one. The effect size for disruption is hardly adjusted as a result.
table2 <- tidy(model, conf.int = TRUE) # Save key results
table2 <- table2[2,] # Extract only healthcare disruption variable
table2$model <- "All ACS - adj" # Note model name

# Any ambulatory care sensitive acute conditions
model_any_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci  + factor(imp_covid), data = all_data, family = "binomial", design = wgt) # Adjusted
summary(model_any_b_a) # Print summary of results
hold <- tidy(model_any_b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Any ambulatory care sensitive chronic conditions
model_any_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_covid), data = all_data, family = "binomial", design = wgt) # Adjusted
summary(model_any_c_a) # Print summary of results
hold <- tidy(model_any_c_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# d: Any ambulatory care sensitive conditions
model_any_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_covid), data = all_data, family = "binomial", design = wgt) # Adjusted
summary(model_any_d_a) # Print summary of results
hold <- tidy(model_any_d_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# e: Emergency Urgent Sensitive conditions
model_any_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_covid), data = all_data, family = "binomial", design = wgt) # Adjusted
summary(model_any_e_a) # Print summary of results
hold <- tidy(model_any_e_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# f: Emergency Urgent Sensitive conditions
model_any_f_a <- svyglm(total_admissions ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_covid), data = all_data, family = "binomial", design = wgt) # Adjusted
summary(model_any_f_a) # Print summary of results
hold <- tidy(model_any_f_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

write.csv(table2, "../../outputs/regression_model1_withcovid.csv") # Save
rm(table2) # Tidy


### 4. Including whether people had an avoidable hospitalisation prior to the pandemic ###

# Load data
load(file = "S:/LLC_0009/data/Cleaned Cohorts/nhs_hes_1yearpre.RData") # 1 year pre 1st March 2020
hes$`Ambulatory Care Sensitive All` <- hes$`Ambulatory Care Sensitive Acute` + hes$`Ambulatory Care Sensitive Chronic` + hes$`Ambulatory Care Sensitive Vaccine-preventable`
# load(file = "S:/LLC_0009/data/Cleaned Cohorts/nhs_hes_2019.RData") # Only 2019

# Join onto main dataset
hold <- merge(all_data, hes, by.x = "LLC_0009_stud_id", by.y = "llc_0009_stud_id", all.x = TRUE)
hold <- hold[!is.na(hold$LLC_0009_stud_id),] # Above creates some blank IDs so lets drop them

# Count numbers who had a previous admission for each outcome
nrow(hold[!is.na (hold$`Ambulatory Care Sensitive All.y`) & hold$`Ambulatory Care Sensitive All.y` >= 1,])
nrow(hold[!is.na (hold$`Ambulatory Care Sensitive Acute.y`) & hold$`Ambulatory Care Sensitive Acute.y` >= 1,])
nrow(hold[!is.na (hold$`Ambulatory Care Sensitive Chronic.y`) & hold$`Ambulatory Care Sensitive Chronic.y` >= 1,])
nrow(hold[!is.na (hold$`Ambulatory Care Sensitive Vaccine-preventable.y`) & hold$`Ambulatory Care Sensitive Vaccine-preventable.y` >= 1,])
nrow(hold[!is.na (hold$`Emergency Urgent Care Sensitive .y`) & hold$`Emergency Urgent Care Sensitive .y` >= 1,])
nrow(hold[!is.na (hold$`total_admissions.y`) & hold$`total_admissions.y` >= 1,])

# Clean data
hold$`Ambulatory Care Sensitive All.y`[is.na(hold$`Ambulatory Care Sensitive All.y`)] <- 0 # Missing data means has not had one
hold$`Ambulatory Care Sensitive All.y`[hold$`Ambulatory Care Sensitive All.y` > 1] <- 1 # Change into binary
hold$`Ambulatory Care Sensitive Acute.y`[is.na(hold$`Ambulatory Care Sensitive Acute.y`)] <- 0 # Repeat one by one
hold$`Ambulatory Care Sensitive Acute.y`[hold$`Ambulatory Care Sensitive Acute.y` > 1] <- 1
hold$`Ambulatory Care Sensitive Chronic.y`[is.na(hold$`Ambulatory Care Sensitive Chronic.y`)] <- 0 
hold$`Ambulatory Care Sensitive Chronic.y`[hold$`Ambulatory Care Sensitive Chronic.y` > 1] <- 1
hold$`Ambulatory Care Sensitive Vaccine-preventable.y`[is.na(hold$`Ambulatory Care Sensitive Vaccine-preventable.y`)] <- 0 
hold$`Ambulatory Care Sensitive Vaccine-preventable.y`[hold$`Ambulatory Care Sensitive Vaccine-preventable.y` > 1] <- 1
hold$`Emergency Urgent Care Sensitive .y`[is.na(hold$`Emergency Urgent Care Sensitive .y`)] <- 0 
hold$`Emergency Urgent Care Sensitive .y`[hold$`Emergency Urgent Care Sensitive .y` > 1] <- 1
hold$`total_admissions.y`[is.na(hold$`total_admissions.y`)] <- 0 
hold$`total_admissions.y`[hold$`total_admissions.y` > 1] <- 1

# Calculate sample weights
wgt_new <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = hold)

## 1. Any disruption

## All ambulatory care sensitive ##
model <- svyglm(`Ambulatory Care Sensitive All.x` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Ambulatory Care Sensitive All.x`), data = hold, family = "binomial", design = wgt_new) # Run model
table3 <- tidy(model, conf.int = TRUE) # Save key results
table3 <- table3[2,] # Extract only healthcare disruption variable
table3$model <- "All ACS - previous adm" # Note model name

# Acute ambulatory care sensitive acute conditions
model <- svyglm(`Ambulatory Care Sensitive Acute.x` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Ambulatory Care Sensitive Acute.x`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2,] # Extract only healthcare disruption variable
temp$model <- "Acute ACS - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table

# Chronic ambulatory care sensitive acute conditions
model <- svyglm(`Ambulatory Care Sensitive Chronic.x` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Ambulatory Care Sensitive Chronic.x`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2,] # Extract only healthcare disruption variable
temp$model <- "Chronic ACS - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table

# Vaccine-preventable ambulatory care sensitive acute conditions
model <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable.x` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Ambulatory Care Sensitive Vaccine-preventable.x`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2,] # Extract only healthcare disruption variable
temp$model <- "Vaccine ACS - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table

# Emergency Urgent Care Sensitive conditions
model <- svyglm(`Emergency Urgent Care Sensitive .x` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Emergency Urgent Care Sensitive .y`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2,] # Extract only healthcare disruption variable
temp$model <- "EUCS ACS - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table

# Total admissions
model <- svyglm(`total_admissions.y` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`total_admissions.y`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2,] # Extract only healthcare disruption variable
temp$model <- "Total adm - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table

write.csv(table3, file = "S:/LLC_0009/outputs/regression_model1_prevadm.csv") # Save
rm(table3)


## 2. Disruption type ##


## All ambulatory care sensitive ##
model <- svyglm(`Ambulatory Care Sensitive All.x` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Ambulatory Care Sensitive All.x`), data = hold, family = "binomial", design = wgt_new) # Run model
table3 <- tidy(model, conf.int = TRUE) # Save key results
table3 <- table3[2:4,] # Extract only healthcare disruption variable
table3$model <- "All ACS - previous adm" # Note model name

# Acute ambulatory care sensitive acute conditions
model <- svyglm(`Ambulatory Care Sensitive Acute.x` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Ambulatory Care Sensitive Acute.x`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2:4,] # Extract only healthcare disruption variable
temp$model <- "Acute ACS - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table

# Chronic ambulatory care sensitive acute conditions
model <- svyglm(`Ambulatory Care Sensitive Chronic.x` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Ambulatory Care Sensitive Chronic.x`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2:4,] # Extract only healthcare disruption variable
temp$model <- "Chronic ACS - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table

# Vaccine-preventable ambulatory care sensitive acute conditions
model <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable.x` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Ambulatory Care Sensitive Vaccine-preventable.x`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2:4,] # Extract only healthcare disruption variable
temp$model <- "Vaccine ACS - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table

# Emergency Urgent Care Sensitive conditions
model <- svyglm(`Emergency Urgent Care Sensitive .x` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`Emergency Urgent Care Sensitive .y`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2:4,] # Extract only healthcare disruption variable
temp$model <- "EUCS ACS - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table

# Total admissions
model <- svyglm(`total_admissions.y` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension) + factor(`total_admissions.y`), data = hold, family = "binomial", design = wgt_new) # Run model
temp <- tidy(model, conf.int = TRUE) # Save key results
temp <- temp[2:4,] # Extract only healthcare disruption variable
temp$model <- "Total adm - previous adm" # Note model name
table3 <- rbind(table3, temp) # Join to main table


write.csv(table3, file = "S:/LLC_0009/outputs/regression_model2_prevadm.csv") # Save
rm(table3)


### 5. Complete case analysis ###

# Data cleaning 
all_data$age_squared <- all_data$age^2

# Part 1: Any type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_any_a_u <- svyglm(`Ambulatory Care Sensitive All` ~ disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
table4 <- tidy(model_any_a_u, conf.int = TRUE) # Save key results
table4 <- table4[2,] # Extract only healthcare disruption variable
table4$model <- "All ACS - unadj" # Note model name

model_any_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ disruption_any + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_a_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

# Model b: Any ambulatory care sensitive acute conditions
model_any_b_u <- svyglm(`Ambulatory Care Sensitive Acute` ~ disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_b_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

model_any_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ disruption_any + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

# Model c: Any ambulatory care sensitive chronic conditions
model_any_c_u <- svyglm(`Ambulatory Care Sensitive Chronic` ~ disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_c_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

model_any_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ disruption_any + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_c_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

# Model d: Any ambulatory care sensitive conditions
model_any_d_u <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_d_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

model_any_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ disruption_any + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_d_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_any_e_u <- svyglm(`Emergency Urgent Care Sensitive ` ~ disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_e_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

model_any_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ disruption_any + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_e_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

# Model f: Emergency Urgent Sensitive conditions
model_any_f_u <- svyglm(total_admissions ~ disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_f_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

model_any_f_a <- svyglm(total_admissions ~ disruption_any + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_f_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table4 <- rbind(table4, hold) # Join to main table

write.csv(table4, "../../outputs/regression_model1_completecase.csv") # Save
rm(table4) # Tidy

# Part 2: Type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_type_a_u <- svyglm(`Ambulatory Care Sensitive All` ~ disruption_appointments + disruption_medications + disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
table5 <- tidy(model_type_a_u, conf.int = TRUE) # Save key results
table5 <- table5[2:4,] # Extract only healthcare disruption variable
table5$model <- "All ACS - unadj" # Note model name

model_type_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ disruption_appointments + disruption_medications + disruption_procedures + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_a_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

# Model b: Acute ambulatory care sensitive acute conditions
model_type_b_u <- svyglm(`Ambulatory Care Sensitive Acute` ~ disruption_appointments + disruption_medications + disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_b_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

model_type_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ disruption_appointments + disruption_medications + disruption_procedures + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_b_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

# Model c: Chronic ambulatory care sensitive chronic conditions
model_type_c_u <- svyglm(`Ambulatory Care Sensitive Chronic` ~ disruption_appointments + disruption_medications + disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_c_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

model_type_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ disruption_appointments + disruption_medications + disruption_procedures + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_c_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

# Model d: Vaccinhe preventable ambulatory care sensitive conditions
model_type_d_u <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ disruption_appointments + disruption_medications + disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_d_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

model_type_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ disruption_appointments + disruption_medications + disruption_procedures + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_d_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_type_e_u <- svyglm(`Emergency Urgent Care Sensitive ` ~ disruption_appointments + disruption_medications + disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_e_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

model_type_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ disruption_appointments + disruption_medications + disruption_procedures + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_e_a, conf.int = TRUE) # Save key results
# If unhappy, then can do manually with confint(model_type_e_a, parm = "disruption_appointments")
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

# Model f: Any hospital admission
model_type_f_u <- svyglm(total_admissions ~ disruption_appointments + disruption_medications + disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_f_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

model_type_f_a <- svyglm(total_admissions ~ disruption_appointments + disruption_medications + disruption_procedures + factor(sex) + age + age_squared + factor(not_white) + factor(own_home) + factor(general_health) + factor(imd2019_income_q5) + factor(cohort) + imp_quan_cci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_f_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table5 <- rbind(table5, hold) # Join to main table

write.csv(table5, file = "S:/LLC_0009/outputs/regression_model2_completecase.csv") # Save


### 6. Use Elixhauser index instead of Charlson ###

# Part 1: Any type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_any_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
table2 <- tidy(model_any_a_a, conf.int = TRUE) # Save key results
table2 <- table2[2,] # Extract only healthcare disruption variable
table2$model <- "All ACS - adj" # Note model name

# Model b: Any ambulatory care sensitive acute conditions
model_any_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model c: Any ambulatory care sensitive chronic conditions
model_any_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_c_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model d: Any ambulatory care sensitive conditions
model_any_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_d_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_any_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_e_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model f: Emergency Urgent Sensitive conditions
model_any_f_a <- svyglm(total_admissions ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_f_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

write.csv(table2, "../../outputs/regression_model1_elix.csv") # Save
rm(table2) # Tidy


# Part 2: Type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_type_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
table3 <- tidy(model_type_a_a, conf.int = TRUE) # Save key results
table3 <- table3[2:4,] # Extract only healthcare disruption variable
table3$model <- "All ACS - adj" # Note model name

# Model b: Acute ambulatory care sensitive acute conditions
model_type_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_b_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model c: Chronic ambulatory care sensitive chronic conditions
model_type_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_c_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model d: Vaccinhe preventable ambulatory care sensitive conditions
model_type_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_d_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_type_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_e_a, conf.int = TRUE) # Save key results
# If unhappy, then can do manually with confint(model_type_e_a, parm = "disruption_appointments")
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model f: Any hospital admission
model_type_f_a <- svyglm(total_admissions ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_unw_eci, data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_f_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

write.csv(table3, file = "S:/LLC_0009/outputs/regression_model2_elix.csv") # Save

### 7. Include four measurable health conditions ###

# Part 1: Any type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_any_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
table2 <- tidy(model_any_a_a, conf.int = TRUE) # Save key results
table2 <- table2[2,] # Extract only healthcare disruption variable
table2$model <- "All ACS - adj" # Note model name

# Model b: Any ambulatory care sensitive acute conditions
model_any_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model c: Any ambulatory care sensitive chronic conditions
model_any_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_c_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model d: Any ambulatory care sensitive conditions
model_any_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_d_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_any_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_e_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model f: Emergency Urgent Sensitive conditions
model_any_f_a <- svyglm(total_admissions ~ imp_disruption_any + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_f_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

write.csv(table2, "../../outputs/regression_model1_hcond.csv") # Save
rm(table2) # Tidy


# Part 2: Type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_type_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
table3 <- tidy(model_type_a_a, conf.int = TRUE) # Save key results
table3 <- table3[2:4,] # Extract only healthcare disruption variable
table3$model <- "All ACS - adj" # Note model name

# Model b: Acute ambulatory care sensitive acute conditions
model_type_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_b_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model c: Chronic ambulatory care sensitive chronic conditions
model_type_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_c_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model d: Vaccinhe preventable ambulatory care sensitive conditions
model_type_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_d_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_type_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_e_a, conf.int = TRUE) # Save key results
# If unhappy, then can do manually with confint(model_type_e_a, parm = "disruption_appointments")
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model f: Any hospital admission
model_type_f_a <- svyglm(total_admissions ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + imp_age2 + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort) + imp_quan_cci + factor(imp_asthma) + factor(imp_cancer) + factor(imp_diabetes) + factor(imp_hypertension), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_f_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

write.csv(table3, file = "S:/LLC_0009/outputs/regression_model2_hcond.csv") # Save

