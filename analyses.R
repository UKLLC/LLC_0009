################
### Analyses ###
################

# Note: Still under construction so please don't judge!

# Libraries
library(data.table)
library(survey)
library(broom)
library(mice)
set.seed(19610704) # So can reproduce results


### 1. Clean and load required data ###

# Load cohort data
# source("clean_cohorts.R") # Produces cleaned versions of all cohorts - only need to run once
load("S:/LLC_0009/data/Cleaned Cohorts/bcs70.RData") # 1970 cohort (currently issues linking data so not ready)
# load("S:/LLC_0009/data/Cleaned Cohorts/bib.RData") # Born in Bradford (we can't do this by disruption type and too much missing data so leave out)
load("S:/LLC_0009/data/Cleaned Cohorts/elsa.RData") # ELSA
# load("S:/LLC_0009/data/Cleaned Cohorts/genscot.RData") # Genscot
load("S:/LLC_0009/data/Cleaned Cohorts/mcs_cm.RData") # MCS - cohort members
# load("S:/LLC_0009/data/Cleaned Cohorts/mcs_parent_guardian.RData") # MCS - parent/guardians (leave out as too much missing data for age/ethnicity)
load("S:/LLC_0009/data/Cleaned Cohorts/ncds58.RData") # 1958 cohort
load("S:/LLC_0009/data/Cleaned Cohorts/nextstep.RData") # Nextsteps
load("S:/LLC_0009/data/Cleaned Cohorts/nshd_1946.RData") # 1946 cohort
load("S:/LLC_0009/data/Cleaned Cohorts/ukhls.RData") # Understanding Society 

# Combine all data into a single object 
all_data <- rbind(bcs70, elsa, mcs, ncds58, nextstep, nshd46, ukhls) # (genscot will be added at a later step post adding on HES data)
rm(bcs70, elsa, mcs, ncds58, nextstep, nshd46, ukhls) # Tidy

# # Save list of IDs and dates for cleaning HES data
# all_ids <- all_data[c("LLC_0009_stud_id", "date")] # Subset list of IDs
# save(all_ids, file = "S:/LLC_0009/data/Cleaned Cohorts/all_ids.RData") # Save
# rm(all_ids)

# Load HES data
# source("clean_hes.R") # Generate outcome data from HES records for England and Wales 
load("S:/LLC_0009/data/Cleaned Cohorts/nhs_hes.RData") # HES outcome vars

# Join on HES data
all_data <- merge(all_data, hes, by.x = "LLC_0009_stud_id", by.y = "llc_0009_stud_id", all.x = TRUE) # Join data together
all_data$`Ambulatory Care Sensitive Acute`[is.na(all_data$`Ambulatory Care Sensitive Acute`)] <- 0 # Fill in NAs with 0s for joined on outcome measures (since if no match then they did not have a hospital admission)
all_data$`Ambulatory Care Sensitive Chronic`[is.na(all_data$`Ambulatory Care Sensitive Chronic`)] <- 0
all_data$`Ambulatory Care Sensitive Vaccine-preventable`[is.na(all_data$`Ambulatory Care Sensitive Vaccine-preventable`)] <- 0
all_data$`Emergency Urgent Care Sensitive `[is.na(all_data$`Emergency Urgent Care Sensitive `)] <- 0
all_data$total_admissions[is.na(all_data$total_admissions)] <- 0
all_data$`Ambulatory Care Sensitive All` <- all_data$`Ambulatory Care Sensitive Acute` + all_data$`Ambulatory Care Sensitive Chronic` + all_data$`Ambulatory Care Sensitive Vaccine-preventable` # Create overall ambulatory care sensitive measure

# Join on vaccination status
load("S:/LLC_0009/data/Cleaned Cohorts/nhs_vaccinated.RData") # England and Wales
all_data <- merge(all_data, vax, by.x = "LLC_0009_stud_id", by.y = "llc_0009_stud_id", all.x = TRUE) # Join data together

# Join on geodata (IMD)
load(file = "S:/LLC_0009/data/Cleaned Cohorts/imd_linked.RData") # Load
all_data <- merge(all_data, geodata, by.x = "LLC_0009_stud_id", by.y = "llc_0009_stud_id", all.x = TRUE) # Join data together

# Keep only people who were are resident in England
all_data$country[is.na(all_data$country)] <- all_data$ctry17nm[is.na(all_data$country)] # If missing country then, populate with linked records
table(all_data$country) # Count number by country
all_data <- all_data[all_data$country == "England" & !is.na(all_data$country),] # Keep only people who live in England

# Join on list of who was linked
load(file = "S:/LLC_0009/data/Cleaned Cohorts/who_was_linked.RData")
all_data <- merge(all_data, linked_ppl, by.x = "LLC_0009_stud_id", by.y = "llc_0009_stud_id", all.x = TRUE) # Join data together
table(all_data$linked, exclude = NULL) # Check how many were linked
table(all_data$linked, all_data$disruption_appointments, exclude = NULL)
table(all_data$linked, all_data$disruption_medications, exclude = NULL)
table(all_data$linked, all_data$disruption_procedures, exclude = NULL)
all_data <- all_data[all_data$linked == 1 & !is.na(all_data$linked),] # Exclude cases not linked
rm(geodata, hes, linked_ppl, vax) # Tidy

# # Join on genscot
# names(genscot)[names(genscot) == "number_covid_vax"] <- "vax_doses" # Until re-run clean_cohorts.R will need this
# # genscot$total_admissions <- NULL # Need to drop until update HES data file
# genscot$`Ambulatory Care Sensitive All` <- genscot$`Ambulatory Care Sensitive Acute` + genscot$`Ambulatory Care Sensitive Chronic` + genscot$`Ambulatory Care Sensitive Vaccine-preventable` # Create overall variable
# all_data <- rbind(all_data, genscot)
# rm(genscot, vax, hes)

# Recode variables for analyses #

# Convert outcome variables to binary variables
all_data$`Ambulatory Care Sensitive All`[all_data$`Ambulatory Care Sensitive All` > 1] <- 1
all_data$`Ambulatory Care Sensitive Acute`[all_data$`Ambulatory Care Sensitive Acute` > 1] <- 1
all_data$`Ambulatory Care Sensitive Chronic`[all_data$`Ambulatory Care Sensitive Chronic` > 1] <- 1
all_data$`Ambulatory Care Sensitive Vaccine-preventable`[all_data$`Ambulatory Care Sensitive Vaccine-preventable` > 1] <- 1
all_data$`Emergency Urgent Care Sensitive `[all_data$`Emergency Urgent Care Sensitive ` > 1] <- 1
all_data$total_admissions[all_data$total_admissions > 1] <- 1

# Ethnicity
all_data$not_white <- NA
all_data$not_white[all_data$ethnicity == "White"] <- 0
all_data$not_white[all_data$ethnicity != "White"] <- 1

# Tenure
all_data$own_home <- NA
all_data$own_home[all_data$tenure == "Mortgage" | all_data$tenure == "Own outright"] <- 1
all_data$own_home[all_data$tenure != "Mortgage" & all_data$tenure != "Own outright"] <- 0

# COVID-19
all_data$covid[all_data$covid == "No"] <- 0
all_data$covid[all_data$covid == "Yes"] <- 1

# Fully vaccinated or not
all_data$vaccinated <- NA # Create new variable
all_data$vaccinated[is.na(all_data$vax_doses) | all_data$vax_doses == 1] <- 0 # If missing, then is 0
all_data$vaccinated[all_data$vax_doses > 1] <- 1

# Missing weights should be 1
all_data$weight[all_data$weight == -1] <- 1


# Accounting for deaths #

# Load data and clean
# load("S:/LLC_0009/data/Cleaned Cohorts/genscot_deaths.RData") # Genscot
load("S:/LLC_0009/data/Cleaned Cohorts/mortality.RData") # England and Wales
# genscot_death_v0001_20211101$died <- 1 # Create variable to note they died
nhsd_mortality_20220716$died <- 1
# genscot_death_v0001_20211101 <- genscot_death_v0001_20211101[, c(1,5)] # Drop unneccessary data
deaths <- nhsd_mortality_20220716[, c(1,4)]
names(deaths)[names(deaths) == "llc_0009_stud_id"] <- "LLC_0009_stud_id" # Rename
# deaths <- rbind(nhsd_mortality_20220716, genscot_death_v0001_20211101) # Join together
rm(nhsd_mortality_20220716)

# Join deaths data onto main dataset
all_data <- merge(all_data, deaths, by = "LLC_0009_stud_id", all.x = TRUE) # Join
all_data <- all_data[is.na(all_data$died),] # Remove participants who died - so the code only selects alive people as they have not died
rm(deaths)


### 2. Deal with missing data ###

# Subset data for imputation purposes
for_analysis <- all_data[, c("LLC_0009_stud_id", "age", "sex", "not_white", "own_home", "general_health", "imd2019_income_q5", "disruption_appointments", "disruption_medications", "disruption_procedures")] # include only variables will use

# Describe extent of missing data
miss_table <- data.frame(sapply(for_analysis, function(x) sum(is.na(x))))
names(miss_table)[names(miss_table) == "sapply.for_analysis..function.x..sum.is.na.x..."] <- "missing"
write.csv(miss_table, "../../outputs/missing_data.csv") # Save
rm(miss_table) # Tidy

# Get data ready for mice (set to factors rather than chr)
for_analysis$sex <- as.factor(for_analysis$sex)
for_analysis$general_health <- as.factor(for_analysis$general_health)


# Multivariate imputation for missing data
init <- mice(for_analysis, maxit = 0) # Run initial model to get starting points (i.e., 0 iterations)
meth <- init$method # Store information for when run main model
newpred <- quickpred(for_analysis) # Helps to prevent overfitting
imputed_data <- mice(for_analysis, meth = meth, maxit = 1, m = 1, pred = newpred) # Impute data (I tried to use multiple imputation but we don't have enough memory for this here in the TRE)
imputed_data <- data.table(complete(imputed_data)) # Create dataset
sapply(imputed_data, function(x) sum(is.na(x))) # Check imputation worked
rm(init, meth, newpred, for_analysis) # Tidy
gc()

# Combine with original dataset
colnames(imputed_data) <- paste("imp", colnames(imputed_data), sep = "_") # Rename all variables
all_data <- merge(all_data, imputed_data, by.x = "LLC_0009_stud_id", by.y = "imp_LLC_0009_stud_id", all.x = TRUE) # Merge

# Save analytical dataset
save(all_data, file = "S:/LLC_0009/data/Cleaned Cohorts/imputed_data.RData")
rm(all_data, imputed_data)
gc()


### 3. Descriptive statistics ###

# Load data and tidy
load(file = "S:/LLC_0009/data/Cleaned Cohorts/imputed_data.RData")

# Calculate any disruption from the imputed dataset
all_data$imp_disruption_any <- all_data$imp_disruption_appointments + all_data$imp_disruption_procedures + all_data$imp_disruption_medications # Add together
all_data$imp_disruption_any[all_data$imp_disruption_any > 1] <- 1 # Recode to binary yes/no option

# Dealing with the weights one last time
# We have an issue where some strata only have one PSU within them
# We can either place these cases into a single strata as below
# df <- data.frame(table(all_data$strata)) # Identify strata with only one case
# names(df)[names(df) == "Var1"] <- "strata" # Rename variable
# all_data <- merge(all_data, df, by = "strata", all.x = T) # Join onto main dataset
# all_data$strata[all_data$Freq == 1] <- 0 # Place all these cases (n=238) into a single strata for ease (rather than delete) - they are all from UKHLS
# # Removing them doesn't have any impact on the analyses
# all_data$Freq <- NULL # Delete
# rm(df) # Tidy
# Or do an automatic adjustment
options(survey.adjust.domain.lonely = TRUE) # Let adjustments be made
options(survey.lonely.psu = "adjust") # Rather than the code fail, we set it to handle single PSUs by centering on the sample grand mean rather than strata mean - this is quite conservative


# Unweighted tables #

# Population size of each cohort
table1 <- data.frame(table(all_data$cohort)) # Save table
table1$measure <- "Cohort" # Note measure

# Outcome variables
hold <- data.frame(table(all_data$`Ambulatory Care Sensitive All`)) # Save frequency table
hold$measure <- "Ambulatory Care Sensitive All" # Note outcome measure
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(all_data$`Ambulatory Care Sensitive Acute`)) # Repeat for each outcome
hold$measure <- "Ambulatory Care Sensitive Acute"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(all_data$`Ambulatory Care Sensitive Chronic`))
hold$measure <- "Ambulatory Care Sensitive Chronic"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(all_data$`Ambulatory Care Sensitive Vaccine-preventable`))
hold$measure <- "Ambulatory Care Sensitive Vaccine-preventable"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(all_data$`Emergency Urgent Care Sensitive `))
hold$measure <- "Emergency Urgent Care Sensitive"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(all_data$total_admissions))
hold$measure <- "Total admissions"
table1 <- rbind(table1, hold) # Join onto main table

hold <- data.frame(table(all_data$vaccinated)) 
hold$measure <- "Vaccinated"
table1 <- rbind(table1, hold)

# Explanatory variables
hold <- data.frame(table(all_data$imp_disruption_any)) # Repeat same process for explanatory variables
hold$measure <- "Disruption any"
table1 <- rbind(table1, hold)

hold <- data.frame(table(all_data$imp_disruption_appointments)) 
hold$measure <- "Disruption appointments"
table1 <- rbind(table1, hold)

hold <- data.frame(table(all_data$imp_disruption_medications))
hold$measure <- "Disruption medications"
table1 <- rbind(table1, hold)

hold <- data.frame(table(all_data$imp_disruption_procedures)) 
hold$measure <- "Disruption procedures"
table1 <- rbind(table1, hold)

# Save
write.csv(table1, "../../outputs/summary_table_unweighted.csv") # Save
rm(hold, table1) # Tidy


# Weighted tables #

# Define survey design
wgt <- svydesign(id = ~LLC_0009_stud_id, weights = ~weight, strata = ~strata, fpc = ~fpc, nest = TRUE, survey.lonely.psu = "adjust", data = all_data)

# Outcome variables
table1w <- data.frame(svymean(all_data$total_admissions, wgt)) # Generate statistics (used mean here but is basically the proportion since binary)
table1w$measure <- "Total admissions" # Note outcome measure

hold <- data.frame(svymean(all_data$`Ambulatory Care Sensitive All`, wgt)) # Repeat process
hold$measure <- "Ambulatory care sensitive any"
table1w <- rbind(table1w, hold) # Join onto main table

hold <- data.frame(svymean(all_data$`Ambulatory Care Sensitive Acute`, wgt))
hold$measure <- "Ambulatory care sensitive acute"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(all_data$`Ambulatory Care Sensitive Chronic`, wgt))
hold$measure <- "Ambulatory care sensitive chronic"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(all_data$`Ambulatory Care Sensitive Vaccine-preventable`, wgt))
hold$measure <- "Ambulatory care sensitive vaccine preventable"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(all_data$`Emergency Urgent Care Sensitive `, wgt))
hold$measure <- "Emergency urgent care sensitive"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(all_data$vaccinated, wgt))
hold$measure <- "Vaccinated"
table1w <- rbind(table1w, hold)

# Explanatory variables
hold <- data.frame(svymean(all_data$imp_disruption_any, wgt))
hold$measure <- "Disruption any"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(all_data$imp_disruption_appointments, wgt))
hold$measure <- "Disruption appointments"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(all_data$imp_disruption_medications, wgt))
hold$measure <- "Disruption medications"
table1w <- rbind(table1w, hold)

hold <- data.frame(svymean(all_data$imp_disruption_procedures, wgt))
hold$measure <- "Disruption procedures"
table1w <- rbind(table1w, hold)

# Save
write.csv(table1w, "../../outputs/summary_table_weighted.csv") # Save
rm(hold, table1w) # Tidy


### 4. Meta-regression analysis ###

# Part 1: Any type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_any_a_u <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
table2 <- tidy(model_any_a_u, conf.int = TRUE) # Save key results
table2 <- table2[2,] # Extract only healthcare disruption variable
table2$model <- "All ACS - unadj" # Note model name

model_any_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_any + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_a_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model b: Any ambulatory care sensitive acute conditions
model_any_b_u <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_b_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_any + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_b_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model c: Any ambulatory care sensitive chronic conditions
model_any_c_u <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_c_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_any + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_c_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model d: Any ambulatory care sensitive conditions
model_any_d_u <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_d_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_any + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_d_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_any_e_u <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_e_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_any + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_e_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

# Model f: Emergency Urgent Sensitive conditions
model_any_f_u <- svyglm(total_admissions ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_any_f_u, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

model_any_f_a <- svyglm(total_admissions ~ imp_disruption_any + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_any_f_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table2 <- rbind(table2, hold) # Join to main table

write.csv(table2, "../../outputs/regression_model1.csv") # Save
rm(table2) # Tidy
# rm(model_any_a_a, model_any_a_u, model_any_b_a, model_any_b_u, model_any_c_a, model_any_c_u, model_any_d_a, model_any_d_u, model_any_e_a, model_any_e_u, model_any_f_a, model_any_f_u)

# Part 2: Type of disruption #

# Model a: Any ambulatory care sensitive conditions
model_type_a_u <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
table3 <- tidy(model_type_a_u, conf.int = TRUE) # Save key results
table3 <- table3[2:4,] # Extract only healthcare disruption variable
table3$model <- "All ACS - unadj" # Note model name

model_type_a_a <- svyglm(`Ambulatory Care Sensitive All` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_a_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "All ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model b: Acute ambulatory care sensitive acute conditions
model_type_b_u <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_b_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_b_a <- svyglm(`Ambulatory Care Sensitive Acute` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_b_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Acute ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model c: Chronic ambulatory care sensitive chronic conditions
model_type_c_u <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_c_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_c_a <- svyglm(`Ambulatory Care Sensitive Chronic` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_c_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Chronic ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model d: Vaccinhe preventable ambulatory care sensitive conditions
model_type_d_u <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_d_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_d_a <- svyglm(`Ambulatory Care Sensitive Vaccine-preventable` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_d_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Vaccine preventable ACS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model e: Emergency Urgent Sensitive conditions
model_type_e_u <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_e_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_e_a <- svyglm(`Emergency Urgent Care Sensitive ` ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_e_a, conf.int = TRUE) # Save key results
# If unhappy, then can do manually with confint(model_type_e_a, parm = "disruption_appointments")
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "EUCS - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

# Model f: Any hospital admission
model_type_f_u <- svyglm(total_admissions ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model_type_f_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - unadj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

model_type_f_a <- svyglm(total_admissions ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model_type_f_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Any adm - adj" # Note model name
table3 <- rbind(table3, hold) # Join to main table

write.csv(table3, file = "S:/LLC_0009/outputs/regression_model2.csv") # Save


### 5. Sensitivity analyses ###

# Part 1: Only events after last survey date
source("S:/LLC_0009/syntax/R/sensitivity_analysis.R") # Run script for re-running logistic regressions
source("S:/LLC_0009/syntax/R/sensitivity_survival.R") # Run script for survival analyses

## Part 2: Stratify by health status
# source("S:/LLC_0009/syntax/R/stratify_analysis.R") # Run script

# Part 3: Falsification tests

# Model a: Any disruption
model8a_u <- svyglm(vaccinated ~ imp_disruption_any, data = all_data, family = "binomial", design = wgt) # Unadjusted
table6 <- tidy(model8a_u, conf.int = TRUE) # Save key results
table6 <- table6[2,] # Extract only healthcare disruption variable
table6$model <- "Any - unadj" # Note model name

model8a_a <- svyglm(vaccinated ~ imp_disruption_any + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model8a_a, conf.int = TRUE) # Save key results
hold <- hold[2,] # Extract only healthcare disruption variable
hold$model <- "Any - adj" # Note model name
table6 <- rbind(table6, hold) # Join to main table

# Model b: Disruption by type
model8b_u <- svyglm(vaccinated ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures, data = all_data, family = "binomial", design = wgt) # Unadjusted
hold <- tidy(model8b_u, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - unadj" # Note model name
table6 <- rbind(table6, hold) # Join to main table

model8b_a <- svyglm(vaccinated ~ imp_disruption_appointments + imp_disruption_medications + imp_disruption_procedures + factor(imp_sex) + imp_age + factor(imp_not_white) + factor(imp_own_home) + factor(imp_general_health) + factor(imp_imd2019_income_q5) + factor(cohort), data = all_data, family = "binomial", design = wgt) # Adjusted
hold <- tidy(model8b_a, conf.int = TRUE) # Save key results
hold <- hold[2:4,] # Extract only healthcare disruption variable
hold$model <- "Type - adj" # Note model name
table6 <- rbind(table6, hold) # Join to main table

write.csv(table6, "../../outputs/falsification_test.csv") # Save
gc()

