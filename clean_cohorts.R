#################################
### Clean all cohort datasets ###
#################################

### Set up R environment ###

# Libraries
library(data.table)

# Source required files and functions
source("r_label_helper.R") # Will tell R which project space to connect to and identify what data is available within the LLC TRE

expss_disable_value_labels_support() # Disable labels for values in data (can be enabled later but this will mess with recoding)
rm(allviews) # List of all data tables (not really needed - viewnames is more help as lists all objects can load)


### Cohorts/Surveys ###

## GENSCOT ##

# Wave 1 #

# Load in data
data_source <- "genscot" # Cohort/survey
table <- "covidlife1_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
genscot <- genscot_covidlife1_v0001_20211101[,  c("LLC_0009_stud_id", "gender", "ethnic_origin", "general_health", "employmentstatus_now", "employmentstatus_before", "enddate", "had_covid", "managingmed_covid", "qualification", "country", "accommodation_status")] 
rm(genscot_covidlife1_v0001_20211101)


# Wave 2 #

# Load in data
data_source <- "genscot" # Cohort/survey
table <- "covidlife2_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
genscot_covidlife2_v0001_20211101 <- genscot_covidlife2_v0001_20211101[,  c("LLC_0009_stud_id", "s2_had_covid", "s2_employstat_before", "s2_employstat_now", "enddate", "s2_postponed_treat", "s2_postponed_type_1", "s2_postponed_type_2", "s2_postponed_type_3", "s2_postponed_type_4", "s2_postponed_type_5", "s2_postponed_type_6", "s2_postponed_type_7", "s2_postponed_type_8", "s2_postponed_type_9", "s2_postponed_type_10")]

# Join onto main data
genscot <- merge(genscot, genscot_covidlife2_v0001_20211101, by = "LLC_0009_stud_id", all.x = TRUE)
rm(genscot_covidlife2_v0001_20211101)

# Wave 3 #

# Has not healthcare disruption data so ignore

# # Load in data
# data_source <- "genscot" # Cohort/survey
# table <- "covidlife3_v0001_20211101" # Specific wave/dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy


# Demographic data #

# Load in data
data_source <- "genscot" # Cohort/survey
table <- "demographics_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset
genscot_demographics_v0001_20211101 <- genscot_demographics_v0001_20211101[,  c("LLC_0009_stud_id", "mob", "yob", "sex")]

# Join onto main data
genscot <- merge(genscot, genscot_demographics_v0001_20211101, by = "LLC_0009_stud_id", all.x = TRUE)
rm(genscot_demographics_v0001_20211101)


# Tidy waves for analysis-ready format #

# Covariates

# Sex 
genscot$sex[genscot$sex == "F"] <- "female" # Rename values
genscot$sex[genscot$sex == "M"] <- "male"

# Age
genscot$age <- 2020 - genscot$yob # Need to tidy to account for month and age at survey date

# Ethnicity
# Note the following commands are helpful in disabling labels - expss_disable_value_labels_support() and expss_enable_value_labels_support()
genscot$ethnicity <- NA # Create blank variable
genscot$ethnicity[genscot$ethnic_origin >= 1 & genscot$ethnic_origin <= 8] <- "White" # White or White British
genscot$ethnicity[genscot$ethnic_origin >= 9 & genscot$ethnic_origin <= 13] <- "Asian" # Asian or Asian British
genscot$ethnicity[genscot$ethnic_origin >= 14 & genscot$ethnic_origin <= 16] <- "Black" # Black or Black British
genscot$ethnicity[genscot$ethnic_origin >= 18 & genscot$ethnic_origin <= 21] <- "Mixed" # Mixed
genscot$ethnicity[genscot$ethnic_origin == 17 & genscot$ethnic_origin == 22] <- "Other" # Other

# Region
genscot$region <- NA # Create blank variable
genscot$region[genscot$country == 1] <- "Scotland" # We use Scotland as own region and leave England blank (as we split it up)
genscot$region[genscot$country == 4] <- "N Ireland"

# Country
genscot$country[genscot$country == 5] <- "Other" # Recode values to text
genscot$country[genscot$country == 4] <- "N Ireland"
genscot$country[genscot$country == 2] <- "England"
genscot$country[genscot$country == 1] <- "Scotland"

# Tenure
genscot$tenure <- NA# Create blank variable then recode below
genscot$tenure[genscot$accommodation_status == 1] <- "Own outright"
genscot$tenure[genscot$accommodation_status == 2 | genscot$accommodation_status == 5] <- "Mortgage"
genscot$tenure[genscot$accommodation_status == 4] <- "Rent - private"
genscot$tenure[genscot$accommodation_status == 3] <- "Rent - social"
genscot$tenure[genscot$accommodation_status == 7 | genscot$accommodation_status == 6] <- "Other"

# Education
genscot$education <- NA # Create blank variable then recode below
genscot$education[genscot$qualification == 1] <- "No qualifications"
genscot$education[genscot$qualification == 3 | genscot$qualification == 4 | genscot$qualification == 5] <- "Secondary" # GCSE or equivalent
genscot$education[genscot$qualification == 6] <- "Tertiary" # A-level or equivalent
genscot$education[genscot$qualification == 7 | genscot$qualification == 9 | genscot$qualification == 10] <- "Degree or above"
genscot$education[genscot$qualification == 2 | genscot$qualification == 8] <- "Other"

# # Employment status (pre-pandemic)
# genscot$employment_status_before <- NA # Create blank variable then recode below
# genscot$employment_status_before[genscot$employmentstatus_before == 3 | genscot$employmentstatus_before == 4] <- "Employed"
# genscot$employment_status_before[genscot$employmentstatus_before == 1 | genscot$employmentstatus_before == 2] <- "Self-employed"
# genscot$employment_status_before[genscot$employmentstatus_before == 9] <- "Retired"
# genscot$employment_status_before[genscot$employmentstatus_before == 10] <- "Student"
# genscot$employment_status_before[genscot$employmentstatus_before == 6 | genscot$employmentstatus_before == 7 | genscot$employmentstatus_before == 8 | genscot$employmentstatus_before == 11 | genscot$employmentstatus_before == 12] <- "Unemployed/Economically Inactive"
# genscot$employment_status_before[genscot$employmentstatus_before == 13] <- "Other"

# General health
genscot$general_health[genscot$general_health == 1 | genscot$general_health == 2 | genscot$general_health == 3] <- "Good" # Excellent to good health
genscot$general_health[genscot$general_health == 4 | genscot$general_health == 5] <- "Fair/Poor" # Fair or poor health

# Had COVID-19
genscot$covid <- NA # Create blank variable
genscot$covid[genscot$had_covid == 0 | genscot$s2_had_covid == 0] <- "No"
genscot$covid[genscot$had_covid >= 1 | genscot$s2_had_covid >= 1] <- "Yes" # Either tested positive or thought had COVID-19 in either wave

# Exposures
genscot$disruption_appointments <- NA # Blank variable to represent if had any disruption to appointments (e.g., GP appointments) - binary (yes = 1, 0 = no)
genscot$disruption_appointments[genscot$s2_postponed_treat <= 1] <- 0 # No disruption for this type (give everyone with complete data a 0, as below variables only have 1s if present - no 0s if not present)
genscot$disruption_appointments[genscot$s2_postponed_type_1 == 1 | genscot$s2_postponed_type_2 == 1 | genscot$s2_postponed_type_3 == 1 | genscot$s2_postponed_type_4 == 1 | genscot$s2_postponed_type_8 == 1 | genscot$s2_postponed_type_9 == 1] <- 1 # 1 = GP, 2 = hospital, 3 = routine clinic, 4 = dental, 8 = cancer test, 9 = cancer screening

genscot$disruption_medications <- NA # Blank variable to represent if had any disruption in receiving their medications
genscot$disruption_medications[genscot$managingmed_covid < 6 | genscot$managingmed_covid == 97] <- 0 # Follow previous NCS paper which used these cut-offs
genscot$disruption_medications[genscot$managingmed_covid >= 6 & genscot$managingmed_covid <= 10] <- 1

genscot$disruption_procedures <- NA # Blank variable to represent if had any disruption to procedures or surgery
genscot$disruption_procedures[genscot$s2_postponed_treat <= 1] <- 0 # No disruption for this type
genscot$disruption_procedures[genscot$s2_postponed_type_5 == 1 | genscot$s2_postponed_type_6 == 1 | genscot$s2_postponed_type_6 == 1] <- 1 # 5 = surgery, 6 = dialysis, 7 = cancer treatment

genscot$disruption_any <- NA # If had any of the above experiences (overall variable)
genscot$disruption_any[genscot$disruption_appointments == 0 | genscot$disruption_medications == 0 | genscot$disruption_procedures == 0] <- 0 # Set as OR in case have missing data, since next line will over-write the 1s anyway
genscot$disruption_any[genscot$disruption_appointments == 1 | genscot$disruption_medications == 1 | genscot$disruption_procedures == 1] <- 1

# Generate sample weights
# Sample weights are missing so replace with the following
genscot$weight <- 1 # Set to one
genscot$psu <- genscot$LLC_0009_stud_id # Unique to each individual
genscot$strata <- 1 # Constant across cohort
genscot$fpc <- 0 # Set as 0

# Subset variables required
genscot$date <- genscot$enddate.y #  Save date as latest wave
genscot <- genscot[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables


# Hospitalisations data #

# Load in data
data_source <- "genscot" # Cohort/survey
table <- "smr01_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Need to subset dates to match HES
genscot_smr01_v0001_20211101 <- genscot_smr01_v0001_20211101[genscot_smr01_v0001_20211101$dt1 >= 202003 & genscot_smr01_v0001_20211101$dt1 < 202201,] # Subset dates and take from March 2020 to end of 2021
genscot_smr01_v0001_20211101 <- as.data.table(genscot_smr01_v0001_20211101) # Convert to data.table format for next step

# Load in codelists
codelist <- read.csv("S:/LLC_0009/data/codelist.csv")

# Classify primary diagnosis position if matches codelist
genscot_smr01_v0001_20211101$diag_3_01 <- substr(genscot_smr01_v0001_20211101$c1, 1, 3) # Create three unit version of ICD10 code
genscot_smr01_v0001_20211101 <- merge(genscot_smr01_v0001_20211101, codelist, by.x = "diag_3_01", by.y = "code", all.x = TRUE) # Join codelist on

# Count total admissions
genscot_hes <- genscot_smr01_v0001_20211101[, list(total_admissions = .N), by = "LLC_0009_stud_id"] # Aggreagte by ID number of admissions
genscot <- merge(genscot, genscot_hes, by = "LLC_0009_stud_id", all.x = T) # Join
genscot$total_admissions[is.na(genscot$total_admissions)] <- 0 # Fill in missing data (should be 0 as no records)

# Reshape data to wide format
genscot_hes <- genscot_smr01_v0001_20211101[, c("LLC_0009_stud_id", "measure")] # Drop unncessary data
genscot_hes$count <- 1 # Create variable to aggregate
genscot_hes <- dcast(data = genscot_hes, LLC_0009_stud_id ~ measure, fun = sum, value.var = "count", fill = 0) # reshape 
genscot_hes$'NA' <- NULL # Drop variable

# Join
genscot <- merge(genscot, genscot_hes, by = "LLC_0009_stud_id", all.x = T) # Join
genscot$`Ambulatory Care Sensitive Acute`[is.na(genscot$`Ambulatory Care Sensitive Acute`)] <- 0 # Fill in missing data (should be 0 as no records)
genscot$`Ambulatory Care Sensitive Chronic`[is.na(genscot$`Ambulatory Care Sensitive Chronic`)] <- 0
genscot$`Ambulatory Care Sensitive Vaccine-preventable`[is.na(genscot$`Ambulatory Care Sensitive Vaccine-preventable`)] <- 0
genscot$`Emergency Urgent Care Sensitive `[is.na(genscot$`Emergency Urgent Care Sensitive `)] <- 0

# Repeat but take only admissions after survey date
dates <- genscot[, c("LLC_0009_stud_id", "date")] # Subset dates to join on below
dates$survey_date <- as.POSIXct(dates$date, format = "%d/%m/%Y %H:%M")  # Convert to date-time
genscot_smr01_v0001_20211101 <- merge(genscot_smr01_v0001_20211101, dates, by = "LLC_0009_stud_id", all.x = TRUE) # Join together
genscot_smr01_v0001_20211101$dt1 <- lubridate::ym(genscot_smr01_v0001_20211101$dt1) # Convert to date
genscot_smr01_v0001_20211101 <- genscot_smr01_v0001_20211101[genscot_smr01_v0001_20211101$survey_date < genscot_smr01_v0001_20211101$dt1] # Drop all records happening before cohort wave
genscot_smr01_v0001_20211101$time_dif <- lubridate::interval(genscot_smr01_v0001_20211101$survey_date, genscot_smr01_v0001_20211101$dt1) # Calculate time interval between end of survey and admission
genscot_smr01_v0001_20211101$months <- as.numeric(lubridate::as.period(genscot_smr01_v0001_20211101$time_dif, unit = "days"), "months") # Calculate months to admission
genscot_tot <- genscot_smr01_v0001_20211101[, list(total_admissions = .N, time_to_total_admissions = min(months)), by = "LLC_0009_stud_id"] # Aggreagte by ID number of admissions
genscot_hes <- genscot_smr01_v0001_20211101[, c("LLC_0009_stud_id", "measure", "months")] # Drop unncessary data
genscot_hes$count <- 1 # Create variable to aggregate
genscot_hes <- dcast(data = genscot_hes, LLC_0009_stud_id ~ measure, fun = list(sum, min), value.var = c("count", "months"), fill = 0) # reshape 

names(genscot_hes)[names(genscot_hes) == "count_sum_Ambulatory Care Sensitive Acute"] <- "Ambulatory Care Sensitive Acute" # Rename variables - counts
names(genscot_hes)[names(genscot_hes) == "count_sum_Ambulatory Care Sensitive Chronic"] <- "Ambulatory Care Sensitive Chronic" 
names(genscot_hes)[names(genscot_hes) == "count_sum_Ambulatory Care Sensitive Vaccine-preventable"] <- "Ambulatory Care Sensitive Vaccine-preventable" 
names(genscot_hes)[names(genscot_hes) == "count_sum_Emergency Urgent Care Sensitive "] <- "Emergency Urgent Care Sensitive " 

names(genscot_hes)[names(genscot_hes) == "months_min_Ambulatory Care Sensitive Acute"] <- "Time to Ambulatory Care Sensitive Acute"# Rename variables - time to event
names(genscot_hes)[names(genscot_hes) == "months_min_Ambulatory Care Sensitive Chronic"] <- "Time to Ambulatory Care Sensitive Chronic" 
names(genscot_hes)[names(genscot_hes) == "months_min_Ambulatory Care Sensitive Vaccine-preventable"] <- "Time to Ambulatory Care Sensitive Vaccine-preventable" 
names(genscot_hes)[names(genscot_hes) == "months_min_Emergency Urgent Care Sensitive "] <- "Time to Emergency Urgent Care Sensitive " 

genscot_hes$`Time to Ambulatory Care Sensitive Acute`[genscot_hes$`Time to Ambulatory Care Sensitive Acute` == 0] <- NA # 0s here really are missing so we need to tidy them up
genscot_hes$`Time to Ambulatory Care Sensitive Chronic`[genscot_hes$`Time to Ambulatory Care Sensitive Chronic` == 0] <- NA
genscot_hes$`Time to Ambulatory Care Sensitive Vaccine-preventable`[genscot_hes$`Time to Ambulatory Care Sensitive Vaccine-preventable` == 0] <- NA
genscot_hes$`Time to Emergency Urgent Care Sensitive `[genscot_hes$`Time to Emergency Urgent Care Sensitive ` == 0] <- NA

genscot_hes <- genscot_hes[, c(1, 3:6, 18:21)] # Subset required vars

genscot_tot <- merge(genscot_tot, genscot_hes, by = "LLC_0009_stud_id", all.x = T) # Join together
save(genscot_tot, file = "S:/LLC_0009/data/Cleaned Cohorts/genscot_admissions_post_wave.RData") # Save
rm(genscot_smr01_v0001_20211101, genscot_hes, genscot_tot) # Tidy


# Vaccines data #

# Load in data
data_source <- "genscot" # Cohort/survey
table <- "vaccine_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset
genscot_vaccine_v0001_20211101 <- genscot_vaccine_v0001_20211101[,  c("LLC_0009_stud_id", "dose_number")]
genscot_vaccine_v0001_20211101 <- as.data.table(genscot_vaccine_v0001_20211101) # Convert to data.table format for next step
genscot_vaccine_v0001_20211101 <- genscot_vaccine_v0001_20211101[, .SD[which.max(dose_number)], by = c("LLC_0009_stud_id")] # Select largest number of vaccines (i.e., if have had 2 doses take that as has both dates for vax 1 and 2, else take 1)
names(genscot_vaccine_v0001_20211101)[names(genscot_vaccine_v0001_20211101) == "dose_number"] <- "vax_doses" # Rename variable

# Join
genscot <- merge(genscot, genscot_vaccine_v0001_20211101, by = "LLC_0009_stud_id", all.x = T)
rm(genscot_vaccine_v0001_20211101)


# Save #
genscot$cohort <- "genscot"
save(genscot, file = "S:/LLC_0009/data/Cleaned Cohorts/genscot.RData")
rm(genscot) # Tidy


# Deaths (for censoring) #

# Load in data
data_source <- "genscot" # Cohort/survey
table <- "death_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset
genscot_death_v0001_20211101 <- genscot_death_v0001_20211101[,  c("LLC_0009_stud_id", "covid", "death28", "cause")]

# Save
save(genscot_death_v0001_20211101, file = "S:/LLC_0009/data/Cleaned Cohorts/genscot_deaths.RData")
rm(genscot_death_v0001_20211101) # Tidy


# ## Twins ##
# 
# # Note: Does have one measure of healthcare disruption, but generic variable so leave out if plan to have stratified by disruption type?
#
# # Load in data
# data_source <- "twinsuk" # Cohort/survey
# table <- "cope1_v0001_20211101" # Specific wave/dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

## UKHLS ##

# Note: there are 8 waves (ca-ch) - the last one goes upto March 2021 so this roughly matches the other cohorts so we will use all of them

# Wave 1 #

# Load in data
data_source <- "ukhls" # Cohort/survey
table <- "ca_indresp_w_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy
 
# Subset required variables
ukhls <- ukhls_ca_indresp_w_v0001_20211101[, c("LLC_0009_stud_id", "ca_betaindin_xw", "ca_betaindin_xw_t", "psu", "strata", "ca_canceltreat", "ca_nhsnowgp", "ca_nhsnowpm", "ca_nhsnowop", "ca_nhsnowip", "ca_nhsnow111", "ca_chscnowpharm", "ca_chscnowotcm", "ca_chscnowcarer", "ca_chscnowpsy", "ca_sex", "ca_age", "racel_dv", "ca_gor_dv", "ca_sempderived", "ca_surveystart", "ca_hadsymp", "ca_testresult", "ca_hcond_cv1", "ca_hcond_cv13", "ca_hcond_cv14", "ca_hcond_cv16")] # These are in order - id, sample weights (4 vars - first two weights depend on survey response method so online v telephone), disruption experienced (10 vars - treatment was cancelled, could access a GP, medications, outpatients, inpatients, NHS 111, pharmacy, otc-medications, carer, counselling), sex, age, race, region, employed, survey date, had covid symptoms, tested for covid, has health conditions (asthma, cancer, diabetes, hypertension)
rm(ukhls_ca_indresp_w_v0001_20211101)

# Rename variables
names(ukhls)[names(ukhls) == "psu"] <- "ca_psu"
names(ukhls)[names(ukhls) == "strata"] <- "ca_strata"
names(ukhls)[names(ukhls) == "racel_dv"] <- "ca_racel_dv"

# Wave 2 #

# Load in data
data_source <- "ukhls" # Cohort/survey
table <- "cb_indresp_w_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ukhls_cb_indresp_w_v0001_20211101 <- ukhls_cb_indresp_w_v0001_20211101[, c("LLC_0009_stud_id", "cb_betaindin_lw", "cb_betaindin_xw", "cb_betaindin_xw_t", "psu", "strata", "cb_canceltreat", "cb_nhsnowgp", "cb_nhsnowpm", "cb_nhsnowop", "cb_nhsnowip", "cb_nhsnow111", "cb_chscnowpharm", "cb_chscnowotcm", "cb_chscnowcarer", "cb_chscnowpsy", "cb_sex", "cb_age", "racel_dv", "cb_gor_dv", "cb_sempderived", "cb_surveystart", "cb_hadsymp", "cb_hadcovid", "cb_testresult", "cb_hcond_cv1", "cb_hcond_cv13", "cb_hcond_cv14", "cb_hcond_cv16", "cb_hsownd_cv")] # These are in order - id, sample weights (5 vars - two weights depend on survey response method so online v telephone), disruption experienced (10 vars - treatment was cancelled, could access a GP, medications, outpatients, inpatients, NHS 111, pharmacy, otc-medications, carer, counselling), sex, age, race, region, employed, survey date, had covid symptoms/covid itself, tested for covid, has health conditions (asthma, cancer, diabetes, hypertension), owns house

# Rename variables
names(ukhls_cb_indresp_w_v0001_20211101)[names(ukhls_cb_indresp_w_v0001_20211101) == "psu"] <- "cb_psu"
names(ukhls_cb_indresp_w_v0001_20211101)[names(ukhls_cb_indresp_w_v0001_20211101) == "strata"] <- "cb_strata"
names(ukhls_cb_indresp_w_v0001_20211101)[names(ukhls_cb_indresp_w_v0001_20211101) == "racel_dv"] <- "cb_racel_dv"

# Join together (note not same people are in each wave)
ukhls <- merge(ukhls, ukhls_cb_indresp_w_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(ukhls_cb_indresp_w_v0001_20211101) # Drop to save space

# Wave 3 #

# Load in data
data_source <- "ukhls" # Cohort/survey
table <- "cc_indresp_w_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ukhls_cc_indresp_w_v0001_20211101 <- ukhls_cc_indresp_w_v0001_20211101[, c("LLC_0009_stud_id", "cc_betaindin_lw", "cc_betaindin_xw", "psu", "strata", "cc_canceltreat", "cc_nhsnowgp", "cc_nhsnowpm", "cc_nhsnowop", "cc_nhsnowip", "cc_nhsnow111", "cc_chscnowpharm", "cc_chscnowotcm", "cc_chscnowcarer", "cc_chscnowpsy", "cc_sex_cv", "cc_age", "racel_dv", "cc_gor_dv", "cc_sempderived", "cc_surveystart", "cc_hadsymp", "cc_hadcovid", "cc_testresult", "cc_hcond_cv1", "cc_hcond_cv13", "cc_hcond_cv14", "cc_hcond_cv16")] # These are in order - id, sample weights (5 vars), disruption experienced (10 vars - treatment was cancelled, could access a GP, medications, outpatients, inpatients, NHS 111, pharmacy, otc-medications, carer, counselling), sex, age, race, region, employed, survey date, had covid symptoms/covid itself, tested for covid, has health conditions (asthma, cancer, diabetes, hypertension)

# Rename variables
names(ukhls_cc_indresp_w_v0001_20211101)[names(ukhls_cc_indresp_w_v0001_20211101) == "psu"] <- "cc_psu"
names(ukhls_cc_indresp_w_v0001_20211101)[names(ukhls_cc_indresp_w_v0001_20211101) == "strata"] <- "cc_strata"
names(ukhls_cc_indresp_w_v0001_20211101)[names(ukhls_cc_indresp_w_v0001_20211101) == "racel_dv"] <- "cc_racel_dv"

# Join together (note not same people are in each wave)
ukhls <- merge(ukhls, ukhls_cc_indresp_w_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(ukhls_cc_indresp_w_v0001_20211101) # Drop to save space

# Wave 4 #

# Load in data
data_source <- "ukhls" # Cohort/survey
table <- "cd_indresp_w_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ukhls_cd_indresp_w_v0001_20211101 <- ukhls_cd_indresp_w_v0001_20211101[, c("LLC_0009_stud_id", "cd_betaindin_lw", "cd_betaindin_xw", "psu", "strata", "cd_canceltreat", "cd_nhsnowgp", "cd_nhsnowpm", "cd_nhsnowop", "cd_nhsnowip", "cd_nhsnow111", "cd_chscnowpharm", "cd_chscnowotcm", "cd_chscnowcarer", "cd_chscnowpsy", "cd_sex_cv", "cd_age", "racel_dv", "cd_gor_dv", "cd_sempderived", "cd_surveystart", "cd_hadsymp", "cd_hadcovid", "cd_testresult", "cd_hcond_cv1", "cd_hcond_cv13", "cd_hcond_cv14", "cd_hcond_cv16", "cd_hsownd_cv")] # These are in order - id, sample weights (5 vars), disruption experienced (10 vars - treatment was cancelled, could access a GP, medications, outpatients, inpatients, NHS 111, pharmacy, otc-medications, carer, counselling), sex, age, race, region, employed, survey date, had covid symptoms/covid itself, tested for covid, has health conditions (asthma, cancer, diabetes, hypertension), house owned

# Rename variables
names(ukhls_cd_indresp_w_v0001_20211101)[names(ukhls_cd_indresp_w_v0001_20211101) == "psu"] <- "cd_psu"
names(ukhls_cd_indresp_w_v0001_20211101)[names(ukhls_cd_indresp_w_v0001_20211101) == "strata"] <- "cd_strata"
names(ukhls_cd_indresp_w_v0001_20211101)[names(ukhls_cd_indresp_w_v0001_20211101) == "racel_dv"] <- "cd_racel_dv"

# Join together (note not same people are in each wave)
ukhls <- merge(ukhls, ukhls_cd_indresp_w_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(ukhls_cd_indresp_w_v0001_20211101) # Drop to save space

# Wave 5 #

# The wave comes in two parts so we need to join them together first

# Load in data (part 1)
data_source <- "ukhls" # Cohort/survey
table <- "ce_indresp_w_v0001_1_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Load in data (part 2)
data_source <- "ukhls" # Cohort/survey
table <- "ce_indresp_w_v0001_2_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Join together
ukhls_ce_indresp_w_v0001_1_20211101 <- merge(ukhls_ce_indresp_w_v0001_1_20211101, ukhls_ce_indresp_w_v0001_2_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(ukhls_ce_indresp_w_v0001_2_20211101) # Drop to save space

# Subset required variables
ukhls_ce_indresp_w_v0001_1_20211101 <- ukhls_ce_indresp_w_v0001_1_20211101[, c("LLC_0009_stud_id", "ce_betaindin_lw", "ce_betaindin_xw", "psu", "strata", "ce_canceltreat", "ce_nhsnowgp", "ce_nhsnowpm", "ce_nhsnowop", "ce_nhsnowip", "ce_nhsnow111", "ce_chscnowpharm", "ce_chscnowotcm", "ce_chscnowcarer", "ce_chscnowpsy", "ce_sex_cv", "ce_age", "racel_dv", "ce_gor_dv", "ce_sempderived", "ce_surveystart", "ce_hadsymp", "ce_hadcovid", "ce_testresult", "ce_hcond_cv1", "ce_hcond_cv13", "ce_hcond_cv14", "ce_hcond_cv16", "ce_hsownd_cv")] # These are in order - id, sample weights (5 vars), disruption experienced (10 vars - treatment was cancelled, could access a GP, medications, outpatients, inpatients, NHS 111, pharmacy, otc-medications, carer, counselling), sex, age, race, region, employed, survey date, had covid symptoms/covid itself, tested for covid, has health conditions (asthma, cancer, diabetes, hypertension), house owned

# Rename variables
names(ukhls_ce_indresp_w_v0001_1_20211101)[names(ukhls_ce_indresp_w_v0001_1_20211101) == "psu"] <- "ce_psu"
names(ukhls_ce_indresp_w_v0001_1_20211101)[names(ukhls_ce_indresp_w_v0001_1_20211101) == "strata"] <- "ce_strata"
names(ukhls_ce_indresp_w_v0001_1_20211101)[names(ukhls_ce_indresp_w_v0001_1_20211101) == "racel_dv"] <-  "ce_racel_dv"

# Join together (note not same people are in each wave)
ukhls <- merge(ukhls, ukhls_ce_indresp_w_v0001_1_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(ukhls_ce_indresp_w_v0001_1_20211101) # Drop to save space

# Wave 6 #

# Load in data
data_source <- "ukhls" # Cohort/survey
table <- "cf_indresp_w_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ukhls_cf_indresp_w_v0001_20211101 <- ukhls_cf_indresp_w_v0001_20211101[, c("LLC_0009_stud_id", "cf_betaindin_lw", "cf_betaindin_lw_t1", "cf_betaindin_lw_t2", "cf_betaindin_xw", "cf_betaindin_xw_t", "psu", "strata", "cf_canceltreat", "cf_nhsnowgp2", "cf_nhsnowpm2", "cf_nhsnowop2", "cf_nhsnowip2", "cf_nhsnow1112", "cf_chscnowpharm2", "cf_chscnowotcm2", "cf_chscnowcarer2", "cf_chscnowpsy2", "cf_sex_cv", "cf_age", "racel_dv", "cf_gor_dv", "cf_sempderived", "cf_surveystart", "cf_hadsymp", "cf_hadcovid", "cf_testresult", "cf_hsownd_cv", "cf_scsf1")] # These are in order - id, sample weights (5 vars), disruption experienced (10 vars - treatment was cancelled, could access a GP, medications, outpatients, inpatients, NHS 111, pharmacy, otc-medications, carer, counselling), sex, age, race, region, employed, survey date, had covid symptoms/covid itself, tested for covid, house owned, health status. Does not have health conditions within it.

# Rename variables
names(ukhls_cf_indresp_w_v0001_20211101)[names(ukhls_cf_indresp_w_v0001_20211101) == "psu"] <- "cf_psu"
names(ukhls_cf_indresp_w_v0001_20211101)[names(ukhls_cf_indresp_w_v0001_20211101) == "strata"] <- "cf_strata"
names(ukhls_cf_indresp_w_v0001_20211101)[names(ukhls_cf_indresp_w_v0001_20211101) == "racel_dv"] <- "cf_racel_dv"

# Join together (note not same people are in each wave)
ukhls <- merge(ukhls, ukhls_cf_indresp_w_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(ukhls_cf_indresp_w_v0001_20211101) # Drop to save space

# Wave 7 #

# Load in data
data_source <- "ukhls" # Cohort/survey
table <- "cg_indresp_w_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ukhls_cg_indresp_w_v0001_20211101 <- ukhls_cg_indresp_w_v0001_20211101[, c("LLC_0009_stud_id", "cg_betaindin_lw", "cg_betaindin_xw", "psu", "strata", "cg_canceltreat", "cg_nhsnowgp2", "cg_nhsnowpm2", "cg_nhsnowop2", "cg_nhsnowip2", "cg_nhsnow1112", "cg_chscnowpharm2", "cg_chscnowotcm2", "cg_chscnowcarer2", "cg_chscnowpsy2", "cg_sex_cv", "cg_age", "racel_dv", "cg_gor_dv", "cg_sempderived", "cg_surveystart", "cg_hadsymp", "cg_hadcovid",  "cg_hsownd_cv", "cg_scsf1")] # These are in order - id, sample weights (5 vars), disruption experienced (10 vars - treatment was cancelled, could access a GP, medications, outpatients, inpatients, NHS 111, pharmacy, otc-medications, carer, counselling), sex, age, race, region, employed, survey date, had covid symptoms/covid itself, house owned, health status. No health conditions.

# Rename variables
names(ukhls_cg_indresp_w_v0001_20211101)[names(ukhls_cg_indresp_w_v0001_20211101) == "psu"] <- "cg_psu"
names(ukhls_cg_indresp_w_v0001_20211101)[names(ukhls_cg_indresp_w_v0001_20211101) == "strata"] <- "cg_strata"
names(ukhls_cg_indresp_w_v0001_20211101)[names(ukhls_cg_indresp_w_v0001_20211101) == "racel_dv"] <- "cg_racel_dv"

# Join together (note not same people are in each wave)
ukhls <- merge(ukhls, ukhls_cg_indresp_w_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(ukhls_cg_indresp_w_v0001_20211101) # Drop to save space

# Wave 8 #

# Load in data
data_source <- "ukhls" # Cohort/survey
table <- "ch_indresp_w_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ukhls_ch_indresp_w_v0001_20211101 <- ukhls_ch_indresp_w_v0001_20211101[, c("LLC_0009_stud_id", "ch_betaindin_lw", "ch_betaindin_xw", "psu", "strata", "ch_canceltreat", "ch_nhsnowgp2", "ch_nhsnowpm2", "ch_nhsnowop2", "ch_nhsnowip2", "ch_nhsnow1112", "ch_chscnowpharm2", "ch_chscnowotcm2", "ch_chscnowcarer2", "ch_chscnowpsy2", "ch_sex_cv", "ch_age", "racel_dv", "ch_gor_dv", "ch_sempderived", "ch_surveystart", "ch_hadsymp", "ch_hadcovid",  "ch_hsownd_cv", "ch_scsf1")] # These are in order - id, sample weights (5 vars), disruption experienced (10 vars - treatment was cancelled, could access a GP, medications, outpatients, inpatients, NHS 111, pharmacy, otc-medications, carer, counselling), sex, age, race, region, employed, survey date, had covid symptoms/covid itself, house owned, health status. No health conditions

# Rename variables
names(ukhls_ch_indresp_w_v0001_20211101)[names(ukhls_ch_indresp_w_v0001_20211101) == "psu"] <- "ch_psu"
names(ukhls_ch_indresp_w_v0001_20211101)[names(ukhls_ch_indresp_w_v0001_20211101) == "strata"] <- "ch_strata"
names(ukhls_ch_indresp_w_v0001_20211101)[names(ukhls_ch_indresp_w_v0001_20211101) == "racel_dv"] <- "ch_racel_dv"

# Join together (note not same people are in each wave)
ukhls <- merge(ukhls, ukhls_ch_indresp_w_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(ukhls_ch_indresp_w_v0001_20211101) # Drop to save space

# Save as takes ages to load each wave in
save(ukhls, file = "S:/LLC_0009/data/Cleaned Cohorts/ukhls_uncleaned.RData")
load("S:/LLC_0009/data/Cleaned Cohorts/ukhls_uncleaned.RData")

# Tidy variables #

# Sex
ukhls$sex <- NA # Create blank variable
ukhls$sex[ukhls$ch_sex_cv == 2 | (is.na(ukhls$ch_sex_cv) & ukhls$cg_sex_cv == 2) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & ukhls$cf_sex_cv == 2) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & ukhls$ce_sex_cv == 2) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & is.na(ukhls$ce_sex_cv) & ukhls$cd_sex_cv == 2) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & is.na(ukhls$ce_sex_cv) & is.na(ukhls$cd_sex_cv) & ukhls$cc_sex == 2) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & is.na(ukhls$ce_sex_cv) & is.na(ukhls$cd_sex_cv) & is.na(ukhls$cc_sex) & ukhls$cb_sex == 2) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & is.na(ukhls$ce_sex_cv) & is.na(ukhls$cd_sex_cv) & is.na(ukhls$cc_sex) & is.na(ukhls$cb_sex) & ukhls$ca_sex == 2)] <- "female" # Take latest unless missing
ukhls$sex[ukhls$ch_sex_cv == 1 | (is.na(ukhls$ch_sex_cv) & ukhls$cg_sex_cv == 1) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & ukhls$cf_sex_cv == 1) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & ukhls$ce_sex_cv == 1) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & is.na(ukhls$ce_sex_cv) & ukhls$cd_sex_cv == 1) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & is.na(ukhls$ce_sex_cv) & is.na(ukhls$cd_sex_cv) & ukhls$cc_sex == 1) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & is.na(ukhls$ce_sex_cv) & is.na(ukhls$cd_sex_cv) & is.na(ukhls$cc_sex) & ukhls$cb_sex == 1) | (is.na(ukhls$ch_sex_cv) & is.na(ukhls$cg_sex_cv) & is.na(ukhls$cf_sex_cv) & is.na(ukhls$ce_sex_cv) & is.na(ukhls$cd_sex_cv) & is.na(ukhls$cc_sex) & is.na(ukhls$cb_sex) & ukhls$ca_sex == 1)] <- "male" # Take latest unless missing
ukhls$sex[ukhls$sex == 3] <- NA # Not male/female (not enough cases to meet statistical disclosure so set as missing)

# Age
ukhls$age <- ukhls$ch_age # Take latest age
ukhls$age[is.na(ukhls$age)] <- ukhls$cg_age[is.na(ukhls$age)] # Update if missing each time
ukhls$age[is.na(ukhls$age)] <- ukhls$cf_age[is.na(ukhls$age)]
ukhls$age[is.na(ukhls$age)] <- ukhls$ce_age[is.na(ukhls$age)]
ukhls$age[is.na(ukhls$age)] <- ukhls$cd_age[is.na(ukhls$age)]
ukhls$age[is.na(ukhls$age)] <- ukhls$cc_age[is.na(ukhls$age)]
ukhls$age[is.na(ukhls$age)] <- ukhls$cb_age[is.na(ukhls$age)]
ukhls$age[is.na(ukhls$age)] <- ukhls$ca_age[is.na(ukhls$age)]

# Ethnicity
ukhls$ethnicity <- NA # Create variable
ukhls$ethnicity[(ukhls$ch_racel_dv >= 1 & ukhls$ch_racel_dv <= 3) | (is.na(ukhls$ch_racel_dv) & (ukhls$cg_racel_dv >= 1 & ukhls$cg_racel_dv <= 3)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & (ukhls$cf_racel_dv >= 1 & ukhls$cf_racel_dv <= 3)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & (ukhls$ce_racel_dv >= 1 & ukhls$ce_racel_dv <= 3)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & (ukhls$cd_racel_dv >= 1 & ukhls$cd_racel_dv <= 3)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & (ukhls$cc_racel_dv >= 1 & ukhls$cc_racel_dv <= 3)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & (ukhls$cb_racel_dv >= 1 & ukhls$cb_racel_dv <= 3)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & is.na(ukhls$cb_sex) & (ukhls$ca_racel_dv >= 1 & ukhls$ca_racel_dv <= 3))] <- "White" # Take latest unless missing
ukhls$ethnicity[(ukhls$ch_racel_dv >= 4 & ukhls$ch_racel_dv <= 8) | (is.na(ukhls$ch_racel_dv) & (ukhls$cg_racel_dv >= 4 & ukhls$cg_racel_dv <= 8)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & (ukhls$cf_racel_dv >= 4 & ukhls$cf_racel_dv <= 8)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & (ukhls$ce_racel_dv >= 4 & ukhls$ce_racel_dv <= 8)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & (ukhls$cd_racel_dv >= 4 & ukhls$cd_racel_dv <= 8)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & (ukhls$cc_racel_dv >= 4 & ukhls$cc_racel_dv <= 8)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & (ukhls$cb_racel_dv >= 4 & ukhls$cb_racel_dv <= 8)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & is.na(ukhls$cb_sex) & (ukhls$ca_racel_dv >= 4 & ukhls$ca_racel_dv <= 8))] <- "Mixed" # Take latest unless missing
ukhls$ethnicity[(ukhls$ch_racel_dv >= 9 & ukhls$ch_racel_dv <= 13) | (is.na(ukhls$ch_racel_dv) & (ukhls$cg_racel_dv >= 9 & ukhls$cg_racel_dv <= 13)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & (ukhls$cf_racel_dv >= 9 & ukhls$cf_racel_dv <= 13)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & (ukhls$ce_racel_dv >= 9 & ukhls$ce_racel_dv <= 13)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & (ukhls$cd_racel_dv >= 9 & ukhls$cd_racel_dv <= 13)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & (ukhls$cc_racel_dv >= 9 & ukhls$cc_racel_dv <= 13)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & (ukhls$cb_racel_dv >= 9 & ukhls$cb_racel_dv <= 13)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & is.na(ukhls$cb_sex) & (ukhls$ca_racel_dv >= 9 & ukhls$ca_racel_dv <= 13))] <- "Asian" # Take latest unless missing
ukhls$ethnicity[(ukhls$ch_racel_dv >= 14 & ukhls$ch_racel_dv <= 16) | (is.na(ukhls$ch_racel_dv) & (ukhls$cg_racel_dv >= 14 & ukhls$cg_racel_dv <= 16)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & (ukhls$cf_racel_dv >= 14 & ukhls$cf_racel_dv <= 16)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & (ukhls$ce_racel_dv >= 14 & ukhls$ce_racel_dv <= 16)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & (ukhls$cd_racel_dv >= 14 & ukhls$cd_racel_dv <= 16)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & (ukhls$cc_racel_dv >= 14 & ukhls$cc_racel_dv <= 16)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & (ukhls$cb_racel_dv >= 14 & ukhls$cb_racel_dv <= 16)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & is.na(ukhls$cb_sex) & (ukhls$ca_racel_dv >= 14 & ukhls$ca_racel_dv <= 16))] <- "Black" # Take latest unless missing
ukhls$ethnicity[(ukhls$ch_racel_dv >= 17 & ukhls$ch_racel_dv <= 97) | (is.na(ukhls$ch_racel_dv) & (ukhls$cg_racel_dv >= 17 & ukhls$cg_racel_dv <= 97)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & (ukhls$cf_racel_dv >= 17 & ukhls$cf_racel_dv <= 97)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & (ukhls$ce_racel_dv >= 17 & ukhls$ce_racel_dv <= 97)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & (ukhls$cd_racel_dv >= 17 & ukhls$cd_racel_dv <= 97)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & (ukhls$cc_racel_dv >= 17 & ukhls$cc_racel_dv <= 97)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & (ukhls$cb_racel_dv >= 17 & ukhls$cb_racel_dv <= 97)) | (is.na(ukhls$ch_racel_dv) & is.na(ukhls$cg_racel_dv) & is.na(ukhls$cf_racel_dv) & is.na(ukhls$ce_racel_dv) & is.na(ukhls$cd_racel_dv) & is.na(ukhls$cc_sex) & is.na(ukhls$cb_sex) & (ukhls$ca_racel_dv >= 17 & ukhls$ca_racel_dv <= 97))] <- "Other" # Take latest unless missing

# Region
ukhls$region <- NA
ukhls$region <- ukhls$ch_gor_dv # Take latest region
ukhls$region[is.na(ukhls$region)] <- ukhls$cg_gor_dv[is.na(ukhls$region)] # Update if missing each time
ukhls$region[is.na(ukhls$region)] <- ukhls$cg_gor_dv[is.na(ukhls$region)]
ukhls$region[is.na(ukhls$region)] <- ukhls$cg_gor_dv[is.na(ukhls$region)]
ukhls$region[is.na(ukhls$region)] <- ukhls$cg_gor_dv[is.na(ukhls$region)]
ukhls$region[is.na(ukhls$region)] <- ukhls$cg_gor_dv[is.na(ukhls$region)]
ukhls$region[is.na(ukhls$region)] <- ukhls$cg_gor_dv[is.na(ukhls$region)]
ukhls$region[is.na(ukhls$region)] <- ukhls$cg_gor_dv[is.na(ukhls$region)]

ukhls$region[ukhls$region == -9] <- NA # Recode values for region so consistent
ukhls$region[ukhls$region == 1] <- "North East"
ukhls$region[ukhls$region == 2] <- "North West"
ukhls$region[ukhls$region == 3] <- "Yorkshire"
ukhls$region[ukhls$region == 4] <- "East Midlands"
ukhls$region[ukhls$region == 5] <- "West Midlands"
ukhls$region[ukhls$region == 6] <- "East"
ukhls$region[ukhls$region == 7] <- "London"
ukhls$region[ukhls$region == 8] <- "South East"
ukhls$region[ukhls$region == 9] <- "South West"
ukhls$region[ukhls$region == 10] <- "Wales"
ukhls$region[ukhls$region == 11] <- "Scotland"
ukhls$region[ukhls$region == 12] <- "N Ireland"

# Country
ukhls$country <- NA
ukhls$country[ukhls$region != "Wales" & ukhls$region != "Scotland" & ukhls$region != "N Ireland"] <- "England"
ukhls$country[ukhls$region == "Wales"] <- "Wales"
ukhls$country[ukhls$region == "Scotland"] <- "Scotland"
ukhls$country[ukhls$region == "N Ireland"] <- "N Ireland"

# Tenure
ukhls$tenure <- NA # Create blank variable then populate with most recent record (not always present in all waves)
ukhls$tenure <- ukhls$ch_hsownd_cv
ukhls$tenure[is.na(ukhls$tenure)] <- ukhls$cg_hsownd_cv[is.na(ukhls$tenure)] # if missing then replace with next value (and then repeat)
ukhls$tenure[is.na(ukhls$tenure)] <- ukhls$cf_hsownd_cv[is.na(ukhls$tenure)]
ukhls$tenure[is.na(ukhls$tenure)] <- ukhls$ce_hsownd_cv[is.na(ukhls$tenure)]
ukhls$tenure[is.na(ukhls$tenure)] <- ukhls$cd_hsownd_cv[is.na(ukhls$tenure)]
ukhls$tenure[is.na(ukhls$tenure)] <- ukhls$cb_hsownd_cv[is.na(ukhls$tenure)]

ukhls$tenure[ukhls$tenure == 1] <- "Own outright" # Then recode values
ukhls$tenure[ukhls$tenure == 2 | ukhls$tenure == 3] <- "Mortgage"
ukhls$tenure[ukhls$tenure == 4] <- "Rent"
ukhls$tenure[ukhls$tenure == 5 | ukhls$tenure == 97] <- "Other"

# Education
ukhls$education <- NA # Create blank variable as not present in data provided

# General health 
ukhls$general_health <- NA # Create blank variable
ukhls$general_health[ukhls$ch_scsf1 < 4 | ukhls$cg_scsf1 < 4 | ukhls$cf_scsf1 < 4] <- "Good" # Reported their health as good (not always present in all waves)
ukhls$general_health[ukhls$ch_scsf1 >= 4 | ukhls$cg_scsf1 >= 4 | ukhls$cf_scsf1 >= 4] <- "Fair/Poor" # Reported their health as fair or poor at any point

# Had COVID-19
ukhls$covid <- NA # Create blank variable
ukhls$covid[ukhls$ca_hadsymp != -1 | ukhls$cb_hadsymp != -1 | ukhls$cc_hadsymp != -1 | ukhls$cd_hadsymp != -1 | ukhls$ce_hadsymp != -1 | ukhls$cf_hadsymp != -1 | ukhls$cg_hadsymp != -1 | ukhls$ch_hadsymp != -1] <- 0 # have data present for whether had COVID-19 symptoms (to act as a baseline to overwrite on the next step)
ukhls$covid[(ukhls$cb_hadcovid >= -1 & ukhls$cb_hadcovid <= 3) | (ukhls$cc_hadcovid >= -1 & ukhls$cc_hadcovid <= 3) | (ukhls$cd_hadcovid >= -1 & ukhls$cd_hadcovid <= 3) | (ukhls$ce_hadcovid >= -1 & ukhls$ce_hadcovid <= 3) | (ukhls$cf_hadcovid >= -1 & ukhls$cf_hadcovid <= 3) | (ukhls$cg_hadcovid >= -1 & ukhls$cg_hadcovid <= 3) | (ukhls$ch_hadcovid >= -1 & ukhls$ch_hadcovid <= 3)] <- 1 # If they report they likely had COVID-19, we treat this as a case (not present in wave a) - this measure is limited but we may not use it in the main analysis therefore

# Asthma
ukhls$asthma <- NA # Create blank variable
ukhls$asthma[ukhls$ca_hcond_cv1 == 0 | ukhls$cb_hcond_cv1 == 0 | ukhls$cc_hcond_cv1 == 0 | ukhls$cd_hcond_cv1 == 0 | ukhls$ce_hcond_cv1 == 0] <- 0 # Reported no at some point
ukhls$asthma[ukhls$ca_hcond_cv1 == 1 | ukhls$cb_hcond_cv1 == 1 | ukhls$cc_hcond_cv1 == 1 | ukhls$cd_hcond_cv1 == 1 | ukhls$ce_hcond_cv1 == 1] <- 1 # Has reported condition at some point (overwrites the nos)

# Cancer
ukhls$cancer <- NA # Create blank variable
ukhls$cancer[ukhls$ca_hcond_cv13 == 0 | ukhls$cb_hcond_cv13 == 0 | ukhls$cc_hcond_cv13 == 0 | ukhls$cd_hcond_cv13 == 0 | ukhls$ce_hcond_cv13 == 0] <- 0 # Reported no at some point
ukhls$cancer[ukhls$ca_hcond_cv13 == 1 | ukhls$cb_hcond_cv13 == 1 | ukhls$cc_hcond_cv13 == 1 | ukhls$cd_hcond_cv13 == 1 | ukhls$ce_hcond_cv13 == 1] <- 1 # Has reported condition at some point

# Diabetes
ukhls$diabetes <- NA # Create blank variable
ukhls$diabetes[ukhls$ca_hcond_cv14 == 0 | ukhls$cb_hcond_cv14 == 0 | ukhls$cc_hcond_cv14 == 0 | ukhls$cd_hcond_cv14 == 0 | ukhls$ce_hcond_cv14 == 0] <- 0 # Reported no at some point
ukhls$diabetes[ukhls$ca_hcond_cv14 == 1 | ukhls$cb_hcond_cv14 == 1 | ukhls$cc_hcond_cv14 == 1 | ukhls$cd_hcond_cv14 == 1 | ukhls$ce_hcond_cv14 == 1] <- 1 # Has reported condition at some point

# Hypertension
ukhls$hypertension <- NA # Create blank variable
ukhls$hypertension[ukhls$ca_hcond_cv16 == 0 | ukhls$cb_hcond_cv16 == 0 | ukhls$cc_hcond_cv16 == 0 | ukhls$cd_hcond_cv16 == 0 | ukhls$ce_hcond_cv16 == 0] <- 0 # Reported no at some point
ukhls$hypertension[ukhls$ca_hcond_cv16 == 1 | ukhls$cb_hcond_cv16 == 1 | ukhls$cc_hcond_cv16 == 1 | ukhls$cd_hcond_cv16 == 1 | ukhls$ce_hcond_cv16 == 1] <- 1 # Has reported condition at some point

# Healthcare disruption
# This is a bit complicated to say the least
# head(ukhls$ch_nhsnowgp2) # Helpful in looking up variable descriptions and value labels
ukhls$disruption_appointments <- NA # Blank variable to represent if had any disruption to appointments (e.g., GP appointments) - binary (yes = 1, 0 = no)
# We define this variable following the Maddock paper - so will do this one by one measure - first for no disruption, then for any disruption
ukhls$disruption_appointments[(ukhls$ca_nhsnowgp == 1 | ukhls$ca_nhsnowgp == 2 | ukhls$ca_nhsnowgp == 5 | ukhls$ca_nhsnowgp == -8) | (ukhls$cb_nhsnowgp == 1 | ukhls$cb_nhsnowgp == 2 | ukhls$cb_nhsnowgp == 5 | ukhls$cb_nhsnowgp == -8) | (ukhls$cc_nhsnowgp == 1 | ukhls$cc_nhsnowgp == 2 | ukhls$cc_nhsnowgp == 5 | ukhls$cc_nhsnowgp == -8) | (ukhls$cd_nhsnowgp == 1 | ukhls$cd_nhsnowgp == 2 | ukhls$cd_nhsnowgp == 5 | ukhls$cd_nhsnowgp == -8) | (ukhls$ce_nhsnowgp == 1 | ukhls$ce_nhsnowgp == 2 | ukhls$ce_nhsnowgp == 5 | ukhls$ce_nhsnowgp == -8) | (ukhls$cf_nhsnowgp2 == 1 | ukhls$cf_nhsnowgp2 == 2 | ukhls$cf_nhsnowgp2 == 5 | ukhls$cf_nhsnowgp2 == -8) | (ukhls$cg_nhsnowgp2 == 1 | ukhls$cg_nhsnowgp2 == 2 | ukhls$cg_nhsnowgp2 == 5 | ukhls$cg_nhsnowgp2 == -8) | (ukhls$ch_nhsnowgp2 == 1 | ukhls$ch_nhsnowgp2 == 2 | ukhls$ch_nhsnowgp2 == 5 | ukhls$ch_nhsnowgp2 == -8)] <- 0 # No disruption in accessing GP services or question inapplicable
ukhls$disruption_appointments[(ukhls$ca_nhsnowip == 1 | ukhls$ca_nhsnowip == 4 | ukhls$ca_nhsnowip == 5 | ukhls$ca_nhsnowip == -8) | (ukhls$cb_nhsnowip == 1 | ukhls$cb_nhsnowip == 4 | ukhls$cb_nhsnowip == 5 | ukhls$cb_nhsnowip == -8) | (ukhls$cc_nhsnowip == 1 | ukhls$cc_nhsnowip == 4 | ukhls$cc_nhsnowip == 5 | ukhls$cc_nhsnowip == -8) | (ukhls$cd_nhsnowip == 1 | ukhls$cd_nhsnowip == 4 | ukhls$cd_nhsnowip == 5 | ukhls$cd_nhsnowip == -8) | (ukhls$ce_nhsnowip == 1 | ukhls$ce_nhsnowip == 4 | ukhls$ce_nhsnowip == 5 | ukhls$ce_nhsnowip == -8) | (ukhls$cf_nhsnowip2 == 1 | ukhls$cf_nhsnowip2 == 4 | ukhls$cf_nhsnowip2 == 5 | ukhls$cf_nhsnowip2 == -8) | (ukhls$cg_nhsnowip2 == 1 | ukhls$cg_nhsnowip2 == 4 | ukhls$cg_nhsnowip2 == 5 | ukhls$cg_nhsnowip2 == -8) | (ukhls$ch_nhsnowip2 == 1 | ukhls$ch_nhsnowip2 == 4 | ukhls$ch_nhsnowip2 == 5 | ukhls$ch_nhsnowip2 == -8)] <- 0 # No disruption in accessing inpatients
ukhls$disruption_appointments[(ukhls$ca_nhsnowop == 1 | ukhls$ca_nhsnowop == 2 | ukhls$ca_nhsnowop == 5 | ukhls$ca_nhsnowop == 6 | ukhls$ca_nhsnowop == -8) | (ukhls$cb_nhsnowop == 1 | ukhls$cb_nhsnowop == 2 | ukhls$cb_nhsnowop == 5 | ukhls$cb_nhsnowop == 6 | ukhls$cb_nhsnowop == -8) | (ukhls$cc_nhsnowop == 1 | ukhls$cc_nhsnowop == 2 | ukhls$cc_nhsnowop == 5 | ukhls$cc_nhsnowop == 6 | ukhls$cc_nhsnowop == -8) | (ukhls$cd_nhsnowop == 1 | ukhls$cd_nhsnowop == 2 | ukhls$cd_nhsnowop == 5 | ukhls$cd_nhsnowop == 6 | ukhls$cd_nhsnowop == -8) | (ukhls$ce_nhsnowop == 1 | ukhls$ce_nhsnowop == 2 | ukhls$ce_nhsnowop == 5 | ukhls$ce_nhsnowop == 6 | ukhls$ce_nhsnowop == -8) | (ukhls$cf_nhsnowop2 == 1 | ukhls$cf_nhsnowop2 == 2 | ukhls$cf_nhsnowop2 == 5 | ukhls$cf_nhsnowop2 == 6 | ukhls$cf_nhsnowop2 == -8) | (ukhls$cg_nhsnowop2 == 1 | ukhls$cg_nhsnowop2 == 2 | ukhls$cg_nhsnowop2 == 5 | ukhls$cg_nhsnowop2 == 6 | ukhls$cg_nhsnowop2 == -8) | (ukhls$ch_nhsnowop2 == 1 | ukhls$ch_nhsnowop2 == 2 | ukhls$ch_nhsnowop2 == 5 | ukhls$ch_nhsnowop2 == 6 | ukhls$ch_nhsnowop2 == -8)] <- 0 # No disruption in accessing outpatients
ukhls$disruption_appointments[(ukhls$ca_nhsnow111 == 1 | ukhls$ca_nhsnow111 == 4 | ukhls$ca_nhsnow111 == -8) | (ukhls$cb_nhsnow111 == 1 | ukhls$cb_nhsnow111 == 4 |  ukhls$cb_nhsnow111 == -8) | (ukhls$cc_nhsnow111 == 1 | ukhls$cc_nhsnow111 == 4 |  ukhls$cc_nhsnow111 == -8) | (ukhls$cd_nhsnow111 == 1 | ukhls$cd_nhsnow111 == 4 |  ukhls$cd_nhsnow111 == -8) | (ukhls$ce_nhsnow111 == 1 | ukhls$ce_nhsnow111 == 4 |  ukhls$ce_nhsnow111 == -8) | (ukhls$cf_nhsnow1112 == 1 | ukhls$cf_nhsnow1112 == 4 |  ukhls$cf_nhsnow1112 == -8) | (ukhls$cg_nhsnow1112 == 1 | ukhls$cg_nhsnow1112 == 4 | ukhls$cg_nhsnow1112 == -8) | (ukhls$ch_nhsnow1112 == 1 | ukhls$ch_nhsnow1112 == 4 | ukhls$ch_nhsnow1112 == -8)] <- 0 # No disruption in accessing NHS 111

ukhls$disruption_appointments[(ukhls$ca_nhsnowgp == 3 | ukhls$ca_nhsnowgp == 4) | (ukhls$cb_nhsnowgp == 3 | ukhls$cb_nhsnowgp == 4) | (ukhls$cc_nhsnowgp == 3 | ukhls$cc_nhsnowgp == 4) | (ukhls$cd_nhsnowgp == 3 | ukhls$cd_nhsnowgp == 4) | (ukhls$ce_nhsnowgp == 3 | ukhls$ce_nhsnowgp == 4) | (ukhls$cf_nhsnowgp2 == 3 | ukhls$cf_nhsnowgp2 == 4) | (ukhls$cg_nhsnowgp2 == 3 | ukhls$cg_nhsnowgp2 == 4) | (ukhls$ch_nhsnowgp2 == 3 | ukhls$ch_nhsnowgp2 == 4)] <- 1 # Disruption in accessing GP services
ukhls$disruption_appointments[(ukhls$ca_nhsnowip == 3 | ukhls$ca_nhsnowip == 2) | (ukhls$cb_nhsnowip == 3 | ukhls$cb_nhsnowip == 2) | (ukhls$cc_nhsnowip == 3 | ukhls$cc_nhsnowip == 2) | (ukhls$cd_nhsnowip == 3 | ukhls$cd_nhsnowip == 2) | (ukhls$ce_nhsnowip == 3 | ukhls$ce_nhsnowip == 2) | (ukhls$cf_nhsnowip2 == 3 | ukhls$cf_nhsnowip2 == 2) | (ukhls$cg_nhsnowip2 == 3 | ukhls$cg_nhsnowip2 == 2) | (ukhls$ch_nhsnowip2 == 3 | ukhls$ch_nhsnowip2 == 2)] <- 1 # Disruption in accessing inpatients
ukhls$disruption_appointments[(ukhls$ca_nhsnowop == 3 | ukhls$ca_nhsnowop == 4) | (ukhls$cb_nhsnowop == 3 | ukhls$cb_nhsnowop == 4) | (ukhls$cc_nhsnowop == 3 | ukhls$cc_nhsnowop == 4) | (ukhls$cd_nhsnowop == 3 | ukhls$cd_nhsnowop == 4) | (ukhls$ce_nhsnowop == 3 | ukhls$ce_nhsnowop == 4) | (ukhls$cf_nhsnowop2 == 3 | ukhls$cf_nhsnowop2 == 4) | (ukhls$cg_nhsnowop2 == 3 | ukhls$cg_nhsnowop2 == 4) | (ukhls$ch_nhsnowop2 == 3 | ukhls$ch_nhsnowop2 == 4)] <- 1 # Disruption in accessing outpatients
ukhls$disruption_appointments[(ukhls$ca_nhsnow111 == 3 | ukhls$ca_nhsnow111 == 2) | (ukhls$cb_nhsnow111 == 3 | ukhls$cb_nhsnow111 == 2) | (ukhls$cc_nhsnow111 == 3 | ukhls$cc_nhsnow111 == 2) | (ukhls$cd_nhsnow111 == 3 | ukhls$cd_nhsnow111 == 2) | (ukhls$ce_nhsnow111 == 3 | ukhls$ce_nhsnow111 == 2) | (ukhls$cf_nhsnow1112 == 3 | ukhls$cf_nhsnow1112 == 2) | (ukhls$cg_nhsnow1112 == 3 | ukhls$cg_nhsnow1112 == 2) | (ukhls$ch_nhsnow1112 == 3 | ukhls$ch_nhsnow1112 == 2)] <- 1 # Disruption in accessing NHS 111

ukhls$disruption_medications <- NA # Blank variable to represent if had any disruption in receiving their medications
ukhls$disruption_medications[(ukhls$ca_nhsnowpm == 1 | ukhls$ca_nhsnowpm == 3 | ukhls$ca_nhsnowpm == -8) | (ukhls$cb_nhsnowpm == 1 | ukhls$cb_nhsnowpm == 3 | ukhls$cb_nhsnowpm == -8) | (ukhls$cc_nhsnowpm == 1 | ukhls$cc_nhsnowpm == 3 | ukhls$cc_nhsnowpm == -8) | (ukhls$cd_nhsnowpm == 1 | ukhls$cd_nhsnowpm == 3 | ukhls$cd_nhsnowpm == -8) | (ukhls$ce_nhsnowpm == 1 | ukhls$ce_nhsnowpm == 3 | ukhls$ce_nhsnowpm == -8) | (ukhls$cf_nhsnowpm2 == 1 | ukhls$cf_nhsnowpm2 == 3 | ukhls$cf_nhsnowpm2 == -8) | (ukhls$cg_nhsnowpm2 == 1 | ukhls$cg_nhsnowpm2 == 3 | ukhls$cg_nhsnowpm2 == -8) | (ukhls$ch_nhsnowpm2 == 1 | ukhls$ch_nhsnowpm2 == 3 | ukhls$ch_nhsnowpm2 == -8) ] <- 0 # No disruption  or inapplicable question
ukhls$disruption_medications[ukhls$ca_nhsnowpm == 2 |ukhls$cb_nhsnowpm == 2 | ukhls$cc_nhsnowpm == 2 | ukhls$cd_nhsnowpm == 2 | ukhls$ce_nhsnowpm == 2 | ukhls$cf_nhsnowpm2 == 2 | ukhls$cg_nhsnowpm2 == 2 | ukhls$ch_nhsnowpm2 == 2] <- 1 # Disruption at any point in all waves
# Need to add in cf_chscnowpharm2 and ch_chscnowotcm2

ukhls$disruption_procedures <- NA # Blank variable for surgery, cancer treatment
ukhls$disruption_procedures[(ukhls$ca_canceltreat == 2 | ukhls$ca_canceltreat == 4 | ukhls$ca_canceltreat == -8) | (ukhls$cb_canceltreat == 2 | ukhls$cb_canceltreat == 4 | ukhls$cb_canceltreat == -8) | (ukhls$cc_canceltreat == 2 | ukhls$cc_canceltreat == 4 | ukhls$cc_canceltreat == -8) | (ukhls$cd_canceltreat == 2 | ukhls$cd_canceltreat == 4 | ukhls$cd_canceltreat == -8) | (ukhls$ce_canceltreat == 2 | ukhls$ce_canceltreat == 4 | ukhls$ce_canceltreat == -8) | (ukhls$cf_canceltreat == 2 | ukhls$cf_canceltreat == 4 | ukhls$cf_canceltreat == -8) | (ukhls$ch_canceltreat == 2 | ukhls$ch_canceltreat == 4 | ukhls$ch_canceltreat == -8) | (ukhls$cg_canceltreat == 2 | ukhls$cg_canceltreat == 4 | ukhls$cg_canceltreat == -8)] <- 0 # No disruption or alternative treatment provided (or question is inapplicable as don't have health condition)
ukhls$disruption_procedures[(ukhls$ca_canceltreat == 1 | ukhls$ca_canceltreat == 3) | (ukhls$cb_canceltreat == 1 | ukhls$cb_canceltreat == 3) | (ukhls$cc_canceltreat == 1 | ukhls$cc_canceltreat == 3) | (ukhls$cd_canceltreat == 1 | ukhls$cd_canceltreat == 3) | (ukhls$ce_canceltreat == 1 | ukhls$ce_canceltreat == 3) | (ukhls$cf_canceltreat == 1 | ukhls$cf_canceltreat == 3) | (ukhls$ch_canceltreat == 1 | ukhls$ch_canceltreat == 3) | (ukhls$cg_canceltreat == 1 | ukhls$cg_canceltreat == 3)] <- 1 # Cancelled or postponed treatment either by NHS or self

ukhls$disruption_any <- NA # If had any of the above experiences (overall variable)
ukhls$disruption_any[ukhls$disruption_appointments == 0 | ukhls$disruption_medications == 0 | ukhls$disruption_procedures == 0] <- 0 # Set as OR in case have missing data, since next line will over-write the 1s anyway
ukhls$disruption_any[ukhls$disruption_appointments == 1 | ukhls$disruption_medications == 1 | ukhls$disruption_procedures == 1] <- 1

# Generate sample weights
# Take most recent weight
ukhls$weight <- ukhls$ch_betaindin_xw # Most recent (we will use the cross-sectional weight since the analysis is cross-sectional, but open to change)
ukhls$weight[is.na(ukhls$weight)] <- ukhls$cg_betaindin_xw[is.na(ukhls$weight)] # If missing, then take previous one
ukhls$weight[is.na(ukhls$weight)] <- ukhls$cf_betaindin_xw[is.na(ukhls$weight)] # repeat
ukhls$weight[is.na(ukhls$weight)] <- ukhls$cf_betaindin_xw_t[is.na(ukhls$weight)] # Two weights for this wave (by survey method)
ukhls$weight[is.na(ukhls$weight)] <- ukhls$ce_betaindin_xw[is.na(ukhls$weight)] # And so on...
ukhls$weight[is.na(ukhls$weight)] <- ukhls$cd_betaindin_xw[is.na(ukhls$weight)]
ukhls$weight[is.na(ukhls$weight)] <- ukhls$cc_betaindin_xw[is.na(ukhls$weight)]
ukhls$weight[is.na(ukhls$weight)] <- ukhls$cb_betaindin_xw[is.na(ukhls$weight)]
ukhls$weight[is.na(ukhls$weight)] <- ukhls$cb_betaindin_xw_t[is.na(ukhls$weight)]
ukhls$weight[is.na(ukhls$weight)] <- ukhls$ca_betaindin_xw[is.na(ukhls$weight)]
ukhls$weight[is.na(ukhls$weight)] <- ukhls$ca_betaindin_xw_t[is.na(ukhls$weight)]
ukhls$weight[is.na(ukhls$weight)] <- 1 # If missing still, follow rules 

ukhls$psu <- ukhls$ch_psu # Again, take the latest
ukhls$psu[is.na(ukhls$psu)] <- ukhls$cg_psu[is.na(ukhls$psu)] # Replace if missing (and so on)
ukhls$psu[is.na(ukhls$psu)] <- ukhls$cf_psu[is.na(ukhls$psu)] 
ukhls$psu[is.na(ukhls$psu)] <- ukhls$ce_psu[is.na(ukhls$psu)] 
ukhls$psu[is.na(ukhls$psu)] <- ukhls$cd_psu[is.na(ukhls$psu)] 
ukhls$psu[is.na(ukhls$psu)] <- ukhls$cc_psu[is.na(ukhls$psu)] 
ukhls$psu[is.na(ukhls$psu)] <- ukhls$cb_psu[is.na(ukhls$psu)] 
ukhls$psu[is.na(ukhls$psu)] <- ukhls$ca_psu[is.na(ukhls$psu)] 

ukhls$strata <- ukhls$ch_strata # Again, take the latest
ukhls$strata[is.na(ukhls$strata)] <- ukhls$cg_strata[is.na(ukhls$strata)] # Replace if missing (and so on)
ukhls$strata[is.na(ukhls$strata)] <- ukhls$cf_strata[is.na(ukhls$strata)] 
ukhls$strata[is.na(ukhls$strata)] <- ukhls$ce_strata[is.na(ukhls$strata)] 
ukhls$strata[is.na(ukhls$strata)] <- ukhls$cd_strata[is.na(ukhls$strata)] 
ukhls$strata[is.na(ukhls$strata)] <- ukhls$cc_strata[is.na(ukhls$strata)] 
ukhls$strata[is.na(ukhls$strata)] <- ukhls$cb_strata[is.na(ukhls$strata)] 
ukhls$strata[is.na(ukhls$strata)] <- ukhls$ca_strata[is.na(ukhls$strata)] 

ukhls$fpc <- 0 # Set as 0 since missing

# Subset variables required
ukhls$date <- format(as.Date(ukhls$ch_surveystart, format = "%Y-%m-%d %H:%M:%S"), "%d/%m/%Y") #  Save date as latest wave (only 6 NAs so I can't be bothered to check if they are covered elsewhere since other waves have larger NAs e.g., 1243 in wave g) abd convert to same format as other survey dates dd/mm/yyyy
ukhls <- ukhls[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "asthma", "cancer", "diabetes", "hypertension", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables


# Save #
ukhls$cohort <- "ukhls"
save(ukhls, file = "S:/LLC_0009/data/Cleaned Cohorts/ukhls.RData")
rm(ukhls) # Tidy


# ## Track C19 ##
# 
# # Load in data
# data_source <- "trackc19" # Cohort/survey
# table <- "baseline_v0001_20210915" # Specific wave/dataset
# # table <- "basicinfo_v0001_20210915" # Specific wave/dataset
# # table <- "followup_v0001_20210915" # Specific wave/dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

## 1946 Birth Cohort (National Survey of Health and Development) ##

# wave 1 #

# Load
data_source <- "nshd46" # Cohort/survey
table <- "covidw1webpostal_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
nshd46 <- nshd46_covidw1webpostal_v0001_20211101[, c("LLC_0009_stud_id", "cw1_surgcan_10", "cw1_surgcan_11", "cw1_surgcan_12", "cw1_surgcan_13", "cw1_surgcan_14", "cw1_surgcan_15", "cw1_sext", "cw1_combwt", "cw1_econactivityb", "cw1_econactivityd", "cw1_covid19", "cw1_ghq", "cw1_lli_3", "cw1_lli_1", "cw1_lli_6", "cw1_lli_9")] # These are in order - id, disruption experienced (6 vars), sex, survey weight, economic activity (2x), if have had COVID-19, self-rated health status, health conditions (asthma, cancer, diabetes, hypertension)
rm(nshd46_covidw1webpostal_v0001_20211101) 


# Wave 2 #

# Load
data_source <- "nshd46" # Cohort/survey
table <- "covidw2webpostal_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
nshd46_covidw2webpostal_v0001_20211101 <- nshd46_covidw2webpostal_v0001_20211101[, c("LLC_0009_stud_id", "cw2_pmeddif", "cw2_apphosp_1", "cw2_apphosp_2", "cw2_apphosp_3", "cw2_apphad", "cw2_apphadh", "cw2_apphadt", "cw2_appnot", "cw2_apphads", "cw2_apphadsh", "cw2_appnots", "cw2_appmen", "cw2_appmenhad", "cw2_appmenhadh", "cw2_apphadth", "cw2_appnoth", "cw2_psex", "cw2_combwt", "cw2_econactivityb", "cw2_econactivityd", "cw2_peconactivityb", "cw2_peconactivityd", "cw2_covid19", "cw2_ghq", "cw2_enddated", "cw2_enddatem", "cw2_lli1_3", "cw2_lli1_1", "cw2_lli1_6", "cw2_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, then cancelled healthcare experiences but this is tricky so see below note for explanation), sex, survey weight, economic activity (current and previous; two measures for each), if had covid-19 and self rated health, date of survey (day and month), health conditions (asthma, cancer, diabetes, hypertension)

# Note: cw2_apphosp_1 = did you have a hospital apointment, so select only those and then define disruption as cw2_apphad == No (did not have appointment), or cw2_apphadh == appointment was delayed. BUT also cw2_appnot gives reasons for not having the appointment - might be easier. Need to do this for each variable? (does not apply to medications variable):
# apphad = hospital consultation, investigation or treatment, apphads = surgery, and appmen = cbt, counselling or psychological therapy


# Join together (note not same people are in each wave)
nshd46 <- merge(nshd46, nshd46_covidw2webpostal_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(nshd46_covidw2webpostal_v0001_20211101) # Drop to save space


# Wave 3 #

# Load
data_source <- "nshd46" # Cohort/survey
table <- "covidw3web_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
nshd46_covidw3web_v0001_20211101 <- nshd46_covidw3web_v0001_20211101[, c("LLC_0009_stud_id", "cw3_pmeddif", "cw3_appcand", "cw3_appcant_1", "cw3_appcant_2", "cw3_appcant_3", "cw3_appcant_4", "cw3_psex", "cw3_combwt", "cw3_econactivityd", "cw3_peconactivityd", "cw3_covid19", "cw3_ghq", "cw3_enddated", "cw3_enddatem", "cw3_lli1_3", "cw3_lli1_1", "cw3_lli1_6", "cw3_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, cancelled or delayed medical appointment overall then 4 types), sex, survey weight, economic activity (current and previous), if had covid-19 and self rated health, date of survey (day and month), health conditions (asthma, cancer, diabetes, hypertension)


# Join onto main data
nshd46 <- merge(nshd46, nshd46_covidw3web_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE)
rm(nshd46_covidw3web_v0001_20211101) # Drop to save space

# Sociodemographics #

# Load
data_source <- "nshd46" # Cohort/survey
table <- "sociodemographics_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Fed, med and lhqr = education (fathers, mothers, own?), inchn15x = income, scr99u and sc1553 = social class 5 groups (first one better), sex = sex, own14xrec = tenure

# Subset required variables
nshd46_sociodemographics_v0001_20211101 <- nshd46_sociodemographics_v0001_20211101[, c("LLC_0009_stud_id", "sex", "agepo14x", "lhqr", "inchn15x", "own14xrec", "scr99u")] # id, sex, age in months at 2014 questionnaire, education, income, tenure, social class  

# Tidy variables
# To do

# Join onto main data
nshd46 <- merge(nshd46, nshd46_sociodemographics_v0001_20211101, by = "LLC_0009_stud_id", all.x = TRUE)
rm(nshd46_sociodemographics_v0001_20211101)


# Tidy variables # 

# Sex
nshd46$sex[nshd46$sex == 2] <- "female"
nshd46$sex[nshd46$sex == 1] <- "male"

# Age
nshd46$age <- 2020 - 1946 # Need to update (all born in this year - one week in March 1946)
# nshd46$agepo14x[nshd46$agepo14x == -7] <- NA
# nshd46$age_estimate <- (nshd46$agepo14x / 12) + 6 # Age in months at 2014 survey, converted to years and added 6 for 2020 - 6 could be adjusted for survey month?

# Ethnicity
nshd46$ethnicity <- "White" # I can't find this so need to update, but assume most are White due to survey design so put as this for now before can update

# Region
nshd46$region <- NA

# Country
nshd46$country <- NA

# Tenure
nshd46$tenure <- NA# Create blank variable then recode below
nshd46$tenure[nshd46$own14xrec == 1] <- "Own outright"
nshd46$tenure[nshd46$own14xrec == 2] <- "Mortgage"
nshd46$tenure[nshd46$own14xrec == 5] <- "Rent - private"
nshd46$tenure[nshd46$own14xrec == 3 | nshd46$own14xrec == 6 ] <- "Rent - social"
nshd46$tenure[nshd46$own14xrec == 7] <- "Other"

# Education
nshd46$education <- NA # Create blank variable then recode below
nshd46$education[nshd46$lhqr == 0 | nshd46$lhqr == 1 | nshd46$lhqr == 2] <- "No qualifications"
nshd46$education[nshd46$lhqr == 3] <- "Secondary" # GCSE or equivalent
nshd46$education[nshd46$lhqr == 4] <- "Tertiary" # A-level or equivalent
nshd46$education[nshd46$lhqr == 6 | nshd46$lhqr == 7 | nshd46$lhqr == 8] <- "Degree or above"
nshd46$education[nshd46$lhqr == 2 | nshd46$lhqr == 8] <- "Other"

# General health
nshd46$general_health <- NA # Create blank variable
nshd46$general_health[nshd46$cw1_ghq < 4 | nshd46$cw2_ghq < 4 | nshd46$cw3_ghq < 4] <- "Good" # Only reported their health as good
nshd46$general_health[nshd46$cw1_ghq >= 4 | nshd46$cw2_ghq >= 4 | nshd46$cw3_ghq >= 4] <- "Fair/Poor" # Reported their health as fair or poor at any point

# Had COVID-19
nshd46$covid <- NA # Create blank variable
nshd46$covid[nshd46$cw1_covid19 > 2 | nshd46$cw2_covid19 > 2 | nshd46$cw3_covid19 > 2] <- "No" # Opposite
nshd46$covid[nshd46$cw1_covid19 < 3 | nshd46$cw2_covid19 < 3 | nshd46$cw3_covid19 < 3] <- "Yes" # Reported they had COVID-19 in any wave

# Asthma
nshd46$asthma <- 0 # Create blank variable - hard to tell who has missing data though due to set up
nshd46$asthma[nshd46$cw1_lli_3 == 1 | nshd46$cw2_lli1_3 == 1 | nshd46$cw3_lli1_3 == 1] <- 1 # Has condition

# Cancer
nshd46$cancer <- 0 # Create blank variable - hard to tell who has missing data though due to set up
nshd46$cancer[nshd46$cw1_lli_1 == 1 | nshd46$cw2_lli1_1 == 1 | nshd46$cw3_lli1_1 == 1] <- 1 # Has condition

# Diabetes
nshd46$diabetes <- 0 # Create blank variable - hard to tell who has missing data though due to set up
nshd46$diabetes[nshd46$cw1_lli_6 == 1 | nshd46$cw2_lli1_6 == 1 | nshd46$cw3_lli1_6 == 1] <- 1 # Has condition

# Hypertension
nshd46$hypertension <- 0 # Create blank variable - hard to tell who has missing data though due to set up
nshd46$hypertension[nshd46$cw1_lli_9 == 1 | nshd46$cw2_lli1_9 == 1 | nshd46$cw3_lli1_9 == 1] <- 1 # Has condition

# Healthcare disruption
# Wave 2 is a bit complicated to say the least
# var_lab(nshd46$cw1_surgcan_10) #Helpful in looking up variable descriptions
nshd46$disruption_appointments <- NA # Blank variable to represent if had any disruption to appointments (e.g., GP appointments) - binary (yes = 1, 0 = no)
nshd46$disruption_appointments[nshd46$cw1_surgcan_10 == 2 | (nshd46$cw2_appmen ==  2 | nshd46$cw2_appmenhadh == 1) | nshd46$cw2_apphosp_3 == 1 | nshd46$cw2_pmeddif == 2 | nshd46$cw3_appcand == 2] <- 0 # No disruption
nshd46$disruption_appointments[nshd46$cw1_surgcan_10 == 1 | nshd46$cw1_surgcan_13 == 1 | (nshd46$cw2_appmen == 1 & (nshd46$cw2_appmenhad == 2 | nshd46$cw2_appmenhadh == 2 | nshd46$cw2_apphadth == 2 | nshd46$cw2_appnoth > 1)) | (nshd46$cw2_apphosp_1 == 1 & (nshd46$cw2_apphad == 2 | nshd46$cw2_apphadh == 2 | nshd46$cw2_apphadt == 2 | nshd46$cw2_appnot > 1)) | nshd46$cw3_appcant_1 == 1 | nshd46$cw3_appcant_3 == 1] <- 1 # Disruption

nshd46$disruption_medications <- NA # Blank variable to represent if had any disruption in receiving their medications
nshd46$disruption_medications[nshd46$cw1_surgcan_14 == 2 | nshd46$cw2_pmeddif == 2 | nshd46$cw3_pmeddif == 2] <- 0 # No disruption (first one is no disruption, as 14 is not a yes/no option and only yes)
nshd46$disruption_medications[nshd46$cw1_surgcan_14 == 1 | nshd46$cw2_pmeddif == 1 | nshd46$cw3_pmeddif == 1] <- 1 # Disruption (at some point) - overwrites any 0s from above

nshd46$disruption_procedures <- NA # Blank variable for surgery, cancer treatment
nshd46$disruption_procedures[nshd46$cw1_surgcan_15 == 2 | (nshd46$cw2_apphosp_2 == 1 & nshd46$cw2_apphadsh == 1) | nshd46$cw3_appcand == 2] <- 0 # No disruption
nshd46$disruption_procedures[nshd46$cw1_surgcan_11 == 1 | nshd46$cw1_surgcan_12 == 1 | (nshd46$cw2_apphosp_2 == 1 & (nshd46$cw2_apphads == 2 | nshd46$cw2_apphadsh == 2 | nshd46$cw2_appnots > 1)) | nshd46$cw3_appcant_2 == 1] <- 1 

nshd46$disruption_any <- NA # If had any of the above experiences (overall variable)
nshd46$disruption_any[nshd46$disruption_appointments == 0 | nshd46$disruption_medications == 0 | nshd46$disruption_procedures == 0] <- 0 # Set as OR in case have missing data, since next line will over-write the 1s anyway
nshd46$disruption_any[nshd46$disruption_appointments == 1 | nshd46$disruption_medications == 1 | nshd46$disruption_procedures == 1] <- 1

# Generate sample weights
# Take most recent weight
nshd46$weight <- nshd46$cw3_combwt # Most recent
nshd46$weight[is.na(nshd46$weight)] <- nshd46$cw2_combwt[is.na(nshd46$weight)] # If missing, then take previous one
nshd46$weight[is.na(nshd46$weight)] <- nshd46$cw1_combwt[is.na(nshd46$weight)] # repeat
nshd46$weight[is.na(nshd46$weight)] <- 1 # If missing still, follow rules 
# Sample weights are missing so replace with the following
nshd46$psu <- nshd46$LLC_0009_stud_id # Unique to each individual
nshd46$strata <- 2 # Constant across cohort
nshd46$fpc <- 0 # Set as 0

# Subset variables required
nshd46$date <- paste0(nshd46$cw3_enddated, "/", nshd46$cw3_enddatem, "/2021", sep = "") #  Save date as latest wave
# survey weight???
nshd46 <- nshd46[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "asthma", "cancer", "diabetes", "hypertension", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables


# Save #
nshd46$cohort <- "nshd46"
save(nshd46, file = "S:/LLC_0009/data/Cleaned Cohorts/nshd_1946.RData")
rm(nshd46) # Tidy


# ## Nicola ##
# 
# # Load in data
# data_source <- "nicola" # Cohort/survey
# table <- "covid_19questionnaire_v0001_20211101" # Specific wave/dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

# ## NIHR BIO ##
# 
# # Load in data
# data_source <- "nihrbio" # Cohort/survey
# table <- "coping_coping_v0001_20210719" # Specific wave/dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

## Next steps ##

# Wave 1 #

# Load in data
data_source <- "nextstep" # Cohort/survey
table <- "covid_w1_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
nextstep <- nextstep_covid_w1_v0001_20211101[, c("LLC_0009_stud_id", "cw1_surgcan", "cw1_psex", "cw1_combwt", "cw1_samppsu", "cw1_sampstratum", "cw1_econactivityb", "cw1_econactivityd", "cw1_covid19", "cw1_ghq", "cw1_region", "cw1_nssec2010an", "cw1_enddate", "cw1_lli_3", "cw1_lli_1", "cw1_lli_6", "cw1_lli_9")] # These are in order - id, disruption experienced (6 vars), sex, survey weight, economic activity (2x), if have had COVID-19, self-rated health status, health conditions (asthma, cancer, diabetes, hypertension)
rm(nextstep_covid_w1_v0001_20211101) 

# Tidy variables (to do)


# Wave 2 #

# Load in data
data_source <- "nextstep" # Cohort/survey
table <- "covid_w2_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
nextstep_covid_w2_v0001_20211101 <- nextstep_covid_w2_v0001_20211101[, c("LLC_0009_stud_id", "cw2_pmeddif", "cw2_apphosp_1", "cw2_apphosp_2", "cw2_apphosp_3", "cw2_apphad", "cw2_apphadh", "cw2_apphadt", "cw2_appnot", "cw2_apphads", "cw2_apphadsh", "cw2_appnots", "cw2_appmen", "cw2_appmenhad", "cw2_appmenhadh", "cw2_apphadth", "cw2_appnoth", "cw2_psex", "cw2_combwt", "cw2_samppsu", "cw2_sampstratum", "cw2_econactivityb", "cw2_econactivityd", "cw2_peconactivityb", "cw2_peconactivityd", "cw2_covid19", "cw2_ghq", "cw2_enddated", "cw2_enddatem", "cw2_region", "cw2_nssec2010an", "cw2_lli1_3", "cw2_lli1_1", "cw2_lli1_6", "cw2_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, then cancelled healthcare experiences but this is tricky so see below note for explanation), sex, survey weight, economic activity (current and previous; two measures for each), if had covid-19 and self rated health, date of survey (day and month), region, NS-SeC, LLTI

# Note: cw2_apphosp_1 = did you have a hospital apointment, so select only those and then define disruption as cw2_apphad == No (did not have appointment), or cw2_apphadh == appointment was delayed. BUT also cw2_appnot gives reasons for not having the appointment - might be easier. Need to do this for each variable? (does not apply to medications variable):
# apphad = hospital conslutation, investigation or treatment, apphads = surgery, and appmen = cbt, counselling or psychological therapy

# Join together (note not same people are in each wave)
nextstep <- merge(nextstep, nextstep_covid_w2_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(nextstep_covid_w2_v0001_20211101) # Drop to save space


# Wave 3 #

# Load in data
data_source <- "nextstep" # Cohort/survey
table <- "covid_w3_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
nextstep_covid_w3_v0001_20211101 <- nextstep_covid_w3_v0001_20211101[, c("LLC_0009_stud_id", "cw3_pmeddif", "cw3_appcand", "cw3_appcant_1", "cw3_appcant_2", "cw3_appcant_3", "cw3_appcant_4", "cw3_psex", "cw3_combwt", "cw3_samppsu", "cw3_sampstratum", "cw3_econactivityd", "cw3_econactivityb2", "cw3_peconactivityd", "cw3_peconactivityb2", "cw3_covid19", "cw3_ghq", "cw3_enddated", "cw3_enddatem", "cw3_region", "cw3_nssec2010an", "cw3_lli1_3", "cw3_lli1_1", "cw3_lli1_6", "cw3_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, cancelled or delayed medical appointment overall then 4 types), sex, survey weight, economic activity (current and previous), if had covid-19 and self rated health, date of survey (day and month)


# Join onto main data
nextstep <- merge(nextstep, nextstep_covid_w3_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE)
rm(nextstep_covid_w3_v0001_20211101) # Drop to save space


# Demographics #

# Load in data
data_source <- "nextstep" # Cohort/survey
table <- "basic_demographic_data_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
nextstep_basic_demographic_data_v0001_20211101 <- nextstep_basic_demographic_data_v0001_20211101[, c("LLC_0009_stud_id", "w1ethgrpyp", "w8bdaty", "w8bdatm", "w8dnssec5", "w8dnssec8", "w8tenure", "w8dhanvqh")] # id, 


# Join onto main data
nextstep <- merge(nextstep, nextstep_basic_demographic_data_v0001_20211101, by = "LLC_0009_stud_id", all.x = TRUE)
rm(nextstep_basic_demographic_data_v0001_20211101)


# Tidy variables # 

# Sex
nextstep$sex <- NA
nextstep$sex[nextstep$cw1_psex == 2 | nextstep$cw2_psex == 2 | nextstep$cw3_psex == 2] <- "female" # No one changes sex, but may not always appear in all waves
nextstep$sex[nextstep$cw1_psex == 1 | nextstep$cw2_psex == 1 | nextstep$cw3_psex == 1] <- "male"

# Age
nextstep$age <- 2020 - nextstep$w8bdaty # Need to update with month?

# Ethnicity
nextstep$ethnicity[nextstep$w1ethgrpyp == 1] <- "White"
nextstep$ethnicity[nextstep$w1ethgrpyp == 3 | nextstep$w1ethgrpyp == 4 | nextstep$w1ethgrpyp == 5] <- "Asian"
nextstep$ethnicity[nextstep$w1ethgrpyp == 6 | nextstep$w1ethgrpyp == 7] <- "Black"
nextstep$ethnicity[nextstep$w1ethgrpyp == 2] <- "Mixed"
nextstep$ethnicity[nextstep$w1ethgrpyp == 8] <- "Other"

# Region
nextstep$region[nextstep$cw3_region == 1 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 1) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 1)] <- "North East" # Take most recent location
nextstep$region[nextstep$cw3_region == 2 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 2) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 2)] <- "North West" 
nextstep$region[nextstep$cw3_region == 3 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 3) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 3)] <- "Yorkshire" 
nextstep$region[nextstep$cw3_region == 4 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 4) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 4)] <- "East Midlands" 
nextstep$region[nextstep$cw3_region == 5 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 5) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 5)] <- "West Midlands" 
nextstep$region[nextstep$cw3_region == 6 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 6) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 6)] <- "East" 
nextstep$region[nextstep$cw3_region == 7 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 7) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 7)] <- "London" 
nextstep$region[nextstep$cw3_region == 8 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 8) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 8)] <- "South East" 
nextstep$region[nextstep$cw3_region == 9 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 9) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 9)] <- "South West"
nextstep$region[nextstep$cw3_region == 10 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 10) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 10)] <- "Wales"
nextstep$region[nextstep$cw3_region == 12 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 12) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 12)] <- "N Ireland" 
nextstep$region[nextstep$cw3_region == 11 | (is.na(nextstep$cw3_region) & nextstep$cw2_region == 11) | (is.na(nextstep$cw3_region) & is.na(nextstep$cw2_region) & nextstep$cw1_region == 11)] <- "Scotland"

# Country
nextstep$country <- nextstep$region #  Can work out from above?
nextstep$country[nextstep$country != "Scotland" & nextstep$country != "N Ireland" & nextstep$country != "Wales"] <- "England"

# Tenure
nextstep$tenure <- NA# Create blank variable then recode below
nextstep$tenure[nextstep$w8tenure == 1] <- "Own outright"
nextstep$tenure[nextstep$w8tenure == 2 | nextstep$w8tenure == 3] <- "Mortgage"
nextstep$tenure[nextstep$w8tenure == 4] <- "Rent"
nextstep$tenure[nextstep$w8tenure == 5 | nextstep$w8tenure == 6 | nextstep$w8tenure == 7] <- "Other"

# Education
nextstep$education <- NA # Create blank variable then recode below
nextstep$education[nextstep$w8dhanvqh == 96] <- "No qualifications"
nextstep$education[nextstep$w8dhanvqh == 1 | nextstep$w8dhanvqh == 2] <- "Secondary" # GCSE or equivalent
nextstep$education[nextstep$w8dhanvqh == 3] <- "Tertiary" # A-level or equivalent
nextstep$education[nextstep$w8dhanvqh == 4 | nextstep$w8dhanvqh == 5] <- "Degree or above"
nextstep$education[nextstep$w8dhanvqh == 95] <- "Other"

# General health
nextstep$general_health <- NA # Create blank variable
nextstep$general_health[nextstep$cw1_ghq < 4 | nextstep$cw2_ghq < 4 | nextstep$cw3_ghq < 4] <- "Good" # Only reported their health as good
nextstep$general_health[nextstep$cw1_ghq >= 4 | nextstep$cw2_ghq >= 4 | nextstep$cw3_ghq >= 4] <- "Fair/Poor" # Reported their health as fair or poor at any point

# Had COVID-19
nextstep$covid <- NA # Create blank variable
nextstep$covid[nextstep$cw1_covid19 > 2 | nextstep$cw2_covid19 > 2 | nextstep$cw3_covid19 > 2] <- "No" # Opposite
nextstep$covid[nextstep$cw1_covid19 < 3 | nextstep$cw2_covid19 < 3 | nextstep$cw3_covid19 < 3] <- "Yes" # Reported they had COVID-19 in any wave

# Asthma
nextstep$asthma <- 0 # Create blank variable - hard to tell who has missing data though due to set up
nextstep$asthma[nextstep$cw1_lli_3 == 1 | nextstep$cw2_lli1_3 == 1 | nextstep$cw3_lli1_3 == 1] <- 1 # Has condition

# Cancer
nextstep$cancer <- 0 # Create blank variable - hard to tell who has missing data though due to set up
nextstep$cancer[nextstep$cw1_lli_1 == 1 | nextstep$cw2_lli1_1 == 1 | nextstep$cw3_lli1_1 == 1] <- 1 # Has condition

# Diabetes
nextstep$diabetes <- 0 # Create blank variable - hard to tell who has missing data though due to set up
nextstep$diabetes[nextstep$cw1_lli_6 == 1 | nextstep$cw2_lli1_6 == 1 | nextstep$cw3_lli1_6 == 1] <- 1 # Has condition

# Hypertension
nextstep$hypertension <- 0 # Create blank variable - hard to tell who has missing data though due to set up
nextstep$hypertension[nextstep$cw1_lli_9 == 1 | nextstep$cw2_lli1_9 == 1 | nextstep$cw3_lli1_9 == 1] <- 1 # Has condition

# Healthcare disruption
# Wave 2 is a bit complicated to say the least
# var_lab(nextstep$cw1_surgcan_10) #Helpful in looking up variable descriptions
nextstep$disruption_appointments <- NA # Blank variable to represent if had any disruption to appointments (e.g., GP appointments) - binary (yes = 1, 0 = no)
nextstep$disruption_appointments[nextstep$cw1_surgcan == 2 | (nextstep$cw2_appmen ==  2 | nextstep$cw2_appmenhadh == 1) | nextstep$cw2_apphosp_3 == 1 | nextstep$cw2_pmeddif == 2 | nextstep$cw3_appcand == 2] <- 0 # No disruption
nextstep$disruption_appointments[nextstep$cw1_surgcan == 1 | (nextstep$cw2_appmen == 1 & (nextstep$cw2_appmenhad == 2 | nextstep$cw2_appmenhadh == 2 | nextstep$cw2_apphadth == 2 | nextstep$cw2_appnoth > 1)) | (nextstep$cw2_apphosp_1 == 1 & (nextstep$cw2_apphad == 2 | nextstep$cw2_apphadh == 2 | nextstep$cw2_apphadt == 2 | nextstep$cw2_appnot > 1)) | nextstep$cw3_appcant_1 == 1 | nextstep$cw3_appcant_3 == 1] <- 1 # Disruption

nextstep$disruption_medications <- NA # Blank variable to represent if had any disruption in receiving their medications
nextstep$disruption_medications[nextstep$cw2_pmeddif == 2 | nextstep$cw3_pmeddif == 2] <- 0 # No disruption (first one is no disruption, as 14 is not a yes/no option and only yes)
nextstep$disruption_medications[nextstep$cw2_pmeddif == 1 | nextstep$cw3_pmeddif == 1] <- 1 # Disruption (at some point) - overwrites any 0s from above

nextstep$disruption_procedures <- NA # Blank variable for surgery, cancer treatment
nextstep$disruption_procedures[nextstep$cw1_surgcan == 2 | (nextstep$cw2_apphosp_2 == 1 & nextstep$cw2_apphadsh == 1) | nextstep$cw3_appcand == 2] <- 0 # No disruption
nextstep$disruption_procedures[nextstep$cw1_surgcan == 1 | (nextstep$cw2_apphosp_2 == 1 & (nextstep$cw2_apphads == 2 | nextstep$cw2_apphadsh == 2 | nextstep$cw2_appnots > 1)) | nextstep$cw3_appcant_2 == 1] <- 1 

nextstep$disruption_any <- NA # If had any of the above experiences (overall variable)
nextstep$disruption_any[nextstep$disruption_appointments == 0 | nextstep$disruption_medications == 0 | nextstep$disruption_procedures == 0] <- 0 # Set as OR in case have missing data, since next line will over-write the 1s anyway
nextstep$disruption_any[nextstep$disruption_appointments == 1 | nextstep$disruption_medications == 1 | nextstep$disruption_procedures == 1] <- 1


# Generate sample weights
# Take most recent weights
nextstep$weight <- nextstep$cw3_combwt # Most recent weight
nextstep$weight[is.na(nextstep$weight)] <- nextstep$cw2_combwt[is.na(nextstep$weight)] # If missing, then take previous one
nextstep$weight[is.na(nextstep$weight)] <- nextstep$cw1_combwt[is.na(nextstep$weight)] # repeat
nextstep$weight[is.na(nextstep$weight)] <- 1 # If missing still, follow rules 

nextstep$psu <- nextstep$cw3_samppsu # Most recent psu
nextstep$psu[is.na(nextstep$psu)] <- nextstep$cw2_samppsu[is.na(nextstep$psu)] # If missing, then take previous one
nextstep$psu[is.na(nextstep$psu)] <- nextstep$cw1_samppsu[is.na(nextstep$psu)] # repeat
nextstep$psu[is.na(nextstep$psu)] <- nextstep$LLC_0009_stud_id[is.na(nextstep$psu)] # If missing still, follow rules 

nextstep$strata <- nextstep$cw3_sampstratum # Most recent strata
nextstep$strata[is.na(nextstep$strata)] <- nextstep$cw2_sampstratum[is.na(nextstep$strata)] # If missing, then take previous one
nextstep$strata[is.na(nextstep$strata)] <- nextstep$cw1_sampstratum[is.na(nextstep$strata)] # repeat
nextstep$strata[is.na(nextstep$strata)] <- 9 # If missing still, follow rules 

# Sample weights are missing so replace with the following
nextstep$fpc <- 0 # Set as 0

# Subset variables required
nextstep$date <- paste0(nextstep$cw3_enddated, "/", nextstep$cw3_enddatem, "/2021", sep = "") #  Save date as latest wave
# survey weight???
nextstep <- nextstep[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "asthma", "cancer", "diabetes", "hypertension", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables

# Save #
nextstep$cohort <- "nextstep" # Save name of cohort for when combine them together
save(nextstep, file = "S:/LLC_0009/data/Cleaned Cohorts/nextstep.RData") # save
rm(nextstep) # Tidy




## National Child Development Study 1958 ##

# Wave 1 #

# Load in data
data_source <- "ncds58" # Cohort/survey
table <- "covid_w1_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ncds58 <- ncds58_covid_w1_v0001_20211101[, c("LLC_0009_stud_id", "cw1_surgcan", "cw1_psex", "cw1_combwt", "cw1_samppsu", "cw1_sampstratum", "cw1_econactivityb", "cw1_econactivityd", "cw1_covid19", "cw1_ghq", "cw1_region", "cw1_nssec2010an", "cw1_enddate", "cw1_lli_3", "cw1_lli_1", "cw1_lli_6", "cw1_lli_9")] # These are in order - id, disruption experienced (6 vars), sex, survey weight, economic activity (2x), if have had COVID-19, self-rated health status, health conditions (asthma, cancer, diabetes, hypertension)
rm(ncds58_covid_w1_v0001_20211101) 



# Wave 2 #

# Load in data
data_source <- "ncds58" # Cohort/survey
table <- "covid_w2_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ncds58_covid_w2_v0001_20211101 <- ncds58_covid_w2_v0001_20211101[, c("LLC_0009_stud_id", "cw2_pmeddif", "cw2_apphosp_1", "cw2_apphosp_2", "cw2_apphosp_3", "cw2_apphad", "cw2_apphadh", "cw2_apphadt", "cw2_appnot", "cw2_apphads", "cw2_apphadsh", "cw2_appnots", "cw2_appmen", "cw2_appmenhad", "cw2_appmenhadh", "cw2_apphadth", "cw2_appnoth", "cw2_psex", "cw2_combwt", "cw2_samppsu", "cw2_sampstratum", "cw2_econactivityb", "cw2_econactivityd", "cw2_peconactivityb", "cw2_peconactivityd", "cw2_covid19", "cw2_ghq", "cw2_enddated", "cw2_enddatem", "cw2_region", "cw2_nssec2010an", "cw2_lli1_3", "cw2_lli1_1", "cw2_lli1_6", "cw2_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, then cancelled healthcare experiences but this is tricky so see below note for explanation), sex, survey weight, economic activity (current and previous; two measures for each), if had covid-19 and self rated health, date of survey (day and month), region, NS-SeC, LLTI

# Note: cw2_apphosp_1 = did you have a hospital apointment, so select only those and then define disruption as cw2_apphad == No (did not have appointment), or cw2_apphadh == appointment was delayed. BUT also cw2_appnot gives reasons for not having the appointment - might be easier. Need to do this for each variable? (does not apply to medications variable):
# apphad = hospital conslutation, investigation or treatment, apphads = surgery, and appmen = cbt, counselling or psychological therapy


# Join together (note not same people are in each wave)
ncds58 <- merge(ncds58, ncds58_covid_w2_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(ncds58_covid_w2_v0001_20211101) # Drop to save space


# Wave 3 #

# Load in data
data_source <- "ncds58" # Cohort/survey
table <- "covid_w3_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ncds58_covid_w3_v0001_20211101 <- ncds58_covid_w3_v0001_20211101[, c("LLC_0009_stud_id", "cw3_pmeddif", "cw3_appcand", "cw3_appcant_1", "cw3_appcant_2", "cw3_appcant_3", "cw3_appcant_4", "cw3_psex", "cw3_combwt", "cw3_samppsu", "cw3_sampstratum", "cw3_econactivityd", "cw3_econactivityb2", "cw3_peconactivityd", "cw3_peconactivityb2", "cw3_covid19", "cw3_ghq", "cw3_enddated", "cw3_enddatem", "cw3_region", "cw3_nssec2010an", "cw3_lli1_3", "cw3_lli1_1", "cw3_lli1_6", "cw3_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, cancelled or delayed medical appointment overall then 4 types), sex, survey weight, economic activity (current and previous), if had covid-19 and self rated health, date of survey (day and month)


# Join onto main data
ncds58 <- merge(ncds58, ncds58_covid_w3_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE)
rm(ncds58_covid_w3_v0001_20211101) # Drop to save space


# Demographics #

# Load in data
data_source <- "ncds58" # Cohort/survey
table <- "basic_demographic_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
ncds58_basic_demographic_v0001_20211101 <- ncds58_basic_demographic_v0001_20211101[, c("LLC_0009_stud_id", "ethnic", "nd9ecact", "n2snssec", "n9ten", "nd9hnvq")] # id, 

# Join onto main data
ncds58 <- merge(ncds58, ncds58_basic_demographic_v0001_20211101, by = "LLC_0009_stud_id", all.x = TRUE)
rm(ncds58_basic_demographic_v0001_20211101)

# Tidy variables # 

# Sex
ncds58$sex <- NA
ncds58$sex[ncds58$cw1_psex == 2 | ncds58$cw2_psex == 2 | ncds58$cw3_psex == 2] <- "female" # No one changes sex, but may not always appear in all waves
ncds58$sex[ncds58$cw1_psex == 1 | ncds58$cw2_psex == 1 | ncds58$cw3_psex == 1] <- "male"

# Age
ncds58$age <- 2020 - 1958 # Need to update (all born in this year in one week in March 1958)

# Ethnicity
ncds58$ethnicity[ncds58$ethnic == 1 | ncds58$ethnic == 2 | ncds58$ethnic == 3] <- "White"
ncds58$ethnicity[ncds58$ethnic == 8 | ncds58$ethnic == 9 | ncds58$ethnic == 10 | ncds58$ethnic == 11] <- "Asian"
ncds58$ethnicity[ncds58$ethnic == 12 | ncds58$ethnic == 13 | ncds58$ethnic == 14] <- "Black"
ncds58$ethnicity[ncds58$ethnic == 4 | ncds58$ethnic == 5 | ncds58$ethnic == 6 | ncds58$ethnic == 7] <- "Mixed"
ncds58$ethnicity[ncds58$ethnic == 16] <- "Other"

# Region
ncds58$region[ncds58$cw3_region == 1 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 1) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 1)] <- "North East" # Take most recent location
ncds58$region[ncds58$cw3_region == 2 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 2) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 2)] <- "North West" 
ncds58$region[ncds58$cw3_region == 3 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 3) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 3)] <- "Yorkshire" 
ncds58$region[ncds58$cw3_region == 4 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 4) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 4)] <- "East Midlands" 
ncds58$region[ncds58$cw3_region == 5 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 5) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 5)] <- "West Midlands" 
ncds58$region[ncds58$cw3_region == 6 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 6) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 6)] <- "East" 
ncds58$region[ncds58$cw3_region == 7 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 7) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 7)] <- "London" 
ncds58$region[ncds58$cw3_region == 8 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 8) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 8)] <- "South East" 
ncds58$region[ncds58$cw3_region == 9 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 9) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 9)] <- "South West"
ncds58$region[ncds58$cw3_region == 10 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 10) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 10)] <- "Wales"
ncds58$region[ncds58$cw3_region == 12 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 12) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 12)] <- "N Ireland" 
ncds58$region[ncds58$cw3_region == 11 | (is.na(ncds58$cw3_region) & ncds58$cw2_region == 11) | (is.na(ncds58$cw3_region) & is.na(ncds58$cw2_region) & ncds58$cw1_region == 11)] <- "Scotland"

# Country
ncds58$country <- ncds58$region #  Can work out from above?
ncds58$country[ncds58$country != "Scotland" & ncds58$country != "N Ireland" & ncds58$country != "Wales"] <- "England"

# Tenure
ncds58$tenure <- NA# Create blank variable then recode below
ncds58$tenure[ncds58$n9ten == 1] <- "Own outright"
ncds58$tenure[ncds58$n9ten == 2 | ncds58$n9ten == 3] <- "Mortgage"
ncds58$tenure[ncds58$n9ten == 4] <- "Rent"
ncds58$tenure[ncds58$n9ten == 5 | ncds58$n9ten == 6 | ncds58$n9ten == 7] <- "Other"

# Education
ncds58$education <- NA # Create blank variable then recode below
ncds58$education[ncds58$nd9hnvq == 0] <- "No qualifications"
ncds58$education[ncds58$nd9hnvq == 1 | ncds58$nd9hnvq == 2] <- "Secondary" # GCSE or equivalent
ncds58$education[ncds58$nd9hnvq == 3] <- "Tertiary" # A-level or equivalent
ncds58$education[ncds58$nd9hnvq == 4 | ncds58$nd9hnvq == 5] <- "Degree or above"

# General health
ncds58$general_health <- NA # Create blank variable
ncds58$general_health[ncds58$cw1_ghq < 4 | ncds58$cw2_ghq < 4 | ncds58$cw3_ghq < 4] <- "Good" # Only reported their health as good
ncds58$general_health[ncds58$cw1_ghq >= 4 | ncds58$cw2_ghq >= 4 | ncds58$cw3_ghq >= 4] <- "Fair/Poor" # Reported their health as fair or poor at any point

# Had COVID-19
ncds58$covid <- NA # Create blank variable
ncds58$covid[ncds58$cw1_covid19 > 2 | ncds58$cw2_covid19 > 2 | ncds58$cw3_covid19 > 2] <- "No" # Opposite
ncds58$covid[ncds58$cw1_covid19 < 3 | ncds58$cw2_covid19 < 3 | ncds58$cw3_covid19 < 3] <- "Yes" # Reported they had COVID-19 in any wave

# Asthma
ncds58$asthma <- 0 # Create blank variable - hard to tell who has missing data though due to set up
ncds58$asthma[ncds58$cw1_lli_3 == 1 | ncds58$cw2_lli1_3 == 1 | ncds58$cw3_lli1_3 == 1] <- 1 # Has condition

# Cancer
ncds58$cancer <- 0 # Create blank variable - hard to tell who has missing data though due to set up
ncds58$cancer[ncds58$cw1_lli_1 == 1 | ncds58$cw2_lli1_1 == 1 | ncds58$cw3_lli1_1 == 1] <- 1 # Has condition

# Diabetes
ncds58$diabetes <- 0 # Create blank variable - hard to tell who has missing data though due to set up
ncds58$diabetes[ncds58$cw1_lli_6 == 1 | ncds58$cw2_lli1_6 == 1 | ncds58$cw3_lli1_6 == 1] <- 1 # Has condition

# Hypertension
ncds58$hypertension <- 0 # Create blank variable - hard to tell who has missing data though due to set up
ncds58$hypertension[ncds58$cw1_lli_9 == 1 | ncds58$cw2_lli1_9 == 1 | ncds58$cw3_lli1_9 == 1] <- 1 # Has condition

# Healthcare disruption
# Wave 2 is a bit complicated to say the least
# var_lab(ncds58$cw1_surgcan_10) #Helpful in looking up variable descriptions
ncds58$disruption_appointments <- NA # Blank variable to represent if had any disruption to appointments (e.g., GP appointments) - binary (yes = 1, 0 = no)
ncds58$disruption_appointments[ncds58$cw1_surgcan == 2 | (ncds58$cw2_appmen ==  2 | ncds58$cw2_appmenhadh == 1) | ncds58$cw2_apphosp_3 == 1 | ncds58$cw2_pmeddif == 2 | ncds58$cw3_appcand == 2] <- 0 # No disruption
ncds58$disruption_appointments[ncds58$cw1_surgcan == 1 | (ncds58$cw2_appmen == 1 & (ncds58$cw2_appmenhad == 2 | ncds58$cw2_appmenhadh == 2 | ncds58$cw2_apphadth == 2 | ncds58$cw2_appnoth > 1)) | (ncds58$cw2_apphosp_1 == 1 & (ncds58$cw2_apphad == 2 | ncds58$cw2_apphadh == 2 | ncds58$cw2_apphadt == 2 | ncds58$cw2_appnot > 1)) | ncds58$cw3_appcant_1 == 1 | ncds58$cw3_appcant_3 == 1] <- 1 # Disruption

ncds58$disruption_medications <- NA # Blank variable to represent if had any disruption in receiving their medications
ncds58$disruption_medications[ncds58$cw2_pmeddif == 2 | ncds58$cw3_pmeddif == 2] <- 0 # No disruption (first one is no disruption, as 14 is not a yes/no option and only yes)
ncds58$disruption_medications[ncds58$cw2_pmeddif == 1 | ncds58$cw3_pmeddif == 1] <- 1 # Disruption (at some point) - overwrites any 0s from above

ncds58$disruption_procedures <- NA # Blank variable for surgery, cancer treatment
ncds58$disruption_procedures[ncds58$cw1_surgcan == 2 | (ncds58$cw2_apphosp_2 == 1 & ncds58$cw2_apphadsh == 1) | ncds58$cw3_appcand == 2] <- 0 # No disruption
ncds58$disruption_procedures[ncds58$cw1_surgcan == 1 | (ncds58$cw2_apphosp_2 == 1 & (ncds58$cw2_apphads == 2 | ncds58$cw2_apphadsh == 2 | ncds58$cw2_appnots > 1)) | ncds58$cw3_appcant_2 == 1] <- 1 

ncds58$disruption_any <- NA # If had any of the above experiences (overall variable)
ncds58$disruption_any[ncds58$disruption_appointments == 0 | ncds58$disruption_medications == 0 | ncds58$disruption_procedures == 0] <- 0 # Set as OR in case have missing data, since next line will over-write the 1s anyway
ncds58$disruption_any[ncds58$disruption_appointments == 1 | ncds58$disruption_medications == 1 | ncds58$disruption_procedures == 1] <- 1

# Generate sample weights
# Take most recent weights
ncds58$weight <- ncds58$cw3_combwt # Most recent weight
ncds58$weight[is.na(ncds58$weight)] <- ncds58$cw2_combwt[is.na(ncds58$weight)] # If missing, then take previous one
ncds58$weight[is.na(ncds58$weight)] <- ncds58$cw1_combwt[is.na(ncds58$weight)] # repeat
ncds58$weight[is.na(ncds58$weight)] <- 1 # If missing still, follow rules

ncds58$psu <- ncds58$cw3_samppsu # Most recent psu
ncds58$psu[is.na(ncds58$psu)] <- ncds58$cw2_samppsu[is.na(ncds58$psu)] # If missing, then take previous one
ncds58$psu[is.na(ncds58$psu)] <- ncds58$cw1_samppsu[is.na(ncds58$psu)] # repeat
ncds58$psu[is.na(ncds58$psu)] <- ncds58$LLC_0009_stud_id[is.na(ncds58$psu)] # If missing still, follow rules

ncds58$strata <- ncds58$cw3_sampstratum # Most recent strata
ncds58$strata[is.na(ncds58$strata)] <- ncds58$cw2_sampstratum[is.na(ncds58$strata)] # If missing, then take previous one
ncds58$strata[is.na(ncds58$strata)] <- ncds58$cw1_sampstratum[is.na(ncds58$strata)] # repeat
ncds58$strata[is.na(ncds58$strata)] <- 8 # If missing still, follow rules

# Sample weights are missing so replace with the following
ncds58$fpc <- 0 # Set as 0


# Subset variables required
ncds58$date <- paste0(ncds58$cw3_enddated, "/", ncds58$cw3_enddatem, "/2021", sep = "") #  Save date as latest wave
# survey weight???
ncds58 <- ncds58[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "asthma", "cancer", "diabetes", "hypertension", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables

# Save #
ncds58$cohort <- "ncds58"
save(ncds58, file = "S:/LLC_0009/data/Cleaned Cohorts/ncds58.RData")
rm(ncds58)


## Millennium Cohort Study  - cohort members ## 

# Wave 1

# Load in data
data_source <- "mcs" # Cohort/survey
table <- "covid_cm_w1_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
mcs <- mcs_covid_cm_w1_v0001_20211101[, c("LLC_0009_stud_id", "cw1_surgcan", "cw1_psex", "cw1_combwt", "cw1_samppsu", "cw1_sampstratum", "cw1_covid19", "cw1_ghq", "cw1_region", "cw1_enddate", "cw1_lli_3", "cw1_lli_1", "cw1_lli_6", "cw1_lli_9")] # These are in order - id, disruption experienced (surgery), sex, survey weight,if have had COVID-19, self-rated health status, region, date, health conditions (asthma, cancer, diabetes, hypertension)
rm(mcs_covid_cm_w1_v0001_20211101) 

# Wave 2

# Load in data
data_source <- "mcs" # Cohort/survey
table <- "covid_cm_w2_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
mcs_covid_cm_w2_v0001_20211101 <- mcs_covid_cm_w2_v0001_20211101[, c("LLC_0009_stud_id", "cw2_pmeddif", "cw2_apphosp_1", "cw2_apphosp_2", "cw2_apphosp_3", "cw2_apphad", "cw2_apphadh", "cw2_apphadt", "cw2_appnot", "cw2_apphads", "cw2_apphadsh", "cw2_appnots", "cw2_appmen", "cw2_appmenhad", "cw2_appmenhadh", "cw2_apphadth", "cw2_appnoth", "cw2_psex", "cw2_combwt", "cw2_samppsu", "cw2_sampstratum", "cw2_covid19", "cw2_ghq", "cw2_enddated", "cw2_enddatem", "cw2_region", "cw2_tenure", "cw2_lli1_3", "cw2_lli1_1", "cw2_lli1_6", "cw2_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, then cancelled healthcare experiences but this is tricky so see below note for explanation), sex, survey weight, if had covid-19 and self rated health, date of survey (day and month), region, tenure

# Note: cw2_apphosp_1 = did you have a hospital apointment, so select only those and then define disruption as cw2_apphad == No (did not have appointment), or cw2_apphadh == appointment was delayed. BUT also cw2_appnot gives reasons for not having the appointment - might be easier. Need to do this for each variable? (does not apply to medications variable):
# apphad = hospital conslutation, investigation or treatment, apphads = surgery, and appmen = cbt, counselling or psychological therapy

# Join onto main data
mcs <- merge(mcs, mcs_covid_cm_w2_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE)
rm(mcs_covid_cm_w2_v0001_20211101) # Drop to save space

# Wave 3

# Load in data
data_source <- "mcs" # Cohort/survey
table <- "covid_cm_w3_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
mcs_covid_cm_w3_v0001_20211101 <- mcs_covid_cm_w3_v0001_20211101[, c("LLC_0009_stud_id", "cw3_pmeddif", "cw3_appcand", "cw3_appcant_1", "cw3_appcant_2", "cw3_appcant_3", "cw3_appcant_4", "cw3_psex", "cw3_combwt", "cw3_samppsu", "cw3_sampstratum", "cw3_covid19", "cw3_ghq", "cw3_enddated", "cw3_enddatem", "cw3_region", "cw3_tenure", "cw3_lli1_3", "cw3_lli1_1", "cw3_lli1_6", "cw3_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, cancelled or delayed medical appointment overall then 4 types), sex, survey weight, if had covid-19 and self rated health, date of survey (day and month), tenure


# Join onto main data
mcs <- merge(mcs, mcs_covid_cm_w3_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE)
rm(mcs_covid_cm_w3_v0001_20211101) # Drop to save space

# Socio-demographics

# Load in data
data_source <- "mcs" # Cohort/survey
table <- "basic_demographic_cm_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
mcs_basic_demographic_cm_v0001_20211101 <- mcs_basic_demographic_cm_v0001_20211101[, c("LLC_0009_stud_id", "adc11e00")] # id, 

# Join onto main data
mcs <- merge(mcs, mcs_basic_demographic_cm_v0001_20211101, by = "LLC_0009_stud_id", all.x = TRUE)
rm(mcs_basic_demographic_cm_v0001_20211101)

# Tidy variables # 

# Sex
mcs$sex <- NA
mcs$sex[mcs$cw1_psex == 2 | mcs$cw2_psex == 2 | mcs$cw3_psex == 2] <- "female" # No one changes sex, but may not always appear in all waves
mcs$sex[mcs$cw1_psex == 1 | mcs$cw2_psex == 1 | mcs$cw3_psex == 1] <- "male"

# Age
mcs$age <- 2020 - 2000 # Need to update (all born in this year)

# Ethnicity
mcs$ethnicity <- NA
mcs$ethnicity[mcs$adc11e00 == 1] <- "White"
mcs$ethnicity[mcs$adc11e00 == 3 | mcs$adc11e00 == 4 | mcs$adc11e00 == 5 | mcs$adc11e00 == 6 | mcs$adc11e00 == 10] <- "Asian"
mcs$ethnicity[mcs$adc11e00 == 7 | mcs$adc11e00 == 8 | mcs$adc11e00 == 9] <- "Black"
mcs$ethnicity[mcs$adc11e00 == 2] <- "Mixed"
mcs$ethnicity[mcs$adc11e00 == 11] <- "Other"

# Region
mcs$region[mcs$cw3_region == 1 | (is.na(mcs$cw3_region) & mcs$cw2_region == 1) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 1)] <- "North East" # Take most recent location
mcs$region[mcs$cw3_region == 2 | (is.na(mcs$cw3_region) & mcs$cw2_region == 2) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 2)] <- "North West" 
mcs$region[mcs$cw3_region == 3 | (is.na(mcs$cw3_region) & mcs$cw2_region == 3) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 3)] <- "Yorkshire" 
mcs$region[mcs$cw3_region == 4 | (is.na(mcs$cw3_region) & mcs$cw2_region == 4) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 4)] <- "East Midlands" 
mcs$region[mcs$cw3_region == 5 | (is.na(mcs$cw3_region) & mcs$cw2_region == 5) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 5)] <- "West Midlands" 
mcs$region[mcs$cw3_region == 6 | (is.na(mcs$cw3_region) & mcs$cw2_region == 6) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 6)] <- "East" 
mcs$region[mcs$cw3_region == 7 | (is.na(mcs$cw3_region) & mcs$cw2_region == 7) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 7)] <- "London" 
mcs$region[mcs$cw3_region == 8 | (is.na(mcs$cw3_region) & mcs$cw2_region == 8) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 8)] <- "South East" 
mcs$region[mcs$cw3_region == 9 | (is.na(mcs$cw3_region) & mcs$cw2_region == 9) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 9)] <- "South West"
mcs$region[mcs$cw3_region == 10 | (is.na(mcs$cw3_region) & mcs$cw2_region == 10) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 10)] <- "Wales"
mcs$region[mcs$cw3_region == 12 | (is.na(mcs$cw3_region) & mcs$cw2_region == 12) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 12)] <- "N Ireland" 
mcs$region[mcs$cw3_region == 11 | (is.na(mcs$cw3_region) & mcs$cw2_region == 11) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 11)] <- "Scotland"

# Country
mcs$country <- mcs$region #  Can work out from above?
mcs$country[mcs$country != "Scotland" & mcs$country != "N Ireland" & mcs$country != "Wales"] <- "England"

# Tenure
mcs$tenure <- NA# Create blank variable then recode below
mcs$tenure <- mcs$cw3_tenure # Take most recent tenure
mcs$tenure[is.na(mcs$cw3_tenure)] <- mcs$cw2_tenure[is.na(mcs$cw3_tenure)] # If above is missing take previous one
mcs$tenure[mcs$tenure == 1] <- "Own outright" # Recode values
mcs$tenure[mcs$tenure == 2 | mcs$tenure == 3] <- "Mortgage"
mcs$tenure[mcs$tenure == 4] <- "Rent"
mcs$tenure[mcs$tenure == 5 | mcs$tenure == 6 | mcs$tenure == 7] <- "Other"

# Education
mcs$education <- NA # Create blank variable then recode below

# General health
mcs$general_health <- NA # Create blank variable
mcs$general_health[mcs$cw1_ghq < 4 | mcs$cw2_ghq < 4 | mcs$cw3_ghq < 4] <- "Good" # Only reported their health as good
mcs$general_health[mcs$cw1_ghq >= 4 | mcs$cw2_ghq >= 4 | mcs$cw3_ghq >= 4] <- "Fair/Poor" # Reported their health as fair or poor at any point

# Had COVID-19
mcs$covid <- NA # Create blank variable
mcs$covid[mcs$cw1_covid19 > 2 | mcs$cw2_covid19 > 2 | mcs$cw3_covid19 > 2] <- "No" # Opposite
mcs$covid[mcs$cw1_covid19 < 3 | mcs$cw2_covid19 < 3 | mcs$cw3_covid19 < 3] <- "Yes" # Reported they had COVID-19 in any wave

# Asthma
mcs$asthma <- 0 # Create blank variable - hard to tell who has missing data though due to set up
mcs$asthma[mcs$cw1_lli_3 == 1 | mcs$cw2_lli1_3 == 1 | mcs$cw3_lli1_3 == 1] <- 1 # Has condition

# Cancer
mcs$cancer <- 0 # Create blank variable - hard to tell who has missing data though due to set up
mcs$cancer[mcs$cw1_lli_1 == 1 | mcs$cw2_lli1_1 == 1 | mcs$cw3_lli1_1 == 1] <- 1 # Has condition

# Diabetes
mcs$diabetes <- 0 # Create blank variable - hard to tell who has missing data though due to set up
mcs$diabetes[mcs$cw1_lli_6 == 1 | mcs$cw2_lli1_6 == 1 | mcs$cw3_lli1_6 == 1] <- 1 # Has condition

# Hypertension
mcs$hypertension <- 0 # Create blank variable - hard to tell who has missing data though due to set up
mcs$hypertension[mcs$cw1_lli_9 == 1 | mcs$cw2_lli1_9 == 1 | mcs$cw3_lli1_9 == 1] <- 1 # Has condition

# Healthcare disruption
# Wave 2 is a bit complicated to say the least
# var_lab(mcs$cw1_surgcan_10) #Helpful in looking up variable descriptions
mcs$disruption_appointments <- NA # Blank variable to represent if had any disruption to appointments (e.g., GP appointments) - binary (yes = 1, 0 = no)
mcs$disruption_appointments[mcs$cw1_surgcan == 2 | (mcs$cw2_appmen ==  2 | mcs$cw2_appmenhadh == 1) | mcs$cw2_apphosp_3 == 1 | mcs$cw2_pmeddif == 2 | mcs$cw3_appcand == 2] <- 0 # No disruption
mcs$disruption_appointments[mcs$cw1_surgcan == 1 | (mcs$cw2_appmen == 1 & (mcs$cw2_appmenhad == 2 | mcs$cw2_appmenhadh == 2 | mcs$cw2_apphadth == 2 | mcs$cw2_appnoth > 1)) | (mcs$cw2_apphosp_1 == 1 & (mcs$cw2_apphad == 2 | mcs$cw2_apphadh == 2 | mcs$cw2_apphadt == 2 | mcs$cw2_appnot > 1)) | mcs$cw3_appcant_1 == 1 | mcs$cw3_appcant_3 == 1] <- 1 # Disruption

mcs$disruption_medications <- NA # Blank variable to represent if had any disruption in receiving their medications
mcs$disruption_medications[mcs$cw2_pmeddif == 2 | mcs$cw3_pmeddif == 2] <- 0 # No disruption (first one is no disruption, as 14 is not a yes/no option and only yes)
mcs$disruption_medications[mcs$cw2_pmeddif == 1 | mcs$cw3_pmeddif == 1] <- 1 # Disruption (at some point) - overwrites any 0s from above

mcs$disruption_procedures <- NA # Blank variable for surgery, cancer treatment
mcs$disruption_procedures[mcs$cw1_surgcan == 2 | (mcs$cw2_apphosp_2 == 1 & mcs$cw2_apphadsh == 1) | mcs$cw3_appcand == 2] <- 0 # No disruption
mcs$disruption_procedures[mcs$cw1_surgcan == 1 | (mcs$cw2_apphosp_2 == 1 & (mcs$cw2_apphads == 2 | mcs$cw2_apphadsh == 2 | mcs$cw2_appnots > 1)) | mcs$cw3_appcant_2 == 1] <- 1 

mcs$disruption_any <- NA # If had any of the above experiences (overall variable)
mcs$disruption_any[mcs$disruption_appointments == 0 | mcs$disruption_medications == 0 | mcs$disruption_procedures == 0] <- 0 # Set as OR in case have missing data, since next line will over-write the 1s anyway
mcs$disruption_any[mcs$disruption_appointments == 1 | mcs$disruption_medications == 1 | mcs$disruption_procedures == 1] <- 1


# Generate sample weights
# Take most recent weights
mcs$weight <- mcs$cw3_combwt # Most recent weight
mcs$weight[is.na(mcs$weight)] <- mcs$cw2_combwt[is.na(mcs$weight)] # If missing, then take previous one
mcs$weight[is.na(mcs$weight)] <- mcs$cw1_combwt[is.na(mcs$weight)] # repeat
mcs$weight[is.na(mcs$weight)] <- 1 # If still missing, follow rules elsewhere


mcs$psu <- mcs$cw3_samppsu # Most recent psu
mcs$psu[is.na(mcs$psu)] <- mcs$cw2_samppsu[is.na(mcs$psu)] # If missing, then take previous one
mcs$psu[is.na(mcs$psu)] <- mcs$cw1_samppsu[is.na(mcs$psu)] # repeat
mcs$psu[is.na(mcs$psu)] <- mcs$LLC_0009_stud_id[is.na(mcs$psu)] # If still missing, follow rules elsewhere

mcs$strata <- mcs$cw3_sampstratum # Most recent strata
mcs$strata[is.na(mcs$strata)] <- mcs$cw2_sampstratum[is.na(mcs$strata)] # If missing, then take previous one
mcs$strata[is.na(mcs$strata)] <- mcs$cw1_sampstratum[is.na(mcs$strata)] # repeat
mcs$strata[is.na(mcs$strata)] <- 7 # If still missing, follow rules elsewhere


# Sample weights are missing so replace with the following
mcs$fpc <- 0 # Set as 0

# Subset variables required
mcs$date <- paste0(mcs$cw3_enddated, "/", mcs$cw3_enddatem, "/2021", sep = "") #  Save date as latest wave
# survey weight???
mcs <- mcs[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "asthma", "cancer", "diabetes", "hypertension", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables

# Save #
mcs$cohort <- "mcs"
save(mcs, file = "S:/LLC_0009/data/Cleaned Cohorts/mcs_cm.RData")
rm(mcs)


## Millennium Cohort Study  - parent/guardians ## 

# Wave 1

# Load in data
data_source <- "mcs" # Cohort/survey
table <- "covid_w1_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
mcs <- mcs_covid_w1_v0001_20211101[, c("LLC_0009_stud_id", "cw1_surgcan", "cw1_psex", "cw1_combwt", "cw1_samppsu", "cw1_sampstratum", "cw1_covid19", "cw1_ghq", "cw1_region", "cw1_enddate")] # These are in order - id, disruption experienced (surgery), sex, survey weight,if have had COVID-19, self-rated health status, region, date
rm(mcs_covid_w1_v0001_20211101) 

# Wave 2

# Load in data
data_source <- "mcs" # Cohort/survey
table <- "covid_w2_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
mcs_covid_w2_v0001_20211101 <- mcs_covid_w2_v0001_20211101[, c("LLC_0009_stud_id", "cw2_pmeddif", "cw2_apphosp_1", "cw2_apphosp_2", "cw2_apphosp_3", "cw2_apphad", "cw2_apphadh", "cw2_apphadt", "cw2_appnot", "cw2_apphads", "cw2_apphadsh", "cw2_appnots", "cw2_appmen", "cw2_appmenhad", "cw2_appmenhadh", "cw2_apphadth", "cw2_appnoth", "cw2_psex", "cw2_combwt", "cw2_samppsu", "cw2_sampstratum", "cw2_covid19", "cw2_ghq", "cw2_enddated", "cw2_enddatem", "cw2_region", "cw2_tenure")] # These are id, healthcare disruption (difficulty getting medicines, then cancelled healthcare experiences but this is tricky so see below note for explanation), sex, survey weight, if had covid-19 and self rated health, date of survey (day and month), region, tenure

# Note: cw2_apphosp_1 = did you have a hospital apointment, so select only those and then define disruption as cw2_apphad == No (did not have appointment), or cw2_apphadh == appointment was delayed. BUT also cw2_appnot gives reasons for not having the appointment - might be easier. Need to do this for each variable? (does not apply to medications variable):
# apphad = hospital conslutation, investigation or treatment, apphads = surgery, and appmen = cbt, counselling or psychological therapy

# Join onto main data
mcs <- merge(mcs, mcs_covid_w2_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE)
rm(mcs_covid_w2_v0001_20211101) # Drop to save space

# Wave 3

# Load in data
data_source <- "mcs" # Cohort/survey
table <- "covid_w3_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
mcs_covid_w3_v0001_20211101 <- mcs_covid_w3_v0001_20211101[, c("LLC_0009_stud_id", "cw3_pmeddif", "cw3_appcand", "cw3_appcant_1", "cw3_appcant_2", "cw3_appcant_3", "cw3_appcant_4", "cw3_psex", "cw3_combwt", "cw3_samppsu", "cw3_sampstratum", "cw3_covid19", "cw3_ghq", "cw3_enddated", "cw3_enddatem", "cw3_region", "cw3_tenure")] # These are id, healthcare disruption (difficulty getting medicines, cancelled or delayed medical appointment overall then 4 types), sex, survey weight, if had covid-19 and self rated health, date of survey (day and month), region, tenure

# Join onto main data
mcs <- merge(mcs, mcs_covid_w3_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE)
rm(mcs_covid_w3_v0001_20211101) # Drop to save space

# Socio-demographics

# Load in data
data_source <- "mcs" # Cohort/survey
table <- "mcs7_parent_derived_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
mcs_mcs7_parent_derived_v0001_20211101 <- mcs_mcs7_parent_derived_v0001_20211101[, c("LLC_0009_stud_id", "gdagi00", "gd11e00")] # id, age, ethnicity

# Join onto main data
mcs <- merge(mcs, mcs_mcs7_parent_derived_v0001_20211101, by = "LLC_0009_stud_id", all.x = TRUE)
rm(mcs_mcs7_parent_derived_v0001_20211101)

# Tidy variables # 

# Sex
mcs$sex <- NA
mcs$sex[mcs$cw1_psex == 2 | mcs$cw2_psex == 2 | mcs$cw3_psex == 2] <- "female" # No one changes sex, but may not always appear in all waves
mcs$sex[mcs$cw1_psex == 1 | mcs$cw2_psex == 1 | mcs$cw3_psex == 1] <- "male"

# Age
mcs$age <- mcs$gdagi00 # Take age

# Ethnicity
mcs$ethnicity <- NA
mcs$ethnicity[mcs$gd11e00 == 1] <- "White"
mcs$ethnicity[mcs$gd11e00 == 3 | mcs$gd11e00 == 4 | mcs$gd11e00 == 5 | mcs$gd11e00 == 6 | mcs$gd11e00 == 10] <- "Asian"
mcs$ethnicity[mcs$gd11e00 == 7 | mcs$gd11e00 == 8 | mcs$gd11e00 == 9] <- "Black"
mcs$ethnicity[mcs$gd11e00 == 2] <- "Mixed"
mcs$ethnicity[mcs$gd11e00 == 11] <- "Other"

# Region
mcs$region[mcs$cw3_region == 1 | (is.na(mcs$cw3_region) & mcs$cw2_region == 1) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 1)] <- "North East" # Take most recent location
mcs$region[mcs$cw3_region == 2 | (is.na(mcs$cw3_region) & mcs$cw2_region == 2) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 2)] <- "North West" 
mcs$region[mcs$cw3_region == 3 | (is.na(mcs$cw3_region) & mcs$cw2_region == 3) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 3)] <- "Yorkshire" 
mcs$region[mcs$cw3_region == 4 | (is.na(mcs$cw3_region) & mcs$cw2_region == 4) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 4)] <- "East Midlands" 
mcs$region[mcs$cw3_region == 5 | (is.na(mcs$cw3_region) & mcs$cw2_region == 5) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 5)] <- "West Midlands" 
mcs$region[mcs$cw3_region == 6 | (is.na(mcs$cw3_region) & mcs$cw2_region == 6) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 6)] <- "East" 
mcs$region[mcs$cw3_region == 7 | (is.na(mcs$cw3_region) & mcs$cw2_region == 7) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 7)] <- "London" 
mcs$region[mcs$cw3_region == 8 | (is.na(mcs$cw3_region) & mcs$cw2_region == 8) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 8)] <- "South East" 
mcs$region[mcs$cw3_region == 9 | (is.na(mcs$cw3_region) & mcs$cw2_region == 9) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 9)] <- "South West"
mcs$region[mcs$cw3_region == 10 | (is.na(mcs$cw3_region) & mcs$cw2_region == 10) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 10)] <- "Wales"
mcs$region[mcs$cw3_region == 12 | (is.na(mcs$cw3_region) & mcs$cw2_region == 12) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 12)] <- "N Ireland" 
mcs$region[mcs$cw3_region == 11 | (is.na(mcs$cw3_region) & mcs$cw2_region == 11) | (is.na(mcs$cw3_region) & is.na(mcs$cw2_region) & mcs$cw1_region == 11)] <- "Scotland"

# Country
mcs$country <- mcs$region #  Can work out from above?
mcs$country[mcs$country != "Scotland" & mcs$country != "N Ireland" & mcs$country != "Wales"] <- "England"

# Tenure
mcs$tenure <- NA# Create blank variable then recode below
mcs$tenure <- mcs$cw3_tenure # Take most recent tenure
mcs$tenure[is.na(mcs$cw3_tenure)] <- mcs$cw2_tenure[is.na(mcs$cw3_tenure)] # If above is missing take previous one
mcs$tenure[mcs$tenure == 1] <- "Own outright" # Recode values
mcs$tenure[mcs$tenure == 2 | mcs$tenure == 3] <- "Mortgage"
mcs$tenure[mcs$tenure == 4] <- "Rent"
mcs$tenure[mcs$tenure == 5 | mcs$tenure == 6 | mcs$tenure == 7] <- "Other"


# Education
mcs$education <- NA # Create blank variable then recode below

# General health
mcs$general_health <- NA # Create blank variable
mcs$general_health[mcs$cw1_ghq < 4 | mcs$cw2_ghq < 4 | mcs$cw3_ghq < 4] <- "Good" # Only reported their health as good
mcs$general_health[mcs$cw1_ghq >= 4 | mcs$cw2_ghq >= 4 | mcs$cw3_ghq >= 4] <- "Fair/Poor" # Reported their health as fair or poor at any point

# Had COVID-19
mcs$covid <- NA # Create blank variable
mcs$covid[mcs$cw1_covid19 > 2 | mcs$cw2_covid19 > 2 | mcs$cw3_covid19 > 2] <- "No" # Opposite
mcs$covid[mcs$cw1_covid19 < 3 | mcs$cw2_covid19 < 3 | mcs$cw3_covid19 < 3] <- "Yes" # Reported they had COVID-19 in any wave

# Healthcare disruption
# Wave 2 is a bit complicated to say the least
# var_lab(mcs$cw1_surgcan_10) #Helpful in looking up variable descriptions
mcs$disruption_appointments <- NA # Blank variable to represent if had any disruption to appointments (e.g., GP appointments) - binary (yes = 1, 0 = no)
mcs$disruption_appointments[mcs$cw1_surgcan == 2 | (mcs$cw2_appmen ==  2 | mcs$cw2_appmenhadh == 1) | mcs$cw2_apphosp_3 == 1 | mcs$cw2_pmeddif == 2 | mcs$cw3_appcand == 2] <- 0 # No disruption
mcs$disruption_appointments[mcs$cw1_surgcan == 1 | (mcs$cw2_appmen == 1 & (mcs$cw2_appmenhad == 2 | mcs$cw2_appmenhadh == 2 | mcs$cw2_apphadth == 2 | mcs$cw2_appnoth > 1)) | (mcs$cw2_apphosp_1 == 1 & (mcs$cw2_apphad == 2 | mcs$cw2_apphadh == 2 | mcs$cw2_apphadt == 2 | mcs$cw2_appnot > 1)) | mcs$cw3_appcant_1 == 1 | mcs$cw3_appcant_3 == 1] <- 1 # Disruption

mcs$disruption_medications <- NA # Blank variable to represent if had any disruption in receiving their medications
mcs$disruption_medications[mcs$cw2_pmeddif == 2 | mcs$cw3_pmeddif == 2] <- 0 # No disruption (first one is no disruption, as 14 is not a yes/no option and only yes)
mcs$disruption_medications[mcs$cw2_pmeddif == 1 | mcs$cw3_pmeddif == 1] <- 1 # Disruption (at some point) - overwrites any 0s from above

mcs$disruption_procedures <- NA # Blank variable for surgery, cancer treatment
mcs$disruption_procedures[mcs$cw1_surgcan == 2 | (mcs$cw2_apphosp_2 == 1 & mcs$cw2_apphadsh == 1) | mcs$cw3_appcand == 2] <- 0 # No disruption
mcs$disruption_procedures[mcs$cw1_surgcan == 1 | (mcs$cw2_apphosp_2 == 1 & (mcs$cw2_apphads == 2 | mcs$cw2_apphadsh == 2 | mcs$cw2_appnots > 1)) | mcs$cw3_appcant_2 == 1] <- 1 

mcs$disruption_any <- NA # If had any of the above experiences (overall variable)
mcs$disruption_any[mcs$disruption_appointments == 0 | mcs$disruption_medications == 0 | mcs$disruption_procedures == 0] <- 0 # Set as OR in case have missing data, since next line will over-write the 1s anyway
mcs$disruption_any[mcs$disruption_appointments == 1 | mcs$disruption_medications == 1 | mcs$disruption_procedures == 1] <- 1

# Generate sample weights
# Take most recent weights
mcs$weight <- mcs$cw3_combwt # Most recent weight
mcs$weight[is.na(mcs$weight)] <- mcs$cw2_combwt[is.na(mcs$weight)] # If missing, then take previous one
mcs$weight[is.na(mcs$weight)] <- mcs$cw1_combwt[is.na(mcs$weight)] # repeat
mcs$weight[is.na(mcs$weight)] <- 1 # If still missing, follow rules elsewhere

mcs$psu <- mcs$cw3_samppsu # Most recent psu
mcs$psu[is.na(mcs$psu)] <- mcs$cw2_samppsu[is.na(mcs$psu)] # If missing, then take previous one
mcs$psu[is.na(mcs$psu)] <- mcs$cw1_samppsu[is.na(mcs$psu)] # repeat
mcs$psu[is.na(mcs$psu)] <- mcs$LLC_0009_stud_id[is.na(mcs$psu)] # If still missing, follow rules elsewhere

mcs$strata <- mcs$cw3_sampstratum # Most recent strata
mcs$strata[is.na(mcs$strata)] <- mcs$cw2_sampstratum[is.na(mcs$strata)] # If missing, then take previous one
mcs$strata[is.na(mcs$strata)] <- mcs$cw1_sampstratum[is.na(mcs$strata)] # repeat
mcs$strata[is.na(mcs$strata)] <- 6 # If still missing, follow rules elsewhere

# Sample weights are missing so replace with the following
mcs$fpc <- 0 # Set as 0

# Subset variables required
mcs$date <- paste0(mcs$cw3_enddated, "/", mcs$cw3_enddatem, "/2021", sep = "") #  Save date as latest wave
# survey weight???
mcs <- mcs[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables

# Save #
mcs$cohort <- "mcs"
mcs_pargua <- mcs
save(mcs_pargua, file = "S:/LLC_0009/data/Cleaned Cohorts/mcs_parent_guardian.RData")
rm(mcs, mcs_pargua)


# ## GLAD ##
# 
# # Load in data
# data_source <- "glad" # Cohort/survey
# table <- "file2_v0001_20211101" # Specific wave/dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

# ## Exceed ##
# 
# # Load in data
# data_source <- "exceed" # Cohort/survey
# table <- "covid19survey_v0001_20211101" # Specific wave/dataset
# # table <- "demographiccore_v0001_20211101" # Specific wave/dataset
# # table <- "exceed_selfcompletion_questionnaire_v0001_20211101" # Specific wave/dataset [Needs exceed bit]
# # table <- "occupation_v0001_20211101" # Specific wave/dataset [Needs exceed bit]
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

## English Longitudinal Study of Ageing ##

# Wave 1 #

# Load in data
data_source <- "elsa" # Cohort/survey
table <- "elsa_covid_w1_eul_v0001_20211101" # Specific wave/dataset [needs extra elsa]
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
elsa <- elsa_elsa_covid_w1_eul_v0001_20211101[, c("LLC_0009_stud_id", "cintdatd", "cintdatm", "cintdaty", "cov19lwgt", "age_arch", "sex", "ethnicity_arch", "rgn_arch", "tenure_arch", "w9edqual", "cvheself", "cvhehosp", "cvhegpb", "cvhemed", "cvcomm", "cvpred", "cvtestb", "cvhecond07", "cvhecond09", "cvhecond11", "cvhecond04", "cvhecond01")] # These are in order - id, interview date (day, month then year), sample weight, age, sex, ethnicity, region, education, self-rated health, healthcare disruption (4 vars - hospital visit cancelled, able to see GP, medication, community services), economic activity, tested for COVID-19, health conditions (asthma, cancer x2, diabetes, hypertension)
rm(elsa_elsa_covid_w1_eul_v0001_20211101) 

# Wave 2 #

# Load in data
data_source <- "elsa" # Cohort/survey
table <- "elsa_covid_w2_eul_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
elsa_elsa_covid_w2_eul_v0001_20211101 <- elsa_elsa_covid_w2_eul_v0001_20211101[, c("LLC_0009_stud_id", "cintdatd", "cintdatm", "cintdaty", "cov19lwgtw2", "age_arch", "sex", "ethnicity_arch", "rgn_arch", "tenure", "cvheself", "cvhehosp", "cvcomm", "cvcommb_final001", "cvcommb_final002", "cvcommb_final003", "cvcommb_final004", "cvcommb_final005", "cvcommb_final006", "cvcommb_final007", "cvcommb_final012", "cvcommb_final013", "cvcommb_final980", "cvcommb_final990", "cvcommb_final995", "cvcommb_final996", "cvcommb_final997", "cvcommb_final999", "cvcommb_final001", "cvpred", "cvtestb", "cvhecond_final007", "cvhecond_final009", "cvhecond_final011", "cvhecond_final004", "cvhecond_final001")] # These are in order - id, interview date (day, month then year), sample weight, age, sex, ethnicity, region, self-rated health, healthcare disruption (hospital visit cancelled, unable to access any service (overall measure), then 15 measures by type what a faff and I only know what abpout half are atm), economic activity, tested for COVID-19, health conditions (asthma, cancer x2, diabetes, hypertension)

# Join onto main data
elsa <- merge(elsa, elsa_elsa_covid_w2_eul_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE)
rm(elsa_elsa_covid_w2_eul_v0001_20211101)

# Tidy variables #

# Sex
elsa$sex <- NA
elsa$sex[elsa$sex.x == 1 | elsa$sex.y == 1] <- "female"
elsa$sex[elsa$sex.x == 2 | elsa$sex.y == 2] <- "male"

# Age
elsa$age <- elsa$age_arch.y # Age at survey
elsa$age[is.na(elsa$age)] <- elsa$age_arch.x[is.na(elsa$age)] # If missing

# Ethnicity
elsa$ethnicity <- NA # Create blank variable
elsa$ethnicity[elsa$ethnicity_arch.x == 1 | elsa$ethnicity_arch.y == 1] <- "White"
elsa$ethnicity[elsa$ethnicity_arch.x == 2 | elsa$ethnicity_arch.y == 2] <- "Non-White"

# Region
elsa$region <- NA
elsa$region[elsa$rgn_arch.y == 1 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 1)] <- "North East" # Take most recent location
elsa$region[elsa$rgn_arch.y == 2 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 2)] <- "North West" 
elsa$region[elsa$rgn_arch.y == 3 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 3)] <- "Yorkshire" 
elsa$region[elsa$rgn_arch.y == 4 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 4)] <- "East Midlands" 
elsa$region[elsa$rgn_arch.y == 5 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 5)] <- "West Midlands" 
elsa$region[elsa$rgn_arch.y == 6 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 6)] <- "East" 
elsa$region[elsa$rgn_arch.y == 7 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 7)] <- "London" 
elsa$region[elsa$rgn_arch.y == 8 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 8)] <- "South East" 
elsa$region[elsa$rgn_arch.y == 9 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 9)] <- "South West"
elsa$region[elsa$rgn_arch.y == 11 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 11)] <- "Wales"
elsa$region[elsa$rgn_arch.y == 12 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 12)] <- "N Ireland" 
elsa$region[elsa$rgn_arch.y == 10 | (is.na(elsa$rgn_arch.y) & elsa$rgn_arch.x == 10)] <- "Scotland"

# Country
elsa$country <- elsa$region #  Can work out from above?
elsa$country[elsa$country != "Scotland" & elsa$country != "N Ireland" & elsa$country != "Wales"] <- "England"

# Tenure #  need to check this is correct
elsa$tenure <- NA # Create blank variable then recode below
elsa$tenure[elsa$tenure_arch == 1] <- "Own outright"
elsa$tenure[elsa$tenure_arch == 2] <- "Mortgage"
elsa$tenure[elsa$tenure_arch == 4] <- "Rent"
elsa$tenure[elsa$tenure_arch == 3 | elsa$tenure_arch == 5 | elsa$tenure_arch == 995] <- "Other"

# Education
elsa$education <- NA # Create blank variable then recode below
elsa$education[elsa$elsa_highest_qual == 7] <- "No qualifications"
elsa$education[elsa$elsa_highest_qual == 4 | elsa$elsa_highest_qual == 5] <- "Secondary" # GCSE or equivalent
elsa$education[elsa$elsa_highest_qual == 2 | elsa$elsa_highest_qual == 3] <- "Tertiary" # A-level or equivalent
elsa$education[elsa$elsa_highest_qual == 1] <- "Degree or above"
elsa$education[elsa$elsa_highest_qual == 6] <- "Other"

# General health
elsa$general_health <- NA # Create blank variable
elsa$general_health[elsa$cvheself.x >= 4 | elsa$cvheself.y >= 4] <- "Fair/Poor" # Reported their health as fair or poor at any point
elsa$general_health[elsa$cvheself.x < 4 & elsa$cvheself.y < 4] <- "Good" # Only reported their health as good
elsa$general_health[elsa$cvheself.x <= -1 & elsa$cvheself.y <= -1] <- NA # Missing

# Had COVID-19
elsa$covid <- NA
elsa$covid[elsa$cvtestb.x >= -1 | elsa$cvtestb.y >= -1] <- 0
elsa$covid[elsa$cvtestb.x == 1 | elsa$cvtestb.y == 1] <- 1

# Asthma
elsa$asthma <- NA # Create blank variable
elsa$asthma[elsa$cvhecond07 == 0 | elsa$cvhecond_final007 == 0] <- 0 # Does not have the condition
elsa$asthma[elsa$cvhecond07 == 1 | elsa$cvhecond_final007 == 1] <- 1 # Has reported that they have the condition

# Cancer
elsa$cancer <- NA # Create blank variable
elsa$cancer[elsa$cvhecond09 == 0 | elsa$cvhecond_final009 == 0 | elsa$cvhecond11 == 0 | elsa$cvhecond_final011 == 0] <- 0 # Does not have the condition
elsa$cancer[elsa$cvhecond09 == 1 | elsa$cvhecond_final009 == 1 | elsa$cvhecond11 == 1 | elsa$cvhecond_final011 == 1] <- 1 # Has reported that they have the condition

# Diabetes
elsa$diabetes <- NA # Create blank variable
elsa$diabetes[elsa$cvhecond04 == 0 | elsa$cvhecond_final004 == 0] <- 0 # Does not have the condition
elsa$diabetes[elsa$cvhecond04 == 1 | elsa$cvhecond_final004 == 1] <- 1 # Has reported that they have the condition

# Hypertension
elsa$hypertension <- NA # Create blank variable
elsa$hypertension[elsa$cvhecond01 == 0 | elsa$cvhecond_final001 == 0] <- 0 # Does not have the condition
elsa$hypertension[elsa$cvhecond01 == 1 | elsa$cvhecond_final001 == 1] <- 1 # Has reported that they have the condition


# Healthcare disruption
elsa$disruption_appointments <- NA 
elsa$disruption_appointments[elsa$cvcomm.x >= 1 | elsa$cvcomm.y >= 1 | elsa$cvhegpb >= 1 | (elsa$cvcommb_final001 < 1 | elsa$cvcommb_final002 < 1 | elsa$cvcommb_final003 < 1 | elsa$cvcommb_final004 < 1 | elsa$cvcommb_final005 < 1 | elsa$cvcommb_final006 < 1 | elsa$cvcommb_final007 < 1 | elsa$cvcommb_final012 < 1 | elsa$cvcommb_final013 < 1)] <- 0
elsa$disruption_appointments[elsa$cvcomm.x == 2 | elsa$cvcomm.y == 2 | elsa$cvhegpb == 2 | (elsa$cvcommb_final001 == 1 | elsa$cvcommb_final002 == 1 | elsa$cvcommb_final003 == 1 | elsa$cvcommb_final004 == 1 | elsa$cvcommb_final005 == 1 | elsa$cvcommb_final006 == 1 | elsa$cvcommb_final007 == 1 | elsa$cvcommb_final012 == 1 | elsa$cvcommb_final013 == 1)] <- 1 # Did not access GP or community care (excluding people who did not try to access) - note will overwrite the 0s with 1s where relevant

elsa$disruption_procedures <- NA
elsa$disruption_procedures[elsa$cvhehosp.x >= 2 | elsa$cvhehosp.y >= 2] <- 0
elsa$disruption_procedures[elsa$cvhehosp.x == 1 | elsa$cvhehosp.y == 1] <- 1 # Cancelled hospital operation or treatment

elsa$disruption_medications <- NA
elsa$disruption_medications[elsa$cvhemed == 1 | elsa$cvhemed == 3] <- 0
elsa$disruption_medications[elsa$cvhemed == 2] <- 1 # could not access medications

elsa$disruption_any <- NA # If had any of the above experiences (overall variable)
elsa$disruption_any[elsa$disruption_appointments == 0 | elsa$disruption_medications == 0 | elsa$disruption_procedures == 0] <- 0 # Set as OR in case have missing data, since next line will over-write the 1s anyway
elsa$disruption_any[elsa$disruption_appointments == 1 | elsa$disruption_medications == 1 | elsa$disruption_procedures == 1] <- 1

# Generate sample weights
# Take most recent weight
elsa$weight <- elsa$cov19lwgtw2 # Most recent
elsa$weight[is.na(elsa$weight)] <- elsa$cov19lwgt[is.na(elsa$weight)] # If missing, then take previous one
elsa$weight[is.na(elsa$weight)] <- 1 # If still missing, give 1
# Sample weights are missing so replace with the following
elsa$psu <- elsa$LLC_0009_stud_id # Unique to each individual
elsa$strata <- 3 # Constant across cohort
elsa$fpc <- 0 # Set as 0

# Date
elsa$date <- paste0(elsa$cintdatd.y, "/", elsa$cintdatm.y, "/", elsa$cintdaty.y, sep = "") #  Save date as latest wave
elsa$date[elsa$date == "NA/NA/NA"] <- NA # Recode as missing

# Subset variables required
elsa <- elsa[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "asthma", "cancer", "diabetes", "hypertension", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables

# Save #
elsa$cohort <- "elsa"
save(elsa, file = "S:/LLC_0009/data/Cleaned Cohorts/elsa.RData")
rm(elsa)




## Born in Bradford ##

# COVID-19 wave 1 #

# Load
data_source <- "bib" # Cohort/survey
table <- "cv_w1_v0001_20220118" # Specific wave/dataset 
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
bib <- bib_cv_w1_v0001_20220118[, c("LLC_0009_stud_id", "as_acc_gp", "as_acc_111", "as_acc_ae", "as_acc_spec", "as_acc_mh", "as_acc_mdwife", "as_acc_hvsn", "as_acc_pharm", "as_acc_othr", "region", "country", "fh_genhealth", "ethnicity")] # These are in order - id, disruption experienced (9 vars - access to GP/nurse, NHS 111, A&E, specialist, mental health, midwife, health visitor, pharmacy, other/charity), self-rated health status, ethnicity
# There does not seem to be anything for whether had COVID-19 - the closest is fh_slfisol_nw_why3 (and past variant) which is if currnently isolating due to symptoms (3 is someone in the household, 2 is personally)
rm(bib_cv_w1_v0001_20220118) 

# COVID-19 wave 2 #

# Has no healthcare disruption variables in it so leave out

# # Load
# data_source <- "bib" # Cohort/survey
# table <- "cv_w2_v0001_20220302" # Specific wave/dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy
# 
# # Subset required variables
# bib_cv_w2_v0001_20220302 <- bib_cv_w2_v0001_20220302[, c("LLC_0009_stud_id", "c19a2_cv_wt_expsymp", "gender", "ethnicity_father", "ethnicity_mother_bib", "age_in_years", "fh_genhealth_c19a2")] # These are in order - id, has had COVID-19, gender, ethnicity of parents, age, economic activity, general health

# # Join onto main data
# bib <- merge(bib, bib_cv_w2_v0001_20220302, by = "LLC_0009_stud_id", all = TRUE)
# rm(bib_cv_w2_v0001_20220302)


# Sociodemographics #

# Load
data_source <- "bib" # Cohort/survey
table <- "sociodemo_v0001_20220118" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
bib_sociodemo_v0001_20220118 <- bib_sociodemo_v0001_20220118[, c("LLC_0009_stud_id", "bib_age_y", "bib_cv1_age_y", "bib_sex", "bib_tenure", "bib_highest_qual", "bib_eth16cat")] # These are in order - id, sex, age, tenure, education, ethnicity

# Join onto main data
bib <- merge(bib, bib_sociodemo_v0001_20220118, by = "LLC_0009_stud_id", all.x = TRUE)
rm(bib_sociodemo_v0001_20220118)

# Tidy up

# Sex
bib$sex <- NA
bib$sex[bib$bib_sex == 1] <- "female"
bib$sex[bib$bib_sex == 0] <- "male"

# Age
bib$age <- bib$bib_cv1_age_y # Age at survey
bib$age <- bib$bib_age_y + 12 # Age at baseline plus difference in years (needs updating as born between 2007-2010)

# Ethnicity
bib$ethnicity <- NA # Create blank variable
bib$ethnicity[bib$bib_eth16cat == 1 | bib$bib_eth16cat == 2 | bib$bib_eth16cat == 3] <- "White"
bib$ethnicity[bib$bib_eth16cat == 8 | bib$bib_eth16cat == 9 | bib$bib_eth16cat == 10 | bib$bib_eth16cat == 11 | bib$bib_eth16cat == 12] <- "Asian"
bib$ethnicity[bib$bib_eth16cat == 13 | bib$bib_eth16cat == 14 | bib$bib_eth16cat == 15] <- "Black"
bib$ethnicity[bib$bib_eth16cat == 4 | bib$bib_eth16cat == 5 | bib$bib_eth16cat == 6 | bib$bib_eth16cat == 7] <- "Mixed"
bib$ethnicity[bib$bib_eth16cat == 16] <- "Other"

# Region
# Already exists
bib$region[bib$region == "FAILED"] <- NA

# Country
bib$country <- "England"

# Tenure
bib$tenure <- NA# Create blank variable then recode below
bib$tenure[bib$bib_tenure == 1] <- "Own outright"
bib$tenure[bib$bib_tenure == 2] <- "Mortgage"
bib$tenure[bib$bib_tenure == 4] <- "Rent - private"
bib$tenure[bib$bib_tenure == 5] <- "Rent - social"
bib$tenure[bib$bib_tenure == 3 | bib$bib_tenure == 6 | bib$bib_tenure == 7] <- "Other"

# Education
bib$education <- NA # Create blank variable then recode below
bib$education[bib$bib_highest_qual == 0] <- "No qualifications"
bib$education[bib$bib_highest_qual == 1 | bib$bib_highest_qual == 2] <- "Secondary" # GCSE or equivalent
bib$education[bib$bib_highest_qual == 3] <- "Tertiary" # A-level or equivalent
bib$education[bib$bib_highest_qual == 4 | bib$bib_highest_qual == 5] <- "Degree or above"
bib$education[bib$bib_highest_qual >= 5] <- "Other"
bib$education[bib$bib_age_y == 0] <- NA # I think this defaults to parents so remove

# General health
bib$general_health <- NA # Create blank variable
bib$general_health[bib$fh_genhealth >= 4] <- "Fair/Poor" # Reported their health as fair or poor at any point
bib$general_health[bib$fh_genhealth < 4] <- "Good" # Only reported their health as good
bib$general_health[bib$fh_genhealth <= -1] <- NA # Missing

# Had COVID-19
# This variable only appears in later waves
bib$covid <- NA

# Healthcare disruption
bib$disruption_appointments <- NA # Unable to do by type
bib$disruption_procedures <- NA
bib$disruption_medications <- NA

bib$disruption_any <- 0
bib$disruption_any[bib$as_acc_gp == 1 | bib$as_acc_111 == 1 | bib$as_acc_ae == 1 | bib$as_acc_spec == 1 | bib$as_acc_mh == 1 | bib$as_acc_mdwife == 1 | bib$as_acc_hvsn == 1 | bib$as_acc_pharm == 1 | bib$as_acc_othr == 1] <- 1

# Generate sample weights
# Sample weights are missing so replace with the following
bib$weight <- 1 # Set to one
bib$psu <- bib$LLC_0009_stud_id # Unique to each individual
bib$strata <- 1 # Constant across cohort
bib$fpc <- 0 # Set as 0

# Date
bib$date <- "01/07/2020" # Data collection ends in June 2020 so create conservative estimate here

# Subset variables required
bib <- bib[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables

# Save #
bib$cohort <- "bib"
save(bib, file = "S:/LLC_0009/data/Cleaned Cohorts/bib.RData")
rm(bib)



## 1970 British Cohort Study ##


# Wave 1 #

# Load in data
data_source <- "bcs70" # Cohort/survey
table <- "covid_w1_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
bcs70 <- bcs70_covid_w1_v0001_20211101[, c("LLC_0009_stud_id", "cw1_surgcan", "cw1_psex", "cw1_combwt", "cw1_samppsu", "cw1_sampstratum", "cw1_econactivityb", "cw1_econactivityd", "cw1_covid19", "cw1_ghq", "cw1_region", "cw1_nssec2010an", "cw1_enddate", "cw1_lli_3", "cw1_lli_1", "cw1_lli_6", "cw1_lli_9")] # These are in order - id, disruption experienced (surgery), sex, survey weight, economic activity (2x), if have had COVID-19, self-rated health status, region, NS-SeC, date, health conditions
rm(bcs70_covid_w1_v0001_20211101) 

# Wave 2 #

# Load in data
data_source <- "bcs70" # Cohort/survey
table <- "covid_w2_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
bcs70_covid_w2_v0001_20211101 <- bcs70_covid_w2_v0001_20211101[, c("LLC_0009_stud_id", "cw2_pmeddif", "cw2_apphosp_1", "cw2_apphosp_2", "cw2_apphosp_3", "cw2_apphad", "cw2_apphadh", "cw2_apphadt", "cw2_appnot", "cw2_apphads", "cw2_apphadsh", "cw2_appnots", "cw2_appmen", "cw2_appmenhad", "cw2_appmenhadh", "cw2_apphadth", "cw2_appnoth", "cw2_psex", "cw2_combwt", "cw2_samppsu", "cw2_sampstratum", "cw2_econactivityb", "cw2_econactivityd", "cw2_peconactivityb", "cw2_peconactivityd", "cw2_covid19", "cw2_ghq", "cw2_enddated", "cw2_enddatem", "cw2_region", "cw2_nssec2010an", "cw2_lli1_3", "cw2_lli1_1", "cw2_lli1_6", "cw2_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, then cancelled healthcare experiences but this is tricky so see below note for explanation), sex, survey weight, economic activity (current and previous; two measures for each), if had covid-19 and self rated health, date of survey (day and month), region, NS-SeC, LLTI

# Note: cw2_apphosp_1 = did you have a hospital apointment, so select only those and then define disruption as cw2_apphad == No (did not have appointment), or cw2_apphadh == appointment was delayed. BUT also cw2_appnot gives reasons for not having the appointment - might be easier. Need to do this for each variable? (does not apply to medications variable):
# apphad = hospital conslutation, investigation or treatment, apphads = surgery, and appmen = cbt, counselling or psychological therapy


# Join together (note not same people are in each wave)
bcs70 <- merge(bcs70, bcs70_covid_w2_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE) # Join files
rm(bcs70_covid_w2_v0001_20211101) # Drop to save space


# Wave 3 #

# Load in data
data_source <- "bcs70" # Cohort/survey
table <- "covid_w3_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
bcs70_covid_w3_v0001_20211101 <- bcs70_covid_w3_v0001_20211101[, c("LLC_0009_stud_id", "cw3_pmeddif", "cw3_appcand", "cw3_appcant_1", "cw3_appcant_2", "cw3_appcant_3", "cw3_appcant_4", "cw3_psex", "cw3_combwt", "cw3_samppsu", "cw3_sampstratum", "cw3_econactivityd", "cw3_econactivityb2", "cw3_peconactivityd", "cw3_peconactivityb2", "cw3_covid19", "cw3_ghq", "cw3_enddated", "cw3_enddatem", "cw3_region", "cw3_nssec2010an", "cw3_lli1_3", "cw3_lli1_1", "cw3_lli1_6", "cw3_lli1_9")] # These are id, healthcare disruption (difficulty getting medicines, cancelled or delayed medical appointment overall then 4 types), sex, survey weight, economic activity (current and previous), if had covid-19 and self rated health, date of survey (day and month)


# Join onto main data
bcs70 <- merge(bcs70, bcs70_covid_w3_v0001_20211101, by = "LLC_0009_stud_id", all = TRUE)
rm(bcs70_covid_w3_v0001_20211101) # Drop to save space


# Demographics #

# Load in data
data_source <- "bcs70" # Cohort/survey
table <- "basic_demographic_v0001_20211101" # Specific wave/dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
bcs70_basic_demographic_v0001_20211101 <- bcs70_basic_demographic_v0001_20211101[, c("LLC_0009_stud_id", "ethnic", "bd10tenure", "bd10hnvq", "bd10ecact", "bd10cns8")] # id, 


# Join onto main data
bcs70 <- merge(bcs70, bcs70_basic_demographic_v0001_20211101, by = "LLC_0009_stud_id", all.x = TRUE)
rm(bcs70_basic_demographic_v0001_20211101)


# Tidy variables # 

# Sex
bcs70$sex <- NA
bcs70$sex[bcs70$cw1_psex == 2 | bcs70$cw2_psex == 2 | bcs70$cw3_psex == 2] <- "female" # No one changes sex, but may not always appear in all waves
bcs70$sex[bcs70$cw1_psex == 1 | bcs70$cw2_psex == 1 | bcs70$cw3_psex == 1] <- "male"

# Age
bcs70$age <- 2020 - 1970 # Need to update (all born in this year in one week in March 1958)

# Ethnicity
bcs70$ethnicity <- NA # Create blank variable
bcs70$ethnicity[bcs70$ethnic == 1 | bcs70$ethnic == 2 | bcs70$ethnic == 3] <- "White"
bcs70$ethnicity[bcs70$ethnic == 8 | bcs70$ethnic == 9 | bcs70$ethnic == 10 | bcs70$ethnic == 11] <- "Asian"
bcs70$ethnicity[bcs70$ethnic == 12 | bcs70$ethnic == 13 | bcs70$ethnic == 14 | bcs70$ethnic == 15] <- "Black"
bcs70$ethnicity[bcs70$ethnic == 4 | bcs70$ethnic == 5 | bcs70$ethnic == 6 | bcs70$ethnic == 7] <- "Mixed"
bcs70$ethnicity[bcs70$ethnic == 16] <- "Other"

# Region
bcs70$region <- NA
bcs70$region[bcs70$cw3_region == 1 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 1) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 1)] <- "North East" # Take most recent location
bcs70$region[bcs70$cw3_region == 2 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 2) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 2)] <- "North West" 
bcs70$region[bcs70$cw3_region == 3 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 3) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 3)] <- "Yorkshire" 
bcs70$region[bcs70$cw3_region == 4 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 4) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 4)] <- "East Midlands" 
bcs70$region[bcs70$cw3_region == 5 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 5) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 5)] <- "West Midlands" 
bcs70$region[bcs70$cw3_region == 6 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 6) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 6)] <- "East" 
bcs70$region[bcs70$cw3_region == 7 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 7) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 7)] <- "London" 
bcs70$region[bcs70$cw3_region == 8 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 8) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 8)] <- "South East" 
bcs70$region[bcs70$cw3_region == 9 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 9) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 9)] <- "South West"
bcs70$region[bcs70$cw3_region == 10 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 10) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 10)] <- "Wales"
bcs70$region[bcs70$cw3_region == 12 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 12) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 12)] <- "N Ireland" 
bcs70$region[bcs70$cw3_region == 11 | (is.na(bcs70$cw3_region) & bcs70$cw2_region == 11) | (is.na(bcs70$cw3_region) & is.na(bcs70$cw2_region) & bcs70$cw1_region == 11)] <- "Scotland"

# Country
bcs70$country <- bcs70$region #  Can work out from above?
bcs70$country[bcs70$country != "Scotland" & bcs70$country != "N Ireland" & bcs70$country != "Wales"] <- "England"

# Tenure
bcs70$tenure <- NA# Create blank variable then recode below
bcs70$tenure[bcs70$bd10tenure == 1] <- "Own outright"
bcs70$tenure[bcs70$bd10tenure == 2 | bcs70$bd10tenure == 3] <- "Mortgage"
bcs70$tenure[bcs70$bd10tenure == 4] <- "Rent"
bcs70$tenure[bcs70$bd10tenure == 5 | bcs70$bd10tenure == 6 | bcs70$bd10tenure == 7] <- "Other"

# Education
bcs70$education <- NA # Create blank variable then recode below
bcs70$education[bcs70$bd10hnvq == 0] <- "No qualifications"
bcs70$education[bcs70$bd10hnvq == 1 | bcs70$bd10hnvq == 2] <- "Secondary" # GCSE or equivalent
bcs70$education[bcs70$bd10hnvq == 3] <- "Tertiary" # A-level or equivalent
bcs70$education[bcs70$bd10hnvq == 4 | bcs70$bd10hnvq == 5] <- "Degree or above"

# General health
bcs70$general_health <- NA # Create blank variable
bcs70$general_health[bcs70$cw1_ghq < 4 | bcs70$cw2_ghq < 4 | bcs70$cw3_ghq < 4] <- "Good" # Only reported their health as good
bcs70$general_health[bcs70$cw1_ghq >= 4 | bcs70$cw2_ghq >= 4 | bcs70$cw3_ghq >= 4] <- "Fair/Poor" # Reported their health as fair or poor at any point

# Had COVID-19
bcs70$covid <- NA # Create blank variable
bcs70$covid[bcs70$cw1_covid19 > 2 | bcs70$cw2_covid19 > 2 | bcs70$cw3_covid19 > 2] <- "No" # Opposite
bcs70$covid[bcs70$cw1_covid19 < 3 | bcs70$cw2_covid19 < 3 | bcs70$cw3_covid19 < 3] <- "Yes" # Reported they had COVID-19 in any wave

# Asthma
bcs70$asthma <- 0 # Create blank variable - hard to tell who has missing data though due to set up
bcs70$asthma[bcs70$cw1_lli_3 == 1 | bcs70$cw2_lli1_3 == 1 | bcs70$cw3_lli1_3 == 1] <- 1 # Has condition

# Cancer
bcs70$cancer <- 0 # Create blank variable - hard to tell who has missing data though due to set up
bcs70$cancer[bcs70$cw1_lli_1 == 1 | bcs70$cw2_lli1_1 == 1 | bcs70$cw3_lli1_1 == 1] <- 1 # Has condition

# Diabetes
bcs70$diabetes <- 0 # Create blank variable - hard to tell who has missing data though due to set up
bcs70$diabetes[bcs70$cw1_lli_6 == 1 | bcs70$cw2_lli1_6 == 1 | bcs70$cw3_lli1_6 == 1] <- 1 # Has condition

# Hypertension
bcs70$hypertension <- 0 # Create blank variable - hard to tell who has missing data though due to set up
bcs70$hypertension[bcs70$cw1_lli_9 == 1 | bcs70$cw2_lli1_9 == 1 | bcs70$cw3_lli1_9 == 1] <- 1 # Has condition

# Healthcare disruption
# Wave 2 is a bit complicated to say the least
# var_lab(bcs70$cw1_surgcan_10) #Helpful in looking up variable descriptions
bcs70$disruption_appointments <- NA # Blank variable to represent if had any disruption to appointments (e.g., GP appointments) - binary (yes = 1, 0 = no)
bcs70$disruption_appointments[bcs70$cw1_surgcan == 2 | (bcs70$cw2_appmen ==  2 | bcs70$cw2_appmenhadh == 1) | bcs70$cw2_apphosp_3 == 1 | bcs70$cw2_pmeddif == 2 | bcs70$cw3_appcand == 2] <- 0 # No disruption
bcs70$disruption_appointments[bcs70$cw1_surgcan == 1 | (bcs70$cw2_appmen == 1 & (bcs70$cw2_appmenhad == 2 | bcs70$cw2_appmenhadh == 2 | bcs70$cw2_apphadth == 2 | bcs70$cw2_appnoth > 1)) | (bcs70$cw2_apphosp_1 == 1 & (bcs70$cw2_apphad == 2 | bcs70$cw2_apphadh == 2 | bcs70$cw2_apphadt == 2 | bcs70$cw2_appnot > 1)) | bcs70$cw3_appcant_1 == 1 | bcs70$cw3_appcant_3 == 1] <- 1 # Disruption

bcs70$disruption_medications <- NA # Blank variable to represent if had any disruption in receiving their medications
bcs70$disruption_medications[bcs70$cw2_pmeddif == 2 | bcs70$cw3_pmeddif == 2] <- 0 # No disruption (first one is no disruption, as 14 is not a yes/no option and only yes)
bcs70$disruption_medications[bcs70$cw2_pmeddif == 1 | bcs70$cw3_pmeddif == 1] <- 1 # Disruption (at some point) - overwrites any 0s from above

bcs70$disruption_procedures <- NA # Blank variable for surgery, cancer treatment
bcs70$disruption_procedures[bcs70$cw1_surgcan == 2 | (bcs70$cw2_apphosp_2 == 1 & bcs70$cw2_apphadsh == 1) | bcs70$cw3_appcand == 2] <- 0 # No disruption
bcs70$disruption_procedures[bcs70$cw1_surgcan == 1 | (bcs70$cw2_apphosp_2 == 1 & (bcs70$cw2_apphads == 2 | bcs70$cw2_apphadsh == 2 | bcs70$cw2_appnots > 1)) | bcs70$cw3_appcant_2 == 1] <- 1 

bcs70$disruption_any <- NA # If had any of the above experiences (overall variable)
bcs70$disruption_any[bcs70$disruption_appointments == 0 | bcs70$disruption_medications == 0 | bcs70$disruption_procedures == 0] <- 0 # Set as OR in case have missing data, since next line will over-write the 1s anyway
bcs70$disruption_any[bcs70$disruption_appointments == 1 | bcs70$disruption_medications == 1 | bcs70$disruption_procedures == 1] <- 1

# Generate sample weights
# Take most recent weights
bcs70$weight <- bcs70$cw3_combwt # Most recent weight
bcs70$weight[is.na(bcs70$weight)] <- bcs70$cw2_combwt[is.na(bcs70$weight)] # If missing, then take previous one
bcs70$weight[is.na(bcs70$weight)] <- bcs70$cw1_combwt[is.na(bcs70$weight)] # repeat
bcs70$weight[is.na(bcs70$weight)] <- 1 # If still missing, give 1

bcs70$psu <- bcs70$cw3_samppsu # Most recent psu
bcs70$psu[is.na(bcs70$psu)] <- bcs70$cw2_samppsu[is.na(bcs70$psu)] # If missing, then take previous one
bcs70$psu[is.na(bcs70$psu)] <- bcs70$cw1_samppsu[is.na(bcs70$psu)] # repeat
bcs70$psu[is.na(bcs70$psu)] <- bcs70$LLC_0009_stud_id[is.na(bcs70$psu)] # If still missing, follow rules elsewhere

bcs70$strata <- bcs70$cw3_sampstratum # Most recent strata
bcs70$strata[is.na(bcs70$strata)] <- bcs70$cw2_sampstratum[is.na(bcs70$strata)] # If missing, then take previous one
bcs70$strata[is.na(bcs70$strata)] <- bcs70$cw1_sampstratum[is.na(bcs70$strata)] # repeat
bcs70$strata[is.na(bcs70$strata)] <- 4 # If still missing, follow rules elsewhere

# Sample weights are missing so replace with the following
bcs70$fpc <- 0 # Set as 0


# Subset variables required
bcs70$date <- paste0(bcs70$cw3_enddated, "/", bcs70$cw3_enddatem, "/2021", sep = "") #  Save date as latest wave
# survey weight???
bcs70 <- bcs70[, c("LLC_0009_stud_id", "date", "sex", "age", "ethnicity", "region", "country", "tenure", "education", "general_health", "covid", "asthma", "cancer", "diabetes", "hypertension", "disruption_appointments", "disruption_medications", "disruption_procedures", "disruption_any", "weight", "psu", "strata", "fpc")] # Subset variables


# Save #
bcs70$cohort <- "bcs70"
save(bcs70, file = "S:/LLC_0009/data/Cleaned Cohorts/bcs70.RData")
rm(bcs70)
gc()
