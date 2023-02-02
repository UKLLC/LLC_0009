#########################################
### Clean Hospital Episode Statistics ###
#########################################

# Note: HES data are fixed to 31st December 2021 - need to subset for same dates in mortality records


## Vaccination records ##

# Load in data
data_source <- "nhsd" # Linked hospital data name
table <- "cvs_v0001" # Specific dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# # Select only relevant dates
# nhsd_cvs_v0001 <- nhsd_cvs_v0001[nhsd_cvs_v0001$event_received_ts < '2022-01-01 00:00:00',]

# Subset required variables
nhsd_cvs_v0001 <- nhsd_cvs_v0001[, c("llc_0009_stud_id", "vaccination_procedure_code")]

# Define dose number
# SNOMED codes for vaccine doses are 1st dose = 1324681000000101, 2nd dose = 1324691000000104, 3rd/4th dose = 1362591000000103
nhsd_cvs_v0001$dose <- 1 # Set all as first dose then update below
nhsd_cvs_v0001$dose[nhsd_cvs_v0001$vaccination_procedure_code == 1324691000000104] <- 2 # 2nd dose
nhsd_cvs_v0001$dose[nhsd_cvs_v0001$vaccination_procedure_code == 1362591000000103] <- 3 #  3rd dose

# Aggregate by number of doses person has had
nhsd_cvs_v0001 <- data.table(nhsd_cvs_v0001) # Convert type for next step
vax <- nhsd_cvs_v0001[, list(vax_doses = max(dose)), by = "llc_0009_stud_id"] # Aggregate to largest number of doses per individual

# Save
save(vax, file = "S:/LLC_0009/data/Cleaned Cohorts/nhs_vaccinated.RData")
rm(vax, nhsd_cvs_v0001)


## HES - Accident & Emergency attendences ##

# # Load in data
# data_source <- "nhsd" # Linked hospital data name
# table <- "hesae_otr_v0001" # Specific dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

# Note: This has 0 rows. I also do not know what OTR means


## HES - HES - Accident & Emergency attendences ##

# # Load in data
# data_source <- "nhsd" # Linked hospital data name
# table <- "hesae_v0001" # Specific dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

# Note: This has 0 rows


## HES - Admitted Patient Care ##

# Note: This includes all admissions, including A&E and outpatient attendences.

# # Load in data - note slow
# data_source <- "nhsd" # Linked hospital data name
# table <- "hesapc_otr_v0001" # Specific dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

# Not sure what this one is, let's use the one below

# Load in data - note slow
data_source <- "nhsd" # Linked hospital data name
table <- "hesapc_v0001" # Specific dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
nhsd_hesapc_v0001 <- as.data.table(nhsd_hesapc_v0001) # Convert to data.table format
rm(data_t1) # Tidy

# Subset required variables
nhsd_hesapc_v0001 <- nhsd_hesapc_v0001[, c("llc_0009_stud_id", "epikey", "admidate", "epiorder", "diag_3_01", "diag_3_02", "diag_3_03", "diag_3_04", "diag_3_05", "diag_3_06", "diag_3_07", "diag_3_08", "diag_3_09", "diag_3_10", "diag_3_11", "diag_3_12", "diag_4_01", "diag_4_02", "diag_4_03", "diag_4_04", "diag_4_05", "diag_4_06", "diag_4_07", "diag_4_08", "diag_4_09", "diag_4_10", "diag_4_11", "diag_4_12")]

# Drop any duplicated rows introduced during the ingestion process via UK LLC
nhsd_hesapc_v0001 <- unique(nhsd_hesapc_v0001)

# Use only first episode of a spell (epiorder == 1)
nhsd_hesapc_v0001 <- nhsd_hesapc_v0001[nhsd_hesapc_v0001$epiorder == 1,]

# Subset dates for outcome 
# nhsd_hesapc_v0001 <- nhsd_hesapc_v0001[nhsd_hesapc_v0001$admidate >= "2020-03-01" & nhsd_hesapc_v0001$admidate < "2022-01-01",] # 1st March 2020 - 31st December 2021
nhsd_hesapc_v0001 <- nhsd_hesapc_v0001[nhsd_hesapc_v0001$admidate >= "2020-03-01",] # 1st March 2020 - 25th August 2022

# Load in codelists
codelist <- read.csv("S:/LLC_0009/data/codelist.csv")

# Classify primary diagnosis position if matches codelist
nhsd_hesapc_v0001 <- merge(nhsd_hesapc_v0001, codelist, by.x = "diag_3_01", by.y = "code", all.x = TRUE, allow.cartesian = TRUE) # Join codelist on

# Create measure for total admissions
nhsd_hesapc_v0001$total <- "total_admissions"

# Reshape data to wide format
hes <- nhsd_hesapc_v0001[, c("llc_0009_stud_id", "measure", "total")] # Drop unncessary data
hes$count <- 1 # Create variable to aggregate
hes <- dcast(data = hes, llc_0009_stud_id ~ measure + total, fun = sum, value.var = "count", fill = 0) # reshape 
# hes$'NA' <- NULL # Drop variable
names(hes)[names(hes) == "NA_total_admissions"] <- "total_admissions" # Rename variables
names(hes)[names(hes) == "Ambulatory Care Sensitive Acute_total_admissions"] <- "Ambulatory Care Sensitive Acute" 
names(hes)[names(hes) == "Ambulatory Care Sensitive Chronic_total_admissions"] <- "Ambulatory Care Sensitive Chronic" 
names(hes)[names(hes) == "Ambulatory Care Sensitive Vaccine-preventable_total_admissions"] <- "Ambulatory Care Sensitive Vaccine-preventable" 
names(hes)[names(hes) == "Emergency Urgent Care Sensitive _total_admissions"] <- "Emergency Urgent Care Sensitive " 

save(hes, file = "S:/LLC_0009/data/Cleaned Cohorts/nhs_hes.RData") # Save
rm(hes)

# Repeat but exclude episodes before survey date
load("S:/LLC_0009/data/Cleaned Cohorts/all_ids.RData") # Load in list of IDs and survey date
all_ids$date[all_ids$date == "NA/NA/2021"] <- "9/3/2021" # Set as max date since missing
all_ids$date[all_ids$date == "-7/-7/2021"] <- "9/3/2021"
all_ids$last_date <- as.POSIXct(all_ids$date, format = "%d/%m/%Y") # Convert date to match same format as in HES
nhsd_hesapc_v0001 <- merge(nhsd_hesapc_v0001, all_ids, by.x = "llc_0009_stud_id", by.y = "LLC_0009_stud_id", all.x = TRUE) # Join together
hes_after <- nhsd_hesapc_v0001[nhsd_hesapc_v0001$admidate > nhsd_hesapc_v0001$last_date] # Exclude episodes before survey date
hes_after$time_dif <- lubridate::interval(hes_after$last_date, hes_after$admidate) # Calculate time interval between end of survey and admission
hes_after$time <- as.numeric(lubridate::as.period(hes_after$time_dif, unit = "days"), "days") # Calculate days to admission

# Repeat cleaning process to extract outcome variables
hes <- hes_after[, c("llc_0009_stud_id", "measure", "total", "time")] # Drop unncessary data
hes$count <- 1 # Create variable to aggregate
hes <- dcast(data = hes, llc_0009_stud_id ~ measure + total, fun = list(sum, min), value.var = c("count", "time"), fill = 0) # Reshape data to wide format 
# hes$'NA' <- NULL # Drop variable
names(hes)[names(hes) == "count_sum_NA_total_admissions"] <- "total_admissions" # Rename variables - count var
names(hes)[names(hes) == "count_sum_Ambulatory Care Sensitive Acute_total_admissions"] <- "Ambulatory Care Sensitive Acute" 
names(hes)[names(hes) == "count_sum_Ambulatory Care Sensitive Chronic_total_admissions"] <- "Ambulatory Care Sensitive Chronic" 
names(hes)[names(hes) == "count_sum_Ambulatory Care Sensitive Vaccine-preventable_total_admissions"] <- "Ambulatory Care Sensitive Vaccine-preventable" 
names(hes)[names(hes) == "count_sum_Emergency Urgent Care Sensitive _total_admissions"] <- "Emergency Urgent Care Sensitive " 

names(hes)[names(hes) == "time_min_NA_total_admissions"] <- "time_to_total_admissions" # Rename variables - time var
names(hes)[names(hes) == "time_min_Ambulatory Care Sensitive Acute_total_admissions"] <- "Time to Ambulatory Care Sensitive Acute" 
names(hes)[names(hes) == "time_min_Ambulatory Care Sensitive Chronic_total_admissions"] <- "Time to Ambulatory Care Sensitive Chronic" 
names(hes)[names(hes) == "time_min_Ambulatory Care Sensitive Vaccine-preventable_total_admissions"] <- "Time to Ambulatory Care Sensitive Vaccine-preventable" 
names(hes)[names(hes) == "time_min_Emergency Urgent Care Sensitive _total_admissions"] <- "Time to Emergency Urgent Care Sensitive " 

hes$time_to_total_admissions[hes$time_to_total_admissions == 0] <- NA # 0s here really are missing so we need to tidy them up
hes$`Time to Ambulatory Care Sensitive Acute`[hes$`Time to Ambulatory Care Sensitive Acute` == 0] <- NA
hes$`Time to Ambulatory Care Sensitive Chronic`[hes$`Time to Ambulatory Care Sensitive Chronic` == 0] <- NA
hes$`Time to Ambulatory Care Sensitive Vaccine-preventable`[hes$`Time to Ambulatory Care Sensitive Vaccine-preventable` == 0] <- NA
hes$`Time to Emergency Urgent Care Sensitive `[hes$`Time to Emergency Urgent Care Sensitive ` == 0] <- NA

hes <- hes[, c(1:6, 17:21)] # Subset required vars
save(hes, file = "S:/LLC_0009/data/Cleaned Cohorts/nhs_hes_post_wave.RData") # Save

# Tidy
rm(hes, nhsd_hesapc_v0001, hes_after, all_ids)

## HES - Critical Care ## - need to check

# Note: Critical care includes intensive care and high dependency care - this is less relevant to our study.

# # Load in data - note slow
# data_source <- "nhsd" # Linked hospital data name
# table <- "hescc_v0001" # Specific dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy


## HES - Outpatient admissions ## 

# Note: I can't get it to load as too big! 

# # Load in data - note slow
# data_source <- "nhsd" # Linked hospital data name
# table <- "hesop_v0001" # Specific dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

## HES - Mortality ## 

# Load in data
data_source <- "nhsd" # Linked hospital data name
# table <- "mortality_20220302" # Specific dataset
table <- "mortality_20220716" # Specific dataset (use this one?)
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

# Subset required variables
nhsd_mortality_20220716 <- nhsd_mortality_20220716[,  c("llc_0009_stud_id", "s_underlying_cod_icd10", "reg_date_of_death")] # ID, underlying cause of death, date of death

# # Only include deaths before end of study period (31st Dec 2021)
# nhsd_mortality_20220716 <- nhsd_mortality_20220716[nhsd_mortality_20220716$reg_date_of_death < 20220101,]

# Save
save(nhsd_mortality_20220716, file = "S:/LLC_0009/data/Cleaned Cohorts/mortality.RData") # Save
rm(nhsd_mortality_20220716)

## HES - Primary Care Medicines ## 

# Note: Did not request these data so we don't need them here.

# # Load in data
# data_source <- "nhsd" # Linked hospital data name
# table <- "pcm_v0001" # Specific dataset
# data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
# name_t1 <- paste0(data_source,"_",table) # Create df name
# assign(name_t1, data_t1) # Assign name to dataset
# rm(data_t1) # Tidy

# Note: This has 0 rows



## Clean core data ##

# Load in data
data_source <- "core" # Linked hospital data name
table <- "denominator_file1_20220122" # Specific dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy
linked_records <- core_denominator_file1_20220122

# Save
save(linked_records, file = "S:/LLC_0009/data/Cleaned Cohorts/data_linkage.RData") # Save
rm(linked_records, core_denominator_file1_20220122)

## Clean geo linked data ##

# Load in data (takes 1ish hour)
data_source <- "core" # Linked hospital data name
table <- "nhsd_geo_indicator_v0004_20221028" # Specific dataset
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

library(data.table) # For next steps
hold <- core_nhsd_geo_indicator_v0004_20221028[, c("llc_0009_stud_id", "imd2019_income_q5", "gor10nm", "ctry17nm", "record_date")]  # Keep variables we need
hold <- data.table(hold) # Convert data type
rm(core_nhsd_geo_indicator_v0004_20221028) # Tidy
hold[order(llc_0009_stud_id, -record_date), num_record:=1:.N, by=.(llc_0009_stud_id)] # Order within ID by date of record (we want to take the most recent - so order in reverse date order)
geodata <- hold[num_record == 1] # Take most recent data

# Save
save(geodata, file = "S:/LLC_0009/data/Cleaned Cohorts/imd_linked.RData") # Save
rm(hold, geodata) # Tidy


## Clean linked demographic data ##
# This tells us who was linked or not #

# Load in data (takes 1ish hour)
data_source <- "core" # Linked hospital data name
table <- "nhsd_derived_indicator_v0004_20221101" # Specific dataset - 
data_t1 <- lab_func(proj_no,data_source,table) # Run labelling function
name_t1 <- paste0(data_source,"_",table) # Create df name
assign(name_t1, data_t1) # Assign name to dataset
rm(data_t1) # Tidy

core_nhsd_derived_indicator_v0004_20221101$linked <- 1 # Create variable for being linked
linked_ppl <- core_nhsd_derived_indicator_v0004_20221101[, c("llc_0009_stud_id", "last_seen_date", "linked", "ethnic")] # Keep variables need

# Save
save(linked_ppl, file = "S:/LLC_0009/data/Cleaned Cohorts/who_was_linked.RData") # Save
rm(core_nhsd_derived_indicator_v0004_20221101, linked_ppl)



