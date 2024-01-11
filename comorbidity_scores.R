########################################################
# author: rich thomas
# date: 20/04/2023
# purpose: calc charlson and elixhauser indexes
########################################################

# define function to check and install packages if required
load_install_packages <-function(x){
  for(i in x){
    # returns true if package loads
    if(!require(i, character.only = T)){
      # if package not able to be loaded (re)install
      install.packages(i, dependencies = T)
      # load package following install
      require(i, character.only = T)
    }
  }
}

# packages
packages = c("tidyr", "dplyr", "RODBC", "comorbidity", "haven", "expss")
# run function to load and install packages
load_install_packages(packages)

# working dir
setwd("S:/LLC_0002/rt_working/charlson_index")

#########################################
# DB connection and project information
#########################################
# setup connection
conn <- odbcConnect(dsn="LLC_DB")
# NOTE: ID inconsistencies coming up in sqlQuery - requires investigation
# define target dataset from DB
#q = "select convert(varchar(20),llc_0002_stud_id) as llc_0002_stud_id, diag_3_concat, diag_4_concat, admidate from llc_0002.nhsd_hesapc_v0001"
# grab data
#data <- sqlQuery(conn, q, stringsAsFactors=F)

# Get data from stata instead 
cols <- c("llc_0002_stud_id", "diag_3_concat", "diag_4_concat", "admidate")
data = read_dta("S:/LLC_0002/data/stata_w_labs/NHSD_HESAPC_v0001.dta", col_select = cols)

# date filtering
st_date <- ("2019-03-01")
end_date <- ("2020-02-29")
# date filter func
date_filter <- function(x,y){data[data$admidate >= x & data$admidate <= y,]}
# replace data filtered on date
data = date_filter(st_date, end_date) %>%
  select(-admidate)

# process data by unpacking wide struc of diag to long
# 3 char codes
diag_3 <- data %>%
  select(-diag_4_concat) %>%
  dplyr::rename(code = diag_3_concat) %>%
  separate_rows(code, sep = ",") 
# 4 char codes
diag_4 <- data %>%
  select(-diag_3_concat) %>%
  dplyr::rename(code = diag_4_concat) %>%
  separate_rows(code, sep = ",") 
# append 3 and 4 char codes together for completeness
inp <- bind_rows(diag_3, diag_4) %>%
  arrange(llc_0002_stud_id)

# CHARLSON
# parameters to check: weights, assign0
# create comorbidity object - charlson ICD10
charlson <- comorbidity(inp, id = "llc_0002_stud_id", code = "code", map = "charlson_icd10_quan", assign0 = FALSE)
# calculate scores and add to df
charl_w_sc <- mutate(charlson, unw_cci = score(charlson, weights = NULL, assign0 = FALSE)) %>%
  mutate(charlson, char_cci = score(charlson, weights = "charlson", assign0 = FALSE)) %>%
  mutate(charlson, quan_cci = score(charlson, weights = "quan", assign0 = FALSE)) %>%
  # remove comorbidity domains fo disc purposes
  select(llc_0002_stud_id, ends_with("cci"))

# ELIXHAUSER
# parameters to check: weights, assign0
# create comorbidity object - elix ICD10
elixhauser <- comorbidity(inp, id = "llc_0002_stud_id", code = "code", map = "elixhauser_icd10_quan", assign0 = FALSE)
# calculate score and add to df
elix_w_sc <- mutate(elixhauser, unw_eci = score(elixhauser, weights = NULL, assign0 = FALSE))%>%
  mutate(elixhauser, vw_eci = score(elixhauser, weights = "vw", assign0 = FALSE)) %>%
  mutate(elixhauser, swiss_eci = score(elixhauser, weights = "swiss", assign0 = FALSE)) %>%
  # remove co morbidity domains
  select(llc_0002_stud_id, ends_with("eci"))

# merge elix and char
comorbidity_scores = merge(charl_w_sc, elix_w_sc, by = "llc_0002_stud_id")

# add labels
comorbidity_scores= apply_labels(comorbidity_scores,
                                 unw_cci = "charlson score - unweighted",
                                 char_cci = "charlson score - original weights by Charlson et al. 1987",
                                 quan_cci = "charlson score - revised weights by Quan et al. 2011",
                                 unw_eci = "elixhauser score - unweighted",
                                 vw_eci = "elixhauser score - weights by van Walraven et al. 2009",
                                 swiss_eci = "elixhauser score - swiss exilhauser weights by Sharma et al. 2021")

# save as dta as csv writing messing with IDS and to preserve var labelling through RTN process
write_dta(comorbidity_scores, "comorbidity_scores_hesapc_mar19feb20_v0001_20230420.dta")









