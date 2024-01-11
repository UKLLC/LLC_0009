########################################################################################
# Title: UK LLC R value and variable labelling helper
#
# This uses the expss package to apply variable and value labels to datasets, this can
# be done on individual datasets or multiple datasets at once. Due to performance issues 
# it is not recommended to load and apply to all datasets at once (if you have a large
# number available as part of your project) as the loading will be slow
#
# ACTIONS: 1) UPDATE project number at ACTION1
# ACTIONS: 2) Decide which method (A,B,C) to use to label data:
#             A: one table at a time - AND UPDATE data_source and table variables
#             B: one data_source at a time - AND UPDATE data_source variable
#             C: all project data in one go - NO UPDATE required
#         
# Date: 13/01/2023 (version 1.1) 
#######################################################################################

#################################
# ACTION1: enter/update project number in form "LLC_0001"
#################################
proj_no = "LLC_0009"

##############################################################
# define function to check and install packages if required
##############################################################
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

##############
# packages
#############
packages = c("tidyr", "dplyr", "RODBC", "expss", "stringr", "tidyr", "rstudioapi")
# run function to load and install packages
load_install_packages(packages)

####################################################################################################################
# set the dir to where this script is save and source the labelling function (assumes both scripts in same location)
####################################################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("r_label_functions.R")

#########################################
# DB connection and project information
#########################################
# setup connection
conn <- odbcConnect(dsn="LLC_DB") 
# Get all table/viewnames avail in connection
allviews <- sqlTables(conn)
# just take ones attached to project schema
viewnames <- tolower(allviews[allviews$TABLE_SCHEM %in% proj_no,"TABLE_NAME"])
# and print to sense-check
print(viewnames) 

# 
# ######################################################################
# # METHOD A: label one table at a time
# # ACTION2a: define/update data_source AND table name to label (use lowercase)
# #######################################################################
# data_source <- "genscot"
# table <- "covidlife1_v0001_20211101"
# # run labelling function
# data_t1 <- lab_func(proj_no,data_source,table)
# # create df name
# name_t1 <- paste0(data_source,"_",table)
# # assign name to dataset
# assign(name_t1, data_t1)
# 
# ######################################################################
# # METHOD B: separate one study/source and label multiple tables at a time
# # ACTION2b: define/update data_source to label (use lowercase)
# ######################################################################
# data_source <- "genscot"
# # get all views names belonging to data source/study
# one_stud_viewnames <- grep(paste0("^",data_source), viewnames, value=TRUE)
# # new
# one_stud_views_split <- str_split_fixed(one_stud_viewnames,"_",2)
# # create empty list to populate
# one_stud_dfs_w_labs <- list()
# # go through each dataframe and apply values and variable labels
# for (i in 1:length(one_stud_viewnames)){
#   # define schema and tables names for function to run
#   data_source <- one_stud_views_split[i,1]
#   table <- one_stud_views_split[i,2]
#   # run labelling function
#   data <- lab_func(proj_no,data_source,table)
#   # create df name
#   name_t1 <- paste0(data_source,"_",table)
#   # add dataframe to list
#   one_stud_dfs_w_labs[[name_t1]] = data.frame(data)
# }
# 
# ####################################################################
# # METHOD C: label all tables at same time - WARNING - SLOW TO RETRIVE ALL DATA
# ####################################################################
# # create empty list to populate
# all_dfs_w_labs <- list()
# # separate study name from descriptive part of table name
# all_views_split <- str_split_fixed(viewnames,"_",2)
# # go through each dataframe and apply values and variable labels
# for (i in 1:length(viewnames)){
#   # define schema and tables names for function to run
#   data_source <- all_views_split[i,1]
#   table <- all_views_split[i,2]
#   # run labelling function
#   data <- lab_func(proj_no,data_source,table)
#   # create df name
#   name_t1 <- paste0(data_source,"_",table)
#   # add dataframe to list
#   all_dfs_w_labs[[name_t1]] = data.frame(data)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
