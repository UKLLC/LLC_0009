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
# Date: 04/03/2022 (version 1) 
#######################################################################################

#################################
# ACTION1: enter/update project number in form "LLC_0001"
#################################
proj_no = "LLC_0009"

####################################################################################################################
# set the dir to where this script is save and source the labelling function (assumes both scripts in same location)
####################################################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("r_label_functions.R")

##############
# packages
#############
packages = c("tidyr", "dplyr", "RODBC", "expss", "stringr", "tidyr")
# run function to load and install packages
load_install_packages(packages)

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
# data_source <- "bcs70"
# # get all views names belonging to data source/study
# one_stud_viewnames <- grep(data_source, viewnames, value=TRUE)
# # pull all views belonging to chosen data source - creates list of dataframes
# one_stud_views <- lapply(one_stud_viewnames, function(X) {
#   sqlQuery(conn, paste0("select * from [UKSERPUKLLC].[",proj_no,"].[", X, "]"), stringsAsFactors=FALSE)})
# # add names to dataframes
# names(one_stud_views) <- one_stud_viewnames
# # create empty list to populate
# one_stud_dfs_w_labs <- list()
# # separate data_source from descriptive part of table name
# one_stud_views_split <- str_split_fixed(names(one_stud_views),"_",2)
# # go through each dataframe and apply values and variable labels
# for (i in 1:length(one_stud_views)){
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
# # pull all data in project - warning this can be slow if project includes large number of datasets
# all_views <- lapply(viewnames, function(X) {
#   sqlQuery(conn, paste0("select * from [UKSERPUKLLC].[",proj_no,"].[", X, "]"), stringsAsFactors=FALSE)})
# # add names to dataframes
# names(all_views) <- viewnames
# # create empty list to populate
# all_dfs_w_labs <- list()
# # separate study name from descriptive part of table name
# all_views_split <- str_split_fixed(names(all_views),"_",2)
# # go through each dataframe and apply values and variable labels
# for (i in 1:length(all_views)){
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










