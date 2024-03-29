##########################################################################
# lab_func - this function takes 3 arguments:
# proj: this is your project number (defined at top of script)
# schema: this is the study or source of data e.g. ALSPAC, MCS, NHSD etc
# table: this is the table name to apply labelling to
# THIS FUNCTION DOES NOT NEED TO BE MODIFIED
# Date: 06/09/2023 (version 1.3)
# Changes since last version: application of nhsd labels bypassed - this is 
# ... a temp measure until nhsd labelling is cleaned and available
##########################################################################
proj = proj_no

# add MHSDS staff details and geo indicator tables - these are large tables and cause mem error so need bypassing
exceptions <- list("mhsds_mhs901staffdetails_v0001", "nhsd_geo_indicator_v0004_20221028")

lab_func = function(proj, schema, table) {
  # remove exceptions - tables which are too large to be loaded to memory and labelled
  if (table %in% exceptions) {
    print(paste0(schema,"_",table," cannot be loaded and labelled - file size too large or table deprecated"))
  } else {
    # print which table is being run
    print(paste0("running load and labelling of ",schema,"_",table))
    
    ########################
    # FIX ONGOING HERE - inconsistent results between 0009 and 9999 in both R and stata
    #######################
    
    # pre processing to ensure integrity of stud_id is retained
    # get len - need to get num of cols from SQL before reading as vector supplied to as.is sqlQuery parameter must equal to num of columns
    col_count_q = paste0("select count(*) from sys.columns where object_id = object_id('",proj,".",schema,"_",table,"')")
    col_count <- sqlQuery(conn, col_count_q)
    col_count = col_count[1,1]
    # create vector
    col_count = vector(mode="logical", length = col_count)
    # switch pos1 to TRUE this is control format of llc_xxxx_stud_id only
    col_count[1] = TRUE
    
    # query data
    q_data <- paste0("select * from [",proj,"].[",schema,"_",table,"];")
    data <- sqlQuery(conn, q_data, stringsAsFactors=F, as.is = col_count) 
    
    # pull data's associated value labels with numeric keys
    q_val <- paste0("select * from VALUE.all_values where table_name = '",table,"' and TABLE_SCHEMA = '",schema,"'
                    and value_value not like '%[A-Z]%' and value_value != '';")
    values <- sqlQuery(conn, q_val, stringsAsFactors=F)
    # temp step to remove duplicates - requires update in DB (this is catch all for now)
    values = values %>%
      group_by(table_name, TABLE_SCHEMA, variable_name, value_value) %>%
      mutate(count = n()) %>%
      filter(count==1)
    # pull data's associated variable labels
    q_des <- paste0("select * from DESCS.all_descriptions where table_name = '",table,"' and TABLE_SCHEMA = '",schema,"';")
    descs <- sqlQuery(conn, q_des, stringsAsFactors=F)
    # create list of column names from actual data
    data_colnames <- as.list(colnames(data))
    
    ## VALUE LABELLING SECTION
    # strip down values to unique variables and convert to list - keeping only tables with value labelling information
    vals <- values$variable_name %>%
      unique() %>%
      as.list(vals) %>%
      lapply(tolower)
    
    # reduce list of value labelling where in both data and where labelling provided
    vals_final <- Reduce(intersect, list(data_colnames, vals))
    
    if (schema != 'nhsd'){
      # for variables in dataset with value labelling 
      for (i in vals_final){
        # conver to lower case
        values <- values %>%
          mutate(variable_name = tolower(variable_name))
        # grab one variable values at a time
        t1 <- values[values$variable_name == i, ]
        # strip down to create essentially a dict
        t1 <- t1[c("value_value","value_label")]
        # create new col with value and variable in form that val_lab can read
        t1$tcol = paste0(t1$value_value," ",t1$value_label,"\n")
        # convert this new col to list
        t2 <- as.list(t1$tcol)
        t3 <- paste(t2, collapse=' ')
        # try and apply
      val_lab(data[[i]]) <- num_lab(t3)
      }
    }
    
    # DESC SECTION
    # do a unique on  tables and convert to list - this way we only keep vars wit an acutal label
    des <- descs$variable_name %>%
      as.list(des) %>%
      lapply(tolower)
    
    # data_colnames vs des compare
    t4 <- Reduce(intersect, list(data_colnames,des))
    
    # for variable in dataset apply desc
    if (schema != 'nhsd'){
      for (i in t4){
        # convert variable name column to lower
        descs <- descs %>%
          mutate(variable_name = tolower(variable_name))
        # grab one variables values at a time
        t5 = descs[descs$variable_name == i, ]
        # then isolate variable label
        t6 <- t5[1,"variable_label"]
        # add variable label to data
        var_lab(data[[i]]) = t6
      }
    }
    # return fully labelled dataset
    return(data)
  }
}