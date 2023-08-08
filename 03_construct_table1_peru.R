## Author: User                                                       ##
## Date: 12/8/21                                                            ##
## Purpose: Finalize table 1 for CHE paper                ##
################################################################################
## clear memory
rm(list=ls())

## runtime configuration
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/home/j'
  h <- file.path('/homes', Sys.getenv('USER'))
} else {
  j <- 'J:/'
  h <- 'H:/'
}

## libraries
pacman::p_load(data.table, haven,readstata13, readxl, ggplot2,dplyr,survey, broom)

## directory 
dir <- 'FILEPATH'

# read in individual, hh, visit, and che calculations
ind <- fread(file.path(dir, 'peru_table1_ind.csv'))[,-c("V1")]
visits <- fread(file.path(dir, 'peru_table1_visits_private.csv'))[,-c("V1")]
hh <- fread( file.path(dir, 'peru_table1_hh.csv'))[,-c("V1")]
che <- fread(file.path(dir, 'peru_table1_che_updated_sep_2022.csv'))[,-c("V1")]
che_ins <- fread(file.path(dir, 'peru_table1_che_ins_updated_sep_2022.csv'))[,-c("V1")]


peru_table1 <- rbind(ind, visits, hh,che, che_ins)
peru_table1[,year := ifelse(year_factor ==0, 2019, 2020)]
peru_table1[,ihme_loc_id := "PER"]
peru_table1 <- peru_table1[,-c("year_factor")]

# reshape
peru_table1_reshape <- reshape(peru_table1, idvar = "variable", timevar = "year", direction = "wide")
peru_table1_reshape <- peru_table1_reshape[,-c("difference.2019", "ihme_loc_id.2019" ,"pvalue.2020" )]
colnames(peru_table1_reshape) <- c("Variable", "P-value", "2019", "2020", "Difference", "Location")
peru_table1_reshape <- peru_table1_reshape[,c("Variable", "2019", "2020", "Difference","P-value", "Location")]

write.csv(peru_table1_reshape, file.path(dir, 'table1_peru_filled_updated_jul_2023.csv'))
