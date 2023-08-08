## Author: User                                                       ##
## Date: 1/4/22                                                           ##
## Purpose: Finalize table 1 for CHE paper        (Mexico)        ##
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
dir <- 'FILEPATH/results'

# read in individual, hh, visit, and che calculations
ind <- fread(file.path(dir, 'mexico_table1_ind.csv'))[,-c("V1")]
visits <- fread(file.path(dir, 'mexico_table1_visits_private.csv'))[,-c("V1")]
hh <- fread( file.path(dir, 'mexico_table1_hh.csv'))[,-c("V1")] 
che <- fread(file.path(dir, 'meixco_table1_che.csv'))[,-c("V1")]
che_ins <- fread(file.path(dir, 'mexico_table1_che_ins.csv'))[, -c("V1")]

mexico_table1 <- rbind(ind, visits, hh,che, che_ins)
mexico_table1[,year := ifelse(year_factor ==0, 2018, 2020)]
mexico_table1[,ihme_loc_id := "MEX"]
mexico_table1 <- mexico_table1[,-c("year_factor")]

# reshape
mexico_table1_reshape <- reshape(mexico_table1, idvar = "variable", timevar = "year", direction = "wide")
mexico_table1_reshape <- mexico_table1_reshape[,-c("difference.2018", "ihme_loc_id.2018" ,"pvalue.2020" )]
colnames(mexico_table1_reshape) <- c("Variable", "P-value", "2018", "2020", "Difference", "Location")
mexico_table1_reshape <- mexico_table1_reshape[,c("Variable", "2018", "2020", "Difference","P-value", "Location")]

write.csv(mexico_table1_reshape, file.path(dir, 'table1_mexico_filled_updated_jul_2023.csv'))
