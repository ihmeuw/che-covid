## Author: USER                                                       ##                                                    ##
## Date: 1/4/22                                                           ##
## Purpose: Finalize table 1 for CHE paper              ##
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

## read in peru and mexico table 1 aggregates
peru <- fread(file.path(dir, 'table1_peru_filled_updated_feb_2023.csv'))
mexico <- fread(file.path(dir, 'table1_mexico_filled_updated_feb_2023.csv'))

table1 <- cbind(mexico, peru)
write.csv(table1, file.path(dir, 'table1_filled_updated_feb_2023.csv'))
