## Author: User                                                       ##
## Date: 11/11/2022                                                       ##
## Purpose: Create diff draws of distribution of mean for each country and each che driver   ##
## create distributions created with SE
################################################################################
## clear memory
rm(list=ls())
library(vctrs, lib.loc='FILEPATH/4.1.3.4')

## runtime configuration
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/home/j'
  h <- file.path('/homes', Sys.getenv('USER'))
} else {
  j <- 'J:/'
  h <- 'H:/'
}

dir <- "FILEPATH/"

## libraries
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2,dplyr, broom )

#read in che drivers data 
che_drivers <- fread(file.path(dir, "che_drivers_means.csv"))

## define functions for creating draws for che drivers
create_driver_draws <- function(current_ihme_loc_id, current_var) {
  current_df <-che_drivers[ihme_loc_id == current_ihme_loc_id& variable == current_var] 
  
  norm_value <- data.table()
  norm_value_lag <- data.table()
  years <- unique(current_df$year_id)
  
  for (index in 1:length(years)){
    temp <- data.table(
      
    )
    lag_temp <- data.table(
      
    )
    current <- current_df[year_id == years[index]]
    dist <- data.table(rnorm(1000, current$value, current$se))
    temp$value <- dist
    
    temp[, year_id := years[index]]
    
    
    lag_temp$lag_value <- dist 
    lag_temp[, year_id := years[index+1]]
    lag_temp$draw <- seq.int(nrow(lag_temp))
    norm_value_lag <- rbind(norm_value_lag, lag_temp)
    
    
    
    temp$draw <- seq.int(nrow(temp))
    norm_value <- rbind(norm_value, temp)
  }
  
  norm_value[, ihme_loc_id := current_ihme_loc_id]
  norm_value_lag[, ihme_loc_id := current_ihme_loc_id]
  
  norm_value[, variable := current_var]
  norm_value_lag[, variable := current_var]
  
  norm_value <- merge(norm_value,norm_value_lag, by = c("year_id", "ihme_loc_id", "draw", "variable"), all.x = T )
  return(norm_value)
  
}

che_driver_draws <- data.table()
for (country in unique(che_drivers$ihme_loc_id)){
  print(country)
  
  for (var in unique(che_drivers$variable)){
    print(var)
    temp_draws <- create_driver_draws(country, var)
    che_driver_draws <- rbind(che_driver_draws, temp_draws)
  }
  
}

che_driver_draws <- che_driver_draws[order(ihme_loc_id, variable,year_id, draw)]
che_driver_draws[, diff := value - lag(value), by = c("ihme_loc_id", "variable", "draw")]
che_driver_draws[, lag_diff := lag(diff), by = c("ihme_loc_id", "variable", "draw")]

## save draws 
write.csv(che_driver_draws,file.path(dir, 'che_driver_diff_draws.csv'), row.names = F)


