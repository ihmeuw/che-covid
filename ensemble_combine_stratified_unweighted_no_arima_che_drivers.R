## Author: User                                                       ##
## Date: 11/11/2022                                                        ##
## Purpose: Combine draws for ensemble model: stratified, weighted , che drivers
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

dir <- '/FILEPATH/'

pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2,dplyr, broom ,DescTools)


## lag level draws
level_lag <-fread(file.path(dir, 'figure_a_draws_lag_model.csv'))

## diff draws
diff_draws <-fread(file.path(dir, 'figure_a_draws_diff_model.csv'))

che_driver_draws <- do.call("rbind", list(level_lag,diff_draws))


che_driver_draws_summary <- data.table()
for (loc in unique(che_driver_draws$ihme_loc_id)){
  for (var in unique(che_driver_draws$variable)){
  temp <- che_driver_draws[ihme_loc_id == eval(loc) & variable == eval(var)]
  temp_sum <- data.table(apply(temp[, c("value_predict")], 2, quantile, probs = c(.025, .5, .975)))
  
  # extract mean
  mean_val <- mean(temp$value_predict)

  temp_sum[, ihme_loc_id := loc]
  temp_sum[, variable := var]
  temp_sum <- dcast(temp_sum, ihme_loc_id + variable ~ value_predict, value.var = "value_predict")
  colnames(temp_sum)[3:5] <- c("lower_value", "mean_value", "upper_value")
  
  temp_sum[, mean_value :=mean_val ]
  
  che_driver_draws_summary <- rbind(che_driver_draws_summary, temp_sum)
  }
}

## add necessary columns for plotting
che_driver_draws_summary[, year_id := 2020]
che_driver_draws_summary[, predict := "Prediction"]

## model 2 for figure 1 
che_driver_draws_summary_model2 <- data.table()
for (loc in unique(level_lag$ihme_loc_id)){
  for (var in unique(level_lag$variable)){
    temp <- level_lag[ihme_loc_id == eval(loc) & variable == eval(var)]
    temp_sum <- data.table(apply(temp[, c("value_predict")], 2, quantile, probs = c(.025, .5, .975)))
    
    # extract mean
    mean_val <- mean(temp$value_predict)
    
    temp_sum[, ihme_loc_id := loc]
    temp_sum[, variable := var]
    temp_sum <- dcast(temp_sum, ihme_loc_id + variable ~ value_predict, value.var = "value_predict")
    colnames(temp_sum)[3:5] <- c("lower_value", "mean_value", "upper_value")
    
    temp_sum[, mean_value :=mean_val ]
    
    che_driver_draws_summary_model2 <- rbind(che_driver_draws_summary_model2, temp_sum)
  }
}
## add necessary columns for plotting
che_driver_draws_summary_model2[, year_id := 2020]
che_driver_draws_summary_model2[, predict := "Prediction"]

# save predictions for plotting
write.csv(che_driver_draws_summary,file.path(dir, 'che_drivers_plotting_predictions.csv'), row.names = F)
write.csv(che_driver_draws_summary_model2,file.path(dir, 'che_drivers_plotting_predictions_model2.csv'), row.names = F)
