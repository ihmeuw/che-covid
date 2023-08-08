## Author: User                                                       ##
## Date: 12/5/2022                                                        ##
## Purpose: Combine draws for ensemble model: stratified, unweighted ,visits peru
################################################################################
## clear memory
rm(list=ls())
library(vctrs, lib.loc='/FILEPATH/4.1.3.4')

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
level_lag <-fread(file.path(dir, 'per_visits_draws_lag_model.csv'))

## diff draws
diff_draws <-fread(file.path(dir, 'per_visits_draws_diff_model.csv'))

visits_draws <- do.call("rbind", list(level_lag,diff_draws))


visits_draws_summary <- data.table()
  for (var in unique(visits_draws$variable)){
  temp <- visits_draws[ variable == eval(var)]
  temp_sum <- data.table(apply(temp[, c("value_predict")], 2, quantile, probs = c(.025, .5, .975)))
  
  # extract mean
  mean_val <- mean(temp$value_predict)
  temp_sum[, variable := var]
  temp_sum <- dcast(temp_sum, variable ~ value_predict, value.var = "value_predict")
  colnames(temp_sum)[2:4] <- c("lower_value", "mean_value", "upper_value")
  
  temp_sum[, mean_value :=mean_val ]
  
  visits_draws_summary <- rbind(visits_draws_summary, temp_sum)
  }


## add necessary columns for plotting
visits_draws_summary[, year_id := 2020]
visits_draws_summary[, predict := "Prediction"]
visits_draws_summary[, predict := "Prediction"]

# save predictions for plotting
write.csv(visits_draws_summary,file.path(dir, 'visits_plotting_predictions.csv'), row.names = F)
