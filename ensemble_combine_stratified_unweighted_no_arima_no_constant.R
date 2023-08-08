## Author: User                                                       ##
## Date: 10/18/2022                                                        ##
## Purpose: Combine draws for ensemble model: stratified, unweighted 
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

dir <- 'FILEPATH/'

pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2,dplyr, broom ,DescTools)

## lag level draws
level_lag_che10 <-fread(file.path(dir, 'che10_predict_draws_lag_level.csv'))
level_lag_che25 <-fread(file.path(dir, 'che25_predict_draws_lag_level.csv'))

## diff draws
diff_che10 <-fread(file.path(dir, 'che10_predict_draws_diff.csv'))
diff_che25 <-fread(file.path(dir, 'che25_predict_draws_diff.csv'))

## lag level draws: no constant
level_lag_che10_no_constant <-fread(file.path(dir, 'che10_predict_draws_lag_level_no_constant.csv'))
level_lag_che25_no_constant <-fread(file.path(dir, 'che25_predict_draws_lag_level_no_constant.csv'))

## diff draws: no constant
diff_che10_no_constant <-fread(file.path(dir, 'che10_predict_draws_diff_no_constant.csv'))
diff_che25_no_constant<-fread(file.path(dir, 'che25_predict_draws_diff_no_constant.csv'))

che10_draws <- do.call("rbind", list(level_lag_che10, diff_che10,level_lag_che10_no_constant,diff_che10_no_constant))
che25_draws <- do.call("rbind", list(level_lag_che25,diff_che25,level_lag_che25_no_constant,diff_che25_no_constant))


che10_draws_summary <- data.table()
for (loc in unique(che10_draws$ihme_loc_id)){
  temp <- che10_draws[ihme_loc_id == eval(loc)]
  temp_sum <- data.table(apply(temp[, c("che10_predict")], 2, quantile, probs = c(.025, .5, .975)))
  
  # extract mean
  mean_val <- mean(temp$che10_predict)
  median_val <- median(temp$che10_predict)
  temp_sum[, ihme_loc_id := loc]
  temp_sum <- dcast(temp_sum, ihme_loc_id ~ che10_predict, value.var = "che10_predict")
  colnames(temp_sum)[2:4] <- c("lower_che10", "mean_che10", "upper_che10")
  
  temp_sum[, mean_che10 :=median_val ]
  
  che10_draws_summary <- rbind(che10_draws_summary, temp_sum)
}

che25_draws_summary <- data.table()
for (loc in unique(che25_draws$ihme_loc_id)){
  temp <- che25_draws[ihme_loc_id == eval(loc)]
  temp_sum <- data.table(apply(temp[, c("che25_predict")], 2, quantile, probs = c(.025, .5, .975)))
  # extract mean
  median_val <- median(temp$che25_predict)
  mean_val <- mean(temp$che25_predict)
  temp_sum[, ihme_loc_id := loc]
  temp_sum <- dcast(temp_sum, ihme_loc_id ~ che25_predict, value.var = "che25_predict")
  colnames(temp_sum)[2:4] <- c("lower_che25", "mean_che25", "upper_che25")
  temp_sum[, mean_che25 := median_val ]
  che25_draws_summary <- rbind(che25_draws_summary, temp_sum)
}

#create percent
che10_draws_summary[, che10_pct := mean_che10*100]
che25_draws_summary[, che25_pct := mean_che25*100]

## add other necessary columns
che10_draws_summary[, year_id := 2020]
che10_draws_summary[, predict := "Prediction"]

che25_draws_summary[, year_id := 2020]
che25_draws_summary[, predict := "Prediction"]

#adjust names
colnames(che10_draws_summary)[2:4] <- c("lower","che10", "upper")
colnames(che25_draws_summary)[2:4] <- c("lower","che25", "upper")

# save predictions for plotting
write.csv(che10_draws_summary,file.path(dir, 'che10_predict_stratified_unweighted_median_no_constant.csv'), row.names = F)
write.csv(che25_draws_summary,file.path(dir, 'che25_predict_stratified_unweighted_median_no_constant.csv'), row.names = F)

# look at skew 
che10_draws_summary[, lower_diff := che10-lower]
che10_draws_summary[, upper_diff := che10-upper]

che25_draws_summary[, lower_diff := che25-lower]
che25_draws_summary[, upper_diff := che25-upper]


