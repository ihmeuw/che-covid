## Author: User                                                       ##
## Date: 5/12/2023                                             ##
## Purpose: Compare RMSE across che prediction models: each year held out           ##
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
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2,dplyr, broom)
##directory
dir <- 'FILEPATH'

weights_df <- fread( file.path(dir, 'weights_rmse.csv'))

che10_weights <- weights_df[stat == 10, c(6:12)]
che10_weights_long <- melt(setDT(che10_weights), variable.name = "model", value.name ="weight")

che25_weights <- weights_df[stat == 25, c(6:12)]
che25_weights_long <- melt(setDT(che25_weights), variable.name = "model", value.name ="weight")

# read in unweighted che 10 
che10_model1_unweighted <- fread('FILEPATH/che10_compare_holdout_model1_unweighted.csv')
che10_model1_unweighted[, model := 'model1']

che10_model2_unweighted <- fread('FILEPATH/che10_compare_holdout_model2_unweighted.csv')
che10_model2_unweighted[, model := 'model2']

che10_model3_unweighted <- fread('FILEPATH/che10_compare_holdout_model3_unweighted.csv')
che10_model3_unweighted[, model := 'model3']

che10_model4_unweighted <- fread('FILEPATH/che10_compare_holdout_model4_unweighted.csv')
che10_model4_unweighted[, model := 'model6']

che10_model5_unweighted <- fread('FILEPATH/che10_compare_holdout_model5_unweighted.csv')
che10_model5_unweighted[, model := 'model7']

# read in unweighted che 25
che25_model1_unweighted <- fread('FILEPATH/che25_compare_holdout_model1_unweighted.csv')
che25_model1_unweighted[, model := 'model1']

che25_model2_unweighted <- fread('FILEPATH/che25_compare_holdout_model2_unweighted.csv')
che25_model2_unweighted[, model := 'model2']

che25_model3_unweighted <- fread('FILEPATH/che25_compare_holdout_model3_unweighted.csv')
che25_model3_unweighted[, model := 'model3']

che25_model4_unweighted <- fread('FILEPATH/che25_compare_holdout_model4_unweighted.csv')
che25_model4_unweighted[, model := 'model6']

che25_model5_unweighted <- fread('FILEPATH/che25_compare_holdout_model5_unweighted.csv')
che25_model5_unweighted[, model := 'model7']

## models without constants
che10_model2_unweighted_no_constant <- fread('FILEPATH/che10_compare_holdout_model2_unweighted_no_constant.csv')
che10_model2_unweighted_no_constant[, model := 'model4']

che10_model3_unweighted_no_constant <- fread('FILEPATH/che10_compare_holdout_model3_unweighted_no_constant.csv')
che10_model3_unweighted_no_constant[, model := 'model5']

che25_model2_unweighted_no_constant <- fread('FILEPATH/che25_compare_holdout_model2_unweighted_no_constant.csv')
che25_model2_unweighted_no_constant[, model := 'model4']

che25_model3_unweighted_no_constant <- fread('FILEPATH/che25_compare_holdout_model3_unweighted_no_constant.csv')
che25_model3_unweighted_no_constant[, model := 'model5']

## create ensemble with constant 
che10_ensemble <- rbind(che10_model2_unweighted, che10_model3_unweighted)
che10_ensemble <- che10_ensemble[, .(che10_predict = mean(che10_predict)), by = c("ihme_loc_id", "year_id")]
che10_ensemble[, model := "ensemble"]

che25_ensemble <- rbind(che25_model2_unweighted, che25_model3_unweighted)
che25_ensemble <- che25_ensemble[, .(che25_predict = mean(che25_predict)), by = c("ihme_loc_id", "year_id")]
che25_ensemble[, model := "ensemble"]

## create ensemble with and without constant
che10_ensemble_no_constant <- rbind(che10_model2_unweighted, che10_model3_unweighted,
                                    che10_model2_unweighted_no_constant,che10_model3_unweighted_no_constant)
che10_ensemble_no_constant <- che10_ensemble_no_constant[, .(che10_predict = mean(che10_predict)), by = c("ihme_loc_id", "year_id")]
che10_ensemble_no_constant[, model := "ensemble_no_constant"]

che25_ensemble_no_constant <- rbind(che25_model2_unweighted, che25_model3_unweighted,
                                    che25_model2_unweighted_no_constant,che25_model3_unweighted_no_constant)
che25_ensemble_no_constant <- che25_ensemble_no_constant[, .(che25_predict = mean(che25_predict)), by = c("ihme_loc_id", "year_id")]
che25_ensemble_no_constant[, model := "ensemble_no_constant"]

## create ensemble: 2 & 4
che10_ensemble_2_4 <- rbind(che10_model2_unweighted,
                            che10_model2_unweighted_no_constant)
che10_ensemble_2_4 <- che10_ensemble_2_4[, .(che10_predict = mean(che10_predict)), by = c("ihme_loc_id", "year_id")]
che10_ensemble_2_4[, model := "ensemble_2_4"]

che25_ensemble_2_4 <- rbind(che25_model2_unweighted,
                            che25_model2_unweighted_no_constant)
che25_ensemble_2_4 <- che25_ensemble_2_4[, .(che25_predict = mean(che25_predict)), by = c("ihme_loc_id", "year_id")]
che25_ensemble_2_4[, model := "ensemble_2_4"]

## create ensemble: 4&5
che10_ensemble_4_5 <- rbind(che10_model2_unweighted_no_constant, che10_model3_unweighted_no_constant)
che10_ensemble_4_5 <- che10_ensemble_4_5[, .(che10_predict = mean(che10_predict)), by = c("ihme_loc_id", "year_id")]
che10_ensemble_4_5[, model := "ensemble_4_5"]

che25_ensemble_4_5 <- rbind(che25_model2_unweighted_no_constant,che25_model3_unweighted_no_constant)
che25_ensemble_4_5 <- che25_ensemble_4_5[, .(che25_predict = mean(che25_predict)), by = c("ihme_loc_id", "year_id")]
che25_ensemble_4_5[, model := "ensemble_4_5"]

## create ensemble: 1-6 weighted 
che10_ensemble_weighted <- rbind(che10_model1_unweighted, che10_model2_unweighted, che10_model3_unweighted,
                            che10_model2_unweighted_no_constant, che10_model3_unweighted_no_constant, che10_model4_unweighted)
che10_ensemble_weighted <- merge(che10_ensemble_weighted, che10_weights_long, by = "model")
che10_ensemble_weighted <- che10_ensemble_weighted[, .(che10_predict = weighted.mean(che10_predict, weight)), by = c("ihme_loc_id", "year_id")]
che10_ensemble_weighted[, model := "ensemble_weighted"]

che25_ensemble_weighted <- rbind(che25_model1_unweighted, che25_model2_unweighted, che25_model3_unweighted,
                                 che25_model2_unweighted_no_constant, che25_model3_unweighted_no_constant,che25_model4_unweighted)
che25_ensemble_weighted <- merge(che25_ensemble_weighted, che25_weights_long, by = "model")
che25_ensemble_weighted <- che25_ensemble_weighted[, .(che25_predict = weighted.mean(che25_predict, weight)), by = c("ihme_loc_id", "year_id")]
che25_ensemble_weighted[, model := "ensemble_weighted"]

## add observed values values onto ensembles
che_10_vals <- che10_model1_unweighted[, c("ihme_loc_id", "year_id", "che10")]
che_25_vals <- che25_model1_unweighted[, c("ihme_loc_id", "year_id", "che25")]

che10_ensemble <- merge(che10_ensemble, che_10_vals, by = c("ihme_loc_id", "year_id"))
che25_ensemble <- merge(che25_ensemble, che_25_vals, by = c("ihme_loc_id", "year_id"))

che10_ensemble_no_constant <- merge(che10_ensemble_no_constant, che_10_vals, by = c("ihme_loc_id", "year_id"))
che25_ensemble_no_constant <- merge(che25_ensemble_no_constant, che_25_vals, by = c("ihme_loc_id", "year_id"))

che10_ensemble_2_4 <- merge(che10_ensemble_2_4, che_10_vals, by = c("ihme_loc_id", "year_id"))
che25_ensemble_2_4 <- merge(che25_ensemble_2_4, che_25_vals, by = c("ihme_loc_id", "year_id"))

che10_ensemble_4_5 <- merge(che10_ensemble_4_5, che_10_vals, by = c("ihme_loc_id", "year_id"))
che25_ensemble_4_5 <- merge(che25_ensemble_4_5, che_25_vals, by = c("ihme_loc_id", "year_id"))

che10_ensemble_weighted <- merge(che10_ensemble_weighted, che_10_vals, by = c("ihme_loc_id", "year_id"))
che25_ensemble_weighted <- merge(che25_ensemble_weighted, che_25_vals, by = c("ihme_loc_id", "year_id"))

#combine
che10_models <- rbind(che10_model1_unweighted, che10_model2_unweighted,che10_model2_unweighted_no_constant,
                      che10_model3_unweighted, che10_model3_unweighted_no_constant,
                      che10_model4_unweighted, che10_model5_unweighted,che10_ensemble,che10_ensemble_no_constant,che10_ensemble_2_4, che10_ensemble_4_5, 
                      che10_ensemble_weighted, fill = T)

che25_models <- rbind(che25_model1_unweighted, che25_model2_unweighted, che25_model2_unweighted_no_constant,
                      che25_model3_unweighted, che25_model3_unweighted_no_constant,
                      che25_model4_unweighted, che25_model5_unweighted,che25_ensemble,che25_ensemble_no_constant, che25_ensemble_2_4, che25_ensemble_4_5,
                      che25_ensemble_weighted, fill = T)

setnames(che10_models, old = c("che10_predict", "che10"), new = c("che_predict", "che"))
setnames(che25_models, old = c("che25_predict", "che25"), new = c("che_predict", "che"))


che10_models[, stat := 10]
che25_models[, stat := 25]

che_both <- rbind(che10_models, che25_models)
che_both <- che_both[!(is.na(che)) & !(is.na(che_predict))]

# not normalized RMSE 
rmse_stat_not_normalized <- che_both[,.( RMSE_stat = sprintf('%.4f', round(mean(sqrt((che - che_predict)^2)),4))), by = c("stat", "model")]
rmse_both_not_normalized <- che_both[,.( RMSE_both = sprintf('%.4f', round(mean(sqrt((che - che_predict)^2)),4))), by = c( "model")]

rmse_stat_not_normalized <- rmse_stat_not_normalized[order(stat,RMSE_stat)]
rmse_both_not_normalized <- rmse_both_not_normalized[order(RMSE_both)]
rmse_both_not_normalized[, stat := "both"]
rmse_stat_wide_not_normalized <- dcast(rmse_stat_not_normalized, stat ~ model)
rmse_both_wide_not_normalized <- dcast(rmse_both_not_normalized, stat ~ model, value.var = "RMSE_both")

rmse_output_not_normalized <- rbind(rmse_stat_wide_not_normalized,rmse_both_wide_not_normalized )

# normalized RMSE 
rmse_stat <- che_both[,.( RMSE_stat = sprintf('%.4f', round(mean(sqrt((che - che_predict)^2)/che),4))), by = c("stat", "model")]

rmse_both <- che_both[,.( RMSE_both = sprintf('%.4f', round(mean(sqrt((che - che_predict)^2)/che),4))), by = c( "model")]

rmse_stat <- rmse_stat[order(stat,RMSE_stat)]
rmse_both <- rmse_both[order(RMSE_both)]
rmse_both[, stat := "both"]
rmse_stat_wide <- dcast(rmse_stat, stat ~ model)
rmse_both_wide <- dcast(rmse_both, stat ~ model, value.var = "RMSE_both")

rmse_output <- rbind(rmse_stat_wide,rmse_both_wide )

write.csv(weights_df, file.path(dir, 'weights_all_holdout.csv'), row.names = F)
write.csv(rmse_output, file.path(dir, 'pretty_rmse_output_constant_no_constant_all_holdout.csv'), row.names = F)
write.csv(rmse_output_not_normalized, file.path(dir, 'pretty_rmse_output_not_normalized_constant_no_constant_all_holdout.csv'), row.names = F)

