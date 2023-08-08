## Author: User                                                       ##
## Date: 9/13/2022                                                            ##
## Purpose: Compare RMSE across che prediction models           ##
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

## libraries
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2,dplyr, broom)

##directory
dir <- 'FILEPATH'

# read in weights(created in previous run of this file)
weights_df <- fread( file.path(dir, 'weights_rmse.csv'))

che10_weights <- weights_df[stat == 10, c(6:12)]
che10_weights_long <- melt(setDT(che10_weights), variable.name = "model", value.name ="weight")

che25_weights <- weights_df[stat == 25, c(6:12)]
che25_weights_long <- melt(setDT(che25_weights), variable.name = "model", value.name ="weight")

# read in unweighted che 10 
che10_model1_unweighted <- fread('FILEPATH/che10_compare_model1_unweighted.csv')
che10_model1_unweighted[, model := 'model1']

che10_model2_unweighted <- fread('FILEPATH/che10_compare_model2_unweighted.csv')
che10_model2_unweighted[, model := 'model2']

che10_model3_unweighted <- fread('FILEPATH/che10_compare_model3_unweighted.csv')
che10_model3_unweighted[, model := 'model3']

che10_model4_unweighted <- fread('FILEPATH/che10_compare_model4_unweighted.csv')
che10_model4_unweighted[, model := 'model6']

che10_model5_unweighted <- fread('FILEPATH/che10_compare_model5_unweighted.csv')
che10_model5_unweighted[, model := 'model7']

## no constant models che 10
che10_model2_unweighted_no_constant <- fread('FILEPATH/che10_compare_model2_unweighted_no_constant.csv')
che10_model2_unweighted_no_constant[, model := 'model4']

che10_model3_unweighted_no_constant <- fread('FILEPATH/che10_compare_model3_unweighted_no_constant.csv')
che10_model3_unweighted_no_constant[, model := 'model5']


# read in unweighted che 25
che25_model1_unweighted <- fread('FILEPATH/che25_compare_model1_unweighted.csv')
che25_model1_unweighted[, model := 'model1']

che25_model2_unweighted <- fread('FILEPATH/che25_compare_model2_unweighted.csv')
che25_model2_unweighted[, model := 'model2']

che25_model3_unweighted <- fread('FILEPATH/che25_compare_model3_unweighted.csv')
che25_model3_unweighted[, model := 'model3']

che25_model4_unweighted <- fread('FILEPATH/che25_compare_model4_unweighted.csv')
che25_model4_unweighted[, model := 'model6']

che25_model5_unweighted <- fread('FILEPATH/che25_compare_model5_unweighted.csv')
che25_model5_unweighted[, model := 'model7']

## no constant models che 25
che25_model2_unweighted_no_constant <- fread('FILEPATH/che25_compare_model2_unweighted_no_constant.csv')
che25_model2_unweighted_no_constant[, model := 'model4']

che25_model3_unweighted_no_constant <- fread('FILEPATH/che25_compare_model3_unweighted_no_constant.csv')
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

## read in prepped data
che10_5years_plot_df <- fread('/ihme/scratch/projects/hssa/che/results/che10_plot_df_sep_2022_model2.csv')[predict == "Estimate from surveys or HEFPI"]
che25_5years_plot_df <- fread('/ihme/scratch/projects/hssa/che/results/che25_plot_df_sep_2022_model2.csv')[predict == "Estimate from surveys or HEFPI"]

che10_ensemble <- merge(che10_ensemble, che10_5years_plot_df[, c("ihme_loc_id", "year_id", "che10")], by = c("ihme_loc_id", "year_id"))
che25_ensemble <- merge(che25_ensemble, che25_5years_plot_df[, c("ihme_loc_id", "year_id", "che25")], by = c("ihme_loc_id", "year_id"))

che10_ensemble_no_constant <- merge(che10_ensemble_no_constant, che10_5years_plot_df[, c("ihme_loc_id", "year_id", "che10")], by = c("ihme_loc_id", "year_id"))
che25_ensemble_no_constant <- merge(che25_ensemble_no_constant, che25_5years_plot_df[, c("ihme_loc_id", "year_id", "che25")], by = c("ihme_loc_id", "year_id"))

che10_ensemble_2_4 <- merge(che10_ensemble_2_4, che10_5years_plot_df[, c("ihme_loc_id", "year_id", "che10")], by = c("ihme_loc_id", "year_id"))
che25_ensemble_2_4 <- merge(che25_ensemble_2_4, che25_5years_plot_df[, c("ihme_loc_id", "year_id", "che25")], by = c("ihme_loc_id", "year_id"))

che10_ensemble_4_5 <- merge(che10_ensemble_4_5, che10_5years_plot_df[, c("ihme_loc_id", "year_id", "che10")], by = c("ihme_loc_id", "year_id"))
che25_ensemble_4_5 <- merge(che25_ensemble_4_5, che25_5years_plot_df[, c("ihme_loc_id", "year_id", "che25")], by = c("ihme_loc_id", "year_id"))

che10_ensemble_weighted<- merge(che10_ensemble_weighted, che10_5years_plot_df[, c("ihme_loc_id", "year_id", "che10")], by = c("ihme_loc_id", "year_id"))
che25_ensemble_weighted <- merge(che25_ensemble_weighted, che25_5years_plot_df[, c("ihme_loc_id", "year_id", "che25")], by = c("ihme_loc_id", "year_id"))


#combine
che10_models <- rbind(che10_model1_unweighted, che10_model2_unweighted,che10_model2_unweighted_no_constant,
                      che10_model3_unweighted, che10_model3_unweighted_no_constant,
                      che10_model4_unweighted, che10_model5_unweighted,che10_ensemble,che10_ensemble_no_constant,che10_ensemble_2_4, che10_ensemble_4_5,
                      che10_ensemble_weighted, fill = T)

che25_models <- rbind(che25_model1_unweighted, che25_model2_unweighted, che25_model2_unweighted_no_constant,
                      che25_model3_unweighted, che25_model3_unweighted_no_constant,
                      che25_model4_unweighted, che25_model5_unweighted,che25_ensemble,che25_ensemble_no_constant, che25_ensemble_2_4,che25_ensemble_4_5, 
                      che25_ensemble_weighted, fill = T)

## calculate RMSE
che10_models <- che10_models[year_id !=2020]
che25_models <- che25_models[year_id !=2020]


colnames(che10_models)[3] <- "che_predict"
colnames(che10_models)[6] <- "che"

colnames(che25_models)[3] <- "che_predict"
colnames(che25_models)[6] <- "che"

che10_models[, stat := 10]
che25_models[, stat := 25]

che_both <- rbind(che10_models, che25_models)

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
## save weights 
func.inverse <- function(arg1){ return(1-arg1)}
weights_df <- copy(rmse_output)
cols.num <- colnames(weights_df)[2:12]
weights_df[, c(2:12)] <- data.table(sapply(weights_df[,c(2:12)],as.numeric))
weights_df[, (cols.num) := lapply(.SD,func.inverse), .SDcols = eval(cols.num)]

write.csv(weights_df, file.path(dir, 'weights_rmse.csv'), row.names = F)

write.csv(rmse_output, file.path(dir, 'pretty_rmse_output_constant_no_constant.csv'), row.names = F)
write.csv(rmse_output_not_normalized, file.path(dir, 'pretty_rmse_output_not_normalized_constant_no_constant.csv'), row.names = F)

