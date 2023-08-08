## Author: User                                                      ##
## Date: 10/10/2022                                                        ##
## Purpose: Predict 2020 CHE based on mean values in previous year with vcov: iterative approach for Russia predictions, no constant
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

adjust_factor <- 10000

## libraries
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2,dplyr, broom )

## read in draws
che10_draws <- fread(file.path(dir, 'all_draws_che10_10000_rate.csv'))
che25_draws <- fread(file.path(dir, 'all_draws_che25_10000_rate.csv'))

## read in mean df's
che10_mean <- fread(file.path(dir, 'che10_mean_df.csv'))
che25_mean <- fread(file.path(dir, 'che25_mean_df.csv'))

## calculate number of years outside of previous 5 that will be downweighted

numpoints <- che10_mean[, .(num_points = .N - 6), by = "ihme_loc_id" ]
predict_che10_russia <- function (current_ihme_loc_id){
  weight_num <- numpoints[ihme_loc_id == current_ihme_loc_id]$num_points
  che10_model <- data.table()
  m1 <- lm(che10 ~ lag_che-1, data = che10_mean[year_id != 2020 & ihme_loc_id == current_ihme_loc_id],weights =  c(c(0.25, 0.2, 0.15,0.075, 0.075), rep(0.025,weight_num)))
  nobs(m1)
  summary_m1 <- tidy(m1)
  set.seed(123)
  n.sim <- 1000
  # draw coefficients from the multivariate normal
  S <- MASS::mvrnorm(n.sim, coef(m1), vcov(m1))
  che10_model[, lag_che := S[,1] ]
  
  
  check <- che10_draws[ihme_loc_id == current_ihme_loc_id]
  check <- check[, c("lag_che", "draw", "year_id", "che10")]
  setnames(che10_model, old = "lag_che", new = "coeff")
  # fill in 2015 
  starting_predict <- data.table()
  starting_predict$lag_che <- check[year_id == 2014]$che10
  starting_predict$year_id <- 2015
  starting_predict$draw <- check[year_id == 2014]$draw
  
  missing_predictions <- data.table()
  temp <- cbind(starting_predict, che10_model)
  temp[, che10_predict := coeff * lag_che]
  missing_predictions <- rbind(missing_predictions,temp[, c("year_id", "che10_predict", "draw")])
  years <- c( 2016, 2017, 2018, 2019, 2020) # years to predict after last point of 2014
  for (year in years){ 
    current <- data.table()
    #check_t <- missing_predictions[year_id == 2015]
    current$lag_che <- missing_predictions[year_id == year-1]$che10_predict
    current$draw <- missing_predictions[year_id == year-1]$draw
    current$year_id <- year

    ## mix up rows 
    current <-  current[sample(1:nrow(current)), ]
    
    
    temp <- cbind(current, che10_model)
    temp[, che10_predict := coeff * lag_che]
    missing_predictions <- rbind(missing_predictions,temp[, c("year_id", "che10_predict", "draw")])
    
  }
  
  missing_predictions_2019 <- missing_predictions[year_id == 2019, c("che10_predict")] # extract these and save to use with diff model
  
  missing_predictions_2020 <- missing_predictions[year_id == 2020, c("che10_predict")]
  
  
  # take 2.5 and 97.5 percentiles
  che10_predictions <- data.table(apply(missing_predictions_2020[, c("che10_predict")], 2, quantile, probs = c(.025, .5, .975)))
  
  #take median
  median_rus <- median(missing_predictions_2020$che10_predict)
  
  che10_predictions[, ihme_loc_id := current_ihme_loc_id]
  che10_predictions <- dcast(che10_predictions, ihme_loc_id ~ che10_predict, value.var = "che10_predict")
  colnames(che10_predictions)[2:4] <- c("lower_che10", "mean_che10", "upper_che10")
  
  che10_predictions[, mean_che10 := median_rus]
  return_list  <- list(che10_predictions,missing_predictions_2019,missing_predictions_2020)
  
  return(return_list)
}

predict_che25_russia <- function (current_ihme_loc_id){
  weight_num <- numpoints[ihme_loc_id == current_ihme_loc_id]$num_points
  che25_model <- data.table()
  m1 <- lm(che25 ~ lag_che -1, data = che25_mean[year_id != 2020 & ihme_loc_id == current_ihme_loc_id],weights =  c(c(0.25, 0.2, 0.15,0.075, 0.075), rep(0.025,weight_num)))

  summary_m1 <- tidy(m1)
  nobs(m1)
  set.seed(123)
  n.sim <- 1000
  # draw coefficients from the multivariate normal
  S <- MASS::mvrnorm(n.sim, coef(m1), vcov(m1))
  che25_model[, lag_che := S[,1] ]
  
  
  check <- che25_draws[ihme_loc_id == current_ihme_loc_id]
  check <- check[, c("lag_che", "draw", "year_id", "che25")]
  setnames(che25_model, old = "lag_che", new = "coeff")
  
  # fill in 2015 
  starting_predict <- data.table()
  starting_predict$lag_che <- check[year_id == 2014]$che25
  starting_predict$year_id <- 2015
  starting_predict$draw <- check[year_id == 2014]$draw
  
  missing_predictions <- data.table()
  temp <- cbind(starting_predict, che25_model)
  temp[, che25_predict := coeff * lag_che]
  missing_predictions <- rbind(missing_predictions,temp[, c("year_id", "che25_predict", "draw")])
  
  years <- c( 2016, 2017, 2018, 2019, 2020) # years to predict after last point of 2014
  for (year in years){ 
    current <- data.table()
    #check_t <- missing_predictions[year_id == 2015]
    current$lag_che <- missing_predictions[year_id == year-1]$che25_predict
    current$draw <- missing_predictions[year_id == year-1]$draw
    current$year_id <- year

    ## mix up rows 
    current <-  current[sample(1:nrow(current)), ]
    
    temp <- cbind(current, che25_model)
    temp[, che25_predict := coeff * lag_che]
    missing_predictions <- rbind(missing_predictions,temp[, c("year_id", "che25_predict", "draw")])
    
  }
  
  missing_predictions_2019 <- missing_predictions[year_id == 2019, c("che25_predict")] # extract these and save to use with diff model
  
  missing_predictions_2020 <- missing_predictions[year_id == 2020, c("che25_predict")]
  
  
  # take 2.5 and 97.5 percentiles
  che25_predictions <- data.table(apply(missing_predictions_2020[, c("che25_predict")], 2, quantile, probs = c(.025, .5, .975)))
  
  # take median
  median_rus <- median(missing_predictions_2020$che25_predict)
  che25_predictions[, ihme_loc_id := current_ihme_loc_id]
  che25_predictions <- dcast(che25_predictions, ihme_loc_id ~ che25_predict, value.var = "che25_predict")
  colnames(che25_predictions)[2:4] <- c("lower_che25", "mean_che25", "upper_che25")
  che25_predictions[, mean_che25 := median_rus]
  return_list  <- list(che25_predictions,missing_predictions_2019,missing_predictions_2020)
  
  return(return_list)
}

predict_che10 <- function (current_ihme_loc_id){
  weight_num <- numpoints[ihme_loc_id == current_ihme_loc_id]$num_points
  che10_model <- data.table()
  m1 <- lm(che10 ~ lag_che -1, data = che10_mean[year_id != 2020 & ihme_loc_id == current_ihme_loc_id])
  nobs(m1)

  summary_m1 <- tidy(m1)
  set.seed(123)
  n.sim <- 1000
  # draw coefficients from the multivariate normal
  S <- MASS::mvrnorm(n.sim, coef(m1), vcov(m1))
  che10_model[, lag_che := S[,1] ]
  
  
  check <- che10_draws[ihme_loc_id == current_ihme_loc_id]
  check <- check[, c("lag_che", "draw", "year_id", "che10")]
  setnames(che10_model, old = "lag_che", new = "coeff")
  predictions <- data.table()
  
  #new_data <- check[draw == i]
  temp <- cbind(check, che10_model)
  temp[, che10_predict := coeff * lag_che]
  predictions <- rbind(predictions, temp[year_id ==2020, c('che10_predict') ] )
  
  
  # take 2.5 and 97.5 percentiles
  che10_predictions <- data.table(apply(predictions[, c("che10_predict")], 2, quantile, probs = c(.025, .5, .975)))
  che10_predictions[, ihme_loc_id := current_ihme_loc_id]
  che10_predictions <- dcast(che10_predictions, ihme_loc_id ~ che10_predict, value.var = "che10_predict")
  colnames(che10_predictions)[2:4] <- c("lower_che10", "mean_che10", "upper_che10")
  
  return_list  <- list(che10_predictions,predictions)
  
  return(return_list)
}

predict_che25 <- function (current_ihme_loc_id){
  weight_num <- numpoints[ihme_loc_id == current_ihme_loc_id]$num_points
  che25_model <- data.table()
  m1 <- lm(che25 ~ lag_che -1, data = che25_mean[year_id != 2020 & ihme_loc_id == current_ihme_loc_id])
  nobs(m1)
 
  
  summary_m1 <- tidy(m1)
  set.seed(123)
  n.sim <- 1000
  # draw coefficients from the multivariate normal
  S <- MASS::mvrnorm(n.sim, coef(m1), vcov(m1))
  che25_model[, lag_che := S[,1] ]
  
  
  check <- che25_draws[ihme_loc_id == current_ihme_loc_id]
  check <- check[, c("lag_che", "draw", "year_id", "che25")]
  setnames(che25_model, old = "lag_che", new = "coeff")
  predictions <- data.table()
  
  
  temp <- cbind(check, che25_model)
  temp[, che25_predict := coeff * lag_che]
  predictions <- rbind(predictions, temp[year_id ==2020, c('che25_predict') ] )
  
  
  # take 2.5 and 97.5 percentiles
  che25_predictions <- data.table(apply(predictions[, c("che25_predict")], 2, quantile, probs = c(.025, .5, .975)))
  che25_predictions[, ihme_loc_id := current_ihme_loc_id]
  che25_predictions <- dcast(che25_predictions, ihme_loc_id ~ che25_predict, value.var = "che25_predict")
  colnames(che25_predictions)[2:4] <- c("lower_che25", "mean_che25", "upper_che25")
  
  return_list  <- list(che25_predictions,predictions)
  
  return(return_list)}

vnm_che10_predict <- predict_che10("VNM")[[1]]
per_che10_predict <- predict_che10("PER")[[1]]
mex_che10_predict <- predict_che10("MEX")[[1]]
blr_che10_predict <- predict_che10("BLR")[[1]]
rus_che10_predict <- predict_che10_russia("RUS")[[1]]

vnm_che25_predict <- predict_che25("VNM")[[1]]
per_che25_predict <- predict_che25("PER")[[1]]
mex_che25_predict <- predict_che25("MEX")[[1]]
blr_che25_predict <- predict_che25("BLR")[[1]]
rus_che25_predict <- predict_che25_russia("RUS")[[1]]

che10_predictions <- do.call("rbind", list(vnm_che10_predict, per_che10_predict, mex_che10_predict, blr_che10_predict, rus_che10_predict))
che25_predictions <- do.call("rbind", list(vnm_che25_predict, per_che25_predict, mex_che25_predict, blr_che25_predict, rus_che25_predict ))

#create percent
che10_predictions[, che10_pct := mean_che10*100]
che25_predictions[, che25_pct := mean_che25*100]

## add other necessary columns
che10_predictions[, year_id := 2020]
che10_predictions[, predict := "Prediction"]

che25_predictions[, year_id := 2020]
che25_predictions[, predict := "Prediction"]

#adjust names
colnames(che10_predictions)[2:4] <- c("lower","che10", "upper")
colnames(che25_predictions)[2:4] <- c("lower","che25", "upper")

# save predictions
write.csv(che10_predictions,file.path(dir, 'che10_predict_10000_adjust_mean_vcov_no_2006_iter_russia_median_no_constant.csv'), row.names = F)
write.csv(che25_predictions,file.path(dir, 'che25_predict_10000_adjust_mean_vcov_no_2006_iter_russia_median_no_constant.csv'), row.names = F)

# save 2019 predictions for Russia to use with diff model 
rus_che10_predict_2019 <-predict_che10_russia("RUS")[[2]]
rus_che25_predict_2019 <- predict_che25_russia("RUS")[[2]]

write.csv(rus_che10_predict_2019,file.path(dir, 'predict_2019_russia_che10_no_constant.csv'), row.names = F)
write.csv(rus_che25_predict_2019,file.path(dir, 'predict_2019_russia_che25_no_constant.csv'), row.names = F)

## save draws for ensemble model
vnm_che10_predict_draws <- predict_che10("VNM")[[2]]
vnm_che10_predict_draws[, ihme_loc_id := "VNM"]

per_che10_predict_draws <- predict_che10("PER")[[2]]
per_che10_predict_draws[, ihme_loc_id := "PER"]

mex_che10_predict_draws <- predict_che10("MEX")[[2]]
mex_che10_predict_draws[, ihme_loc_id := "MEX"]

blr_che10_predict_draws <- predict_che10("BLR")[[2]]
blr_che10_predict_draws[, ihme_loc_id := "BLR"]

rus_che10_predict_draws <- predict_che10_russia("RUS")[[2]]  
rus_che10_predict_draws[, ihme_loc_id := "RUS"]

vnm_che25_predict_draws <- predict_che25("VNM")[[2]]
vnm_che25_predict_draws[, ihme_loc_id := "VNM"]

per_che25_predict_draws <- predict_che25("PER")[[2]]
per_che25_predict_draws[, ihme_loc_id := "PER"]

mex_che25_predict_draws <- predict_che25("MEX")[[2]]
mex_che25_predict_draws[, ihme_loc_id := "MEX"]

blr_che25_predict_draws <- predict_che25("BLR")[[2]]
blr_che25_predict_draws[, ihme_loc_id := "BLR"]

rus_che25_predict_draws <- predict_che25_russia("RUS")[[2]]  
rus_che25_predict_draws[, ihme_loc_id := "RUS"]

che10_predict_draws <- do.call("rbind", list(vnm_che10_predict_draws, per_che10_predict_draws, mex_che10_predict_draws, blr_che10_predict_draws, rus_che10_predict_draws))
che25_predict_draws <- do.call("rbind", list(vnm_che25_predict_draws, per_che25_predict_draws, mex_che25_predict_draws, blr_che25_predict_draws, rus_che25_predict_draws ))

write.csv(che10_predict_draws,file.path(dir, 'che10_predict_draws_lag_level_no_constant.csv'), row.names = F)
write.csv(che25_predict_draws,file.path(dir, 'che25_predict_draws_lag_level_no_constant.csv'), row.names = F)
