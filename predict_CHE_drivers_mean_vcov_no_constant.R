## Author: User                                                       ##
## Date: 11/11/2022                                                        ##
## Purpose: Predict 2020 CHE based on mean values in previous year with vcov: value ~ lag value, no constant
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

## libraries
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2,dplyr, broom )

## create not in function
'%nin%' <- function(x,y)!('%in%'(x,y))

## read in draws
che_driver_draws <- fread(file.path(dir, 'che_driver_draws.csv'))

## read in mean df
che_driver_means <- fread(file.path(dir, "che_drivers_means.csv"))

# calc lag value
che_driver_means <- che_driver_means[order(variable, ihme_loc_id, year_id)]
che_driver_means[, lag_value := lag(value), by = c("ihme_loc_id", "variable")]

predict_var <- function (current_ihme_loc_id, current_var){
  driver_model <- data.table()
  m1 <- lm(value ~ lag_value -1, data = che_driver_means[year_id != 2020 & ihme_loc_id == current_ihme_loc_id & variable == current_var])

  nobs(m1)
  summary(m1)
  set.seed(123)
  n.sim <- 1000
  
  # draw coefficients from the multivariate normal
  S <- MASS::mvrnorm(n.sim, coef(m1), vcov(m1)) ## this might not work for visits in mexico 
  driver_model[, lag_che := S[,1] ]

  
  check <- che_driver_draws[ihme_loc_id == current_ihme_loc_id & variable == current_var]
  check <- check[, c("lag_value", "draw", "year_id", "value")]
  setnames(driver_model, old = "lag_che", new = "coeff")
  predictions <- data.table()
  nrow(check)
  nrow(driver_model)
  
  temp <- cbind(check, driver_model)
  temp[, value_predict := coeff * lag_value]
  predictions <- rbind(predictions, temp[year_id ==2020, c('value_predict') ] )
  predictions[, ihme_loc_id := current_ihme_loc_id]
  predictions[, variable := current_var]
  
  # take 2.5 and 97.5 percentiles
  value_predictions <- data.table(apply(predictions[, c("value_predict")], 2, quantile, probs = c(.025, .5, .975)))
  value_predictions[, ihme_loc_id := current_ihme_loc_id]
  value_predictions[, variable := current_var]
  value_predictions <- dcast(value_predictions, ihme_loc_id ~ value_predict, value.var = "value_predict")
  colnames(value_predictions)[2:4] <- c("lower_value", "mean_value", "upper_value")
  
  return_list  <- list(value_predictions,predictions)
  
  return(return_list)
}

# Predict drivers Peru
# figure a vars
per_consumption <- predict_var("PER", "consumption")
per_health <- predict_var("PER", "health")
per_any_coverage <- predict_var("PER", "any_coverage")

# visits 
per_visits <- predict_var("PER", "any_visit")
per_private <- predict_var("PER", "is_private")
per_share_private <- predict_var("PER", "share_private")

# Predict drivers Mexico
# figure a vars
mex_consumption <- predict_var("MEX", "consumption")
mex_health <- predict_var("MEX", "health")
mex_any_coverage <- predict_var("MEX", "any_coverage")

## save draws for ensemble model 
figure_a_draws <- do.call("rbind", list(per_consumption[[2]], per_health[[2]], per_any_coverage[[2]], 
                                        mex_consumption[[2]], mex_health[[2]], mex_any_coverage[[2]]))

write.csv(figure_a_draws,file.path(dir, 'figure_a_draws_lag_model_no_constant.csv'), row.names = F)

visits_draws <- do.call("rbind", list(per_visits[[2]], per_private[[2]], per_share_private[[2]]))
write.csv(visits_draws,file.path(dir, 'per_visits_draws_lag_model_no_constant.csv'), row.names = F)

