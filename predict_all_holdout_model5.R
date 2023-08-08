## Author: User                                                       ##
## Purpose: Predict out of sample each year CHE using diff ~ lag diff + lag diff (2), unweighted  (model 5)             ##
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

# create not in function
`%nin%` <- Negate(`%in%`)


# Use each model to predict given year and compare to estimated value
# Model 1: level ~ year (unweighted)

#read in peru che data
peru_che10 <- fread('FILEPATH/peru_che_10_updated_sep_2022.csv')[,-c("V1")]
peru_che25 <- fread('FILEPATH/peru_che_25_updated_sep_2022.csv')[,-c("V1")]
peru_che10 <- peru_che10[order(-year_id)]
peru_che25 <- peru_che25[order(-year_id)]

#read in mexico che data
mexico_che10 <- fread('FILEPATH/mexico_che10_updated_aug_2022.csv')[location_id==130 & year_id > 2004]
mexico_che25 <-  fread('FILEPATH/mexico_che25_updated_aug_2022.csv')[location_id==130 & year_id > 2004]
mexico_che10 <- mexico_che10[order(-year_id)]
mexico_che25 <- mexico_che25[order(-year_id)]

#read in belarus che data
belarus_che10 <- fread('FILEPATH/hefpi_21.csv')[location_id==57]
belarus_che25 <- fread('FILEPATH/hefpi_21_25.csv')[location_id==57]
belarus_che10 <- belarus_che10[order(-year_id)]
belarus_che25 <- belarus_che25[order(-year_id)]

#read in Russia che data
russia_che10 <- fread('FILEPATH/hefpi_21.csv')[location_id==62]
russia_che25 <- fread('FILEPATH/hefpi_21_25.csv')[location_id==62]

## manually add 2020 value from WHO monitoring report
## che10
russia_2020_che10 <- russia_che10[12,]
russia_2020_che10[, year_id := 2020]
russia_2020_che10[, val := 0.077]

russia_che10 <- rbind(russia_che10, russia_2020_che10)

## che25
russia_2020_che25 <- russia_che25[12,]
russia_2020_che25[, year_id := 2020]
russia_2020_che25[, val := 0.009]

russia_che25 <- rbind(russia_che25, russia_2020_che25)

russia_che10 <- russia_che10[order(-year_id)]
russia_che25 <- russia_che25[order(-year_id)]

#read in Vietnam che data
vietnam_che10 <- fread('FILEPATH/hefpi_21.csv')[location_id==20]
vietnam_che25 <- fread('FILEPATH/hefpi_21_25.csv')[location_id==20]

## manually add 2020 value from WHO monitoring report
## che10
vietnam_2020_che10 <- vietnam_che10[12,]
vietnam_2020_che10[, year_id := 2020]
vietnam_2020_che10[, val := 0.085]

vietnam_che10 <- rbind(vietnam_che10, vietnam_2020_che10)

## che25
vietnam_2020_che25 <- vietnam_che25[12,]
vietnam_2020_che25[, year_id := 2020]
vietnam_2020_che25[, val := 0.017]

vietnam_che25 <- rbind(vietnam_che25, vietnam_2020_che25)

vietnam_che10 <- vietnam_che10[order(-year_id)]
vietnam_che25 <- vietnam_che25[order(-year_id)]



## CHE 10
colnames(mexico_che10)[2] <- "che10"
mexico_che10 <- mexico_che10 %>% 
  mutate(ihme_loc_id = "MEX")
colnames(belarus_che10)[2] <- "che10"
belarus_che10 <- belarus_che10 %>% 
  mutate(ihme_loc_id = "BLR")
colnames(russia_che10)[2] <- "che10"
russia_che10 <- russia_che10 %>% 
  mutate(ihme_loc_id = "RUS")
colnames(vietnam_che10)[2] <- "che10"
vietnam_che10 <- vietnam_che10 %>% 
  mutate(ihme_loc_id = "VNM")

che10_df <- rbind(peru_che10[,c("year_id", "che10", "ihme_loc_id")],
                  mexico_che10[,c("year_id", "che10", "ihme_loc_id")],
                  belarus_che10[,c("year_id", "che10", "ihme_loc_id")],
                  russia_che10[,c("year_id", "che10", "ihme_loc_id")],
                  vietnam_che10[,c("year_id", "che10", "ihme_loc_id")])
che10_df <- che10_df[order(ihme_loc_id, year_id)]
che10_df[, lag_che := lag (che10), by = c("ihme_loc_id") ] 
che10_df[, che_diff := (che10 -  lag (che10))/(year_id - lag(year_id)), by = c("ihme_loc_id") ] # year standardized diff 
che10_df[, lag_diff := lag (che_diff), by = c("ihme_loc_id") ] 
che10_df[, lag_diff_2 := lag (lag_diff), by = c("ihme_loc_id") ] 

# re order for regression models
che10_df <- che10_df[order(-year_id)]

## CHE 25
colnames(mexico_che25)[2] <- "che25"
mexico_che25 <- mexico_che25 %>% 
  mutate(ihme_loc_id = "MEX")
colnames(belarus_che25)[2] <- "che25"
belarus_che25 <- belarus_che25 %>% 
  mutate(ihme_loc_id = "BLR")
colnames(russia_che25)[2] <- "che25"
russia_che25 <- russia_che25 %>% 
  mutate(ihme_loc_id = "RUS")
colnames(vietnam_che25)[2] <- "che25"
vietnam_che25 <- vietnam_che25 %>% 
  mutate(ihme_loc_id = "VNM")

che25_df <- rbind(peru_che25[,c("year_id", "che25", "ihme_loc_id")],
                  mexico_che25[,c("year_id", "che25", "ihme_loc_id")],
                  belarus_che25[,c("year_id", "che25", "ihme_loc_id")],
                  russia_che25[,c("year_id", "che25", "ihme_loc_id")],
                  vietnam_che25[,c("year_id", "che25", "ihme_loc_id")])
che25_df <- che25_df[order(ihme_loc_id, year_id)]
che25_df <- che25_df[, lag_che := lag (che25), by = c("ihme_loc_id") ]
che25_df <- che25_df[, che_diff :=che25 -  lag (che25), by = c("ihme_loc_id") ] 
che25_df <- che25_df[, lag_diff := lag (che_diff), by = c("ihme_loc_id") ] 
che25_df[, lag_diff_2 := lag (lag_diff), by = c("ihme_loc_id") ] 

# re order for regression models
che25_df <- che25_df[order(-year_id)]

## filter to 2004 and later
che10_df <- che10_df[year_id > 2003]
che25_df <- che25_df[year_id > 2003]

# ## function to add 2020 prediction with uncertainty
predict_cur_2020_uncertainty <- function(df, model, var, cur_year){
  new.dat <- df[year_id %in% c(cur_year)]
  predict_cur_20 <- data.frame(predict(model, newdata = new.dat, interval = 'confidence'))
  x <- which( colnames(df)== eval(var))
  y <- which(df$year_id == eval(cur_year))
  
  df[y, x] <- predict_cur_20[[1,1]]
  
  df[, lower := ifelse(year_id == cur_year,  predict_cur_20[[1,2]],NA) ]
  df[, upper := ifelse(year_id == cur_year, predict_cur_20[[1,3]],NA) ]
  return(df[year_id == eval(cur_year)])
}

predict_holdouts <- function(loc) {
  predictions_che10 <- data.table()
  predictions_che25 <- data.table()
  
  data_che10 <- che10_df[ihme_loc_id == `loc`]
  data_che25 <- che25_df[ihme_loc_id == `loc`]
  
  years <- data.table(unique(data_che10$year_id))
  years <- years[V1 != 2020]
  for (year in unique(years$V1)){
    temp_lm_che10 <- lm(che_diff ~ lag_diff + lag_diff_2, data = data_che10[year_id %nin% c(`year`,2020)])
    temp_lm_che25 <- lm(che_diff ~ lag_diff + lag_diff_2, data = data_che25[year_id %nin% c(`year`,2020)])
    
    che10_df_predict_temp<- data_che10 %>%
      mutate(che10_predict = ifelse(year_id %in% c(`year`), NA, che10))
    che25_df_predict_temp<- data_che25 %>%
      mutate(che25_predict = ifelse(year_id %in% c(`year`), NA, che25))
    
    temp_che10_predict <- predict_cur_2020_uncertainty(che10_df_predict_temp, temp_lm_che10, "che10_predict", `year`)
    temp_che25_predict <- predict_cur_2020_uncertainty(che25_df_predict_temp, temp_lm_che25, "che25_predict", `year`)
    
    predictions_che10 <- rbind(predictions_che10, temp_che10_predict)
    predictions_che25 <- rbind(predictions_che25, temp_che25_predict)
  }
  return_list <- list(as.data.table(predictions_che10), as.data.table(predictions_che25))
  return(return_list)
}
vnm_predict <- predict_holdouts("VNM")
per_predict <- predict_holdouts("PER")
mex_predict <- predict_holdouts("MEX")
rus_predict <- predict_holdouts("RUS")
blr_predict <- predict_holdouts("BLR")

che10_compare <- rbind(vnm_predict[[1]], per_predict[[1]], mex_predict[[1]], rus_predict[[1]], blr_predict[[1]])
che25_compare <- rbind(vnm_predict[[2]], per_predict[[2]], mex_predict[[2]], rus_predict[[2]], blr_predict[[2]])

## add predicted diff to lag CHE value
che10_compare[, che10_predict := che10_predict + lag_che]
che25_compare[, che25_predict := che25_predict + lag_che]

#save CSV's 
write.csv(che10_compare,'FILEPATH/che10_compare_holdout_model5_unweighted.csv', row.names = F)
write.csv(che25_compare,'FILEPATH/che25_compare_holdout_model5_unweighted.csv', row.names = F)
