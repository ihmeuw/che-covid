## Author: User                                                       ##
## Date: 10/10/2022                                                       ##
## Purpose: Create draws of distribution of mean for each country   ##
## SE, CHE adjusted by 10,000
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

dir <- "/FILEPATH"

adjust_factor <- 10000

## libraries
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2,dplyr, broom )


#Use each model to predict 2020 and plot and compare to 2020 estimated value

# read in df with se and sd
se_sd <- fread('FILEPATH/che5years_plot_df_estimates_ui_model2.csv')[, c("ihme_loc_id", "year_id", "se_che10", "se_che25")]

#read in peru che data
peru_che10 <- fread('FILEPATH/peru_che_10_updated_sep_2022.csv')[,-c("V1")]
peru_che25 <- fread('FILEPATH/peru_che_25_updated_sep_2022.csv')[,-c("V1")]
peru_che10 <- peru_che10[order(-year_id)]
peru_che25 <- peru_che25[order(-year_id)]

#read in mexico che data
mexico_che10 <- fread('FILEPATH/mexico_che10_updated_aug_2022.csv')[location_id==130 & year_id > 2004 ]
mexico_che25 <-  fread('FILEPATH/mexico_che25_updated_aug_2022.csv')[location_id==130& year_id > 2004]
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
che10_df <- che10_df[, lag_che := lag (che10), by = c("ihme_loc_id") ] 
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

# re order for regression models
che25_df <- che25_df[order(-year_id)]

#count how many points we have per country (minus the original 5 in the regression that are more heavily weighted)
numpoints <- che10_df[, .(num_points = .N - 6), by = "ihme_loc_id" ]

# predict 2020 values to compare with estimates
# che value
che10_df_predict <- che10_df %>%
  mutate(che10_predict = ifelse(year_id == 2020, NA, che10))
che25_df_predict <- che25_df %>%
  mutate(che25_predict = ifelse(year_id == 2020, NA, che25))

## add on standard error
che10_df_predict <- merge(che10_df_predict, se_sd, by = c("ihme_loc_id", 'year_id'))
che25_df_predict <- merge(che25_df_predict, se_sd, by = c("ihme_loc_id", 'year_id'))

#multiply by 10000
adjust_cols <- colnames(che10_df_predict)[3:7]
che10_df_predict[, (adjust_cols) := lapply(.SD,  function(x) x*adjust_factor), .SDcols = adjust_cols]

adjust_cols <- colnames(che25_df_predict)[3:7]
che25_df_predict[, (adjust_cols) := lapply(.SD,  function(x) x*adjust_factor), .SDcols = adjust_cols]

## create variance
che10_df_predict[, var_che10 := se_che10]
che25_df_predict[, var_che25 := se_che25]

## define functions for creating draws for che10 and che25
create_che10_draws <- function(current_ihme_loc_id) {
  norm_che10 <- data.table()
  norm_che10_lag <- data.table()
  years <- unique(che10_df_predict[ihme_loc_id == current_ihme_loc_id & year_id]$year_id)
  for (index in 1:length(years)){
    temp <- data.table(
      
    )
    lag_temp <- data.table(
      
    )
    current <- che10_df_predict[ihme_loc_id ==current_ihme_loc_id & year_id == years[index]]
    dist <- data.table(rnorm(1000, current$che10, current$var_che10))
    temp$che10 <- dist
    
    temp[, year_id := years[index]]
    
    
    lag_temp$lag_che <- dist 
    lag_temp[, year_id := years[index+1]]
    lag_temp$draw <- seq.int(nrow(lag_temp))
    norm_che10_lag <- rbind(norm_che10_lag, lag_temp)
    
    
    
    temp$draw <- seq.int(nrow(temp))
    norm_che10 <- rbind(norm_che10, temp)
  }
  
  norm_che10[, ihme_loc_id := current_ihme_loc_id]
  norm_che10_lag[, ihme_loc_id := current_ihme_loc_id]
  
  norm_che10 <- merge(norm_che10,norm_che10_lag, by = c("year_id", "ihme_loc_id", "draw"), all.x = T )
  return(norm_che10)
  
}

create_che25_draws <- function(current_ihme_loc_id) {
  norm_che25 <- data.table()
  norm_che25_lag <- data.table()
  years <- unique(che25_df_predict[ihme_loc_id == current_ihme_loc_id & year_id]$year_id)
  
  
  for (index in 1:length(years)){
    temp <- data.table(
      
    )
    lag_temp <- data.table(
      
    )
    current <- che25_df_predict[ihme_loc_id ==current_ihme_loc_id & year_id == years[index]]
    dist <- data.table(rnorm(1000, current$che25, current$var_che25))
    temp$che25 <- dist
    
    temp[, year_id := years[index]]
    
    
    lag_temp$lag_che <- dist 
    lag_temp[, year_id := years[index+1]]
    lag_temp$draw <- seq.int(nrow(lag_temp))
    norm_che25_lag <- rbind(norm_che25_lag, lag_temp)
    
    
    
    temp$draw <- seq.int(nrow(temp))
    norm_che25 <- rbind(norm_che25, temp)
  }
  
  norm_che25[, ihme_loc_id := current_ihme_loc_id]
  norm_che25_lag[, ihme_loc_id := current_ihme_loc_id]
  
  norm_che25 <- merge(norm_che25,norm_che25_lag, by = c("year_id", "ihme_loc_id", "draw"), all.x = T )
  return(norm_che25)
  
}


per_che10 <- create_che10_draws("PER")
mex_che10 <- create_che10_draws("MEX")
vnm_che10 <- create_che10_draws("VNM")
rus_che10 <- create_che10_draws("RUS")
blr_che10 <- create_che10_draws("BLR")

che10_draws <- do.call("rbind", list(per_che10, mex_che10, vnm_che10, rus_che10,blr_che10 ))


per_che25 <- create_che25_draws("PER")
mex_che25 <- create_che25_draws("MEX")
vnm_che25 <- create_che25_draws("VNM")
rus_che25 <- create_che25_draws("RUS")
blr_che25 <- create_che25_draws("BLR")

che25_draws <- do.call("rbind", list(per_che25, mex_che25, vnm_che25, rus_che25,blr_che25 ))

write.csv(che10_draws,file.path(dir, 'all_draws_che10_10000_rate.csv'), row.names = F)
write.csv(che25_draws, file.path(dir, 'all_draws_che25_10000_rate.csv'), row.names = F)


## save mean df's
write.csv(che10_df_predict,file.path(dir, 'che10_mean_df.csv'), row.names = F)
write.csv(che25_df_predict, file.path(dir, 'che25_mean_df.csv'), row.names = F)

