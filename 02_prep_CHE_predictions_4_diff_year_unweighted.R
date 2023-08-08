## Author: User                                                   ##
## Purpose: Predict 2019 CHE using diff ~ year, unweighted               ##
################################################################################
## clear memory
rm(list=ls())
library(vctrs, lib.loc='/ihme/singularity-images/rstudio/lib/4.1.3.4')


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


# Use each model to predict 2020 and 2019 and compare to estimated value
# Model 4: diff ~ year (unweighted)

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
che25_df[, che_diff := (che25 -  lag (che25))/(year_id - lag(year_id)), by = c("ihme_loc_id") ] # year standardized diff 
che25_df <- che25_df[, lag_diff := lag (che_diff), by = c("ihme_loc_id") ] 

# re order for regression models
che25_df <- che25_df[order(-year_id)]

## filter to 2004 and later
che10_df <- che10_df[year_id > 2003]
che25_df <- che25_df[year_id > 2003]

# take out 2020 and most recent year 
most_recent_year <- che10_df[year_id != 2020, .(max_year = max(year_id)), by = "ihme_loc_id"]

#run regression models 
#Peru 
per_5_che10 <- lm(che_diff ~ year_id, data = che10_df[ihme_loc_id == "PER" & year_id %nin% c(2019,2020)])
per_5_che25 <-lm(che_diff ~ year_id, data = che25_df[ihme_loc_id == "PER" & year_id %nin% c(2019,2020)])

#Mexico
mex_5_che10 <- lm(che_diff ~ year_id, data = che10_df[ihme_loc_id == "MEX"& year_id %nin% c(2018,2020)])
mex_5_che25 <- lm(che_diff ~ year_id, data = che25_df[ihme_loc_id == "MEX"& year_id %nin% c(2018,2020)])

#Belarus
bel_5_che10 <- lm(che_diff ~ year_id, data = che10_df[ihme_loc_id == "BLR"& year_id %nin% c(2019,2020)])
bel_5_che25 <-  lm(che_diff ~ year_id, data = che25_df[ihme_loc_id == "BLR"& year_id %nin% c(2019,2020)])

#Russia
rus_5_che10 <- lm(che_diff ~ year_id, data = che10_df[ihme_loc_id == "RUS"& year_id %nin% c(2014,2020)])
rus_5_che25 <-  lm(che_diff ~ year_id, data = che25_df[ihme_loc_id == "RUS"& year_id %nin% c(2014,2020)])

#Vietnam.
vnm_5_che10 <- lm(che_diff ~ year_id, data = che10_df[ihme_loc_id == "VNM"& year_id %nin% c(2018,2020)])
vnm_5_che25 <-  lm(che_diff ~ year_id, data = che25_df[ihme_loc_id == "VNM"& year_id %nin% c(2018,2020)])

# predict 2019 and 2020 values to compare with estimates
# create dataframe by country 
# Peru
che10_df_predict_5_years_per <- che10_df %>%
  filter(ihme_loc_id == "PER") %>%
  mutate(che10_predict = ifelse(year_id %in% c(2019,2020), NA, che10))%>%
  mutate(che10_diff_predict = ifelse(year_id %in% c(2019,2020), NA, che_diff))
che25_df_predict_5_years_per <- che25_df %>%
  filter(ihme_loc_id == "PER") %>%
  mutate(che25_predict = ifelse(year_id %in% c(2019,2020), NA, che25))%>%
  mutate(che25_diff_predict = ifelse(year_id %in% c(2019,2020), NA, che_diff))

# Mexico
che10_df_predict_5_years_mex <- che10_df %>%
  filter(ihme_loc_id == "MEX") %>%
  mutate(che10_predict = ifelse(year_id %in% c(2018,2020), NA, che10))%>%
  mutate(che10_diff_predict = ifelse(year_id %in% c(2018,2020), NA, che_diff))
che25_df_predict_5_years_mex <- che25_df %>%
  filter(ihme_loc_id == "MEX") %>%
  mutate(che25_predict = ifelse(year_id %in% c(2018,2020), NA, che25))%>%
  mutate(che25_diff_predict = ifelse(year_id %in% c(2018,2020), NA, che_diff))

# Belarus
che10_df_predict_5_years_blr <- che10_df %>%
  filter(ihme_loc_id == "BLR") %>%
  mutate(che10_predict = ifelse(year_id %in% c(2019,2020), NA, che10))%>%
  mutate(che10_diff_predict = ifelse(year_id %in% c(2019,2020), NA, che_diff))
che25_df_predict_5_years_blr <- che25_df %>%
  filter(ihme_loc_id == "BLR") %>%
  mutate(che25_predict = ifelse(year_id %in% c(2019,2020), NA, che25))%>%
  mutate(che25_diff_predict = ifelse(year_id %in% c(2019,2020), NA, che_diff))

# Russia
che10_df_predict_5_years_rus <- che10_df %>%
  filter(ihme_loc_id == "RUS") %>%
  mutate(che10_predict = ifelse(year_id %in% c(2014,2020), NA, che10))%>%
  mutate(che10_diff_predict = ifelse(year_id %in% c(2014,2020), NA, che_diff))
che25_df_predict_5_years_rus <- che25_df %>%
  filter(ihme_loc_id == "RUS") %>%
  mutate(che25_predict = ifelse(year_id %in% c(2014,2020), NA, che25))%>%
  mutate(che25_diff_predict = ifelse(year_id %in% c(2014,2020), NA, che_diff))

# Vietnam
che10_df_predict_5_years_vnm <- che10_df %>%
  filter(ihme_loc_id == "VNM") %>%
  mutate(che10_predict = ifelse(year_id %in% c(2018,2020), NA, che10))%>%
  mutate(che10_diff_predict = ifelse(year_id %in% c(2018,2020), NA, che_diff))
che25_df_predict_5_years_vnm <- che25_df %>%
  filter(ihme_loc_id == "VNM") %>%
  mutate(che25_predict = ifelse(year_id %in% c(2018,2020), NA, che25))%>%
  mutate(che25_diff_predict = ifelse(year_id %in% c(2018,2020), NA, che_diff))

# ## function to add prediction with uncertainty
predict_most_recent_2020_uncertainty <- function(df, model, var, most_recent_year){
  new.dat <- df[year_id %in% c(most_recent_year,2020)]
  predict_19_20 <- data.frame(predict(model, newdata = new.dat, interval = 'confidence'))
  x <- which( colnames(df)== eval(var))
  df[1, x] <- predict_19_20[[1,1]]
  df[2, x] <- predict_19_20[[2,1]]
  df[, lower := ifelse(year_id == 2020,  predict_19_20[[1,2]],NA) ]
  df[, upper := ifelse(year_id == 2020, predict_19_20[[1,3]],NA) ]
  df[, lower := ifelse(year_id == most_recent_year,  predict_19_20[[2,2]],lower) ]
  df[, upper := ifelse(year_id == most_recent_year, predict_19_20[[2,3]],upper) ]
  return(df)
}

#Peru
#CHE 10
peru_che10_predict <- predict_most_recent_2020_uncertainty(che10_df_predict_5_years_per, per_5_che10, "che10_diff_predict", 2019)

#CHE 25
peru_che25_predict <- predict_most_recent_2020_uncertainty(che25_df_predict_5_years_per, per_5_che25, "che25_diff_predict", 2019)

#Mexico
#CHE 10
mexico_che10_predict <-  predict_most_recent_2020_uncertainty(che10_df_predict_5_years_mex, mex_5_che10, "che10_diff_predict", 2018)

#CHE 25
mexico_che25_predict <-  predict_most_recent_2020_uncertainty(che25_df_predict_5_years_mex, mex_5_che25, "che25_diff_predict", 2018)

#Belarus
#CHE 10
belarus_che10_predict <-predict_most_recent_2020_uncertainty(che10_df_predict_5_years_blr, bel_5_che10, "che10_diff_predict", 2019)

#CHE 25
belarus_che25_predict <-  predict_most_recent_2020_uncertainty(che25_df_predict_5_years_blr, bel_5_che25, "che25_diff_predict", 2019)

#Russia
#CHE 10
russia_che10_predict <- predict_most_recent_2020_uncertainty(che10_df_predict_5_years_rus, rus_5_che10, "che10_diff_predict", 2014)

#CHE 25
russia_che25_predict <-  predict_most_recent_2020_uncertainty(che25_df_predict_5_years_rus, rus_5_che25, "che25_diff_predict", 2014)

#Vietnam
#CHE 10
vietnam_che10_predict <- predict_most_recent_2020_uncertainty(che10_df_predict_5_years_vnm, vnm_5_che10, "che10_diff_predict", 2018)

#CHE 25
vietnam_che25_predict <- predict_most_recent_2020_uncertainty(che25_df_predict_5_years_vnm, vnm_5_che25, "che25_diff_predict", 2018)


#merge dfs 
## CHE 10
che10_predict <- rbind(peru_che10_predict[1:2],
                       mexico_che10_predict[1:2],
                       belarus_che10_predict[1:2],
                       russia_che10_predict[1:2],
                       vietnam_che10_predict[1:2])

## CHE 25
che25_predict <- rbind(peru_che25_predict[1:2],
                       mexico_che25_predict[1:2],
                       belarus_che25_predict[1:2],
                       russia_che25_predict[1:2],
                       vietnam_che25_predict[1:2])

## add predicted diff to lag CHE value
che10_predict[, che10_predict := che10_diff_predict + lag_che]
che25_predict[, che25_predict := che25_diff_predict + lag_che]

## add on estimates
che10_compare <- merge(che10_predict[, c('che10_predict', 'lower', 'upper',"ihme_loc_id", "year_id")], che10_df[, c('che10', "ihme_loc_id", "year_id")], by = c("ihme_loc_id", "year_id"))
che25_compare <- merge(che25_predict[, c('che25_predict', 'lower', 'upper',"ihme_loc_id", "year_id")], che25_df[, c('che25', "ihme_loc_id", "year_id")], by = c("ihme_loc_id", "year_id"))

#save CSV's 
write.csv(che10_compare,'FILEPATH/che10_compare_model4_unweighted.csv', row.names = F)

write.csv(che25_compare,'FILEPATH/che25_compare_model4_unweighted.csv', row.names = F)
