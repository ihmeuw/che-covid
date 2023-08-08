## Author: USER                                                  ##
## Purpose: Predict 2019 CHE using level ~ lag level, unweighted               ##
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
# Model 2: level ~ lag level

#read in peru che data
peru_che10 <- fread('FILEPATH/peru_che_10_updated_sep_2022.csv')[,-c("V1")]
peru_che25 <- fread('FILEPATH/peru_che_25_updated_sep_2022.csv')[,-c("V1")]
peru_che10 <- peru_che10[order(-year_id)]
peru_che25 <- peru_che25[order(-year_id)]

#read in mexico che data
mexico_che10 <- fread('FILEPATH/mexico_che10_updated_aug_2022.csv')[location_id==130]
mexico_che25 <-  fread('FILEPATH/mexico_che25_updated_aug_2022.csv')[location_id==130]
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

# re order for regression models
che25_df <- che25_df[order(-year_id)]

## filter to 2004 and later
che10_df <- che10_df[year_id > 2003]
che25_df <- che25_df[year_id > 2003]

#count how many points we have per country (minus the original 5 in the regression that are more heavily weighted)
numpoints <- che10_df[, .(num_points = .N - 7), by = "ihme_loc_id" ]

# take out 2020 and most recent year 
most_recent_year <- che10_df[year_id != 2020, .(max_year = max(year_id)), by = "ihme_loc_id"]

## extract se
se_che_10 <- data.table()
se_che_25 <- data.table()

## create dataframe to save r2 values
r_2_models <- data.table()

#run regression models 
#Peru 
#last 5 data points (plus 10 extra)
per_5_che10 <- lm(che10 ~ lag_che, data = che10_df[ihme_loc_id == "PER" & year_id %nin% c(2020)])
per_5_che25 <-lm(che25 ~ lag_che, data = che25_df[ihme_loc_id == "PER" & year_id %nin% c(2020)])

per_r2 <- data.table(ihme_loc_id = "PER", r2_che10 =summary(per_5_che10)$r.squared, r2_che25 =summary(per_5_che25)$r.squared )
r_2_models <- rbind(r_2_models, per_r2)

#Mexico
mex_5_che10 <- lm(che10 ~ lag_che, data = che10_df[ihme_loc_id == "MEX"& year_id %nin% c(2020)])
mex_5_che25 <- lm(che25 ~ lag_che, data = che25_df[ihme_loc_id == "MEX"& year_id %nin% c(2020)])

mex_r2 <- data.table(ihme_loc_id = "MEX", r2_che10 =summary(mex_5_che10)$r.squared, r2_che25 =summary(mex_5_che25)$r.squared )
r_2_models <- rbind(r_2_models, mex_r2)

#Belarus
bel_5_che10 <- lm(che10 ~ lag_che, data = che10_df[ihme_loc_id == "BLR"& year_id %nin% c(2020)])
bel_5_che25 <-  lm(che25 ~ lag_che, data = che25_df[ihme_loc_id == "BLR"& year_id %nin% c(2020)])

blr_r2 <- data.table(ihme_loc_id = "BLR", r2_che10 =summary(bel_5_che10)$r.squared, r2_che25 =summary(bel_5_che25)$r.squared )
r_2_models <- rbind(r_2_models, blr_r2)

#Russia
rus_5_che10 <- lm(che10 ~ lag_che, data = che10_df[ihme_loc_id == "RUS"& year_id %nin% c(2020)])
rus_5_che25 <-  lm(che25 ~ lag_che, data = che25_df[ihme_loc_id == "RUS"& year_id %nin% c(2020)])

rus_r2 <- data.table(ihme_loc_id = "RUS", r2_che10 =summary(rus_5_che10)$r.squared, r2_che25 =summary(rus_5_che25)$r.squared )
r_2_models <- rbind(r_2_models, rus_r2)

#Vietnam.
vnm_5_che10 <- lm(che10 ~ lag_che, data = che10_df[ihme_loc_id == "VNM"& year_id %nin% c(2020)])
vnm_5_che25 <-  lm(che25 ~ lag_che, data = che25_df[ihme_loc_id == "VNM"& year_id %nin% c(2020)])

vnm_r2 <- data.table(ihme_loc_id = "VNM", r2_che10 =summary(vnm_5_che10)$r.squared, r2_che25 =summary(vnm_5_che10)$r.squared )

# create dataframe by country 
# Peru
che10_df_predict_5_years_per <- che10_df %>%
  filter(ihme_loc_id == "PER") %>%
  mutate(che10_predict = NA)
che25_df_predict_5_years_per <- che25_df %>%
  filter(ihme_loc_id == "PER") %>%
  mutate(che25_predict =NA)

# Mexico
che10_df_predict_5_years_mex <- che10_df %>%
  filter(ihme_loc_id == "MEX") %>%
  mutate(che10_predict = NA)
che25_df_predict_5_years_mex <- che25_df %>%
  filter(ihme_loc_id == "MEX") %>%
  mutate(che25_predict =NA)

# Belarus
che10_df_predict_5_years_blr <- che10_df %>%
  filter(ihme_loc_id == "BLR") %>%
  mutate(che10_predict = NA)
che25_df_predict_5_years_blr <- che25_df %>%
  filter(ihme_loc_id == "BLR") %>%
  mutate(che25_predict = NA)

# Russia
che10_df_predict_5_years_rus <- che10_df %>%
  filter(ihme_loc_id == "RUS") %>%
  mutate(che10_predict = NA)
che25_df_predict_5_years_rus <- che25_df %>%
  filter(ihme_loc_id == "RUS") %>%
  mutate(che25_predict = NA)

# Vietnam
che10_df_predict_5_years_vnm <- che10_df %>%
  filter(ihme_loc_id == "VNM") %>%
  mutate(che10_predict = NA)
che25_df_predict_5_years_vnm <- che25_df %>%
  filter(ihme_loc_id == "VNM") %>%
  mutate(che25_predict = NA)

# ## function to add predictions with uncertainty
predict_uncertainty <- function(df, model, var){
  new.dat <- df
  predictions <- data.frame(predict(model, newdata = new.dat, interval = 'confidence'))
  x <- which( colnames(df)== eval(var))
  df[,x]<- predictions$fit
  return(df)
}

#Peru
#CHE 10
peru_che10_predict <- predict_uncertainty(che10_df_predict_5_years_per, per_5_che10, "che10_predict")

#CHE 25
peru_che25_predict <- predict_uncertainty(che25_df_predict_5_years_per, per_5_che25, "che25_predict")

#Mexico
#CHE 10
mexico_che10_predict <-  predict_uncertainty(che10_df_predict_5_years_mex, mex_5_che10, "che10_predict")

#CHE 25
mexico_che25_predict <-  predict_uncertainty(che25_df_predict_5_years_mex, mex_5_che25, "che25_predict")

#Belarus
#CHE 10
belarus_che10_predict <-predict_uncertainty(che10_df_predict_5_years_blr, bel_5_che10, "che10_predict")

#CHE 25
belarus_che25_predict <-  predict_uncertainty(che25_df_predict_5_years_blr, bel_5_che25, "che25_predict")

#Russia
#CHE 10
russia_che10_predict <- predict_uncertainty(che10_df_predict_5_years_rus, rus_5_che10, "che10_predict")

#CHE 25
russia_che25_predict <-  predict_uncertainty(che25_df_predict_5_years_rus, rus_5_che25, "che25_predict")

#Vietnam
#CHE 10
vietnam_che10_predict <- predict_uncertainty(che10_df_predict_5_years_vnm, vnm_5_che10, "che10_predict")

#CHE 25
vietnam_che25_predict <- predict_uncertainty(che25_df_predict_5_years_vnm, vnm_5_che25, "che25_predict")


#merge dfs 
## CHE 10
che10_predict <- rbind(peru_che10_predict[,c("year_id", "che10", "ihme_loc_id","che10_predict")],
                       mexico_che10_predict[,c("year_id", "che10", "ihme_loc_id","che10_predict")],
                       belarus_che10_predict[,c("year_id", "che10", "ihme_loc_id","che10_predict")],
                       russia_che10_predict[,c("year_id", "che10", "ihme_loc_id","che10_predict")],
                       vietnam_che10_predict[,c("year_id", "che10", "ihme_loc_id","che10_predict")])

## CHE 25
che25_predict <- rbind(peru_che25_predict[,c("year_id", "che25", "ihme_loc_id","che25_predict")],
                       mexico_che25_predict[,c("year_id", "che25", "ihme_loc_id","che25_predict")],
                       belarus_che25_predict[,c("year_id", "che25", "ihme_loc_id","che25_predict")],
                       russia_che25_predict[,c("year_id", "che25", "ihme_loc_id","che25_predict")],
                       vietnam_che25_predict[,c("year_id", "che25", "ihme_loc_id","che25_predict")])



#save CSV's 
write.csv(che10_predict,'FILEPATH/che10_compare_model2_all_years_unweighted.csv', row.names = F)

write.csv(che25_predict,'FILEPATH/che25_compare_model2_all_years_unweighted.csv', row.names = F)

