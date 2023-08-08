#####################################################################
## Author: User                                           ##
## Description: ITS regressions for CHE 2020 paper ##
##                                                                  ##
#####################################################################

## clean working environment 
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
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2, gridExtra, dplyr, broom, sandwich, lmtest)

## directories 
indir <- 'FILEPATH'
outdir <- 'FILEPATH'

#read in Mexico utilization data 
indiv_private_2018 <- fread('FILEPATH/mexico_utilization_2018.csv')
indiv_private_2018[,month := factor(indiv_private_2018$year_month, levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
indiv_private_2020 <- fread('FILEPATH/mexico_utilization_2020.csv')
indiv_private_2020[,month := factor(indiv_private_2020$year_month, levels = c("2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
indiv_private_18_20 <- rbind(indiv_private_2020, indiv_private_2018)

indiv_private_18_20[,year_month := factor(indiv_private_18_20$year_month, 
                                          levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12",
                                                     "2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"),
                                          labels = c("January 2018", "February 2018", "March 2018", "April 2018", "May 2018", "June 2018", "July 2018", "August 2018", 
                                                     "September 2018", "October 2018", "November 2018", "December 2018", "January 2020", "February 2020", "March 2020",
                                                     "April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020", "October 2020", "November 2020", 
                                                     "December 2020"))]
indiv_private_18_20$year_month_check <- as.double(indiv_private_18_20$year_month)

#make dec na due to missing survey data
indiv_private_18_20 <- indiv_private_18_20 %>%
  mutate(any_visit_month = ifelse((year_month_check %in% c(10,11,12) |year_month_check %in% c(22,23,24)), NA, any_visit_month))
indiv_private_18_20$month_check <- as.double(indiv_private_18_20$month)
indiv_private_18_20$year <- as.factor(indiv_private_18_20$year_id)


#read in prepped peru data
visits_19 <- fread("FILEPATH/peru_utilization_2019_updated.csv")
visits_19[,month := factor(visits_19$year_month, levels = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
visits_20 <- fread("FILEPATH/peru_utilization_2020_updated.csv")
visits_20[,month := factor(visits_20$year_month, levels = c("2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
visits_19_20 <- rbind(visits_19, visits_20)
visits_19_20[,year_month := factor(visits_19_20$year_month, 
                                   levels = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12",
                                              "2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"),
                                   labels = c("January 2019", "February 2019", "March 2019", "April 2019", "May 2019", "June 2019", "July 2019", "August 2019", 
                                              "September 2019", "October 2019", "November 2019", "December 2019", "January 2020", "February 2020", "March 2020",
                                              "April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020", "October 2020", "November 2020", 
                                              "December 2020"))]
visits_19_20$year_month_check <- as.double(visits_19_20$year_month)
visits_19_20$month_check <- as.double(visits_19_20$month)
visits_19_20$year <- as.factor(visits_19_20$year_id)


## convert to difference space 
# Mexico
indiv_private_18_20_diff <- indiv_private_18_20[, -c("private_visit_month", "private_share_month", "V1")]
indiv_private_18_20_diff <- indiv_private_18_20_diff[order(month,year)]
indiv_private_18_20_diff[, any_visit_month_diff := any_visit_month - lag(any_visit_month), by ="month"]

#Peru

visits_19_20_diff <- visits_19_20[, -c("private_visit_month", "private_share_month", "V1")]
visits_19_20_diff <- visits_19_20_diff[order(month,year)]
visits_19_20_diff[, any_visit_month_diff := any_visit_month - lag(any_visit_month), by ="month"]

## convert to difference relative space 
# Mexico
indiv_private_18_20_diff[, any_visit_month_diff_relative := (any_visit_month / lag(any_visit_month))-1, by ="month"]

#Peru
visits_19_20_diff[, any_visit_month_diff_relative  := (any_visit_month /lag(any_visit_month))-1, by ="month"]

# Mexico regression
lm_df_mex <- indiv_private_18_20_diff
lm_df_mex[, march_binary := ifelse(month_check > 3, 1, 0)]

# percent difference 
lm_df_mex[, combo := march_binary*month_check ]
lm_df_mex[, x3 := 1]

mex_rel_diff <- lm(any_visit_month_diff_relative ~  month_check + march_binary + combo + x3+0, data = lm_df_mex)
tidy_mex_rel_diff <- setDT(tidy(mex_rel_diff))
tidy_mex_rel_diff[, variable := "utilization_share_respondents_relative_diff"]
tidy_mex_rel_diff[, ihme_loc_id := "MEX"]


## number plug ui
new_dt <- data.table(month_check = 1, march_binary = 1,  combo =4, x3=0)
predict(mex_rel_diff, new_dt, interval = "confidence")


# Peru regression
lm_df_per <- visits_19_20_diff
lm_df_per[, march_binary := ifelse(month_check > 3, 1, 0)]

# percent difference 
lm_df_per[, combo := march_binary * month_check]
lm_df_per[, x3 := 1]

per_rel_diff <- lm(any_visit_month_diff_relative ~  month_check + march_binary + combo + x3 + 0, data =lm_df_per)
tidy_per_rel_diff <- setDT(tidy(per_rel_diff))
tidy_per_rel_diff[, variable := "utilization_share_respondents_relative_diff"]
tidy_per_rel_diff[, ihme_loc_id := "PER"]

## number plug ui
new_dt <- data.table(month_check = 1, march_binary = 1, intercept = 0, combo =4, x3=0)
predict(per_rel_diff, new_dt, interval = "confidence")
## newey-west standard errors
# https://www.rdocumentation.org/packages/sandwich/versions/2.4-0/topics/NeweyWest
# https://www.econometrics-with-r.org/15-4-hac-standard-errors.html#eq:nwhacf

## Mexico
nobs_mexico <- nobs(mex_rel_diff)
truncation_mexico <- floor(0.75 * nobs_mexico^(1/3))
vccov_mex <- NeweyWest(mex_rel_diff,
                   lag = truncation_mexico -1)
lm_df_mex <- lm_df_mex[, c("any_visit_month_diff_relative", "month_check", "march_binary")]
lm_df_mex <- lm_df_mex[!is.na(any_visit_month_diff_relative)]
example_mod_mex <- lm(any_visit_month_diff_relative ~  month_check + march_binary + march_binary*month_check, data = lm_df_mex)
post_mod_mex <- lm(any_visit_month_diff_relative ~  month_check, data = lm_df_mex[month_check >3])
newey_west_se_mex <- coeftest(example_mod_mex, vcov = vccov_mex)[, 2]
tidy_mex_rel_diff$std.error <- newey_west_se_mex

## Peru
nobs_peru <- nobs(per_rel_diff)
truncation_peru <- floor(0.75 * nobs_peru^(1/3))
vccov_per <- NeweyWest(per_rel_diff,
                   lag = truncation_peru -1)
lm_df_per <- lm_df_per[, c("any_visit_month_diff_relative", "month_check", "march_binary")]
lm_df_per <- lm_df_per[!is.na(any_visit_month_diff_relative)]
example_mod_per <- lm(any_visit_month_diff_relative ~  month_check + march_binary + march_binary*month_check, data =lm_df_per)
post_mod_per <- lm(any_visit_month_diff_relative ~  month_check, data = lm_df_per[month_check >3])

newey_west_se_per <- coeftest(example_mod_per, vcov = vccov_per)[, 2]
tidy_per_rel_diff$std.error <- newey_west_se_per

## save regressions of interest for mexico and peru for appendix supplementary table 2
table2_regressions <- rbind(tidy_per_rel_diff, tidy_mex_rel_diff)
write.csv(table2_regressions, file.path(outdir, "peru_mex_utilization_regression.csv"))

