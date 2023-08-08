#####################################################################
## Author: User                                           ##
## Description: Private sector utilization plots for CHE 2020 paper ##
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


# read in prepped utilization data from 2019 and 2020 from Peru
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
visits_19_20$year_id <- as.factor(visits_19_20$year_id)

#read private sector utilization data from Mexico
#read in Mexico utilization data 
indiv_private_2018 <- fread('FILEPATH/mexico_utilization_2018.csv')
indiv_private_2018[,month := factor(indiv_private_2018$year_month, levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
indiv_private_2020 <- fread('FILEPATH/mexico_utilization_2020.csv')
indiv_private_2020[,month := factor(indiv_private_2020$year_month, levels = c("2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
indiv_private_18_20 <- rbind(indiv_private_2020, indiv_private_2018)


df_2019 <- data.table(year_month = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12"), 
                      year_id = 2019 )

df_2019[,month := factor(df_2019$year_month, levels = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12"), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))]
indiv_private_18_20_predict <- rbind(df_2019, indiv_private_18_20, fill = T)

indiv_private_18_20[,year_month := factor(indiv_private_18_20$year_month, 
                                          levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12",
                                                     "2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"),
                                          labels = c("Jan 2018", "Feb 2018", "Mar 2018", "Apr 2018", "May 2018", "Jun 2018", "Jul 2018", "Aug 2018", 
                                                     "Sept 2018", "Oct 2018", "Nov 2018", "Dec 2018", "Jan 2020", "Feb 2020", "Mar 2020",
                                                     "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sept 2020", "Oct 2020", "Nov 2020", 
                                                     "Dec 2020"))]
indiv_private_18_20$year_month_check <- as.double(indiv_private_18_20$year_month)

#repeat for predict df
indiv_private_18_20_predict[,year_month := factor(indiv_private_18_20_predict$year_month, 
                                                  levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12",
                                                             "2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12",
                                                             "2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"),
                                                  labels = c("Jan 2018", "Feb 2018", "Mar 2018", "Apr 2018", "May 2018", "Jun 2018", "Jul 2018", "Aug 2018", 
                                                             "Sept 2018", "Oct 2018", "Nov 2018", "Dec 2018","Jan 2019", "Feb 2019", 
                                                             "Mar 2019", "Apr 2019", "May 2019", "Jun 2019", "Jul 2019", "Aug 2019", 
                                                             "Sept 2019", "Oct 2019", "Nov 2019", "Dec 2019", "Jan 2020", "Feb 2020", "Mar 2020",
                                                             "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sept 2020", "Oct 2020", "Nov 2020", 
                                                             "Dec 2020"))]
indiv_private_18_20_predict$year_month_check <- as.double(indiv_private_18_20_predict$year_month)

#make dec na due to missing survey data
indiv_private_18_20 <- indiv_private_18_20 %>%
  mutate(private_share_month = ifelse((year_month_check==12 |year_month_check==24), NA, private_share_month))

#make dec na due to missing survey data (predict df)
indiv_private_18_20_predict <- indiv_private_18_20_predict %>%
  mutate(private_share_month = ifelse((year_month_check==12 |year_month_check==24 | year_month_check==36), NA, private_share_month))

#Extend 2018 line to beginning of 2020 by insurance type and then all insurance
model_18 <- lm(private_share_month ~ year_month_check, data = indiv_private_18_20_predict[year_month_check <13])

# predict 2019 values to compare with estimates
indiv_private_18_20_predict_20 <- copy(indiv_private_18_20_predict)
indiv_private_18_20_predict_20 <- indiv_private_18_20_predict_20 %>%
  mutate(private_share_month = ifelse(year_id==2019, NA, private_share_month))
indiv_private_18_20_predict_20<- indiv_private_18_20_predict_20 %>% 
  mutate(private_share_month = ifelse(is.na(private_share_month), predict(model_18, .), private_share_month))

indiv_private_18_20_predict_20$year_id <- as.factor(indiv_private_18_20_predict_20$year_id)
indiv_private_18_20_predict$year_id <- as.factor(indiv_private_18_20_predict$year_id)

indiv_private_18_20$month_check <- as.double(indiv_private_18_20$month)
indiv_private_18_20$year_id <- as.factor(indiv_private_18_20$year_id)

#make dec na due to missing survey data (predict df)
indiv_private_18_20_predict_20 <- indiv_private_18_20_predict_20 %>%
  mutate(private_share_month = ifelse((year_month_check==12 |year_month_check==24 | year_month_check==36), NA, private_share_month))


# Mexico: private share 
lm_df_mex <- indiv_private_18_20_predict_20[year_id != 2019, c("year_month_check", "private_share_month")]
lm_df_mex[, month_check := ifelse(year_month_check>12 & year_month_check<25, year_month_check-12, 
                                  ifelse(year_month_check>24, year_month_check-24, year_month_check))]
lm_df_mex[, march_binary := ifelse(year_month_check > 27, 1, 0)]
lm_df_mex[, year_2020 := ifelse(year_month_check > 24, 1, 0)]
lm_df_mex[, x3 := 1]
lm_df_mex[, combo := march_binary*month_check]

mex_share_private <- lm(private_share_month ~  month_check + march_binary + year_2020 + x3 + combo + 0, data = lm_df_mex)
## number plug ui
new_df <- data.table(month_check = 1, march_binary = 1, x3=0, combo = 4, year_2020 = 0)

predict(mex_share_private, new_df, interval = "confidence")


tidy_mex_share_private <- setDT(tidy(mex_share_private))
tidy_mex_share_private[, variable := "private_share"]
tidy_mex_share_private[, ihme_loc_id := "MEX"]

indiv_private_18_20_diff <- indiv_private_18_20[, c("year_month", "year_month_check", "year_id", "month", "month_check", "private_share_month")] 
indiv_private_18_20_diff <- indiv_private_18_20_diff[order(month,year_id)]
indiv_private_18_20_diff[, month_check := ifelse(year_month_check>12 & year_month_check<25, year_month_check-12, 
                                  ifelse(year_month_check>24, year_month_check-24, year_month_check))]
indiv_private_18_20_diff[, private_share_month_diff := private_share_month - lag(private_share_month), by ="month"]
indiv_private_18_20_diff[,  private_share_month_diff_relative := (private_share_month / lag(private_share_month))-1, by ="month"]
indiv_private_18_20_diff[, march_binary := ifelse(year_month_check > 15, 1, 0)]

# absolute difference
mex_abs_diff <- lm(private_share_month_diff ~  month_check + march_binary + march_binary*month_check, data = indiv_private_18_20_diff)
tidy_mex_abs_diff <- setDT(tidy(mex_abs_diff))
tidy_mex_abs_diff[, variable := "private_share_abs_diff"]
tidy_mex_abs_diff[, ihme_loc_id := "MEX"]

# percent difference 
mex_rel_diff <- lm(private_share_month_diff_relative ~  month_check + march_binary + march_binary*month_check, data = indiv_private_18_20_diff)
tidy_mex_rel_diff <- setDT(tidy(mex_rel_diff))
tidy_mex_rel_diff[, variable := "private_share_relative_diff"]
tidy_mex_rel_diff[, ihme_loc_id := "MEX"]

tidy_mexico_private_share <- rbind(tidy_mex_share_private, tidy_mex_abs_diff, tidy_mex_rel_diff)

# Peru:
lm_df_per <- visits_19_20
lm_df_per[, march_binary := ifelse(year_month_check > 15, 1, 0)]
lm_df_per[, x3 := 1]
lm_df_per[, combo := march_binary*year_month_check]

per_share_private <- lm(private_share_month ~  year_month_check + march_binary + combo+x3+0, data = lm_df_per)
tidy_per_share_private <- setDT(tidy(per_share_private))
tidy_per_share_private[, variable := "private_share"]
tidy_per_share_private[, ihme_loc_id := "PER"]

## number plug ui
new_df <- data.table(year_month_check = 1, march_binary = 1, x3=0, combo = 16)
predict(per_share_private, new_df, interval = "confidence")

visits_19_20_diff <- lm_df_per[,c("year_month", "year_month_check", "year_id", "month", "month_check", "private_share_month")]
visits_19_20_diff <- visits_19_20_diff[order(month,year_id)]
visits_19_20_diff[,private_share_month_diff := private_share_month - lag(private_share_month), by ="month"]
visits_19_20_diff[, private_share_month_diff_relative := (private_share_month /lag(private_share_month))-1, by ="month"]
visits_19_20_diff[, march_binary := ifelse(year_month_check > 15, 1, 0)]

# absolute difference
per_abs_diff <- lm(private_share_month_diff ~  month_check + march_binary + march_binary*month_check, data = visits_19_20_diff)
tidy_per_abs_diff <- setDT(tidy(per_abs_diff))
tidy_per_abs_diff[, variable := "private_share_abs_diff"]
tidy_per_abs_diff[, ihme_loc_id := "PER"]

# percent difference 
per_rel_diff <- lm(private_share_month_diff_relative ~  month_check + march_binary + march_binary*month_check, data = visits_19_20_diff)
tidy_per_rel_diff <- setDT(tidy(per_rel_diff))
tidy_per_rel_diff[, variable := "private_share_relative_diff"]
tidy_per_rel_diff[, ihme_loc_id := "PER"]

tidy_per_private_share <- rbind(tidy_per_share_private, tidy_per_abs_diff, tidy_per_rel_diff)

## newey-west standard errors
# https://www.rdocumentation.org/packages/sandwich/versions/2.4-0/topics/NeweyWest
# https://www.econometrics-with-r.org/15-4-hac-standard-errors.html#eq:nwhacf

## Mexico
nobs_mexico <- nobs(mex_share_private)
truncation_mexico <- floor(0.75 * nobs_mexico^(1/3))
vccov_mex <- NeweyWest(mex_share_private,
                       lag = truncation_mexico -1,
                       prewhite=FALSE, adjust=TRUE, verbose=TRUE)
example_mod_mex <- lm(private_share_month ~  month_check + march_binary + year_2020 + march_binary*month_check, data = lm_df_mex)
newey_west_se_mex <- coeftest(example_mod_mex,vccov_mex)[,2]
tidy_mex_share_private$std.error <- newey_west_se_mex

## Peru
nobs_peru <- nobs(per_share_private)
truncation_peru <- floor(0.75 * nobs_peru^(1/3))
vccov_per <- NeweyWest(per_share_private,
                       lag = truncation_peru -1)
example_mod_per <- lm(private_share_month ~  year_month_check + march_binary + march_binary*year_month_check, data = lm_df_per)
newey_west_se_per <- coeftest(example_mod_per, vcov = vccov_per)[,2]
tidy_per_share_private$std.error <- newey_west_se_per

## save results
write.csv(tidy_mexico_private_share, file.path(h, "mexico_private_share_its_regression.csv"))
write.csv(tidy_per_private_share, file.path(h, "peru_private_share_its_regression.csv"))

## save regressions of interest for mexico and peru for appendix supplementary table 2
table2_regressions <- rbind(tidy_per_share_private, tidy_mex_share_private)
write.csv(table2_regressions, file.path(outdir, "peru_mex_private_share_its_regression.csv"))

