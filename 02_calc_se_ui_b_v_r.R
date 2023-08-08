#####################################################################
## Author: User                           ##
## Description: Add samples sizes, calculate SE and UI for CHE estimates
## in Vietnam, Belarus, and Russia
##                                                                  ##
#####################################################################

## clean working environment 
rm(list=ls())
library(vctrs, lib.loc="/ihme/singularity-images/rstudio/lib/4.1.3.4")

## runtime configuration
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/home/j'
  h <- file.path('/homes', Sys.getenv('USER'))
} else {
  j <- 'J:/'
  h <- 'H:/'
}

## libraries
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2, gridExtra, dplyr, grid)

## directories 
indir <- 'FILEPATH'

## read in prepped data
che25_5years_plot_df <- fread(file.path(indir, 'che25_plot_df_sep_2022_model2.csv'))
che10_5years_plot_df <- fread(file.path(indir, 'che10_plot_df_sep_2022_model2.csv'))

colnames(che10_5years_plot_df)[4:5] <- c("upper_10", "lower_10")
colnames(che25_5years_plot_df)[4:5] <- c("upper_25", "lower_25")

che5years_plot_df <- merge(che25_5years_plot_df, che10_5years_plot_df, by = c("year_id", "ihme_loc_id", "predict"))

## filter to estimates
che5years_plot_df_estimates <- che5years_plot_df[predict == "Estimate from surveys or HEFPI"]

## read in sample sizes
samples <- fread(file.path(h, "sample_sizes_che.csv") )

## sample dataframe
sample_df <- merge(samples,che5years_plot_df_estimates, by = c("year_id", "ihme_loc_id") )

## calculate SE
sample_df[, se_che10 := (sqrt(N*che10*(1-che10))/N)* 1.96 ]
sample_df[, se_che25 := (sqrt(N*che25*(1-che25))/N)*1.96 ]


## calculate UIs
sample_df[, upper_10 := che10 + (1.96 * se_che10)]
sample_df[, lower_10 := che10 - (1.96 * se_che10)]

sample_df[, upper_25 := che25 + (1.96 * se_che25)]
sample_df[, lower_25 := che25 - (1.96 * se_che25)]

## read in mexico and peru estimates with uis
peru_che10 <- fread(file.path('FILEPATH'))[, c("year_id", "ihme_loc_id", 'weighted_che10', "weighted_lower", "weighted_upper", "N", "se_manual_unadjusted")]
peru_che25 <- fread(file.path('FILEPATH'))[, c("year_id", "ihme_loc_id", 'weighted_che25', "weighted_lower", "weighted_upper", "N","se_manual_unadjusted")]

mexico_che10 <- fread(file.path('FILEPATH')) [, c("year_id", "ihme_loc_id", 'weighted_che10', "weighted_lower", "weighted_upper", "N","se_manual_unadjusted")]
mexico_che25 <- fread(file.path('FILEPATH')) [, c("year_id", "ihme_loc_id", 'weighted_che25', "weighted_lower", "weighted_upper", "N","se_manual_unadjusted")]

## update column names
setnames(peru_che10, old = c('weighted_che10', "weighted_lower", "weighted_upper", "se_manual_unadjusted"), 
         new = c('che10', 'lower_10', 'upper_10', 'se_che10'))

setnames(peru_che25, old = c('weighted_che25', "weighted_lower", "weighted_upper", "se_manual_unadjusted"), 
         new = c('che25', 'lower_25', 'upper_25', 'se_che25'))

setnames(mexico_che10, old = c('weighted_che10', "weighted_lower", "weighted_upper", "se_manual_unadjusted"), 
         new = c('che10', 'lower_10', 'upper_10','se_che10'))

setnames(mexico_che25, old = c('weighted_che25', "weighted_lower", "weighted_upper", "se_manual_unadjusted"), 
         new = c('che25', 'lower_25', 'upper_25', 'se_che25'))

# combine by country
peru_df <- merge(peru_che10, peru_che25, by = c("year_id", "ihme_loc_id", "N"))
peru_df [, predict := 'Estimate from surveys or HEFPI']
peru_df [, che25_pct := che25 * 100]
peru_df [, che10_pct := che10 * 100]


mexico_df <- merge(mexico_che10, mexico_che25, by = c("year_id", "ihme_loc_id", "N"))
mexico_df [, predict := 'Estimate from surveys or HEFPI']
mexico_df [, che25_pct := che25 * 100]
mexico_df [, che10_pct := che10 * 100]


## merge peru and mexico onto dataframe
che5years_plot_df_estimates_ui <- rbind(sample_df, peru_df)
che5years_plot_df_estimates_ui <- rbind(che5years_plot_df_estimates_ui, mexico_df)

## calculate SD
che5years_plot_df_estimates_ui[, sd_che10 := se_che10 * sqrt(N)]
che5years_plot_df_estimates_ui[, sd_che25 := se_che25 * sqrt(N)]

# save df
write.csv(che5years_plot_df_estimates_ui,'FILEPATH', row.names = F) 

