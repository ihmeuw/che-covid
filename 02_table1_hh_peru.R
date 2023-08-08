#################################################################################
## Author: User                                                     ##
## Date: 12/22/21                                                              ##
## Purpose: Calculate survey weighted mean household level characteristics:    ##
## share rural, OOP total health, OOP drugs, OOP outpatient                    ##
## OOP hospitalizations, consumption                                           ##
#################################################################################
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
pacman::p_load(data.table, haven,readstata13, readxl, ggplot2,dplyr,survey, broom)

## source currency conversion script
source(file.path(h, 'frp/currency_conversion.R'))

## indir
indir <- 'FILEPATH'
outdir <- 'FILEPATH'

## read in Peru 2019
peru_agg19 <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2019.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)

## read in Peru 2020
peru_agg20 <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2020.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)

## combine years 
peru_agg <- rbind(peru_agg19, peru_agg20)

## add year as factor column
peru_agg[, year_factor := as.factor(as.numeric(year_id)-2019)]
peru_agg[, ihme_loc_id := "PER"]

## run currency conversion script 
peru_2019 <- peru_agg[year_id == 2019]
peru_2019 <- currency_conversion(data = peru_2019,
                                 col.loc = "ihme_loc_id",
                                 col.value = c("drugs", "outpatient", "hospital", "consumption_expenditure", "total_health"),
                                 currency = 'lcu',
                                 col.currency.year = 'year_id',
                                 base.unit = 'ppp',
                                 base.year = 2021)
peru_2019[,year_id := 2019]

peru_2020 <- peru_agg[year_id == 2020]
peru_2020 <- currency_conversion(data = peru_2020, 
                                 col.loc = "ihme_loc_id",
                                 col.value = c("drugs", "outpatient", "hospital", "consumption_expenditure", "total_health"), 
                                 currency = 'lcu',
                                 col.currency.year = 'year_id', 
                                 base.unit = 'ppp',base.year = 2021)
peru_2020[,year_id := 2020]

peru_agg_table <- rbind(peru_2019, peru_2020)

## define survey design
design <-  svydesign(ids = ~nconglome, strata = ~estrsocial, weights = ~factor07, data = peru_agg_table, variables = ~total_health+drugs+hospital+consumption_expenditure+outpatient+year_factor+rural+other+insurance, nest = T)

## write function to get p values
get_pvalue <- function(var){
  var_lm <- as.formula(paste0(var," ~ ", "year_factor"))
  lm_model <- svyglm(var_lm, design, data = peru_agg_table)
  tidy_table <- setDT(tidy(lm_model))
  pvalue <- tidy_table[[2,5]]
  return(pvalue)
}

## compute p values
vars <- c("total_health", "drugs", "hospital", "consumption_expenditure", "outpatient", "rural", "insurance")
pvalue_df <- data.table(variable = vars, pvalue = numeric(7))

for (i in 1:length(vars)){
  var <- vars[i]
  pvalue <- get_pvalue(var)
  pvalue_df[i,2] <- pvalue
}

## compute p values (by insurance)
lm_model_ins <- svyglm(total_health ~ year_factor, design, data = peru_agg_table[insurance == 1,])
lm_model_unins <- svyglm(total_health ~ year_factor, design, data = peru_agg_table[insurance == 0,])

tidy_table_ins <- setDT(tidy(lm_model_ins))
tidy_table_unins <- setDT(tidy(lm_model_unins))

pvalue_ins <- tidy_table_ins[[2,5]]
pvalue_unins <- tidy_table_unins[[2,5]]

pvalue_ins <- data.table(variable = c('total_health_ins', 'total_health_unins'), pvalue = c(pvalue_ins, pvalue_unins))

## compute differences

means <- svyby(~total_health+consumption_expenditure+rural, ~year_factor, design, svymean, vartype = c("ci", "se")) 
means_ins <- svyby(~total_health, ~year_factor+insurance, design, svymean, vartype = c("ci", "se")) 

## compute UI using se's
means <- data.table(means)
means_ins <- data.table(means_ins)

health_ins <- means_ins[insurance== 1]
setnames(health_ins, old= c("total_health","se", "ci_l", "ci_u"), c("total_health_ins", "se.total_health_ins", "ci_l.total_health_ins", "ci_u.total_health_ins"))

health_unins <- means_ins[insurance == 0]
setnames(health_unins, old= c("total_health","se", "ci_l", "ci_u"), c("total_health_unins", "se.total_health_unins", "ci_l.total_health_unins", "ci_u.total_health_unins"))

health_ins_unins <- merge(health_ins[, -c("insurance")], health_unins[, -c("insurance")], by = "year_factor")
means <- merge(means, health_ins_unins,by = "year_factor")
se_table <-  melt(means, id.vars = "year_factor")

se_table_means <- se_table[c(1:6, 25:26, 33:34) ]
se_table_means_wide <- dcast(se_table_means, variable  ~ year_factor)
setnames(se_table_means_wide, old = c("0","1"), new = c("mean_old", "mean_new"))

se_table_se <- se_table[c(7:12, 27:28, 35:36) ]
se_table_se_wide <- dcast(se_table_se, variable  ~ year_factor)
setnames(se_table_se_wide, old = c("0","1"), new = c("se_old", "se_new"))
se_table_se_wide[, variable := str_replace(variable, "se.", "")]

se_table <- merge(se_table_means_wide, se_table_se_wide, by = "variable")

percentage_df <- data.table()
vars <- unique(se_table$variable)
for (var in vars){
  cur_df <- se_table[variable == eval(var)]
  percentage_change <- rnorm(1000, cur_df$mean_new, cur_df$se_new)/rnorm(1000, cur_df$mean_old, cur_df$se_old ) -1
  quantiles <- data.table(quantile(percentage_change, probs = c(.025, 0.5,0.975)))
  
  tmp_df <- data.table(variable = var, pct_change = paste0(sprintf('%.1f', round(quantiles[2]*100, 1)), 
                                                           "\n (", sprintf('%.1f',round(quantiles[1]*100, 1)), 
                                                           ' to ',  sprintf('%.1f',round(quantiles[3]*100, 1)), ")"))
  percentage_df <- rbind(percentage_df, tmp_df)
}



ci_table <- means[, .(total_health = paste0(sprintf('%.1f', round(`total_health`, 1)), 
                                      "\n (", sprintf('%.1f',round(`ci_l.total_health`, 1)), 
                                      ' to ',  sprintf('%.1f',round(`ci_u.total_health`, 1)), ")"),
                      
                      consumption_expenditure = paste0(sprintf('%.1f', round(`consumption_expenditure`, 1)), 
                                           "\n (", sprintf('%.1f',round(`ci_l.consumption_expenditure`, 1)), 
                                           ' to ',  sprintf('%.1f',round(`ci_u.consumption_expenditure`, 1)), ")"),
                      rural = paste0(sprintf('%.1f', round(`rural`*100, 1)), 
                                     "\n (", sprintf('%.1f',round(`ci_l.rural`*100, 1)), 
                                     ' to ',  sprintf('%.1f',round(`ci_u.rural`*100, 1)), ")")), by = c("year_factor")]

ci_table_ins <- means_ins[, .(total_health = paste0(sprintf('%.1f', round(`total_health`, 1)), 
                                              "\n (", sprintf('%.1f',round(`ci_l`, 1)), 
                                              ' to ',  sprintf('%.1f',round(`ci_u`, 1)), ")")), by = c("year_factor", "insurance")]

long_ci <- melt(setDT(ci_table), id.vars = "year_factor", variable.name = "variable")
long_ci_ins <- melt(setDT(ci_table_ins), id.vars = c("year_factor", "insurance"), variable.name = "variable")

long_ci_ins[, variable := ifelse(insurance== 0, 'total_health_unins', 'total_health_ins')]

long_ci <- rbind(long_ci, long_ci_ins[, -c("insurance")])

long <- melt(setDT(means), id.vars = "year_factor", variable.name = "variable")
long <- long[order(variable,year_factor)]
long[, difference := value - lag(value), by = c("variable")]

long_ins <- melt(setDT(means_ins), id.vars = c("year_factor", 'insurance'), variable.name = "variable")
long_ins[long_ins[,do.call(order, .SD), .SDcols = c('variable', 'insurance', 'year_factor')]]
long_ins[, difference := value - lag(value), by = c("variable", 'insurance')]
long_ins <- long_ins[1:4, ]
long_ins[, variable := ifelse(insurance == 0, 'total_health_unins', 'total_health_ins')]

long_pvalue <- merge(pvalue_df, long, by = "variable")
long_pvalue_ins <- merge(pvalue_ins, long_ins, by = "variable")
long_pvalue <- rbind(long_pvalue, long_pvalue_ins[, !('insurance')])

long_result <- merge(long_pvalue[, -c("value")], long_ci, by = c("variable", "year_factor"))
long_result <- merge(long_result, percentage_df, by = "variable")

write.csv(long_pvalue, file.path(outdir, 'peru_table1_hh.csv'))
write.csv(long_result, file.path(outdir, 'peru_table1_hh_ins_ci_se.csv'))
