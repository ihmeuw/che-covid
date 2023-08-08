################################################################################
## Author: User                                                               ##
## Date: 12/28/21                                                             ##
## Purpose: Calculate survey weighted mean household level characteristics:   ##
## share rural, OOP total health, OOP drugs, OOP outpatient                   ##
## OOP hospitalizations, consumption                                          ##
################################################################################
## clear memory
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
pacman::p_load(data.table, haven,readstata13, readxl, ggplot2,dplyr,survey, broom)

## source currency conversion script
source(file.path(h, 'frp/currency_conversion.R'))

## indir
indir <- 'FILEPATH'
outdir <- 'FILEPATH'

#read in mexico 2020
mexico_agg20 <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2020.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)
mexico_agg20[, year_id := 2020]

#read in mexico 2018
mexico_agg18 <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2018.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)
mexico_agg18[, year_id := 2018]

#combine
mexico_agg <- rbind(mexico_agg18, mexico_agg20)

#add year as factor column
mexico_agg[, year_factor := as.factor(as.numeric(year_id)-2018)]
mexico_agg[, ihme_loc_id := "MEX"]

# run currency conversion script 
mexico_2018 <- mexico_agg[year_id == 2018]
mexico_2018 <- currency_conversion(data = mexico_2018, 
                                   col.loc = "ihme_loc_id", 
                                   col.value = c("drugs", "outpatient", "hospital", "consumption", "health"), 
                                   currency = 'lcu',
                                   col.currency.year = 'year_id', 
                                   base.unit = 'ppp',
                                   base.year = 2021)
mexico_2018[,year_id := 2018]

mexico_2020 <- mexico_agg[year_id == 2020]

mexico_2020 <- currency_conversion(data = mexico_2020, 
                                   col.loc = "ihme_loc_id", 
                                   col.value = c("drugs", "outpatient", "hospital", "consumption", "health"), 
                                   currency = 'lcu',
                                   col.currency.year = 'year_id', 
                                   base.unit = 'ppp',
                                   base.year = 2021)
mexico_2020[,year_id := 2020]

mexico_agg_table <- rbind(mexico_2018, mexico_2020)

#define survey design
design <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, variables = ~consumption+outpatient+rural+hospital+drugs+health+year_factor+insurance_id, data = mexico_agg_table[!is.na(insurance_id)], nest = T)
design_non_insurance <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, variables = ~consumption+rural+health+year_factor, data = mexico_agg_table, nest = T) #to match appendix 

#write function to get p values
get_pvalue <- function(var){
  var_lm <- as.formula(paste0(var," ~ ", "year_factor"))
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  lm_model <- svyglm(var_lm, design_non_insurance, data = mexico_agg_table)
  tidy_table <- setDT(tidy(lm_model))
  pvalue <- tidy_table[[2,5]]
  return(pvalue)
}

# compute p values
vars <- c("health", "consumption",  "rural")
pvalue_df <- data.table(variable = vars, pvalue = numeric(3))

for (i in 1:length(vars)){
  var <- vars[i]
  pvalue <- get_pvalue(var)
  pvalue_df[i,2] <- pvalue
}
nrow(mexico_agg_table[insurance_id == 1])
nrow(mexico_agg_table[insurance_id == 0])

# compute p values (by insurance)
lm_model_ins <- svyglm(health ~ year_factor , design, data = mexico_agg_table[insurance_id == 1])
lm_model_unins <- svyglm(health ~ year_factor , design, data = mexico_agg_table[insurance_id == 0])

tidy_table_ins <- setDT(tidy(lm_model_ins))
tidy_table_unins <- setDT(tidy(lm_model_unins))

pvalue_ins <- tidy_table_ins[[2,5]]
pvalue_unins <- tidy_table_unins[[2,5]]

pvalue_ins <- data.table(variable = c('health_ins', 'health_unins'), pvalue = c(pvalue_ins, pvalue_unins))

# compute differences
means <- svyby(~health+consumption+rural, ~year_factor, design_non_insurance, svymean, vartype = c("se", "ci")) 
means_ins <- svyby(~health, ~year_factor+insurance_id, design, svymean, vartype = c("se", "ci"), se.fit = T) 

means <- data.table(means)
means_ins <- data.table(means_ins)

health_ins <- means_ins[insurance_id == 1]
setnames(health_ins, old= c("health","se", "ci_l", "ci_u"), c("health_ins", "se.health_ins", "ci_l.health_ins", "ci_u.health_ins"))

health_unins <- means_ins[insurance_id == 0]
setnames(health_unins, old= c("health","se", "ci_l", "ci_u"), c("health_unins", "se.health_unins", "ci_l.health_unins", "ci_u.health_unins"))

health_ins_unins <- merge(health_ins[, -c("insurance_id")], health_unins[, -c("insurance_id")], by = "year_factor")
means <- merge(means, health_ins_unins)

se_table <-  melt(means, id.vars = "year_factor")

se_table_means <- se_table[c(1:6, 25:26, 33:34) ]
se_table_means_wide <- dcast(se_table_means, variable  ~ year_factor)
setnames(se_table_means_wide, old = c("0","2"), new = c("mean_old", "mean_new"))

se_table_se <- se_table[c(7:12, 27:28, 35:36) ]
se_table_se_wide <- dcast(se_table_se, variable  ~ year_factor)
setnames(se_table_se_wide, old = c("0","2"), new = c("se_old", "se_new"))
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


ci_table <- means[, .(health = paste0(sprintf('%.1f', round(`health`, 1)), 
                                                          "\n (", sprintf('%.1f',round(`ci_l.health`, 1)), 
                                                          ' to ',  sprintf('%.1f',round(`ci_u.health`, 1)), ")"),
                    
                      consumption = paste0(sprintf('%.1f', round(`consumption`, 1)), 
                                          "\n (", sprintf('%.1f',round(`ci_l.consumption`, 1)), 
                                          ' to ',  sprintf('%.1f',round(`ci_u.consumption`, 1)), ")"),
                      rural = paste0(sprintf('%.1f', round(`rural`*100, 1)), 
                                            "\n (", sprintf('%.1f',round(`ci_l.rural`*100, 1)), 
                                            ' to ',  sprintf('%.1f',round(`ci_u.rural`*100, 1)), ")")), by = c("year_factor")]
ci_table_ins <- means_ins[, .(health = paste0(sprintf('%.1f', round(`health`, 1)), 
                                      "\n (", sprintf('%.1f',round(`ci_l`, 1)), 
                                      ' to ',  sprintf('%.1f',round(`ci_u`, 1)), ")")), by = c("year_factor", "insurance_id")]

long_ci <- melt(setDT(ci_table), id.vars = "year_factor", variable.name = "variable")
long_ci_ins <- melt(setDT(ci_table_ins), id.vars = c("year_factor", "insurance_id"), variable.name = "variable")

long_ci_ins[, variable := ifelse(insurance_id == 0, 'health_unins', 'health_ins')]

long_ci <- rbind(long_ci, long_ci_ins[, -c("insurance_id")])


long <- melt(setDT(means), id.vars = "year_factor", variable.name = "variable")
long <- long[order(variable,year_factor)]
long[, difference := value - lag(value), by = c("variable")]

long_ins <- melt(setDT(means_ins), id.vars = c("year_factor", 'insurance_id'), variable.name = "variable")
long_ins[long_ins[,do.call(order, .SD), .SDcols = c('variable', 'insurance_id', 'year_factor')]]
long_ins[, difference := value - lag(value), by = c("variable", 'insurance_id')]
long_ins <- long_ins[1:4, ]
long_ins[, variable := ifelse(insurance_id == 0, 'health_unins', 'health_ins')]

long_pvalue <- merge(pvalue_df, long, by = "variable")
long_pvalue_ins <- merge(pvalue_ins, long_ins, by = "variable")
long_pvalue <- rbind(long_pvalue, long_pvalue_ins[, !('insurance_id')])

long_result <- merge(long_pvalue[, -c("value")], long_ci, by = c("variable", "year_factor"))
long_result <- merge(long_result, percentage_df, by = "variable")

## output new csv 
write.csv(long_pvalue, file.path(outdir, 'mexico_table1_hh.csv'))

write.csv(long_ci, file.path(outdir, 'mexico_table1_hh_ci.csv'))
write.csv(long_ci_ins, file.path(outdir, 'mexico_table1_hh_ins_ci.csv'))
write.csv(long_result, file.path(outdir, 'mexico_table1_hh_ins_ci_se.csv'))
