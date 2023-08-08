#####################################################################
## Name: Corinne Bintz                                             ##
## Purpose: Calculate CHE trends in Mexico 2018 and 2020           ##
## Date: 12/28/2021                                                ##
#####################################################################
## clear memory
rm(list=ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
  h <- paste0("/homes/", Sys.getenv("USER"), "/")
  k <- "/ihme/cc_resources/libraries"
} else {
  j <- "J:/"
  h <- "H:/"
  k <- "K:/libraries"
}

## libraries
pacman::p_load(data.table, readstata13, plyr, stringi, ggplot2, survey)

## source functions
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

## define directory 
indir <- 'FILEPATH'
outdir <- 'FILEPATH'

## read summary file with all survey years
df <- rbind(as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2020.dta')))[, year_id := 2020],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2018.dta')))[, year_id := 2018],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2016.dta')))[, year_id := 2016],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2014.dta')))[, year_id := 2014],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2012.dta')))[, year_id := 2012],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2010.dta')))[, year_id := 2010],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2008.dta')))[, year_id := 2008],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2006.dta')))[, year_id := 2006],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2004.dta')))[, year_id := 2004],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2002.dta')))[, year_id := 2002],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_2000.dta')))[, year_id := 2000],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_1998.dta')))[, year_id := 1998],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_1996.dta')))[, year_id := 1996],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_1994.dta')))[, year_id := 1994],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_1992.dta')))[, year_id := 1992],
                 as.data.table(read.dta13(file.path(indir, 'MEX_ENIGH_1989.dta')))[, year_id := 1989], fill = T)

## calculate proportion of health expenditure relative to consumption
df[, health_exp_prop := health/consumption]

## calculate che indicators at household level 
df[, che10 := ifelse(health_exp_prop > 0.1, 1, 0)]
df[, che25 := ifelse(health_exp_prop > 0.25, 1, 0)]

## calculate year_factor 
df[, year_factor := as.factor(as.numeric(year_id)-2018)]

## set up survey design by period 
## 2008-2020
svy18_20 <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, vars = ~year_factor+che10+che25,data = df[year_id %in% c(2018,2020),], nest = T)
svy12_16 <- svydesign(ids = ~est_dis, strata = ~upm, weights = ~factor, variables = ~che10+che25+year_id, data = df[year_id %in% c(2016,2014,2012), ], nest = T)
svy08_10 <- svydesign(ids = ~strata1, strata = ~upm, weights = ~factor, variables = ~che10+che25+year_id, data = df[year_id %in% c(2010, 2008), ], nest = T)
svy89_06 <- svydesign(ids = ~strata1,  weights = ~factor, variables = ~che10+che25+year_id, data = df[year_id %in% c(2006, 2004, 2002, 2000, 1998, 1996,1994, 1992,1989) & !is.na(health), ], nest = T)

## calculate survey weighted che by year 
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

che10_18_20 <- svyby(~che10, ~year_id, svy18_20, svymean)
che10_12_16 <- svyby(~che10, ~year_id, svy12_16, svymean)
che10_08_10 <- svyby(~che10, ~year_id, svy08_10, svymean)
che10_89_06 <- svyby(~che10, ~year_id, svy89_06, svymean)

che25_18_20 <- svyby(~che25, ~year_id, svy18_20, svymean)
che25_12_16 <- svyby(~che25, ~year_id, svy12_16, svymean)
che25_08_10 <- svyby(~che25, ~year_id, svy08_10, svymean)
che25_89_06 <- svyby(~che25, ~year_id, svy89_06, svymean)

## count number of households for sample size 
sample_size <- df[, .N, by = 'year_id']

## prep summary file for CHE10
## combine measures across survey years
che10 <- as.data.table(rbind(che10_18_20,
                             che10_12_16,
                             che10_08_10,
                             che10_89_06))

## attach sample sizes
che10 <- merge(che10, sample_size, by = 'year_id')

## attach NIDs 
## TODO: probably should move this to an excel file ...
che10[, nid:= ifelse(year_id == 1989, 25154, 
                     ifelse(year_id == 1992, 25173, 
                            ifelse(year_id == 1994, 25194,
                                   ifelse(year_id == 1996, 25215, 
                                          ifelse(year_id == 1998, 25235, 
                                                 ifelse(year_id == 2000, 25254, 
                                                        ifelse(year_id == 2002, 25273,
                                                               ifelse(year_id == 2004, 25293, 
                                                                      ifelse(year_id == 2006, 25335, 
                                                                             ifelse(year_id == 2008, 25358, 
                                                                                    ifelse(year_id == 2010, 93321, 
                                                                                           ifelse(year_id == 2012, 165610, 
                                                                                                  ifelse(year_id == 2014, 223633, 
                                                                                                         ifelse(year_id == 2016, 339653, 
                                                                                                                ifelse(year_id == 2018, 424269, 483875)))))))))))))))]

## attach misc parameters
che10[, location_id := 130][, age_group_id := 22][, sex_id := 3][, is_outlier := 0][, measure_id := 18]

## update column names
setnames(che10, old = c('che10', 'se', 'N'), 
         new = c('val', 'variance', 'sample_size'))

## export summary file 
write.csv(che10, 'FILEPATH', row.names = F)

## prep summary file for CHE25
## combine measures across survey years
che25 <- as.data.table(rbind(che25_18_20,
                             che25_12_16,
                             che25_08_10,
                             che25_89_06))

## attach sample sizes
che25 <- merge(che25, sample_size, by = 'year_id')

## attach NIDs 
## TODO: probably should move this to an excel file ...
che25[, nid:= ifelse(year_id == 1989, 25154, 
                     ifelse(year_id == 1992, 25173, 
                            ifelse(year_id == 1994, 25194,
                                   ifelse(year_id == 1996, 25215, 
                                          ifelse(year_id == 1998, 25235, 
                                                 ifelse(year_id == 2000, 25254, 
                                                        ifelse(year_id == 2002, 25273,
                                                               ifelse(year_id == 2004, 25293, 
                                                                      ifelse(year_id == 2006, 25335, 
                                                                             ifelse(year_id == 2008, 25358, 
                                                                                    ifelse(year_id == 2010, 93321, 
                                                                                           ifelse(year_id == 2012, 165610, 
                                                                                                  ifelse(year_id == 2014, 223633, 
                                                                                                         ifelse(year_id == 2016, 339653, 
                                                                                                                ifelse(year_id == 2018, 424269, 483875)))))))))))))))]

## attach misc parameters
che25[, location_id := 130][, age_group_id := 22][, sex_id := 3][, is_outlier := 0][, measure_id := 18]

## update column names
setnames(che25, old = c('che25', 'se', 'N'), 
         new = c('val', 'variance', 'sample_size'))

## export summary file 
write.csv(che25, 'FILEPATH', row.names = F)

## combine country-level results for table 1
che10 <-as.data.table(che10_18_20)
che25 <-as.data.table(che25_18_20)

## attach location to results 
che10[, location_name := 'Mexico']
che25[, location_name := 'Mexico']

## subset years for table 1 only
mexico_df <- df[year_id %in% c(2018,2020)]

#write function to get p values
get_pvalue <- function(var){
  var_lm <- as.formula(paste0(var," ~ ", "year_factor"))
  lm_model <- svyglm(var_lm, svy18_20, data = mexico_df)
  tidy_table <- setDT(tidy(lm_model))
  pvalue <- tidy_table[[2,5]]
  return(pvalue)
}

# compute p values
vars <- c("che10", "che25")
pvalue_df <- data.table(variable = vars, pvalue = numeric(2))

for (i in 1:length(vars)){
  var <- vars[i]
  pvalue <- get_pvalue(var)
  pvalue_df[i,2] <- pvalue
}

# compute p values (by insurance)
lm_che10_ins <- svyglm(che10 ~ year_factor, svy18_20, data = mexico_df[insurance_id == 1,])
lm_che10_unins <- svyglm(che10 ~ year_factor, svy18_20, data = mexico_df[insurance_id == 0,])

lm_che25_ins <- svyglm(che25 ~ year_factor, svy18_20, data = mexico_df[insurance_id == 1,])
lm_che25_unins <- svyglm(che25 ~ year_factor, svy18_20, data = mexico_df[insurance_id == 0,])

tidy_che10_ins <- setDT(tidy(lm_che10_ins))
tidy_che10_unins <- setDT(tidy(lm_che10_unins))

tidy_che25_ins <- setDT(tidy(lm_che25_ins))
tidy_che25_unins <- setDT(tidy(lm_che25_unins))

pvalue_che10_ins <- tidy_che10_ins[[2,5]]
pvalue_che10_unins <- tidy_che10_unins[[2,5]]

pvalue_che25_ins <- tidy_che25_ins[[2,5]]
pvalue_che25_unins <- tidy_che25_unins[[2,5]]

pvalue_che10_ins <- data.table(variable = c('che10_ins', 'che10_unins'), pvalue = c(pvalue_che10_ins, pvalue_che10_unins))
pvalue_che25_ins <- data.table(variable = c('che25_ins', 'che25_unins'), pvalue = c(pvalue_che25_ins, pvalue_che25_unins))

# compute differences
means <- svyby(~che10+che25, ~year_factor, svy18_20, svymean, na.rm =T, vartype = c("ci", "se")) 
means_che10_ins <- svyby(~che10, ~year_factor+insurance_id, svy18_20, svymean, vartype = c("ci", "se")) 
means_che25_ins <- svyby(~che25, ~year_factor+insurance_id, svy18_20, svymean, vartype =c("ci", "se")) 

long <- melt(setDT(means), id.vars = "year_factor", variable.name = "variable")
long <- long[order(variable,year_factor)]
long[, difference := value - lag(value), by = c("variable")]

long_ins <- melt(setDT(means_che10_ins), id.vars = c("year_factor", 'insurance_id'), variable.name = "variable")
long_ins[long_ins[,do.call(order, .SD), .SDcols = c('variable', 'insurance_id', 'year_factor')]]
long_ins[, difference := value - lag(value), by = c("variable", 'insurance_id')]
long_ins <- long_ins[1:4, ]
long_ins[, variable := ifelse(insurance_id == 0, 'che10_unins', 'che10_ins')]

long_che25_ins <- melt(setDT(means_che25_ins), id.vars = c("year_factor", 'insurance_id'), variable.name = "variable")
long_che25_ins[long_ins[,do.call(order, .SD), .SDcols = c('variable', 'insurance_id', 'year_factor')]]
long_che25_ins[, difference := value - lag(value), by = c("variable", 'insurance_id')]
long_che25_ins <- long_che25_ins[1:4, ]
long_che25_ins[, variable := ifelse(insurance_id == 0, 'che25_unins', 'che25_ins')]

long_pvalue <- merge(pvalue_df, long, by = "variable")
long_pvalue_che10_ins <- merge(pvalue_che10_ins, long_ins, by = "variable")
long_pvalue_che25_ins <- merge(pvalue_che25_ins, long_che25_ins, by = "variable")

long_pvalue_ins <- rbind(long_pvalue_che10_ins[, !c('insurance_id')], long_pvalue_che25_ins[, !c('insurance_id')])

## calculate percent change
means <- as.data.table(means)
means_che10_ins <- as.data.table(means_che10_ins)
means_che25_ins <- as.data.table(means_che25_ins)

che10_ins <- means_che10_ins[insurance_id== 1]
setnames(che10_ins, old= c("che10","se", "ci_l", "ci_u"), c("che10_ins", "se.che10_ins", "ci_l.che10_ins", "ci_u.che10_ins"))

che10_unins <- means_che10_ins[insurance_id == 0]
setnames(che10_unins, old= c("che10","se", "ci_l", "ci_u"), c("che10_unins", "se.che10_unins", "ci_l.che10_unins", "ci_u.che10_unins"))

che25_ins <- means_che25_ins[insurance_id== 1]
setnames(che25_ins, old= c("che25","se", "ci_l", "ci_u"), c("che25_ins", "se.che25_ins", "ci_l.che25_ins", "ci_u.che25_ins"))

che25_unins <- means_che25_ins[insurance_id == 0]
setnames(che25_unins, old= c("che25","se", "ci_l", "ci_u"), c("che25_unins", "se.che25_unins", "ci_l.che25_unins", "ci_u.che25_unins"))

che10_means <- merge(che10_ins[, -c("insurance_id")], che10_unins[, -c("insurance_id")], by = "year_factor")
che25_means <- merge(che25_ins[, -c("insurance_id")], che25_unins[, -c("insurance_id")], by = "year_factor")

means <- merge(means, che10_means, by = "year_factor")
means <- merge(means, che25_means, by = "year_factor")

ci_table <- means[, .(che10 = paste0(sprintf('%.1f', round(`che10`*100, 1)), 
                                      "\n (", sprintf('%.1f',round(`ci_l.che10`*100, 1)), 
                                      ' to ',  sprintf('%.1f',round(`ci_u.che10`*100, 1)), ")"),
                      che25 = paste0(sprintf('%.1f', round(`che25`*100, 1)), 
                                     "\n (", sprintf('%.1f',round(`ci_l.che25`*100, 1)), 
                                     ' to ',  sprintf('%.1f',round(`ci_u.che25`*100, 1)), ")"),
                      che10_ins = paste0(sprintf('%.1f', round(`che10_ins`*100, 1)), 
                                     "\n (", sprintf('%.1f',round(`ci_l.che10_ins`*100, 1)), 
                                     ' to ',  sprintf('%.1f',round(`ci_u.che10_ins`*100, 1)), ")"),
                      che10_unins = paste0(sprintf('%.1f', round(`che10_unins`*100, 1)), 
                                         "\n (", sprintf('%.1f',round(`ci_l.che10_unins`*100, 1)), 
                                         ' to ',  sprintf('%.1f',round(`ci_u.che10_unins`*100, 1)), ")"),
                      che25_ins = paste0(sprintf('%.1f', round(`che25_ins`*100, 1)), 
                                         "\n (", sprintf('%.1f',round(`ci_l.che25_ins`*100, 1)), 
                                         ' to ',  sprintf('%.1f',round(`ci_u.che25_ins`*100, 1)), ")"),
                      che25_unins = paste0(sprintf('%.1f', round(`che25_unins`*100, 1)), 
                                           "\n (", sprintf('%.1f',round(`ci_l.che25_unins`*100, 1)), 
                                           ' to ',  sprintf('%.1f',round(`ci_u.che25_unins`*100, 1)), ")")), by = c("year_factor")]
long_ci <- melt(setDT(ci_table), id.vars = "year_factor", variable.name = "variable")


se_table <-  melt(means, id.vars = "year_factor")

se_table_means <- se_table[c(1:4, 17:18, 25:26, 33:34, 41:42) ]
se_table_means_wide <- dcast(se_table_means, variable  ~ year_factor)
setnames(se_table_means_wide, old = c("0","2"), new = c("mean_old", "mean_new"))

se_table_se <- se_table[c(5:8, 19:20, 27:28, 35:36, 43:44) ]
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
long_pvalue <- rbind(long_pvalue, long_pvalue_ins)
long_result <- merge(long_pvalue[, -c("value")], long_ci, by = c("variable", "year_factor"))
long_result <- merge(long_result, percentage_df, by = "variable")

write.csv(long_pvalue, file.path(outdir, 'mexico_table1_che.csv')) 
write.csv(long_pvalue_ins, file.path(outdir, 'mexico_table1_che_ins.csv'))
write.csv(long_result, file.path(outdir, 'mexico_table1_che_ins_ci_se.csv'))
