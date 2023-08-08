################################################################################
## Author: User                                  ##
## Date: 12/21/21                                                             ##
## Purpose: Calculate Peru CHE                                                ##
################################################################################
## clear memory
rm(list=ls())
library(vctrs, lib.loc = "/ihme/singularity-images/rstudio/lib/4.1.3.4")
## runtime configuration
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/home/j'
  h <- file.path('/homes', Sys.getenv('USER'))
} else {
  j <- 'J:/'
  h <- 'H:/'
}

## libraries
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl,survey,dplyr, broom, stringr)

## define directory 
indir <- "FILEPATH"
outdir <- "FILEPATH"

  ## read all extracted datasets 
  peru_df <- rbind(as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2020.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2019.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2018.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2016.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2015.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2014.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2012.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2011.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2010.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2009.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2008.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2007.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2006.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2005.dta'))),
              as.data.table(read.dta13(file.path(indir, 'PER_ENAHO_2004.dta'))), fill = T)
  
peru_df[, year_factor := as.factor(as.numeric(year_id)-2019)]


## calculate proportion of health expenditure relative to consumption
peru_df[, health_exp_prop := total_health/consumption_expenditure]

## calculate che indicators at household level 
peru_df[, che10 := ifelse(health_exp_prop > 0.1, 1, 0)]
peru_df[, che25 := ifelse(health_exp_prop > 0.25, 1, 0)]

#survey design 
svy16_20<- svydesign(ids = ~nconglome, strata = ~estrsocial, vars = ~che10+che25+year_factor, weights = ~factor07, data = peru_df[year_id %in% seq(2016,2020)], nest = T) #2016-2020

#use estrato for 2014-2015 because there are NA's in estrsocial in 2014
svy14_15<- svydesign(ids = ~conglome, strata = ~estrato, vars = ~che10+che25+year_factor, weights = ~factor07, data = peru_df[year_id %in% c(2014,2015)], nest = T) #2014-2015
svy04_13<- svydesign(ids = ~conglome,  strata= ~estrato, vars = ~che10+che25+year_factor, weights = ~factor07, data = peru_df[year_id %in% seq(2004,2013)], nest = T) #2004-2013

## calculate survey-weighted mean for CHE at the 10 and 25 percent thresholds at the country-level
che10_16_20 <- svyby(~che10, ~year_id, svy16_20 , svymean) %>%
  mutate(ihme_loc_id = "PER")
che10_14_15 <- svyby(~che10, ~year_id, svy14_15 , svymean) %>%
  mutate(ihme_loc_id = "PER")
che10_04_13<- svyby(~che10, ~year_id, svy04_13 , svymean) %>%
  mutate(ihme_loc_id = "PER")

che10 <- rbind(che10_16_20, che10_14_15,che10_04_13 )

che25_16_20 <- svyby(~che25, ~year_id, svy16_20 , svymean) %>%
  mutate(ihme_loc_id = "PER")
che25_14_15 <- svyby(~che25, ~year_id, svy14_15 , svymean) %>%
  mutate(ihme_loc_id = "PER")
che25_04_13<- svyby(~che25, ~year_id, svy04_13 , svymean) %>%
  mutate(ihme_loc_id = "PER")

che25 <- rbind(che25_16_20, che25_14_15,che25_04_13 )

peru_df[, che10_unins := ifelse(insurance == 0, che10, NA)]
peru_df[, che10_ins := ifelse(insurance == 1, che10, NA)]

peru_df[, che25_unins := ifelse(insurance == 0, che25, NA)]
peru_df[, che25_ins := ifelse(insurance == 1, che25, NA)]


svy19_20<- svydesign(ids = ~nconglome, strata = ~estrsocial, vars = ~che10_ins + che10_unins+ che10+che25_unins + che25_ins+ che25+year_factor, weights = ~factor07, data = peru_df[year_id %in% seq(2019,2020)], nest = T) #2019-2020

#write function to get p values
get_pvalue <- function(var){
  var_lm <- as.formula(paste0(var," ~ ", "year_factor"))
  lm_model <- svyglm(var_lm, svy19_20, data = peru_df[year_id %in% seq(2019,2020)])
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
lm_che10_ins <- svyglm(che10_ins ~ year_factor, svy19_20, data = peru_df[year_id %in% seq(2019,2020)])
lm_che10_unins <- svyglm(che10_unins ~ year_factor, svy19_20, data = peru_df[year_id %in% seq(2019,2020)])

lm_che25_ins <- svyglm(che25_ins ~ year_factor, svy19_20, data = peru_df[ year_id %in% seq(2019,2020)])
lm_che25_unins <- svyglm(che25_unins ~ year_factor, svy19_20, data = peru_df[ year_id %in% seq(2019,2020)])

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
means <- svyby(~che10+che25, ~year_factor, svy19_20, svymean, na.rm =T, vartype = c("ci", "se") )
means_che10_ins <- svyby(~che10, ~year_factor+insurance, svy19_20, svymean, vartype =  c("ci", "se") )
means_che25_ins <- svyby(~che25, ~year_factor+insurance, svy19_20, svymean, vartype =  c("ci", "se") )

means <- data.table(means)
means_che10_ins <- data.table(means_che10_ins)
means_che25_ins <- data.table(means_che25_ins)

che10_ins <- means_che10_ins[insurance== 1]
setnames(che10_ins, old= c("che10","se", "ci_l", "ci_u"), c("che10_ins", "se.che10_ins", "ci_l.che10_ins", "ci_u.che10_ins"))

che10_unins <- means_che10_ins[insurance == 0]
setnames(che10_unins, old= c("che10","se", "ci_l", "ci_u"), c("che10_unins", "se.che10_unins", "ci_l.che10_unins", "ci_u.che10_unins"))

che25_ins <- means_che25_ins[insurance == 1]
setnames(che25_ins, old= c("che25","se", "ci_l", "ci_u"), c("che25_ins", "se.che25_ins", "ci_l.che25_ins", "ci_u.che25_ins"))

che25_unins <- means_che25_ins[insurance == 0]
setnames(che25_unins, old= c("che25","se", "ci_l", "ci_u"), c("che25_unins", "se.che25_unins", "ci_l.che25_unins", "ci_u.che25_unins"))

che10_means <- merge(che10_ins[, -c("insurance")], che10_unins[, -c("insurance")], by = "year_factor")
che25_means <- merge(che25_ins[, -c("insurance")], che25_unins[, -c("insurance")], by = "year_factor")

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
setnames(se_table_means_wide, old = c("0","1"), new = c("mean_old", "mean_new"))

se_table_se <- se_table[c(5:8, 19:20, 27:28, 35:36, 43:44) ]
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

long <- se_table_means[order(variable,year_factor)]
long[, difference := value - lag(value), by = c("variable")]

pvalue_df <- rbind(pvalue_df, pvalue_che10_ins)
pvalue_df <- rbind(pvalue_df, pvalue_che25_ins)

long_pvalue <- merge(pvalue_df, long, by = "variable")

long_result <- merge(long_pvalue[, -c("value")], long_ci, by = c("variable", "year_factor"))
long_result <- merge(long_result, percentage_df, by = "variable")

write.csv(long_result, file.path(outdir, 'peru_table1_che_ins_ci_se.csv'))


write.csv(long_pvalue, file.path(outdir, 'peru_table1_che_updated_sep_2022.csv'))
write.csv(long_pvalue_ins, file.path(outdir, 'peru_table1_che_ins_updated_sep_2022.csv'))

write.csv(che10, file.path(outdir, 'peru_che_10_updated_sep_2022.csv'))
write.csv(che25, file.path(outdir, 'peru_che_25_updated_sep_2022.csv'))

check <- fread(file.path(outdir, 'peru_che_25_updated_sep_2022.csv'))
check <- fread(file.path(outdir, 'peru_table1_che_ins_updated_sep_2022.csv'))
check <- fread(file.path(outdir, 'peru_table1_che_updated_sep_2022.csv'))
