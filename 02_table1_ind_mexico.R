## Author: User                                                        ##
## Date: 12/29/21                                                               ##
## Purpose: Calculate survey weighted mean individual level characteristics:
## share female, age, level of education,
## number of visits, number of visits in private sector, and insurance coverage
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

## directories
indir <-  'FILEPATH'
outdir <- 'FILEPATH'

## read in prepped individual level data 
indiv18 <- data.table(read.dta13(file.path(indir, 'MEX_ENIGH_IND_2018.dta'), convert.factors=FALSE), 
                      stringsAsFactors = FALSE)[,c("folioviv", "foliohog", "is_female", "age", "share_below_post_secondary", "any_visit", 
                                                   "is_private", "private_coverage", "public_coverage", "other_coverage", 
                                                   "no_coverage")]
indiv18[, year_id := 2018]

agg18 <- data.table(read.dta13("FILEPATH/MEX_ENIGH_2018.dta", convert.factors=FALSE), 
                    stringsAsFactors = FALSE)[,c("hh_id", "hh_id2", "strata1", "strata2", "upm", "factor")]

indiv18 <- merge(indiv18, agg18, by.x = c("folioviv", "foliohog"), by.y = c("hh_id", "hh_id2"))

indiv20 <- data.table(read.dta13(file.path(indir, 'MEX_ENIGH_IND_2020.dta'), convert.factors=FALSE), 
                      stringsAsFactors = FALSE)[,c("folioviv", "foliohog", "is_female", "age", "share_below_post_secondary", "any_visit", 
                                                   "is_private", "private_coverage", "public_coverage", "other_coverage", 
                                                   "no_coverage")]
indiv20[, year_id := 2020]
agg20 <- data.table(read.dta13("FILEPATH/MEX_ENIGH_2020.dta", convert.factors=FALSE), 
                    stringsAsFactors = FALSE)[,c("hh_id", "hh_id2", "strata1", "strata2", "upm", "factor")]
indiv20 <- merge(indiv20, agg20, by.x = c("folioviv", "foliohog"), by.y = c("hh_id", "hh_id2"))


## combine 2018 and 2020 
indiv <- rbind(indiv18, indiv20)
indiv[, year_factor := as.factor(as.numeric(year_id)-2018)]
indiv[, any_coverage := 1-no_coverage]

## count number of respondents by sex
count_indiv <- copy(indiv)
count_indiv[, is_male := ifelse(is_female == 1, 0, 1)]

count_indiv <- count_indiv[, .(num_male = sum(is_male),
                               num_female = sum(is_female),
                               num_indiv = sum(is_male) + sum(is_female)), by = "year_id"]

#define survey design
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
design <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, variables = ~is_female+age+
                      year_factor+share_below_post_secondary+any_visit+is_private+
                      private_coverage+public_coverage+other_coverage+no_coverage+any_coverage, data = indiv, nest = T)

#write function to get p values
get_pvalue <- function(var){
  var_lm <- as.formula(paste0(var," ~ ", "year_factor"))
  lm_model <- svyglm(var_lm, design, data = indiv)
  tidy_table <- setDT(tidy(lm_model))
  pvalue <- tidy_table[[2,5]]
  return(pvalue)
}

# compute p values
vars <- c("is_female", "age", "share_below_post_secondary", "any_visit", "is_private", "any_coverage")
pvalue_df <- data.table(variable = vars, pvalue = numeric(6))

for (i in 1:length(vars)){
  var <- vars[i]
  pvalue <- get_pvalue(var)
  pvalue_df[i,2] <- pvalue
}

# compute means and differences
means_ed <-  svyby(~share_below_post_secondary, ~year_factor, design, svymean, na.rm =T, vartype = c("ci", "se")) # has to be done separately because is only for people over 25
means <- svyby(~age+is_female+any_visit+is_private+any_coverage, ~year_factor, design, svymean, na.rm =T, vartype = c("ci", "se")) 

## save confidence intervals 
setnames(means_ed, c("ci_l", "ci_u", "se"), c("ci_l.share_below_post_secondary", "ci_u.share_below_post_secondary", "se.share_below_post_secondary"))

means <- merge(means, means_ed, by = "year_factor")
means <- as.data.table(means)
ci_table <- means[, .(share_below_post_secondary = paste0(sprintf('%.1f', round(`share_below_post_secondary`*100, 1)), 
                                                    "\n (", sprintf('%.1f',round(`ci_l.share_below_post_secondary`*100, 1)), 
                                                    ' to ',  sprintf('%.1f',round(`ci_u.share_below_post_secondary`*100, 1)), ")"),
                      age = paste0(sprintf('%.1f', round(`age`, 1)), 
                                                          "\n (", sprintf('%.1f',round(`ci_l.age`, 1)), 
                                                          ' to ',  sprintf('%.1f',round(`ci_u.age`, 1)), ")"),
                      is_female = paste0(sprintf('%.1f', round(`is_female`*100, 1)), 
                                   "\n (", sprintf('%.1f',round(`ci_l.is_female`*100, 1)), 
                                   ' to ',  sprintf('%.1f',round(`ci_u.is_female`*100, 1)), ")"),
                      any_visit = paste0(sprintf('%.1f', round(`any_visit`*100, 1)), 
                                         "\n (", sprintf('%.1f',round(`ci_l.any_visit`*100, 1)), 
                                         ' to ',  sprintf('%.1f',round(`ci_u.any_visit`*100, 1)), ")"),
                      is_private = paste0(sprintf('%.1f', round(`is_private`*100, 1)), 
                                         "\n (", sprintf('%.1f',round(`ci_l.is_private`*100, 1)), 
                                         ' to ',  sprintf('%.1f',round(`ci_u.is_private`*100, 1)), ")"),
                      any_coverage = paste0(sprintf('%.1f', round(`any_coverage`*100, 1)), 
                                          "\n (", sprintf('%.1f',round(`ci_l.any_coverage`*100, 1)), 
                                          ' to ',  sprintf('%.1f',round(`ci_u.any_coverage`*100, 1)), ")")), by = c("year_factor")]

long <- melt(setDT(means), id.vars = "year_factor", variable.name = "variable")
long_ci <- melt(setDT(ci_table), id.vars = "year_factor", variable.name = "variable")
long <- long[order(variable,year_factor)]
long[, difference := value - lag(value), by = c("variable")]

long_pvalue <- merge(pvalue_df, long, by = "variable")


se_table <-  melt(means, id.vars = "year_factor")

se_table_means <- se_table[c(1:10, 41:42) ]
se_table_means_wide <- dcast(se_table_means, variable  ~ year_factor)
setnames(se_table_means_wide, old = c("0","2"), new = c("mean_old", "mean_new"))

se_table_se <- se_table[c(11:20, 43:44) ]
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

long_result <- merge(long_pvalue[, -c("value")], long_ci, by = c("variable", "year_factor"))
long_result <- merge(long_result, percentage_df, by = "variable")

write.csv(long_pvalue, file.path(outdir, 'mexico_table1_ind.csv'))
write.csv(long_ci, file.path(outdir, 'mexico_table1_ind_ci.csv'))
write.csv(long_result, file.path(outdir, 'mexico_table1_ind_ins_ci_se.csv'))

