## Author: User                                                       ##
## Date: 11/30/21                                                               ##
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
indiv19 <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_IND_2019.dta'), convert.factors=FALSE), stringsAsFactors = FALSE)
indiv20 <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_IND_2020.dta'), convert.factors=FALSE), stringsAsFactors = FALSE)

## combine 2019 and 2020 
indiv <- rbind(indiv19, indiv20)
indiv[, year_factor := as.factor(as.numeric(year_id)-2019)]
indiv[, any_coverage :=1-no_coverage]

#define survey design
design <-  svydesign(ids = ~nconglome, strata = ~estrato, weights = ~factor07, data = indiv, variables = ~is_female+age+
                       year_factor+share_below_post_secondary+any_visit+is_private+
                       private_coverage+public_coverage+other_coverage+no_coverage+any_coverage, nest = T)

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
means <- data.table(means)
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
ci_table_per <- means[, .(share_below_post_secondary = paste0(sprintf('%.1f', round(`share_below_post_secondary`*100, 1)))), by = c("year_factor")]

long <- melt(setDT(means), id.vars = "year_factor", variable.name = "variable")
long <- long[order(variable,year_factor)]
long[, difference := value - lag(value), by = c("variable")]
long_ci <- melt(setDT(ci_table), id.vars = "year_factor", variable.name = "variable")


long_pvalue <- merge(pvalue_df, long, by = "variable")

se_table <-  melt(means, id.vars = "year_factor")

se_table_means <- se_table[c(1:10, 41:42) ]
se_table_means_wide <- dcast(se_table_means, variable  ~ year_factor)
setnames(se_table_means_wide, old = c("0","1"), new = c("mean_old", "mean_new"))

se_table_se <- se_table[c(11:20, 43:44) ]
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


long_result <- merge(long_pvalue[, -c("value")], long_ci, by = c("variable", "year_factor"))
long_result <- merge(long_result, percentage_df, by = "variable")

write.csv(long_pvalue, file.path(outdir, 'peru_table1_ind.csv'))
write.csv(long_ci, file.path(outdir, 'peru_table1_ind_ci.csv'))

write.csv(long_result, file.path(outdir, 'peru_table1_ind_ins_ci_se.csv'))
