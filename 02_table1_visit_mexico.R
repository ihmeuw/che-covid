#####################################################################
## Author: User                                                    ##
## Description: Mexico visit analysis table 1                      ##
##                                                                 ##
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
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2)

## directories 
indir <- 'FILEPATH'
outdir <- 'FILEPATH'

visits20 <- fread(file.path(indir, 'MEX_ENIGH_VISITS_2020.csv'))
visits18 <- fread(file.path(indir, 'MEX_ENIGH_VISITS_2018.csv'))

visits <- rbind(visits18, visits20)
visits[, year_factor := as.factor(as.numeric(year_id)-2018)]

## filter to only people who had a visit 
visits <- visits[any_visit == 1]
visits[, share_private := is_private]

#define survey design
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
design <-  svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, data = visits, variables = ~share_private+year_factor, nest = T)

#write function to get p values
get_pvalue <- function(var){
  var_lm <- as.formula(paste0(var," ~ ", "year_factor"))
  lm_model <- svyglm(var_lm, design, data = visits)
  tidy_table <- setDT(tidy(lm_model))
  pvalue <- tidy_table[[2,5]]
  return(pvalue)
}

# compute p values
vars <- c("share_private")
pvalue_df <- data.table(variable = vars, pvalue = numeric(1))

for (i in 1:length(vars)){
  var <- vars[i]
  pvalue <- get_pvalue(var)
  pvalue_df[i,2] <- pvalue
}

# compute differences
means <- svyby(~share_private, ~year_factor, design, svymean, na.rm =T, vartype = c("ci", "se")) 
long <- melt(setDT(means), id.vars = "year_factor", variable.name = "variable")
long <- long[order(variable,year_factor)]
long[, difference := value - lag(value), by = c("variable")]

long_pvalue <- merge(pvalue_df, long, by = "variable")

## calculate percent change
means <- data.table(means)

ci_table <- means[, .(share_private = paste0(sprintf('%.1f', round(`share_private`*100, 1)), 
                                             "\n (", sprintf('%.1f',round(`ci_l`*100, 1)), 
                                             ' to ',  sprintf('%.1f',round(`ci_u`*100, 1)), ")")), by = c("year_factor")]
long_ci <- melt(setDT(ci_table), id.vars = "year_factor", variable.name = "variable")

se_table <-  melt(means, id.vars = "year_factor")

se_table_means <- se_table[c(1:2) ]
se_table_means_wide <- dcast(se_table_means, variable  ~ year_factor)
setnames(se_table_means_wide, old = c("0","2"), new = c("mean_old", "mean_new"))

se_table_se <- se_table[c(3:4) ]
se_table_se_wide <- dcast(se_table_se, variable  ~ year_factor)
setnames(se_table_se_wide, old = c("0","2"), new = c("se_old", "se_new"))
se_table_se_wide[, variable := "share_private"]

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


write.csv(long_pvalue, file.path(outdir, 'mexico_table1_visits_private.csv'))
write.csv(long_result, file.path(outdir, 'mexico_table1_visit_ins_ci_se.csv'))


