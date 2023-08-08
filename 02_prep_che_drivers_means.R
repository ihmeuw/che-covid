################################################################################
## Author: User                                                              ##
## Date: 11/10/2022                                                           ##
## Purpose: Prep means of CHE drivers                                        ##
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
pacman::p_load(data.table, haven,readstata13, readxl, ggplot2,dplyr,survey, broom, stringr)

## source currency conversion script
source(file.path(h, 'frp/currency_conversion.R'))

## indir
indir <- 'FILEPATH'
outdir <- 'FILEPATH'

#read in household data 
mexico_hh <- data.table(rbind(data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/processed/MEX_ENIGH_2020.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2020], 
                              data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/processed/MEX_ENIGH_2018.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2018],
                              data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/processed/MEX_ENIGH_2016.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2016],
                              data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/processed/MEX_ENIGH_2014.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2014],
                              data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/processed/MEX_ENIGH_2012.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2012], 
                              data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/processed/MEX_ENIGH_2010.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2010], 
                              data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/processed/MEX_ENIGH_2008.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2008],
                              data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/processed/MEX_ENIGH_2006.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2006],fill = T))

peru_hh <- rbind(as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2020.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2019.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2018.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2016.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2015.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2014.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2012.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2011.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2010.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2009.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2008.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2007.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2006.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2005.dta'))),
                 as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_2004.dta'))), fill = T)

## read in individual level data 
mexico_indiv <- rbind(as.data.table(read.dta13(file.path("/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_IND_2020.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2020][,c("folioviv", "foliohog", "is_female", "age", "share_below_post_secondary", "any_visit", 
                                                                                                                                                                                                                     "is_private", "private_coverage", "public_coverage", "other_coverage", 
                                                                                                                                                                                                                     "no_coverage", "year_id")],
                      as.data.table(read.dta13(file.path("/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_IND_2018.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2018][,c("folioviv", "foliohog", "is_female", "age", "share_below_post_secondary", "any_visit", 
                                                                                                                                                                                                                     "is_private", "private_coverage", "public_coverage", "other_coverage", 
                                                                                                                                                                                                                     "no_coverage", "year_id")],
                      as.data.table(read.dta13(file.path("/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_IND_2016.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2016][,c("folioviv", "foliohog", "is_female", "age", "share_below_post_secondary", "any_visit", 
                                                                                                                                                                                                                     "is_private", "private_coverage", "public_coverage", "other_coverage", 
                                                                                                                                                                                                                     "no_coverage", "year_id")], 
                      as.data.table(read.dta13(file.path("/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_IND_2014.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2014][,c("folioviv", "foliohog", "is_female", "age", "share_below_post_secondary", "any_visit", 
                                                                                                                                                                                                                     "is_private", "private_coverage", "public_coverage", "other_coverage", 
                                                                                                                                                                                                                     "no_coverage", "year_id")],
                      as.data.table(read.dta13(file.path("/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_IND_2012.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2012][,c("folioviv", "foliohog", "is_female", "age", "share_below_post_secondary",  "private_coverage", "public_coverage", "other_coverage", 
                                                                                                                                                                                                                     "no_coverage", "year_id")], 
                      as.data.table(read.dta13(file.path("/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_IND_2010.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2010][,c("folioviv", "foliohog", "is_female", "age", "share_below_post_secondary", "private_coverage", "public_coverage", "other_coverage", 
                                                                                                                                                                                                                     "no_coverage", "year_id")],
                      as.data.table(read.dta13(file.path("/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_IND_2008.dta"), convert.factors=FALSE), stringsAsFactors = FALSE)[, year_id := 2008][,c("folioviv", "foliohog", "is_female", "age", "share_below_post_secondary",  "private_coverage", "public_coverage", "other_coverage", 
                                                                                                                                                                                                                    "no_coverage", "year_id")], fill=T)
## convert no coverage to any coverage
mexico_indiv[, any_coverage := 1-no_coverage]
         
peru_indiv<- rbind(as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2020.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2019.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2018.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2016.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2015.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2014.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2012.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2011.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2010.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2009.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2008.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2007.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2006.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2005.dta'))),
                   as.data.table(read.dta13(file.path('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_IND_2004.dta'))),fill = T)

## convert no coverage to any coverage
peru_indiv[, any_coverage := 1-no_coverage]

mexico_visits <- rbind(fread('/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_VISITS_2008.csv'),
                       fread('/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_VISITS_2010.csv'),
                       fread('/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_VISITS_2012.csv'),
                       fread('/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_VISITS_2014.csv'),
                       fread('/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_VISITS_2016.csv'),
                       fread('/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_VISITS_2018.csv'),
                       fread('/mnt/share/scratch/projects/hssa/frp/processed/MEX_ENIGH/MEX_ENIGH_VISITS_2020.csv'), fill = T)


peru_visits <- rbind(fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2004_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2005_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2006_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2007_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2008_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2009_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2010_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2011_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2012_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2014_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2015_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2016_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2018_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2019_updated.csv'),
                     fread('/ihme/scratch/projects/hssa/frp/processed/peru_enaho/PER_ENAHO_VISITS_2020_updated.csv'), fill = T)

## attach weighting variables to individual level data in Mexico
mexico_indiv <- merge(mexico_hh[, c('strata1', 'strata2', 'upm', 'est_dis', 'factor', 'hh_id', 'hh_id2', 'year_id')], mexico_indiv, by.x = c('hh_id', 'hh_id2', 'year_id'), by.y = c('folioviv', 'foliohog', 'year_id'))

## currency conversion
mexico_hh[, ihme_loc_id := 'MEX']
mexico_df1 <- data.table()
for (year in unique(mexico_hh$year_id)){
  print(year)
  mex_cc <- as.data.table(currency_conversion(data = mexico_hh[year_id == year,],
                                              col.loc = "ihme_loc_id",
                                              col.value = c("consumption"),
                                              currency = 'lcu',
                                              col.currency.year = 'year_id',
                                              base.unit = 'ppp', base.year = 2021))
  mex_cc[, year_id := year]
  mexico_df1 <- as.data.table(rbind(mexico_df1, mex_cc, fill = T))
}

mexico_df2 <- data.table()
for (year in unique(mexico_df1$year_id)){
  print(year)
  mex_cc <- as.data.table(currency_conversion(data = mexico_df1[year_id == year,],
                                              col.loc = "ihme_loc_id",
                                              col.value = c("health"),
                                              currency = 'lcu',
                                              col.currency.year = 'year_id',
                                              base.unit = 'ppp', base.year = 2021))
  mex_cc[, year_id := year]
  mexico_df2 <- as.data.table(rbind(mexico_df2, mex_cc, fill = T))
}

peru_hh[, ihme_loc_id := 'PER']
peru_df1 <- data.table()
for (year in unique(peru_hh$year_id)){
  print(year)
  per_cc <- as.data.table(currency_conversion(data = peru_hh[year_id == year,],
                                              col.loc = "ihme_loc_id",
                                              col.value = c("consumption_expenditure"),
                                              currency = 'lcu',
                                              col.currency.year = 'year_id',
                                              base.unit = 'ppp',
                                              base.year = 2021))
  per_cc[, year_id := year]
  peru_df1 <- as.data.table(rbind(peru_df1, per_cc, fill = T))
}

peru_df2 <- data.table()
for (year in unique(peru_hh$year_id)){
  print(year)
  per_cc <- as.data.table(currency_conversion(data = peru_hh[year_id == year,],
                                              col.loc = "ihme_loc_id",
                                              col.value = c("total_health"),
                                              currency = 'lcu',
                                              col.currency.year = 'year_id',
                                              base.unit = 'ppp',
                                              base.year = 2021))
  per_cc[, year_id := year]
  peru_df2 <- as.data.table(rbind(peru_df2, per_cc, fill = T))
}

## combine measures 
mexico_df <- cbind(mexico_df1[, !c('health')], mexico_df2[, c('health')])
peru_df <- cbind(peru_df1[, !c('total_health')], peru_df2[, c('total_health')])

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

## define survey design
## mexico 
svy18_20 <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, variables = ~consumption+health+year_id, data = mexico_df[year_id %in% c(2018, 2020)], nest = T)
svy12_16 <- svydesign(ids = ~est_dis, strata = ~upm, weights = ~factor, variables = ~consumption+health+year_id, data = mexico_df[year_id %in% c(2016,2014, 2012), ], nest = T)
svy08_10 <- svydesign(ids = ~strata1, strata = ~upm, weights = ~factor, variables = ~consumption+health+year_id, data = mexico_df[year_id %in% c(2010, 2008), ], nest = T)

svy06 <- svydesign(ids = ~strata1, weights = ~factor, variables = ~consumption+health+year_id, data = mexico_df[year_id %in% c(2006) & !is.na(health), ], nest = T)

ind18_20 <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, variables = ~any_coverage+any_visit+is_private+year_id, data = mexico_indiv[year_id %in% c(2018, 2020)], nest = T)
ind14_16 <- svydesign(ids = ~est_dis, strata = ~upm, weights = ~factor, variables = ~any_coverage+any_visit+is_private+year_id, data = mexico_indiv[year_id %in% c(2016,2014), ], nest = T)
ind12 <- svydesign(ids = ~est_dis, strata = ~upm, weights = ~factor, variables = ~any_coverage+year_id, data = mexico_indiv[year_id %in% c(2012), ], nest = T)
ind08_10 <- svydesign(ids = ~strata1, strata = ~upm, weights = ~factor, variables = ~any_coverage+year_id, data = mexico_indiv[year_id %in% c(2010, 2008) & !is.na(any_coverage), ], nest = T)

## here need to filter to any visit == 1
vis18_20 <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, data = mexico_visits[year_id %in% c(2020, 2018) & any_visit == 1], variables = ~is_private+year_id, nest = T)
vis16 <- svydesign(ids = ~upm, weights = ~factor, data = mexico_visits[year_id %in% c(2016) & any_visit == 1], variables = ~is_private+year_id, nest = T)
vis14 <- svydesign(ids = ~hh_id, weights = ~factor, data = mexico_visits[year_id %in% c(2014) & any_visit == 1], variables = ~is_private+year_id, nest = T)

## peru 
svy16_20<- svydesign(ids = ~nconglome, strata = ~estrsocial, vars = ~total_health+consumption_expenditure+any_coverage+year_id, weights = ~factor07, data = peru_df[year_id %in% seq(2016,2020)], nest = T) #2016-2020
svy14_15<- svydesign(ids = ~conglome, strata = ~estrato, vars = ~total_health+consumption_expenditure+any_coverage+year_id, weights = ~factor07, data = peru_df[year_id %in% c(2014,2015)], nest = T) #2014-2015
svy10_13<- svydesign(ids = ~conglome, strata= ~estrato, vars = ~total_health+consumption_expenditure+any_coverage+year_id, weights = ~factor07, data = peru_df[year_id %in% seq(2010,2013)], nest = T) #2010-2013
svy04_09<- svydesign(ids = ~conglome, strata= ~estrato, vars = ~total_health+consumption_expenditure+any_coverage+year_id, weights = ~factor07, data = peru_df[year_id %in% seq(2004,2009)], nest = T) #2006-2009

ind16_20<- svydesign(ids = ~nconglome, strata = ~estrato, vars = ~no_coverage+any_visit+is_private+year_id, weights = ~factor07, data = peru_indiv[year_id %in% seq(2016,2020) & !is.na(no_coverage) & !is.na(any_visit) & !is.na(is_private)], nest = T) #2016-2020
ind14_15<- svydesign(ids = ~conglome, strata = ~estrato,  vars = ~no_coverage+any_visit+is_private+year_id, weights = ~factor07, data = peru_indiv[year_id %in% c(2014,2015) & !is.na(no_coverage) & !is.na(any_visit) & !is.na(is_private)], nest = T) #2014-2015
ind10_13<- svydesign(ids = ~conglome, strata = ~estrato, vars = ~no_coverage+any_visit+is_private+year_id, weights = ~factor07, data = peru_indiv[year_id %in% seq(2010,2013) & !is.na(no_coverage) & !is.na(any_visit) & !is.na(is_private)], nest = T) #2010-2013
ind04_09<- svydesign(ids = ~conglome, strata = ~estrato, vars = ~no_coverage+any_visit+is_private+year_id, weights = ~factor, data = peru_indiv[year_id %in% seq(2004,2009) & !is.na(factor) & !is.na(no_coverage) & !is.na(any_visit) & !is.na(is_private)], nest = T) #2006-2009

## filter to any visit == 1
vis16_20 <- svydesign(ids = ~nconglome, strata = ~estrato, weights = ~factor07, data = peru_visits[year_id %in% seq(2016, 2020) & any_visit == 1], variables = ~is_private+year_id, nest = T)
vis10_15 <- svydesign(ids = ~conglome, strata = ~estrato, weights = ~factor07, data = peru_visits[year_id %in% seq(2010,2015) & any_visit == 1], variables = ~is_private+year_id, nest = T)
vis04_09<- svydesign(ids = ~conglome, strata = ~estrato, vars = ~is_private+year_id, weights = ~factor, data = peru_visits[year_id %in% seq(2004,2009) & !is.na(factor) & any_visit == 1], nest = T) #2006-2009

# compute survey weighted values by year (household level)
mex18_20 <- svyby(~health+consumption, ~year_id, svy18_20, svymean, vartype = c("ci", "se")) 
mex12_16 <- svyby(~health+consumption, ~year_id, svy12_16, svymean,vartype = c("ci", "se")) 
mex08_10 <- svyby(~health+consumption, ~year_id, svy08_10, svymean,vartype = c("ci", "se")) 
mex06 <- svyby(~health+consumption, ~year_id, svy06, svymean,vartype = c("ci", "se")) 
mex_hh <- as.data.table(rbind(mex18_20, mex12_16, mex08_10))
mex_hh <- as.data.table(rbind(mex_hh, mex06, fill = T))

per16_20 <- svyby(~total_health+consumption_expenditure, ~year_id, svy16_20, svymean,vartype = c("ci", "se")) 
per14_15 <- svyby(~total_health+consumption_expenditure, ~year_id, svy14_15, svymean,vartype = c("ci", "se")) 
per10_13 <- svyby(~total_health+consumption_expenditure, ~year_id, svy10_13, svymean,vartype = c("ci", "se")) 
per04_09 <- svyby(~total_health+consumption_expenditure, ~year_id, svy04_09, svymean,vartype = c("ci", "se")) 
per_hh <- as.data.table(rbind(per16_20, per14_15 , per10_13, per04_09))

# compute survey weighted values by year (indvidual level)
mex_ind18_20 <- svyby(~any_coverage+any_visit+is_private, ~year_id, ind18_20, svymean,vartype = c("ci", "se")) 
mex_ind14_16 <- svyby(~any_coverage+any_visit+is_private, ~year_id, ind14_16, svymean,vartype = c("ci", "se")) 
mex_ind12 <- svyby(~any_coverage, ~year_id, ind12, svymean,vartype = c("ci", "se")) 
setnames(mex_ind12, old = c('ci_l', 'ci_u', 'se'), new = c('ci_l.any_coverage', 'ci_u.any_coverage', 'se.any_coverage'))
mex_ind08_10 <- svyby(~any_coverage, ~year_id, ind08_10, svymean,vartype = c("ci", "se")) 
setnames(mex_ind08_10, old = c('ci_l', 'ci_u', 'se'), new = c('ci_l.any_coverage', 'ci_u.any_coverage','se.any_coverage'))
mex_ind <- as.data.table(rbind(mex_ind18_20, mex_ind14_16))
mex_ind <- as.data.table(rbind(mex_ind, mex_ind12, mex_ind08_10, fill = T))

per_ind16_20 <- svyby(~any_coverage+any_visit+is_private, ~year_id, ind16_20, svymean,vartype = c("ci", "se")) 
per_ind14_15 <- svyby(~any_coverage+any_visit+is_private, ~year_id, ind14_15, svymean,vartype = c("ci", "se")) 
per_ind10_13 <- svyby(~any_coverage+any_visit+is_private, ~year_id, ind10_13, svymean,vartype = c("ci", "se")) 
per_ind04_09 <- svyby(~any_coverage+any_visit+is_private, ~year_id, ind04_09, svymean,vartype = c("ci", "se")) 
per_ind <- as.data.table(rbind(per_ind16_20, per_ind14_15 , per_ind10_13, per_ind04_09))

# compute survey weighted values by year (visit level)
mex_vis18_20 <- svyby(~is_private, ~year_id, vis18_20, svymean,vartype = c("ci", "se")) 
mex_vis16 <- svyby(~is_private, ~year_id, vis16, svymean,vartype = c("ci", "se")) 
mex_vis14 <- svyby(~is_private, ~year_id, vis14, svymean,vartype = c("ci", "se")) 
mex_vis <- as.data.table(rbind(mex_vis18_20, mex_vis16, mex_vis14))
colnames(mex_vis)[2] <- "share_private"

per_vis16_20 <- svyby(~is_private, ~year_id, vis16_20, svymean,vartype = c("ci", "se")) 
per_vis10_15 <- svyby(~is_private, ~year_id, vis10_15, svymean,vartype = c("ci", "se")) 
per_vis04_09 <- svyby(~is_private, ~year_id, vis04_09, svymean,vartype = c("ci", "se")) 
per_vis <- as.data.table(rbind(per_vis16_20, per_vis10_15, per_vis04_09))
colnames(per_vis)[2] <- "share_private"

## format for plotting: issue with 2006
mex_hh_vals <- melt.data.table(mex_hh[,c('health', 'consumption', 'year_id')], id='year_id', value.name = 'value')
mex_hh_lower <- melt.data.table(mex_hh[,c('ci_l.health', 'ci_l.consumption',  'year_id')], id='year_id', value.name = 'lower')
mex_hh_lower[, variable := str_replace(variable, 'ci_l.', '')]
mex_hh_upper <- melt.data.table(mex_hh[,c('ci_u.health', 'ci_u.consumption',  'year_id')], id='year_id', value.name = 'upper')
mex_hh_upper[, variable := str_replace(variable, 'ci_u.', '')]
mex_hh_se <- melt.data.table(mex_hh[,c('se.health', 'se.consumption',  'year_id')], id='year_id', value.name = 'se')
mex_hh_se[, variable := str_replace(variable, 'se.', '')]

mex_hh <- merge(mex_hh_vals, mex_hh_lower, by = c('year_id', 'variable'))
mex_hh <- merge(mex_hh, mex_hh_upper, by = c('year_id', 'variable'))
mex_hh <- merge(mex_hh, mex_hh_se, by = c('year_id', 'variable'))
mex_hh[, location_name := 'Mexico']

per_hh_vals <- melt.data.table(per_hh[,c('total_health', 'consumption_expenditure', 'year_id')], id='year_id', value.name = 'value')
per_hh_lower <- melt.data.table(per_hh[,c('ci_l.total_health', 'ci_l.consumption_expenditure', 'year_id')], id='year_id', value.name = 'lower')
per_hh_lower[, variable := str_replace(variable, 'ci_l.', '')]
per_hh_upper <- melt.data.table(per_hh[,c('ci_u.total_health', 'ci_u.consumption_expenditure', 'year_id')], id='year_id', value.name = 'upper')
per_hh_upper[, variable := str_replace(variable, 'ci_u.', '')]
per_hh_se <- melt.data.table(per_hh[,c('se.total_health', 'se.consumption_expenditure', 'year_id')], id='year_id', value.name = 'se')
per_hh_se[, variable := str_replace(variable, 'se.', '')]
per_hh <- merge(per_hh_vals, per_hh_lower, by = c('year_id', 'variable'))
per_hh <- merge(per_hh, per_hh_upper, by = c('year_id', 'variable'))
per_hh <- merge(per_hh, per_hh_se, by = c('year_id', 'variable'))
per_hh[, variable := ifelse(variable == 'total_health', 'health', 
                            ifelse(variable == 'any_coverage', 'any_coverage', 'consumption'))]
per_hh[, location_name := 'Peru']

mex_ind_vals <- melt.data.table(mex_ind[,c('any_visit','any_coverage', 'is_private','year_id')], id='year_id', value.name = 'value')
mex_ind_lower <- melt.data.table(mex_ind[,c('ci_l.any_visit','ci_l.any_coverage', 'ci_l.is_private', 'year_id')], id='year_id', value.name = 'lower')
mex_ind_lower[, variable := str_replace(variable, 'ci_l.', '')]
mex_ind_upper <- melt.data.table(mex_ind[,c('ci_u.any_visit', 'ci_u.any_coverage', 'ci_u.is_private', 'year_id')], id='year_id', value.name = 'upper')
mex_ind_upper[, variable := str_replace(variable, 'ci_u.', '')]
mex_ind_se <- melt.data.table(mex_ind[,c('se.any_visit', 'se.any_coverage', 'se.is_private', 'year_id')], id='year_id', value.name = 'se')
mex_ind_se[, variable := str_replace(variable, 'se.', '')]
mex_ind <- merge(mex_ind_vals, mex_ind_lower, by = c('year_id', 'variable'))
mex_ind <- merge(mex_ind, mex_ind_upper, by = c('year_id', 'variable'))
mex_ind <- merge(mex_ind, mex_ind_se, by = c('year_id', 'variable'))

mex_ind[, location_name := 'Mexico']

per_ind_vals <- melt.data.table(per_ind[,c('any_visit', 'any_coverage', 'is_private','year_id')], id='year_id', value.name = 'value')
per_ind_lower <- melt.data.table(per_ind[,c('ci_l.any_visit', 'ci_l.any_coverage', 'ci_l.is_private', 'year_id')], id='year_id', value.name = 'lower')
per_ind_lower[, variable := str_replace(variable, 'ci_l.', '')]
per_ind_upper <- melt.data.table(per_ind[,c('ci_u.any_visit', 'ci_u.any_coverage', 'ci_u.is_private', 'year_id')], id='year_id', value.name = 'upper')
per_ind_upper[, variable := str_replace(variable, 'ci_u.', '')]
per_ind_se <- melt.data.table(per_ind[,c('se.any_visit', 'se.any_coverage', 'se.is_private', 'year_id')], id='year_id', value.name = 'se')
per_ind_se[, variable := str_replace(variable, 'se.', '')]
per_ind <- merge(per_ind_vals, per_ind_lower, by = c('year_id', 'variable'))
per_ind <- merge(per_ind, per_ind_upper, by = c('year_id', 'variable'))
per_ind <- merge(per_ind, per_ind_se, by = c('year_id', 'variable'))
per_ind[, location_name := 'Peru']

mex_vis_vals <- melt.data.table(mex_vis[,c('share_private','year_id')], id='year_id', value.name = 'value')
mex_vis_lower <- melt.data.table(mex_vis[,c('ci_l', 'year_id')], id='year_id', value.name = 'lower')
mex_vis_upper <- melt.data.table(mex_vis[,c('ci_u', 'year_id')], id='year_id', value.name = 'upper')
mex_vis_se <- melt.data.table(mex_vis[,c('se', 'year_id')], id='year_id', value.name = 'se')
mex_vis <- merge(mex_vis_vals, mex_vis_lower[, !c('variable')], by = c('year_id'))
mex_vis <- merge(mex_vis, mex_vis_upper[, !c('variable')], by = c('year_id'))
mex_vis <- merge(mex_vis, mex_vis_se[, !c('variable')], by = c('year_id'))
mex_vis[, location_name := 'Mexico']

per_vis_vals <- melt.data.table(per_vis[,c('share_private','year_id')], id='year_id', value.name = 'value')
per_vis_lower <- melt.data.table(per_vis[,c('ci_l', 'year_id')], id='year_id', value.name = 'lower')
per_vis_upper <- melt.data.table(per_vis[,c('ci_u', 'year_id')], id='year_id', value.name = 'upper')
per_vis_se <- melt.data.table(per_vis[,c('se', 'year_id')], id='year_id', value.name = 'se')
per_vis <- merge(per_vis_vals, per_vis_lower[, !c('variable')], by = c('year_id'))
per_vis <- merge(per_vis, per_vis_upper[, !c('variable')], by = c('year_id'))
per_vis <- merge(per_vis, per_vis_se[, !c('variable')], by = c('year_id'))
per_vis[, location_name := 'Peru']

## combine data 
my_df <- as.data.table(rbind(mex_hh, mex_ind, per_hh, per_ind, mex_vis, per_vis))

## add ihme_loc_id on
my_df[, ihme_loc_id := ifelse(location_name == "Mexico", "MEX", "PER")]

## remove NA's
my_df <- my_df[!is.na(value)]

## save mean csv
write.csv(my_df, file.path(outdir, "che_drivers_means.csv"), row.names = FALSE)
