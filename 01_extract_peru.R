################################################################################
## Author: USER                                                               ##
## Date: 10/27/21                                                             ##
## Purpose: Preprocess PERU ENAHO surveys to combine to HH level              ##
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
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl,dplyr)

################################################################################
## PERU ENAHO 2020                                                            ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, 'PER_ENAHO_2020_ANNUAL_MODULE_34_SUMARIA_2020_12G_Y2021M07D23.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, 'PER_ENAHO_2020_ANNUAL_MODULE_04_ENAHO01A_2020_400_Y2021M07D23.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep health and consumption variables
agg[, total_health :=  gru61hd ]

# gross spending
agg[, consumption_expenditure := gashog2d]

## extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

## save prepped extraction dataset 
colnames(agg) <- tolower(colnames(agg))
# 
# # create drugs, hospital, and outpatient sums
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602","i418202","i418302"))]
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613","i418213","i418313"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i418201","i418301", "i41603", "i418203","i418303", "i41604","i418204","i418304", "i41605","i418205","i418305", "i41607","i418207","i418307", "i41609","i418209","i418309","i41610","i418210","i418310", "i41615","i418215","i418315" ))]
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614","i418206","i418208", "i418211","i418212", "i418214","i418306","i418308", "i418311","i418312", "i418314","i41616", "i418216","i418316"))]

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
health_ag <- aggregate(.~factor07+aÑo+mes+conglome+nconglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
ins_ag <- aggregate(.~factor07+aÑo+mes+conglome+nconglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## merge datasets 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "nconglome", "factor07"))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "nconglome", "factor07", "aÑo"))

## rename column to avoid special character
setnames(combo, old = "aÑo", new = "year_id")

## save dataset 
save.dta13(combo[,c("mieperho", "total_health", "consumption_expenditure", "drugs", "hospital", "outpatient", "other", "estrato", "pobreza", "year_id", "factor07", "insurance",
                    "rural", "nconglome", "conglome", "estrsocial", "ubigeo", "mes", "vivienda", "hogar", "dominio")], "/FILEPATH/PER_ENAHO_2020.dta")

################################################################################
## PERU ENAHO 2019                                                            ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read_sav(file.path(dir, '/PER_ENAHO_2019_SUMM_12G_Y2020M07D27.SAV')), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, 'PER_ENAHO_2019_01A_400_Y2020M07D27.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

## remove labeled columns 
agg <- zap_labels(agg)

## adjust columns for merging
colnames(agg) <- tolower(colnames(agg))

## prep health and consumption variables
agg[, total_health :=  gru61hd ]

#includes out of pocket aggregate and self consumption sub aggregates
agg[, consumption_expenditure := gashog2d]
#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# # create drugs, hospital, and outpatient sums
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602","i418202","i418302"))]
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613","i418213","i418313"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i418201","i418301", "i41603", "i418203","i418303", "i41604","i418204","i418304", "i41605","i418205","i418305", "i41607","i418207","i418307", "i41609","i418209","i418309","i41610","i418210","i418310", "i41615","i418215","i418315" ))]
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614","i418206","i418208", "i418211","i418212", "i418214","i418306","i418308", "i418311","i418312", "i418314","i41616", "i418216","i418316"))]

## sum health to household level 
health_ag <- health[,c("drugs","hospital", "outpatient","other", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
health_ag <- aggregate(.~factor07+aÑo+mes+conglome+nconglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
ins_ag <- aggregate(.~factor07+aÑo+mes+conglome+nconglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## merge datasets 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "nconglome", "factor07"))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "nconglome", "factor07", "aÑo"))

## rename column to avoid special character
setnames(combo, old = "aÑo", new = "year_id")

## save dataset 
save.dta13(combo[,c("mieperho", "total_health", "consumption_expenditure", "drugs", "hospital", "outpatient", "other", "estrato", "pobreza", "year_id", "factor07", "insurance",
                    "rural", "nconglome", "conglome", "estrsocial", "ubigeo", "mes", "vivienda", "hogar", "dominio")], "/FILEPATH/PER_ENAHO_2019.dta")

################################################################################
## PERU ENAHO 2018                                                           ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_ENAHO_2018_CALCULATED_VARIABLES_SUMM_12_GROUPS_Y2019M06D19.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, 'PER_ENAHO_2018_400_Y2019M06D19.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep health and consumption variables
agg[, total_health :=  gru61hd ]

#includes out of pocket aggregate and self consumption sub aggregates
agg[, consumption_expenditure := gashog2d]

## extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
health_ag <- aggregate(. ~factor07 +aÑo +mes+conglome+nconglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
ins_ag <- aggregate(.~factor07+aÑo+mes+conglome+nconglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "nconglome", "factor07","aÑo" ))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "nconglome", "factor07", "aÑo"))

## rename column to avoid special character  
setnames(combo, old = "aÑo", new = "year_id")

## save prepped extraction dataset 
save.dta13(combo[,c("mieperho", "total_health", "consumption_expenditure", "drugs", "hospital", "outpatient", "other", "estrato", "pobreza", "year_id", "factor07", "insurance",
                    "rural", "nconglome", "conglome", "estrsocial", "ubigeo", "mes", "vivienda", "hogar", "dominio")], "/FILEPATH/PER_ENAHO_2018.dta")

################################################################################
## PERU ENAHO 2017        (only one quarter available)                                                  ##
################################################################################

################################################################################
## PERU ENAHO 2016                                                           ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2016_SUM_12G_Y2017M09D08.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2016_HEALTH_400_Y2017M09D08.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep health and consumption variables
agg[, total_health :=  gru61hd ]

#includes out of pocket aggregate and self consumption sub aggregates
agg[, consumption_expenditure := gashog2d]
#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
health_ag <- aggregate(. ~factor07 +aÑo +mes+conglome+nconglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
ins_ag <- aggregate(.~factor07+aÑo+mes+conglome+nconglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "nconglome", "factor07","aÑo" ))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato", "nconglome", "factor07", "aÑo"))

## rename column to avoid special character  
setnames(combo, old = "aÑo", new = "year_id")

## save prepped extraction dataset 
save.dta13(combo[,c("mieperho", "total_health", "consumption_expenditure", "drugs", "hospital", "outpatient", "other", "estrato", "pobreza", "year_id", "factor07", "insurance",
                    "rural", "nconglome", "conglome", "estrsocial", "ubigeo", "mes", "vivienda", "hogar", "dominio")], "/FILEPATH/PER_ENAHO_2016.dta")

################################################################################
## PERU ENAHO 2015                                          
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2015_SUM_CALC_VAR_Y2016M09D19.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2015_ANNUAL_HEALTH_400_Y2016M09D19.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variables
agg[, consumption_expenditure := gashog2d]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615", "i41616"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07", "total_health")]
health_ag <- aggregate(. ~factor07 +aÑo +mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
ins_ag <- aggregate(.~factor07 +aÑo +mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","factor07","aÑo" ))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","factor07","aÑo" ))

## rename columns
setnames(combo, old = "aÑo", new = "year_id")
# try saving with no na estrsocial
## save prepped extraction dataset 
save.dta13(combo[,c("mieperho", "total_health", "consumption_expenditure", "drugs", "hospital", "outpatient", "other", "estrato", "pobreza", "year_id", "factor07", "insurance",
                    "rural", "conglome", "estrsocial", "ubigeo", "mes", "vivienda", "hogar", "dominio")], "/FILEPATH/PER_ENAHO_2015.dta")

################################################################################
## PERU ENAHO 2014                                          
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2014_SUM_CALC_VAR_Y2016M09D19.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2014_ANNUAL_HEALTH_400_Y2016M09D19.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variables
agg[, consumption_expenditure := gashog2d ]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615", "i41616"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07", "total_health")]
health_ag <- aggregate(. ~factor07 +aÑo +mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
ins_ag <- aggregate(.~factor07 +aÑo +mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","factor07","aÑo" ))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","factor07","aÑo" ))

## rename columns 
setnames(combo, old = "aÑo", new = "year_id")

nrow(combo)
## save prepped extraction dataset 
save.dta13(combo[,c("mieperho", "total_health", "consumption_expenditure", "drugs", "hospital", "outpatient", "other", "estrato", "pobreza", "year_id", "factor07", "insurance",
                    "rural", "conglome", "estrsocial", "ubigeo", "mes", "vivienda", "hogar", "dominio")], "/FILEPATH/PER_ENAHO_2014.dta")

################################################################################
## PERU ENAHO 2012                                          
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_ENAHO_2012_ANNUAL_324_MOD_34_SUMARIA_Y2013M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, "PER_ENAHO_2012_ANNUAL_324_MOD_04_01A_400_Y2013M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variables
agg[, consumption_expenditure := gashog2d]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615", "i41616"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07", "total_health")]
health_ag <- aggregate(. ~factor07+aÑo +mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
ins_ag <- aggregate(.~factor07+aÑo +mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","aÑo", "factor07" ))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","aÑo", "factor07" ))

## rename columns 
setnames(combo, old = "aÑo", new = "year_id")

## save prepped extraction dataset
save.dta13(combo[,c("mieperho", "total_health", "consumption_expenditure", "drugs", "hospital", "outpatient", "other", "estrato", "pobreza", "year_id", "factor07", "insurance",
                    "rural", "conglome", "ubigeo", "mes", "vivienda", "hogar", "dominio")], "/FILEPATH/PER_ENAHO_2012.dta")

################################################################################
## PERU ENAHO 2011                                  
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_ENAHO_2011_ANNUAL_291_MOD_34_SUMARIA_Y2013M07D12_Y2013M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, "PER_ENAHO_2011_ANNUAL_291_MOD_04_01A_400_Y2013M07D12_Y2013M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variable
agg[, consumption_expenditure := gashog2d]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
colnames(health) <- tolower(colnames(health))
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07", "total_health")]
health_ag <- aggregate(. ~factor07+año+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07")]
ins_ag <- aggregate(.~factor07 +año +mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## rename columns
setnames(agg, old = 'aÑo', new = 'year_id')
setnames(health_ag, old = 'año', new = 'year_id')
setnames(ins_ag, old = 'año', new = 'year_id')

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id","factor07" ))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id", "factor07" ))

## save prepped extraction dataset 
colnames(combo) <- tolower(colnames(combo) )
save.dta13(combo[,c("mieperho","total_health", "consumption_expenditure", "estrato", "pobreza", "year_id","factor07", "rural", "conglome", "ubigeo", "insurance")], "/FILEPATH/PER_ENAHO_2011.dta")

################################################################################
## PERU ENAHO 2010                                  
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2010_SUM_CALC_VAR_Y2011M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2010_HEALTH_400_Y2011M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variable
agg[, consumption_expenditure := GASHOG2D]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
colnames(health) <- tolower(colnames(health))
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615"))] 
colnames(health)

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor", "total_health")]
health_ag <- aggregate(. ~factor+ano+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor")]
ins_ag <- aggregate(.~factor +ano +mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## rename column names
setnames(agg, old = 'ano', new = 'year_id')
setnames(health_ag, old = 'ano', new = 'year_id')
setnames(ins_ag, old = 'ano', new = 'year_id')

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" ))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" ))

## save prepped extraction dataset 
colnames(combo) <- tolower(colnames(combo) )

save.dta13(combo[,c("mieperho","total_health", "consumption_expenditure", "estrato", "pobreza", "year_id","factor07", "rural", "conglome", "ubigeo", "insurance")], "/FILEPATH/PER_ENAHO_2010.dta")

################################################################################
## PERU ENAHO 2009                                
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2009_EXP_INC_TOTALS_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2009_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variable
agg[, consumption_expenditure := GASHOG2D]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
colnames(health) <- tolower(colnames(health))
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor", "total_health")]
health_ag <- aggregate(. ~factor+año+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor")]
ins_ag <- aggregate(.~factor+año+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## rename columns
setnames(agg, old = 'año', new = 'year_id')
setnames(health_ag, old = 'año', new = 'year_id')
setnames(ins_ag, old = 'año', new = 'year_id')

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id", "factor" ))

## save prepped extraction dataset 
colnames(combo) <- tolower(colnames(combo) )
setnames(combo, old = c('factor'), new = c('factor07'))
save.dta13(combo[,c("mieperho","total_health", "consumption_expenditure", "estrato", "pobreza", "year_id","factor07", "rural", "conglome", "ubigeo", 'insurance')], "/FILEPATH/PER_ENAHO_2009.dta")

###############################################################################
## PERU ENAHO 2008                             
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2008_ANNUAL_SUM_CALC_VAR_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2008_ANNUAL_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variable
agg[, consumption_expenditure := GASHOG2D]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
colnames(health) <- tolower(colnames(health))
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor", "total_health")]
health_ag <- aggregate(. ~factor+año+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor")]
ins_ag <- aggregate(.~factor+año+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## rename columns 
setnames(agg, old = 'año', new = 'year_id')
setnames(health_ag, old = 'año', new = 'year_id')
setnames(ins_ag, old = 'año', new = 'year_id')

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id", "factor" ))

## save prepped extraction dataset 
colnames(combo) <- tolower(colnames(combo) )
setnames(combo, old = c('factor'), new = c('factor07'))
save.dta13(combo[,c("mieperho","total_health", "consumption_expenditure", "estrato", "pobreza", "year_id", "factor07", "rural", "conglome", "ubigeo", "insurance")], "/FILEPATH/PER_ENAHO_2008.dta")

###############################################################################
## PERU ENAHO 2007                           
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2007_ANNUAL_SUMM_CALC_VAR_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2007_ANNUAL_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variable
agg[, consumption_expenditure := GASHOG2D]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
colnames(health) <- tolower(colnames(health))
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor", "total_health")]
health_ag <- aggregate(. ~factor+año+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor")]
ins_ag <- aggregate(.~factor+año+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## rename columns 
setnames(agg, old = 'ano', new = 'year_id')
setnames(health_ag, old = 'año', new = 'year_id')
setnames(ins_ag, old = 'año', new = 'year_id')

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))

## save prepped extraction dataset 
colnames(combo) <- tolower(colnames(combo) )
setnames(combo, old = c('factor'), new = c('factor07'))

save.dta13(combo[,c("mieperho","total_health", "consumption_expenditure", "estrato", "pobreza", "year_id", "factor07", "rural", "conglome", "ubigeo", "insurance")], "/FILEPATH/PER_ENAHO_2007.dta")

###############################################################################
## PERU ENAHO 2006                         
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2006_SUMM_CALC_VAR_EXP_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2006_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variable
agg[, consumption_expenditure := GASHOG2D]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
colnames(health) <- tolower(colnames(health))
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor", "total_health")]
health_ag <- aggregate(. ~factor+ano+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor")]
ins_ag <- aggregate(.~factor+ano+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## rename columns
setnames(agg, old = 'ano', new = 'year_id')
setnames(health_ag, old = 'ano', new = 'year_id')
setnames(ins_ag, old = 'ano', new = 'year_id')

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))

## save prepped extraction dataset 
colnames(combo) <- tolower(colnames(combo) )
setnames(combo, old = c('factor'), new = c('factor07'))

save.dta13(combo[,c("mieperho","total_health", "consumption_expenditure", "estrato", "pobreza", "year_id", "factor07", "rural", "conglome", "ubigeo", "insurance")], "/FILEPATH/PER_ENAHO_2006.dta")

###############################################################################
## PERU ENAHO 2005                         
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2005_SUMM_CALC_VAR_EXP_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2005_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variable
agg[, consumption_expenditure := GASHOG2D]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
colnames(health) <- tolower(colnames(health))
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor", "total_health")]
health_ag <- aggregate(. ~factor+ano+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor")]
ins_ag <- aggregate(.~factor+ano+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## rename columns
setnames(agg, old = 'ano', new = 'year_id')
setnames(health_ag, old = 'ano', new = 'year_id')
setnames(ins_ag, old = 'ano', new = 'year_id')

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))

## save prepped extraction dataset 
colnames(combo) <- tolower(colnames(combo) )
setnames(combo, old = c('factor'), new = c('factor07'))

## save prepped extraction dataset 
save.dta13(combo[,c("mieperho","total_health", "consumption_expenditure", "estrato", "pobreza", "year_id", "factor07", "rural", "conglome", "ubigeo", "insurance")], "/FILEPATH/PER_ENAHO_2005.dta")

###############################################################################
## PERU ENAHO 2004                         
##note: only 8 groups of sub-aggregates instead of 12
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read dataset
agg <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2004_SUMM_CALC_VAR_EXP_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
health <- data.table(read.dta13(file.path(dir, "PER_NATIONAL_HH_SURVEY_ENAHO_2004_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## prep consumption variable
agg[, consumption_expenditure := GASHOG2D]

#extract urban/rural indicator
agg[, rural := ifelse(estrato == 7 | estrato == 8, 1, 0)]

# create drugs, hospital, and outpatient sums
colnames(health) <- tolower(colnames(health))
health[, drugs := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41602"))] 
health[, hospital := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41613"))]
health[, outpatient := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41603", "i41604","i41605", "i41607","i41609","i41610","i41615" ))] 
health[, other := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41606","i41608", "i41611","i41612", "i41614" ))] 
health[, total_health := rowSums(.SD, na.rm = T), .SDcols = eval(c("i41601","i41602", "i41603","i41604", "i41605","i41606", "i41607", "i41608", "i41609","i41610", "i41611", "i41612", "i41613", "i41614", "i41615"))] 

## sum health to household level 
health_ag <- health[,c("drugs", "hospital", "outpatient","other", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor", "total_health")]
health_ag <- aggregate(. ~factor+ano+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, health_ag, sum)
## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1 | p4198 == 1 | p4191 == 1 |p4195 == 1 | p4194 == 1, 1, 0)]
ins_ag <- health[,c("insurance", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor")]
ins_ag <- aggregate(.~factor+ano+mes+conglome+vivienda+hogar+dominio+estrato+ubigeo, ins_ag, max)

## rename columns
setnames(agg, old = 'ano', new = 'year_id')
setnames(health_ag, old = 'ano', new = 'year_id')
setnames(ins_ag, old = 'ano', new = 'year_id')

## merge dataset 
combo <- merge(agg, health_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))
combo <- merge(combo, ins_ag, by = c("mes", "conglome", "vivienda", "hogar", "ubigeo", "dominio", "estrato","year_id" , 'factor'))

## save prepped extraction dataset 
colnames(combo) <- tolower(colnames(combo) )
setnames(combo, old = c('factor'), new = c('factor07'))

save.dta13(combo[,c("mieperho","total_health", "consumption_expenditure", "estrato", "pobreza", "year_id", "factor07", "rural", "conglome", "ubigeo", "insurance")], "/FILEPATH/PER_ENAHO_2004.dta")

#pre 2004 in quarters, will need diff approach, only 2000 in HEFPI pre 2006