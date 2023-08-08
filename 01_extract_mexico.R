################################################################################
## Author: USER                                                               ##
## Date: 8/19/21                                                              ##
## Purpose: Preprocess MEX ENIGH surveys to combine to HH level               ##
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
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, dplyr, plyr)

## functions
income_prep <- function(data, hhid){
  ## convert from quarterly to annual measure
  data$income <- data$ing_tri * 4
  
  ## sum by household
  data <- data[, .(inc_sum = sum(income, na.rm = T)), by = hhid]
  
  ## return data table
  return(data)
}

create_loc_ids <- function(data){
  ## create state id (first two digits)
  data$state_id <- substr(agg$ubica_geo, 1, 2)
  
  ## attach state names
  data <- merge(data, state_ids, by = 'state_id')
  
  ## convert state id 
  data$state_id <- as.numeric(data$state_id)
  
  return(data)
}

calc_forgone_care <- function(data, var){
  data[, ate_sal7 := as.numeric(eval(var))]
  was_ill <- data[, ifelse(mean(ate_sal7, na.rm = T) == 2, 0, 1), by = c('folioviv', 'foliohog')]
  setnames(was_ill, old = 'V1', new = 'was_ill')
  
  data[, ate_sal8 := as.numeric(ate_sal8)]
  received_care <- data[, mean(ate_sal8, na.rm = T), by = c('folioviv', 'foliohog')]
  received_care[, V1 := ifelse(V1 == 2.0, 0, 1)]
  setnames(received_care, old = c('V1'), new = c('received_care'))
  
  data[, noatenc_2 := as.numeric(noatenc_2)][, noatenc_3 := as.numeric(noatenc_3)]
  forgone_care <- data[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('noatenc_2', 'noatenc_3')]
  forgone_care[, forgone_care := ifelse(is.na(noatenc_2) & is.na(noatenc_3), 0, 1)]
  
  df <- merge(was_ill, received_care, by = c('folioviv', 'foliohog'))
  df <- merge(df, forgone_care, by = c('folioviv', 'foliohog'))
  
  return(df)
}

## read state ids 
state_ids <- read_excel('/FILEPATH/mex_enigh_states.xlsx')

################################################################################
## MEXICO ENIGH 1983-1984                                                     ##
################################################################################
## directory 
dir <- file.path('FILEPATH')

## read datasets
agg <- data.table(read.dta13(file.path(dir, "/MEX_ENIGH_1983_1984_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1983_1984_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1983_1984_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1983_1984_EXPENSES.DTA")))

## prep income variables
income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri')
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J00', as.character(c(1:8))), paste0('J0', as.character(c(15:27))), paste0('J0', as.character(c(32:36)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J009'), paste0('J0', as.character(c(10:14)))))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = J028:J031]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J037:J038]
consume_wide[, health := rowSums(.SD*4, na.rm = T), .SDcols = J001:J036]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'zona')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance', 'health')], by = c('folio'), all.x = T)
df <- merge(df,
            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tot_resi' ,'alimentos', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folio', 'tot_resi', 'hog', 'gascor', 'zona', 'ingtot', 'alimentos'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_1983_1984.dta")

################################################################################
## MEXICO ENIGH 1989                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1989_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1989_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1989_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1989_EXPENSES.DTA")))

## prep income variables
income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J00', as.character(c(1:8))), paste0('J0', as.character(c(15:27))), paste0('J0', as.character(c(32:36)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J009'), paste0('J0', as.character(c(10:14)))))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = J028:J031]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J037:J038]
consume_wide[, health := rowSums(.SD*4, na.rm = T), .SDcols = J001:J036]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'zona')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance', 'health')], by = c('folio'), all.x = T)
df <- merge(df,
            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tot_resi' ,'alimentos', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folio', 'tot_resi', 'hog', 'gascor', 'zona', 'ingtot', 'alimentos'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_1989.dta")

################################################################################
## MEXICO ENIGH 1992                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1992_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1992_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1992_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1992_EXPENSES.DTA")))

## prep income variables
income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J00', as.character(c(1:9))), paste0('J0', as.character(c(16:28))), paste0('J0', as.character(c(37:38)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J0', as.character(c(10:15)))))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = J029:J036]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J042:J043]
consume_wide[, health := outpatient + hospital + drugs]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'estrato')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance', 'health')], by = c('folio'), all.x = T)
df <- merge(df,
            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tot_resi' ,'alimentos', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folio', 'tot_resi', 'hog', 'gascor', 'estrato', 'ingtot', 'alimentos'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_1992.dta")

################################################################################
## MEXICO ENIGH 1994                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1994_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1994_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1994_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1994_EXPENSES.DTA")))

## prep income variables
income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J00', as.character(c(1:9))), paste0('J0', as.character(c(16:28))), paste0('J0', as.character(c(37:41)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J0', as.character(c(10:15)))))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = J029:J036]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J042:J043]
consume_wide[, health := rowSums(.SD*4, na.rm = T), .SDcols = J001:J041]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'estrato')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance', 'health')], by = c('folio'), all.x = T)
df <- merge(df,
            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tot_resi' ,'alimentos', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folio', 'tot_resi', 'hog', 'gascor', 'estrato', 'ingtot', 'alimentos'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_1994.dta")

################################################################################
## MEXICO ENIGH 1996                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1996_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1996_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1996_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1996_EXPENSES.DTA")))

## prep income variables
income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J00', as.character(c(1:9))), paste0('J0', as.character(c(16:32))), paste0('J0', as.character(c(39:43)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J0', as.character(c(10:15)))))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = J033:J038]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J044:J045]
consume_wide[, health := rowSums(.SD*4, na.rm = T), .SDcols = J001:J043]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'estrato')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance', 'health')], by = c('folio'), all.x = T)
df <- merge(df,
            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tam_hog' , 'alimentos',  'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folio', 'tam_hog', 'hog', 'gascor', 'estrato', 'ingtot', 'alimentos'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_1996.dta")

################################################################################
## MEXICO ENIGH 1998                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1998_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1998_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1998_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_1998_EXPENSES.DTA")))

## prep income variables
income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J00', as.character(c(1:9))), paste0('J0', as.character(c(16:32))), paste0('J0', as.character(c(39:43)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J0', as.character(c(10:15)))))]
consume_wide[, drugs := rowSums(.SD, na.rm = T), .SDcols = J033:J038]
consume_wide[, insurance := rowSums(.SD, na.rm = T), .SDcols = J044:J045]
consume_wide[, health := outpatient + hospital + drugs]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'estrato')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance', 'health')], by = c('folio'), all.x = T)
df <- merge(df,
            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tam_hog' , 'alimentos', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folio', 'tam_hog', 'hog', 'gascor', 'estrato', 'ingtot', 'alimentos'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_1998.dta")

################################################################################
## MEXICO ENIGH 2000                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2000_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2000_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2000_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2000_EXPENSES.DTA")))

## prep income variables
income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J00', as.character(c(1:9))), paste0('J0', as.character(c(16:32))), paste0('J0', as.character(c(39:43)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J0', as.character(c(10:15)))))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = J033:J038]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J044:J045]
consume_wide[, health := rowSums(.SD*4, na.rm = T), .SDcols = J001:J043]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'estrato')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance', 'health')], by = c('folio'), all.x = T)
df <- merge(df,
            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tam_hog' , 'alimentos', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folio', 'tam_hog', 'hog', 'gascor', 'estrato', 'ingtot', 'alimentos'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2000.dta")

################################################################################
## MEXICO ENIGH 2002                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2002_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2002_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2002_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2002_EXPENSES.DTA")))

## prep income variables
income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J00', as.character(c(1:9))),paste0('J0', as.character(c(10:25))), paste0('J0', as.character(c(31:36))),paste0('J0', as.character(c(38:47))), paste0('J0', as.character(c(70:75)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c(paste0('J0', as.character(c(26:30)))))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = J048:J069]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J076:J077]
consume_wide[, health := rowSums(.SD*4, na.rm = T), .SDcols = J001:J075]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'estrato', 'conapo')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance', 'health')], by = c('folio'), all.x = T)
df <- merge(df,
            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tam_hog' , 'alimentos', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folio', 'tam_hog', 'hog', 'gascor', 'estrato', 'ingtot', 'alimentos'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2002.dta")

################################################################################
## MEXICO ENIGH 2004                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2004_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2004_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2004_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2004_EXPENSES.DTA")))

## prep income variables
## income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c('J001', 'J003', 'J011', 'J013', 'J015', paste0('J0', as.character(c(16:19))), 'J036', paste0('J0', as.character(c(38:39))), 'J041', 'J043',  paste0('J0', as.character(c(65:69)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c('J002', 'J012', 'J040'))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = eval(c('J004', 'J009', 'J010', 'J014',paste0('J0', as.character(c(20:35))), 'J037', 'J042', paste0('J0', as.character(c(44:64)))))]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J070:J072]
## consume_wide[, health := hospital + outpatient + drugs]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4][, salud := salud*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'estrato', 'conapo')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance')], by = c('folio'), all.x = T)
#df <- merge(df,
#            income[, c('folio', 'inc_sum')], by = c('folio'), all = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tam_hog' , 'alimentos',  'state_name', 'state_id', 'ubica_geo', 'salud')], by = c('folio'))

## update variable names
setnames(df, old = c('folio', 'tam_hog', 'hog', 'gascor', 'estrato', 'ingtot', 'alimentos', 'salud'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food', 'health'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2004.dta")

################################################################################
## MEXICO ENIGH 2005                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2005_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2005_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2005_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2005_EXPENSES.DTA")))

## prep income variables
## income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c('J001', 'J003', 'J011', 'J013', 'J015', paste0('J0', as.character(c(16:19))), 'J036', paste0('J0', as.character(c(38:39))), 'J041', 'J043',  paste0('J0', as.character(c(65:69)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c('J002', 'J012', 'J040'))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = eval(c('J004', 'J009', 'J010', 'J014',paste0('J0', as.character(c(20:35))), 'J037', 'J042', paste0('J0', as.character(c(44:64)))))]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J070:J072]
consume_wide[, health := hospital + outpatient + drugs]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4][, salud := salud*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'estrato')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance', 'health')], by = c('folio'), all.x = T)
df <- merge(df,
            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tam_hog' , 'alimentos', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folio', 'tam_hog', 'hog', 'gascor', 'estrato', 'ingtot', 'alimentos'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/ihme/scratch/projects/hssa/frp/processed/MEX_ENIGH_2005.dta")

################################################################################
## MEXICO ENIGH 2006                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2006_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2006_HOUSEHOLD.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2006_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2006_EXPENSES.DTA")))

## prep income variables
## income <- income_prep(income, hhid = 'folio')

## prep consumption variables 
consume_wide <- dcast(consume[, c('folio', 'clave', 'gas_tri')], folio  ~ clave, value.var = 'gas_tri', fun.aggregate = sum)
consume_wide[, outpatient := rowSums(.SD*4, na.rm = T), .SDcols = eval(c('J001', 'J003', 'J011', 'J013', 'J015', paste0('J0', as.character(c(16:19))), 'J036', paste0('J0', as.character(c(38:39))), 'J041', 'J043',  paste0('J0', as.character(c(65:69)))))]
consume_wide[, hospital := rowSums(.SD*4, na.rm = T), .SDcols = eval(c('J002', 'J012', 'J040'))]
consume_wide[, drugs := rowSums(.SD*4, na.rm = T), .SDcols = eval(c('J004', 'J009', 'J010', 'J014',paste0('J0', as.character(c(20:35))), 'J037', 'J042', paste0('J0', as.character(c(44:64)))))]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J070:J072]
## consume_wide[, health := hospital + outpatient + drugs]

## prep aggregate variables
agg[, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4][, salud := salud*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folio', 'estrato', 'conapo')], 
            consume_wide[, c('folio', 'outpatient', 'hospital', 'drugs', 'insurance')], by = c('folio'), all.x = T)
## df <- merge(df,
##            income[, c('folio', 'inc_sum')], by = c('folio'), all.x = T)
df <- merge(df, 
            agg[, c('folio', 'hog', 'ingtot', 'gascor', 'tam_hog' , 'alimentos',  'state_name', 'state_id', 'ubica_geo', 'salud')])

## update variable names
setnames(df, old = c('folio', 'tam_hog', 'hog', 'gascor', 'estrato', 'ingtot', 'alimentos', 'salud'), new = c('hh_id', 'hhsize', 'factor', 'consumption', 'strata1', 'inc_tot', 'food', 'health'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2006.dta")

################################################################################
## MEXICO ENIGH 2008                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2008_CONCENTRATED.DTA")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2008_HOUSEHOLD.DTA")))
indiv <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2008_SOCIODEMOGRAPHICS.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2008_CAPITAL_INCOME.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2008_EXPENSES.DTA")))
nonmon <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2008_NONMONETARY_INCOME.DTA")))

## prep income variables
income <- income_prep(income, hhid =  c('folioviv', 'foliohog'))

## prep consumption variables 
consume_wide <- as.data.frame(dcast(consume[, c('folioviv', 'foliohog', 'clave', 'gas_tri')], folioviv + foliohog ~ clave, value.var = 'gas_tri'))
nonmon_wide <- as.data.frame(dcast(nonmon[, c('folioviv', 'foliohog', 'clave', 'gas_tri')], folioviv + foliohog ~ clave, value.var = 'gas_tri', fun.aggregate = sum))

consume_wide <- consume_wide[,grep("J|folio",colnames(consume_wide))]
nonmon_wide <- nonmon_wide[,grep("J|folio",colnames(nonmon_wide))]

consume_wide <- as.data.table(rbind(consume_wide, nonmon_wide))
consume_wide <- consume_wide[, lapply(.SD, sum, na.rm=TRUE), by=c('folioviv', 'foliohog')]
consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J070:J072]

## prep forgone care variables 
indiv[, ate_sal7 := as.numeric(ate_sal7)]
was_ill <- indiv[, ifelse(mean(ate_sal7, na.rm = T) == 2, 0, 1), by = c('folioviv', 'foliohog')]
setnames(was_ill, old = 'V1', new = 'was_ill')

indiv[, ate_sal8 := as.numeric(ate_sal8)]
received_care <- indiv[, mean(ate_sal8, na.rm = T), by = c('folioviv', 'foliohog')]
received_care[, V1 := ifelse(V1 == 4.0, 0, 1)]
setnames(received_care, old = c('V1'), new = c('received_care'))

indiv[, noatenc_2 := as.numeric(noatenc_2)][, noatenc_3 := as.numeric(noatenc_3)]
forgone_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('noatenc_2', 'noatenc_3')]
forgone_care[, forgone_care := ifelse(is.na(noatenc_2) & is.na(noatenc_3), 0, 1)]

df <- merge(was_ill, received_care, by = c('folioviv', 'foliohog'))
forgone_care <- merge(df, forgone_care, by = c('folioviv', 'foliohog'))

## prep insurance variables 
indiv <- cbind(indiv[, !c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], sapply(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], as.numeric))
indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')][is.na(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')])] <- 0

## calculate insurance type by household
indiv[, insurance_id := ifelse(inst_1 == 1 |  inst_2 == 2 | inst_3 == 3 | inst_4 == 4 | inst_5 == 1 | segpop == 1, 1,
                        ifelse(segpop == 2 & atemed == 2, 0, NA))]
insurance <- indiv[, .(insurance_id = max(insurance_id)), by = c('folioviv', 'foliohog')]
# insurance[, insurance_type := ifelse(insurance_id == 3, 'IMSS/ISSSTE/PEMEX',
#                                      ifelse(insurance_id == 2, 'SP/INSABI/No coverage', 
#                                             ifelse(insurance_id == 3, 'Other', NA)))] 

## prep aggregate variables
agg[, salud := salud*4][, aten_pri := aten_pri*4][, hospital := hospital*4][, medica := medica*4][, gascor := gascor*4][, alimentos := alimentos*4][, ingtot := ingtot*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folioviv', 'foliohog', 'est_dis', 'upm', 'factor')], 
            consume_wide[, c('folioviv', 'foliohog', 'insurance')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df,
            income[, c('folioviv', 'foliohog', 'inc_sum')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            forgone_care[, c('folioviv', 'foliohog', 'was_ill', 'received_care', 'forgone_care')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            insurance[, c('folioviv', 'foliohog', 'insurance_id')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            agg[, c('folioviv', 'foliohog', 'conapo', 'sexo', 'ingtot', 'gascor', 'tam_hog' , 'alimentos', 'aten_pri', 'hospital', 'medica', 'salud', 'state_name', 'state_id', 'ubica_geo')], by = c('folioviv', 'foliohog'))

## update variable names
setnames(df, old = c('folioviv', 'foliohog', 'tam_hog',  'gascor',  'ingtot', 'alimentos', 'aten_pri', 'medica', 'sexo', 'est_dis', 'salud'), 
         new = c('hh_id', 'hh_id2', 'hhsize',  'consumption',  'inc_tot', 'food', 'outpatient', 'drugs', 'hhsex', 'strata1', 'health'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2008.dta")

################################################################################
## MEXICO ENIGH 2010                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, 'MEX_ENIGH_2010_CONCENTRATED_Y2013M03D29.DTA')))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2010_HH_Y2013M03D29.DTA")))
indiv <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2010_SOCIODEMOGRAPHICS_Y2013M03D29.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2010_INCOME_Y2013M03D29.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2010_EXPENSES_Y2013M03D29.DTA")))
nonmon <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2010_NONMONETARY_INCOME_Y2013M03D29.DTA")))
costs <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2010_COSTS_Y2013M03D29.DTA")))

## prep income variables
income <- income_prep(income, hhid =  c('folioviv', 'foliohog'))

## prep consumption variables 
consume_wide <- as.data.frame(dcast(consume[, c('folioviv', 'foliohog', 'clave', 'gas_tri')], folioviv + foliohog ~ clave, value.var = 'gas_tri', fun.aggregate = sum))
nonmon_wide <- as.data.frame(dcast(nonmon[, c('folioviv', 'foliohog', 'clave', 'gas_tri')], folioviv + foliohog ~ clave, value.var = 'gas_tri', fun.aggregate = sum))
costs_wide <- as.data.frame(dcast(costs[, c('folioviv', 'foliohog', 'clave', 'costo_tri')], folioviv + foliohog ~ clave, value.var = 'costo_tri', fun.aggregate = sum))

consume_wide <- consume_wide[,grep("J|folio",colnames(consume_wide))]
nonmon_wide <- nonmon_wide[,grep("J|folio",colnames(nonmon_wide))]
costs_wide <- costs_wide[,grep("J|folio",colnames(costs_wide))]

consume_wide <- as.data.frame(rbind.fill(consume_wide, nonmon_wide, costs_wide))
consume_wide <- as.data.table(aggregate(. ~ folioviv + foliohog, consume_wide, sum, na.rm =T))

consume_wide[, insurance := rowSums(.SD, na.rm = T), .SDcols = J070:J072]

## prep forgone care variables 
indiv[, prob_sal := as.numeric(prob_sal)]
was_ill <- indiv[, ifelse(mean(prob_sal, na.rm = T) == 2, 0, 1), by = c('folioviv', 'foliohog')]
setnames(was_ill, old = 'V1', new = 'was_ill')

indiv[, aten_sal1 := as.numeric(aten_sal1)]
received_care <- indiv[, mean(aten_sal1, na.rm = T), by = c('folioviv', 'foliohog')]
received_care[, V1 := ifelse(V1 == 4.0, 0, 1)]
setnames(received_care, old = c('V1'), new = c('received_care'))

indiv[, noatenc_2 := as.numeric(noatenc_2)][, noatenc_3 := as.numeric(noatenc_3)]
forgone_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('noatenc_2', 'noatenc_3')]
forgone_care[, forgone_care := ifelse(is.na(noatenc_2) & is.na(noatenc_3), 0, 1)]

df <- merge(was_ill, received_care, by = c('folioviv', 'foliohog'))
forgone_care <- merge(df, forgone_care, by = c('folioviv', 'foliohog'))

## prep insurance variables 
indiv <- cbind(indiv[, !c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], sapply(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], as.numeric))
indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')][is.na(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')])] <- 0

## calculate insurance type by household
indiv[, insurance_id := ifelse(inst_1 == 1 |  inst_2 == 2 | inst_3 == 3 | inst_4 == 4 | inst_5 == 5 | segpop == 1, 1,
                        ifelse(atemed == 2 & segpop == 2, 0, NA))]
insurance <- indiv[, .(insurance_id = max(insurance_id)), by = c('folioviv', 'foliohog')]
# insurance[, insurance_type := ifelse(insurance_id == 3, 'IMSS/ISSSTE/PEMEX',
#                                      ifelse(insurance_id == 2, 'SP/INSABI/No coverage', 
#                                             ifelse(insurance_id == 3, 'Other', NA)))] 

## prep aggregate variables
agg[, salud := salud*4][, aten_pri := aten_pri*4][, hospital := hospital*4][, medica := medica*4][, gascor := gascor * 4][, alimentos := alimentos * 4][, ingtot := ingtot * 4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folioviv', 'foliohog', 'upm', 'factor')], 
            consume_wide[, c('folioviv', 'foliohog', 'insurance')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df,
            income[, c('folioviv', 'foliohog', 'inc_sum')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            forgone_care[, c('folioviv', 'foliohog', 'was_ill', 'received_care', 'forgone_care')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            insurance[, c('folioviv', 'foliohog', 'insurance_id')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            agg[, c('folioviv', 'foliohog', 'est_dis', 'conapo', 'tam_hog',  'ingtot', 'gascor', 'alimentos',  'salud', 'aten_pri', 'hospital', 'medica', 'sexo', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folioviv', 'foliohog', 'tam_hog',  'ingtot', 'alimentos', 'aten_pri', 'medica', 'gascor', 'sexo', 'conapo', 'salud'), 
         new = c('hh_id', 'hh_id2', 'hhsize',  'inc_tot', 'food', 'outpatient', 'drugs', 'consumption', 'hhsex', 'strata1', 'health'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2010.dta")

################################################################################
## MEXICO ENIGH 2012                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, 'MEX_ENIGH_2012_TR_CONCEN_Y2015M02D26.DTA')))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2012_TR_HH_Y2015M02D26.DTA")))
housing <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2012_TR_HOUSING_Y2015M02D26.DTA")))
indiv <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2012_TR_POPULATION_Y2015M02D26.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2012_TR_REVENUE_Y2015M02D26.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2012_TR_G_PERSON_Y2015M02D26.DTA")))
spend <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2012_TR_G_HH_Y2015M02D26.DTA")))

## prep income variables
income <- income_prep(income, hhid =  c('folioviv', 'foliohog'))

## prep consumption variables 
consume_wide <- as.data.frame(dcast(consume[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum))
spend_wide <- as.data.frame(dcast(spend[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum))

consume_wide <- consume_wide[,grep("J|folio",colnames(consume_wide))]
spend_wide <- spend_wide[,grep("J|folio",colnames(spend_wide))]

consume_wide <- as.data.frame(rbind.fill(consume_wide, spend_wide))
consume_wide[is.na(consume_wide)] <- 0
consume_wide <- as.data.table(aggregate(. ~ folioviv + foliohog, consume_wide, sum, na.rm =T))

consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J070:J072]

## prep forgone care variables 
indiv[, prob_sal := as.numeric(prob_sal)]
was_ill <- indiv[, ifelse(mean(prob_sal, na.rm = T) == 2, 0, 1), by = c('folioviv', 'foliohog')]
setnames(was_ill, old = 'V1', new = 'was_ill')

indiv[, aten_sal1 := as.numeric(aten_sal1)]
received_care <- indiv[, mean(aten_sal1, na.rm = T), by = c('folioviv', 'foliohog')]
received_care[, V1 := ifelse(V1 == 4.0, 0, 1)]
setnames(received_care, old = c('V1'), new = c('received_care'))

indiv[, noatenc_2 := as.numeric(noatenc_2)][, noatenc_3 := as.numeric(noatenc_3)]
forgone_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('noatenc_2', 'noatenc_3')]
forgone_care[, forgone_care := ifelse(is.na(noatenc_2) & is.na(noatenc_3), 0, 1)]

df <- merge(was_ill, received_care, by = c('folioviv', 'foliohog'))
forgone_care <- merge(df, forgone_care, by = c('folioviv', 'foliohog'))

## prep insurance variables 
indiv <- cbind(indiv[, !c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], sapply(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], as.numeric))
indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')][is.na(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')])] <- 0

## calculate insurance type by household
indiv[, insurance_id := ifelse(inst_1 == 1 |  inst_2 == 2 | inst_3 == 3 | inst_4 == 4 | inst_5 == 5 | segpop == 1, 1,
                        ifelse(atemed == 2 & segpop == 2, 0, NA))]
insurance <- indiv[, .(insurance_id = max(insurance_id)), by = c('folioviv', 'foliohog')]
# insurance[, insurance_type := ifelse(insurance_id == 3, 'IMSS/ISSSTE/PEMEX',
#                                      ifelse(insurance_id == 2, 'SP/INSABI/No coverage', 
#                                             ifelse(insurance_id == 3, 'Other', NA)))]

## prep aggregate variables
agg[, salud := salud*4][, atenc_ambu := atenc_ambu*4][, hospital := hospital*4][, medicinas := medicinas*4][, gasto_cor:= gasto_cor*4][, alimentos := alimentos*4][, ing_total := ing_total*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folioviv', 'foliohog', 'factor_hog')], 
            consume_wide[, c('folioviv', 'foliohog', 'insurance')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            housing[, c('folioviv', 'ageb', 'est_dis', 'upm')], by = c('folioviv'), all.x = T)
df <- merge(df,
            income[, c('folioviv', 'foliohog', 'inc_sum')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            forgone_care[, c('folioviv', 'foliohog', 'was_ill', 'received_care', 'forgone_care')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            insurance[, c('folioviv', 'foliohog', 'insurance_id')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            agg[, c('folioviv', 'foliohog', 'tot_integ', 'est_socio',  'ing_total', 'gasto_cor', 'alimentos',  'salud', 'atenc_ambu', 'hospital', 'medicinas', 'sexo_jefe', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folioviv', 'foliohog', 'tot_integ',  'ing_total', 'alimentos', 'atenc_ambu', 'medicinas', 'gasto_cor', 'sexo_jefe', 'est_socio', 'factor_hog', 'salud'), 
         new = c('hh_id', 'hh_id2', 'hhsize',  'inc_tot', 'food', 'outpatient', 'drugs', 'consumption', 'hhsex', 'strata1', 'factor', 'health'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2012.dta")

################################################################################
## MEXICO ENIGH 2014                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path(dir, 'MEX_ENIGH_2014_TR_HH_CONCENTRATED_Y2015M009D08.DTA')))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2014_TR_HH_Y2015M009D08.DTA")))
indiv <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2014_TR_POP_Y2015M009D08.DTA")))
housing <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2014_TR_LIVING_COND_Y2015M009D08.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2014_TR_INCOME_Y2015M009D08.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2014_TR_PERSONAL_EXPENDITURES_Y2015M009D08.DTA")))
hh_expend <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2014_HH_EXPENDITURES_Y2015M009D08.DTA")))
personal_expend <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2014_TR_PERSONAL_EXPENDITURES_Y2015M009D08.DTA")))

## prep income variables
income <- income_prep(income, hhid =  c('folioviv', 'foliohog'))

## prep consumption variables 
consume_wide <- as.data.frame(dcast(consume[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum))
hh_expend_wide <- as.data.frame(dcast(hh_expend[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum))
personal_expend_wide <- as.data.frame(dcast(personal_expend[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum))

consume_wide <- consume_wide[,grep("J|folio",colnames(consume_wide))]
hh_expend_wide <- hh_expend_wide[,grep("J|folio",colnames(hh_expend_wide))]
personal_expend_wide <- personal_expend_wide[,grep("J|folio",colnames(personal_expend_wide))]

consume_wide <- as.data.frame(rbind.fill(consume_wide, hh_expend_wide, personal_expend_wide))
consume_wide[is.na(consume_wide)] <- 0
consume_wide <- as.data.table(aggregate(. ~ folioviv + foliohog, consume_wide, sum, na.rm =T))

consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J070:J072]

## prep forgone care variables 
indiv[, prob_sal := as.numeric(prob_sal)]
was_ill <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('prob_sal')]
was_ill[, was_ill := ifelse(prob_sal == 1, 1, 0)]

indiv[, aten_sal := as.numeric(aten_sal)]
received_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('aten_sal')]
received_care[, received_care := ifelse(aten_sal == 1, 1, 0)]

indiv[, noatenc_2 := as.numeric(noatenc_2)]
forgone_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('noatenc_2')]
forgone_care[, forgone_care := ifelse(is.na(noatenc_2), 0, 1)]

df <- merge(was_ill, received_care, by = c('folioviv', 'foliohog'))
forgone_care <- merge(df, forgone_care, by = c('folioviv', 'foliohog'))

## prep insurance variables 
indiv <- cbind(indiv[, !c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], sapply(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], as.numeric))
indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')][is.na(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')])] <- 0

## calculate insurance type by household
indiv[, insurance_id := ifelse(inst_1 == 1 |  inst_2 == 2 | inst_3 == 3 | inst_4 == 4 | inst_5  == 1 | segpop == 1, 1,
                        ifelse(atemed == 2 & segpop == 2, 0, NA))]
insurance <- indiv[, .(insurance_id = max(insurance_id)), by = c('folioviv', 'foliohog')]
# insurance[, insurance_type := ifelse(insurance_id == 3, 'IMSS/ISSSTE/PEMEX',
#                                      ifelse(insurance_id == 2, 'SP/INSABI/No coverage', 
#                                             ifelse(insurance_id == 3, 'Other', NA)))]

## prep aggregate variables
agg[, salud := salud*4][, atenc_ambu := atenc_ambu*4][, hospital := hospital*4][, medicinas := medicinas*4][, gasto_cor := gasto_cor*4][, alimentos := alimentos*4][, ing_total := ing_total*4]
agg <- create_loc_ids(data = agg)

## merge datasets 
df <- merge(hh[, c('folioviv', 'foliohog', 'factor_hog')], 
            consume_wide[, c('folioviv', 'foliohog', 'insurance')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            housing[, c('folioviv', 'ageb', 'est_dis', 'upm', 'factor_viv')], by = c('folioviv'), all.x = T)
df <- merge(df,
            income[, c('folioviv', 'foliohog', 'inc_sum')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            forgone_care[, c('folioviv', 'foliohog', 'was_ill', 'received_care', 'forgone_care')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            insurance[, c('folioviv', 'foliohog', 'insurance_id')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            agg[, c('folioviv', 'foliohog', 'tot_integ', 'est_socio',  'ing_total', 'gasto_cor', 'alimentos',  'salud', 'atenc_ambu', 'hospital', 'medicinas', 'sexo_jefe', 'state_name', 'state_id', 'ubica_geo')])

## update variable names
setnames(df, old = c('folioviv', 'foliohog', 'tot_integ',  'ing_total', 'alimentos', 'atenc_ambu', 'medicinas', 'gasto_cor', 'sexo_jefe', 'est_socio', 'factor_hog', 'salud'), 
         new = c('hh_id', 'hh_id2', 'hhsize',  'inc_tot', 'food', 'outpatient', 'drugs', 'consumption', 'hhsex', 'strata1', 'factor', 'health'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2014.dta")

################################################################################
## MEXICO ENIGH 2016                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/mex_enigh/concentradohogar2016.dta")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2016_HH_AND_MEMBER_CHARACTERISTICS_Y2018M09D19.DTA")))
indiv <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2016_SOCIODEMO_HH_MEMBERS_Y2018M09D19.DTA")))
housing <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2016_HH_CHARACTERISTICS_Y2018M09D19.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2016_INCOME_FINANCES_Y2018M09D19.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2016_HH_EXPENDITURES_Y2018M09D19.DTA")))
hh_expend <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2016_HH_MEMBER_EXPENSES_Y2018M09D19.DTA")))
ag <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2016_AGRICULTURE_HH_BUSINESS_Y2018M09D19.DTA")))
nonag <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2016_HH_BUSINESS_Y2018M09D19.DTA")))

## prep income variables
income <- income_prep(income, hhid =  c('folioviv', 'foliohog'))

## prep consumption variables
ag[, self_consumption_ag := auto_tri*4]
ag <- as.data.table(aggregate(self_consumption_ag ~ folioviv + foliohog, ag, sum, na.rm =T))

nonag <- nonag[autocons == 1,]
nonag[, self_consumption_nonag := auto_tri*4]
nonag <- as.data.table(aggregate(self_consumption_nonag ~ folioviv + foliohog, nonag, sum, na.rm =T))

self_consumption <- merge(ag[, c('folioviv', 'foliohog', 'self_consumption_ag')], nonag[, c('folioviv', 'foliohog', 'self_consumption_nonag')], by = c('folioviv', 'foliohog'), all.x = T, all.y = T)
self_consumption[, self_consumption := rowSums(.SD,na.rm = T), .SDcols = self_consumption_ag:self_consumption_nonag]

## prep consumption variables 
consume_wide <- dcast(consume[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum)
hh_expend_wide <- dcast(hh_expend[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum)

consume_wide <- as.data.frame(consume_wide)
consume_wide <- consume_wide[,grep("J|folio",colnames(consume_wide))]

hh_expend_wide <- as.data.frame(hh_expend_wide)
hh_expend_wide <- hh_expend_wide[,grep("J|folio",colnames(hh_expend_wide))]

consume_wide <- as.data.frame(rbind.fill(consume_wide, hh_expend_wide))
consume_wide[is.na(consume_wide)] <- 0
consume_wide <- as.data.table(aggregate(. ~ folioviv + foliohog, consume_wide, sum, na.rm =T))

consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J070:J072]

## prep forgone care variables 
indiv[, prob_sal := as.numeric(prob_sal)]
was_ill <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('prob_sal')]
was_ill[, was_ill := ifelse(prob_sal == 1, 1, 0)]

indiv[, aten_sal := as.numeric(aten_sal)]
received_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('aten_sal')]
received_care[, received_care := ifelse(aten_sal == 1, 1, 0)]

indiv[, noatenc_2 := as.numeric(noatenc_2)]
forgone_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('noatenc_2')]
forgone_care[, forgone_care := ifelse(is.na(noatenc_2), 0, 1)]

df <- merge(was_ill, received_care, by = c('folioviv', 'foliohog'))
forgone_care <- merge(df, forgone_care, by = c('folioviv', 'foliohog'))

## prep insurance variables 
indiv <- cbind(indiv[, !c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], sapply(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], as.numeric))
indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')][is.na(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')])] <- 0

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
indiv[, insurance_id := ifelse(inst_1 == 1 |  inst_2 == 2 | inst_3 == 3 | inst_4 == 4 | inst_5 == 5 | segpop == 1, 1,
                        ifelse(atemed == 2 & segpop == 2, 0, NA))]
insurance <- indiv[, .(insurance_id = max(insurance_id)), by = c('folioviv', 'foliohog')]
# insurance[, insurance_type := ifelse(insurance_id == 3, 'IMSS/ISSSTE/PEMEX',
#                                      ifelse(insurance_id == 2, 'SP/INSABI/No coverage', 
#                                             ifelse(insurance_id == 3, 'Other', NA)))]

## merge datasets 
df <- merge(hh[, c('folioviv', 'foliohog',  'autocons')], 
            consume_wide[, c('folioviv', 'foliohog',  'insurance')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            self_consumption[, c('folioviv', 'foliohog', 'self_consumption')], all.x = T)
df <- merge(df, 
            housing[, c('folioviv', 'ageb', 'est_dis', 'upm', 'factor')], by = c('folioviv'), all.x = T)
df <- merge(df,
            income[, c('folioviv', 'foliohog', 'inc_sum')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            forgone_care[, c('folioviv', 'foliohog', 'was_ill', 'received_care', 'forgone_care')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            insurance[, c('folioviv', 'foliohog', 'insurance_id')], by = c('folioviv', 'foliohog'))
df <- merge(df, 
            agg[, c('folioviv', 'foliohog', 'est_socio',  'tot_integ',  'ing_cor', 'alimentos',  'salud', 'atenc_ambu', 'hospital', 'medicinas', 'sexo_jefe',  'percep_tot', 'gasto_mon', 'remu_espec', 'transf_hog', 'trans_inst', 'estim_alqu', 'ubica_geo')])

## prep aggregate variables
df[, salud := salud * 4][, atenc_ambu := atenc_ambu * 4][, hospital := hospital * 4][, medicinas := medicinas*4][, alimentos := alimentos * 4][, ing_cor := (ing_cor + percep_tot) * 4][, gasto_cor := ifelse(!(is.na(self_consumption)), (gasto_mon + self_consumption + remu_espec + transf_hog + trans_inst + estim_alqu) * 4, (gasto_mon +  remu_espec + transf_hog + trans_inst + estim_alqu) * 4)]
agg <- create_loc_ids(data = agg)

## update variable names
setnames(df, old = c('folioviv', 'foliohog', 'tot_integ',  'ing_cor', 'alimentos', 'atenc_ambu', 'medicinas', 'gasto_cor', 'sexo_jefe', 'est_socio', 'salud'), 
         new = c('hh_id', 'hh_id2', 'hhsize',  'inc_tot', 'food', 'outpatient', 'drugs', 'consumption', 'hhsex', 'strata1', 'health'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2016.dta")

################################################################################
## MEXICO ENIGH 2018                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/mex_enigh/concentradohogar2018.dta")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2018_HH_AND_MEMBER_CHARACTERISTICS_Y2019M10D31.DTA")))
indiv <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2018_SOCIODEMOGRAPHIC_HH_MEMBERS_Y2019M10D31.DTA")))
housing <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2018_HH_CHARACTERISTICS_Y2019M10D31.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2018_INCOME_FINANCES_Y2019M10D31.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2018_HH_EXPENSES_Y2019M10D31.DTA")))
member_expend <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2018_HH_MEMBER_EXPENSES_Y2019M10D31.DTA")))
ag <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2018_AGRICULTURE_HH_BUSINESS_Y2019M10D31.DTA")))
nonag <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2018_HH_BUSINESS_Y2019M10D31.DTA")))

## prep income variables
income <- income_prep(income, hhid =  c('folioviv', 'foliohog'))

## prep consumption variables 
consume_wide <- dcast(consume[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum)
member_expend_wide <- dcast(member_expend[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum)

consume_wide <- as.data.frame(consume_wide)
consume_wide <- consume_wide[,grep("J|folio",colnames(consume_wide))]

member_expend_wide <- as.data.frame(member_expend_wide)
member_expend_wide <- member_expend_wide[,grep("J|folio",colnames(member_expend_wide))]

consume_wide <- as.data.frame(rbind.fill(consume_wide, member_expend_wide))
consume_wide[is.na(consume_wide)] <- 0
consume_wide <- as.data.table(aggregate(. ~ folioviv + foliohog, consume_wide, sum, na.rm =T))

consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J070:J072]

ag[, self_consumption_ag := auto_tri * 4]
ag <- as.data.table(aggregate(self_consumption_ag ~ folioviv + foliohog, ag, sum, na.rm =T))

nonag <- nonag[autocons == 1,]
nonag[, self_consumption_nonag := auto_tri * 4]
nonag <- as.data.table(aggregate(self_consumption_nonag ~ folioviv + foliohog, nonag, sum, na.rm =T))

self_consumption <- merge(ag[, c('folioviv', 'foliohog', 'self_consumption_ag')], nonag[, c('folioviv', 'foliohog', 'self_consumption_nonag')], by = c('folioviv', 'foliohog'), all = T)
self_consumption[, self_consumption := rowSums(.SD,na.rm = T), .SDcols = self_consumption_ag:self_consumption_nonag]

## prep forgone care variables 
indiv[, prob_sal := as.numeric(prob_sal)]
was_ill <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('prob_sal')]
was_ill[, was_ill := ifelse(prob_sal == 1, 1, 0)]

indiv[, aten_sal := as.numeric(aten_sal)]
received_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('aten_sal')]
received_care[, received_care := ifelse(aten_sal == 1, 1, 0)]

indiv[, noatenc_2 := as.numeric(noatenc_2)]
forgone_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('noatenc_2')]
forgone_care[, forgone_care := ifelse(is.na(noatenc_2), 0, 1)]

df <- merge(was_ill, received_care, by = c('folioviv', 'foliohog'))
forgone_care <- merge(df, forgone_care, by = c('folioviv', 'foliohog'))

## prep insurance variables 
indiv <- cbind(indiv[, !c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], sapply(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')], as.numeric))
indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')][is.na(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'segpop', 'atemed')])] <- 0

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
indiv[, insurance_id := ifelse(inst_1 == 1 |  inst_2 == 2 | inst_3 == 3 | inst_4 == 4 | inst_5 == 5 | inst_6 == 6 | segpop == 1, 1,
                        ifelse(atemed == 2 & segpop == 2, 0, NA))]
insurance <- indiv[, .(insurance_id = max(insurance_id)), by = c('folioviv', 'foliohog')]
# insurance[, insurance_type := ifelse(insurance_id == 3, 'IMSS/ISSSTE/PEMEX',
#                                      ifelse(insurance_id == 2, 'SP/INSABI', 
#                                             ifelse(insurance_id == 1, 'No coverage', NA)))]

demographics <- indiv %>% 
  mutate(is_female = ifelse(sexo == 2, 1, 0))

demographics <- demographics[, .(is_female = mean(is_female, na.rm = T), age = mean(edad, na.rm = T)), by = c('folioviv', 'foliohog')]

educ <- indiv %>% 
  filter(edad >25)

educ <- educ %>% 
  mutate(share_below_post_secondary = ifelse(nivelaprob <4, 1, 0))

educ <- educ[, .(share_below_post_secondary = mean(share_below_post_secondary, na.rm = T)), by = c('folioviv', 'foliohog')]

## prep rural/urban indicator
agg[, rural := ifelse(tam_loc == 4, 1, 0)]

## merge datasets 
df <- merge(hh[, c('folioviv', 'foliohog', 'autocons')], 
            consume_wide[, c('folioviv', 'foliohog',  'insurance')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            self_consumption[, c('folioviv', 'foliohog', 'self_consumption')], all.x = T)
df <- merge(df, 
            housing[, c('folioviv', 'est_dis', 'upm', 'factor', 'ubica_geo')], by = c('folioviv'), all.x = T)
df <- merge(df,
            income[, c('folioviv', 'foliohog', 'inc_sum')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            forgone_care[, c('folioviv', 'foliohog', 'was_ill', 'received_care', 'forgone_care')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            insurance[, c('folioviv', 'foliohog', 'insurance_id')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            demographics[, c('folioviv', 'foliohog', 'is_female', 'age')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            educ[, c('folioviv', 'foliohog', 'share_below_post_secondary')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            agg[, c('rural', 'folioviv', 'foliohog', 'est_socio', 'tot_integ',  'ing_cor', 'alimentos',  'salud', 'atenc_ambu', 'hospital', 'medicinas', 'sexo_jefe',  'percep_tot', 'gasto_mon', 'remu_espec', 'transf_hog', 'trans_inst', 'estim_alqu', 'tam_loc')], all.x = T)

## aggregate 
df[, salud := salud * 4][, atenc_ambu := atenc_ambu * 4][, hospital := hospital * 4][, medicinas := medicinas*4][, alimentos := alimentos * 4][, ing_cor := (ing_cor + percep_tot) * 4][, gasto_cor := ifelse(!(is.na(self_consumption)), (gasto_mon + self_consumption + remu_espec + transf_hog + trans_inst + estim_alqu) * 4, (gasto_mon +  remu_espec + transf_hog + trans_inst + estim_alqu) * 4)]
agg <- create_loc_ids(data = agg)

## update variable names
setnames(df, old = c('folioviv', 'foliohog', 'tot_integ',  'ing_cor', 'alimentos', 'atenc_ambu', 'medicinas', 'gasto_cor', 'sexo_jefe', 'est_socio', 'salud', 'est_dis'), 
         new = c('hh_id', 'hh_id2', 'hhsize',  'inc_tot', 'food', 'outpatient', 'drugs', 'consumption', 'hhsex', 'strata1', 'health', 'strata2'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2018.dta")

################################################################################
## MEXICO ENIGH 2020                                                          ##
################################################################################
## directory 
dir <- file.path("FILEPATH")

## read datasets
agg <- data.table(read.dta13(file.path("/ihme/scratch/projects/hssa/frp/mex_enigh/concentradohogar2020.dta")))
hh <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2020_HH_MEMBER_CHARACTERISTICS_Y2021M08D06.DTA")))
indiv <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2020_SOCIODEMOGRAPHIC_HH_MEMBERS_Y2021M08D06.DTA")))
housing <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2020_HH_CHARACTERISTICS_Y2021M08D06.DTA")))
income <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2020_INCOME_FINANCES_Y2021M08D06.DTA")))
consume <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2020_HH_EXPENSES_Y2021M08D06.DTA")))
member_expend <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2020_HH_MEMBER_EXPENSES_Y2021M08D06.DTA")))
ag <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2020_AGRICULTURAL_HH__BUSINESS_Y2021M08D06.DTA")))
nonag <- data.table(read.dta13(file.path(dir, "MEX_ENIGH_2020_HH_BUSINESS_Y2021M08D06.DTA")))

## prep income variables
income <- income_prep(income, hhid =  c('folioviv', 'foliohog'))

## prep consumption variables 
consume_wide <- as.data.frame(dcast(consume[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum))
member_expend_wide <- as.data.frame(dcast(member_expend[, c('folioviv', 'foliohog', 'clave', 'gas_nm_tri')], folioviv + foliohog ~ clave, value.var = 'gas_nm_tri', fun.aggregate = sum))

consume_wide <- consume_wide[,grep("J|folio",colnames(consume_wide))]
member_expend_wide <- member_expend_wide[,grep("J|folio",colnames(member_expend_wide))]

consume_wide <- as.data.frame(rbind.fill(consume_wide, member_expend_wide))
consume_wide[is.na(consume_wide)] <- 0
consume_wide <- as.data.table(aggregate(. ~ folioviv + foliohog, consume_wide, sum, na.rm =T))

consume_wide[, insurance := rowSums(.SD*4, na.rm = T), .SDcols = J070:J072]

ag[, self_consumption_ag := auto_tri * 4]
ag <- as.data.table(aggregate(self_consumption_ag ~ folioviv + foliohog, ag, sum, na.rm =T))

nonag <- nonag[autocons == 1,]
nonag[, self_consumption_nonag := auto_tri * 4]
nonag <- as.data.table(aggregate(self_consumption_nonag ~ folioviv + foliohog, nonag, sum, na.rm =T))

self_consumption <- merge(ag[, c('folioviv', 'foliohog', 'self_consumption_ag')], nonag[, c('folioviv', 'foliohog', 'self_consumption_nonag')], by = c('folioviv', 'foliohog'), all.x = T, all.y = T)
self_consumption[, self_consumption := rowSums(.SD,na.rm = T), .SDcols = self_consumption_ag:self_consumption_nonag]

## prep forgone care variables 
indiv[, prob_sal := as.numeric(prob_sal)]
was_ill <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('prob_sal')]
was_ill[, was_ill := ifelse(prob_sal == 1, 1, 0)]

indiv[, aten_sal := as.numeric(aten_sal)]
received_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('aten_sal')]
received_care[, received_care := ifelse(aten_sal == 1, 1, 0)]

indiv[, noatenc_2 := as.numeric(noatenc_2)]
forgone_care <- indiv[, lapply(.SD, mean, na.rm = T), by = c('folioviv', 'foliohog'), .SDcols = c('noatenc_2')]
forgone_care[, forgone_care := ifelse(is.na(noatenc_2), 0, 1)]

df <- merge(was_ill, received_care, by = c('folioviv', 'foliohog'))
forgone_care <- merge(df, forgone_care, by = c('folioviv', 'foliohog'))

## prep insurance variables 
indiv <- cbind(indiv[, !c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'pop_insabi', 'atemed')], sapply(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'pop_insabi', 'atemed')], as.numeric))
indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'pop_insabi', 'atemed')][is.na(indiv[,c('inst_1', 'inst_2', 'inst_3', 'inst_4', 'inst_5', 'pop_insabi', 'atemed')])] <- 0

## calculate insurance status by household
## indicator that anyone in household has insurance coverage (not by type)
indiv[, insurance_id := ifelse(inst_1 == 1 |  inst_2 == 2 | inst_3 == 3 | inst_4 == 4 | inst_5 == 5 | inst_6 == 6 | pop_insabi == 1, 1,
                        ifelse(atemed == 2 & pop_insabi == 2, 0, NA))]
insurance <- indiv[, .(insurance_id = max(insurance_id)), by = c('folioviv', 'foliohog')]
# insurance[, insurance_type := ifelse(insurance_id == 3, 'IMSS/ISSSTE/PEMEX',
#                                      ifelse(insurance_id == 2, 'SP/INSABI', 
#                                             ifelse(insurance_id == 1, 'No coverage', NA)))]

demographics <- indiv %>% 
  mutate(is_female = ifelse(sexo == 2, 1, 0))

demographics <- demographics[, .(is_female = mean(is_female, na.rm = T), age = mean(edad, na.rm = T)), by = c('folioviv', 'foliohog')]

educ <- indiv %>% 
  filter(edad >25)

educ <- educ %>% 
  mutate(share_below_post_secondary = ifelse(nivelaprob <4, 1, 0))

educ <- educ[, .(share_below_post_secondary = mean(share_below_post_secondary, na.rm = T)), by = c('folioviv', 'foliohog')]

## prep rural/urban indicator
agg[, rural := ifelse(tam_loc == 4, 1, 0)]

## merge datasets 
df <- merge(hh[, c('folioviv', 'foliohog',  'autocons')], 
            consume_wide[, c('folioviv', 'foliohog',  'insurance')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            self_consumption[, c('folioviv', 'foliohog', 'self_consumption')], all.x = T)
df <- merge(df, 
            housing[, c('folioviv', 'est_dis', 'upm', 'factor', 'ubica_geo')], by = c('folioviv'), all.x = T)
df <- merge(df,
            income[, c('folioviv', 'foliohog', 'inc_sum')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            forgone_care[, c('folioviv', 'foliohog', 'was_ill', 'received_care', 'forgone_care')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            insurance[, c('folioviv', 'foliohog', 'insurance_id')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            demographics[, c('folioviv', 'foliohog', 'is_female', 'age')], by = c('folioviv', 'foliohog'), all.x = T)
df <- merge(df, 
            educ[, c('folioviv', 'foliohog', 'share_below_post_secondary')], by = c('folioviv', 'foliohog'), all.x = T)

df <- merge(df, 
            agg[, c('rural', 'folioviv', 'foliohog', 'est_socio',  'tot_integ',  'ing_cor', 'alimentos',  'salud', 'atenc_ambu', 'hospital', 'medicinas', 'sexo_jefe',  'percep_tot', 'gasto_mon', 'remu_espec', 'transf_hog', 'trans_inst', 'estim_alqu', 'tam_loc')], all.x = T)

## aggregate 
df[, salud := salud * 4][, atenc_ambu := atenc_ambu * 4][, hospital := hospital * 4][, medicinas := medicinas*4][, alimentos := alimentos * 4][, ing_cor := (ing_cor + percep_tot) * 4][, gasto_cor := ifelse(!(is.na(self_consumption)), (gasto_mon + self_consumption + remu_espec + transf_hog + trans_inst + estim_alqu) * 4, (gasto_mon +  remu_espec + transf_hog + trans_inst + estim_alqu) * 4)]
agg <- create_loc_ids(data = agg)

## update variable names
setnames(df, old = c('folioviv', 'foliohog','tot_integ',  'ing_cor', 'alimentos', 'atenc_ambu', 'medicinas', 'gasto_cor', 'sexo_jefe', 'est_socio', 'salud', 'est_dis'), 
         new = c('hh_id', 'hh_id2', 'hhsize',  'inc_tot', 'food', 'outpatient', 'drugs', 'consumption', 'hhsex', 'strata1', 'health', 'strata2'))

## check to make sure all households are included in extraction
if (nrow(df) == nrow(hh)){
  print("Extraction complete")
}  else {
  print("Check extraction")
}

## save prepped extraction dataset 
save.dta13(df, "/FILEPATH/MEX_ENIGH_2020.dta")
