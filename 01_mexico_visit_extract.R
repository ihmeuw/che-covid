################################################################################
## Author: USER                                                               ##
## Date: 12/28/21                                                             ##
## Purpose: Extract visit level information from Mexico  ENIGH 2018 2020      ##
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

## directories
indir <-  file.path("FILEPATH")
outdir <- file.path("FILEPATH")

################################################################################
## MEXICO ENIGH 2020                                                         ##
################################################################################

#read in mexico 2020 data
indiv <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2020_SOCIODEMOGRAPHIC_HH_MEMBERS_Y2021M08D06.DTA")))

#combine month and year of visit
indiv[, prob_mes := as.numeric(prob_mes)]
indiv[, prob_anio := as.numeric(prob_anio)]

indiv <- indiv %>%
  filter(!is.na(prob_mes))%>%
  filter(!is.na(prob_anio))

visits20 <- setDT(indiv)

## count number of visits (max of 1)
visits20[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                servmed_9 == '09' | servmed_12 == '12') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020 , 1, 0)]
visits20[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020, 1, 0)]
visits20[, is_private_pharmacy := ifelse((servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020, 1, 0)]
visits20[, is_public := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                  servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_12 == '12') & 
                                 !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020, 1, 0)]

# combine with agg to get weights
colnames(visits20)[1:2] <- c("hh_id","hh_id2")
visits20 <- visits20[,c("hh_id","hh_id2", "is_private", "is_public", "numren", "prob_anio", "prob_mes", "any_visit", "is_private_pharmacy")]

# get survey design factors from aggregate file 
agg20 <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2020.dta")))
agg20 <- agg20[,c("hh_id", "hh_id2", "strata1", "strata2", "upm", "factor")]

visits20 <- merge(visits20, agg20, by = c("hh_id", "hh_id2"))
visits20[,year_id := 2020]

write.csv(visits20, file.path(outdir, "MEX_ENIGH_VISITS_2020.csv"))

################################################################################
## MEXICO ENIGH 2018                                                         ##
################################################################################

#read in mexico 2018 data
indiv <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2018_SOCIODEMOGRAPHIC_HH_MEMBERS_Y2019M10D31.DTA")))

#combine month and year of visit
indiv[, prob_mes := as.numeric(prob_mes)]
indiv[, prob_anio := as.numeric(prob_anio)]

indiv <- indiv %>%
  filter(!is.na(prob_mes))%>%
  filter(!is.na(prob_anio))

visits18 <- setDT(indiv)

visits18[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                  servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                  servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2018, 1, 0)]
visits18[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2018, 1, 0)]
visits18[, is_public := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                  servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07') & 
                                 !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2018, 1, 0)]
visits18[, is_private_pharmacy := ifelse((servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2018, 1, 0)]

# get survey design factors from aggregate file 
agg18 <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2018.dta")))
agg18 <- agg18[,c("hh_id", "hh_id2", "strata1", "strata2", "upm", "factor")]

# combine with agg to get weights
colnames(visits18)[1:2] <- c("hh_id","hh_id2")
visits18 <- visits18[,c("hh_id","hh_id2","is_private", "is_public", "numren","prob_anio","prob_mes", "any_visit","is_private_pharmacy")]

visits18 <- merge(visits18, agg18, by = c("hh_id", "hh_id2"))
visits18[,year_id := 2018]

write.csv(visits18, file.path(outdir, "MEX_ENIGH_VISITS_2018.csv"))

################################################################################
## MEXICO ENIGH 2016                                                         ##
################################################################################
#read in mexico 2016 data
indiv <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2016_SOCIODEMO_HH_MEMBERS_Y2018M09D19.DTA")))

#combine month and year of visit
indiv[, prob_mes := as.numeric(prob_mes)]
indiv[, prob_anio := as.numeric(prob_anio)]

indiv <- indiv %>%
  filter(!is.na(prob_mes))%>%
  filter(!is.na(prob_anio))

visits16 <- setDT(indiv)

## count number of visits (max of 1)
visits16[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                  servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                  servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2016, 1, 0)]
visits16[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2016, 1, 0)]

# get survey design factors from aggregate file 
agg16 <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2016.dta")))
agg16 <- agg16[,c("hh_id", "hh_id2", "strata1", "upm", "factor")]

# combine with agg to get weights
colnames(visits16)[1:2] <- c("hh_id","hh_id2")
visits16 <- visits16[,c("hh_id","hh_id2", "is_private", "numren", "prob_anio", "prob_mes", "any_visit")]

visits16 <- merge(visits16, agg16, by = c("hh_id", "hh_id2"))
visits16[,year_id := 2016]

write.csv(visits16, file.path(outdir, "MEX_ENIGH_VISITS_2016.csv"))

################################################################################
## MEXICO ENIGH 2014                                                         ##
################################################################################
#read in mexico 2014 data
indiv <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2014_NC_POP_Y2015M09D08.DTA")))

#combine month and year of visit
indiv[, prob_mes := as.numeric(prob_mes)]
indiv[, prob_anio := as.numeric(prob_anio)]

indiv <- indiv %>%
  filter(!is.na(prob_mes))%>%
  filter(!is.na(prob_anio))

visits14 <- setDT(indiv)

## count number of visits (max of 1)
visits14[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                  servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                  servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2014, 1, 0)]
visits14[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2014, 1, 0)]

colnames(visits14)[1:2] <- c("hh_id","hh_id2")

# get survey design factors from aggregate file 
agg14 <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2014.dta")))
agg14 <- agg14[,c("hh_id", "hh_id2", "factor")]

# combine with agg to get weights
colnames(visits14)[1:2] <- c("hh_id","hh_id2")
visits14 <- visits14[,c("hh_id","hh_id2", "is_private", "numren","prob_anio", "prob_mes", "any_visit")]

visits14 <- merge(visits14, agg14, by = c("hh_id", "hh_id2"))
visits14[,year_id := 2014]

write.csv(visits14, file.path(outdir, "MEX_ENIGH_VISITS_2014.csv"))

################################################################################
## MEXICO ENIGH 2012                                                          ##
################################################################################
#read in mexico 2012 data
indiv <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2012_NC_POPULATION_Y2015M02D26.DTA")))

#there is no health problem month/year variable prior to 2012
#combine month and year of visit
#indiv[, prob_mes := as.numeric(prob_mes)]
#indiv[, prob_anio := as.numeric(prob_anio)]

#indiv <- indiv %>%
#  filter(!is.na(prob_mes))%>%
#  filter(!is.na(prob_anio))

visits12 <- setDT(indiv)

## count number of visits (max of 1)
visits12[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                  servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                  servmed_9 == '09') , 1, 0)]
visits12[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09'), 1, 0)]

colnames(visits12)[1:2] <- c("hh_id","hh_id2")

# get survey design factors from aggregate file 
agg12 <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2012.dta")))
agg12 <- agg12[,c("hh_id", "hh_id2", "factor")]

# combine with agg to get weights
colnames(visits12)[1:2] <- c("hh_id","hh_id2")
visits12 <- visits12[,c("hh_id","hh_id2", "is_private", "numren", "any_visit")]

visits12 <- merge(visits12, agg12, by = c("hh_id", "hh_id2"))
visits12[,year_id := 2012]

write.csv(visits12, file.path(outdir, "MEX_ENIGH_VISITS_2012.csv"))

################################################################################
## MEXICO ENIGH 2010                                                          ##
################################################################################
#read in mexico 2010 data
indiv <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2010_SOCIODEMOGRAPHICS_Y2013M03D29.DTA")))

#there is no health problem month/year variable prior to 2012
#combine month and year of visit
#indiv[, prob_mes := as.numeric(prob_mes)]
#indiv[, prob_anio := as.numeric(prob_anio)]

#indiv <- indiv %>%
#  filter(!is.na(prob_mes))%>%
#  filter(!is.na(prob_anio))

visits10 <- setDT(indiv)

## count number of visits (max of 1)
visits10[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                  servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                  servmed_9 == '09') , 1, 0)]
visits10[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09'), 1, 0)]

colnames(visits10)[1:2] <- c("hh_id","hh_id2")

# get survey design factors from aggregate file 
agg10 <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2010.dta")))
agg10 <- agg10[,c("hh_id", "hh_id2", "upm", "factor")]


# combine with agg to get weights
visits10 <- visits10[,c("hh_id","hh_id2", "is_private", "numren", "any_visit")]

visits10 <- merge(visits10, agg10, by = c("hh_id", "hh_id2"))
visits10[,year_id := 2010]

write.csv(visits10, file.path(outdir, "MEX_ENIGH_VISITS_2010.csv"))

################################################################################
## MEXICO ENIGH 2008                                                          ##
################################################################################
#read in mexico 2010 data
indiv <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2008_SOCIODEMOGRAPHICS.DTA")))

#there is no health problem month/year variable prior to 2012
#combine month and year of visit
#indiv[, prob_mes := as.numeric(prob_mes)]
#indiv[, prob_anio := as.numeric(prob_anio)]

#indiv <- indiv %>%
#  filter(!is.na(prob_mes))%>%
#  filter(!is.na(prob_anio))

visits08 <- setDT(indiv)

## count number of visits (max of 1)
visits08[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                  servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                  servmed_9 == '09') , 1, 0)]
visits08[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09'), 1, 0)]

colnames(visits08)[1:2] <- c("hh_id","hh_id2")

# get survey design factors from aggregate file 
agg08 <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2008.dta")))
agg08 <- agg08[,c("hh_id", "hh_id2", "upm", "factor")]

# combine with agg to get weights
visits08 <- visits08[,c("hh_id","hh_id2", "is_private", "numren", "any_visit")]

visits08 <- merge(visits08, agg08, by = c("hh_id", "hh_id2"))
visits08[,year_id := 2008]

write.csv(visits08, file.path(outdir, "MEX_ENIGH_VISITS_2008.csv"))

################################################################################
## MEXICO ENIGH 2006                                                          ##
################################################################################
## question about utilization (servmed variables) was not in 2006 questionnaire
