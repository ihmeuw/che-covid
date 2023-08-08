################################################################################
## Author: USER                                                               ##
## Date: 12/27/21                                                             ##
## Purpose: Preprocess MEX ENIGH surveys to combine to individual level       ##
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
outdir <-  file.path("FILEPATH")

## 2020 
# read in hh member file for for average age, share female, share below post-secondary, number of visits, number of
# private visits, and type of health insurance information
member <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2020_SOCIODEMOGRAPHIC_HH_MEMBERS_Y2021M08D06.DTA"), convert.factors=FALSE))

## extract share female 
member[, is_female := ifelse(sexo == 2, 1, 0)]

## extract average age 
member[, age := edad]

## count number of visits (max of 1)
member[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                          servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                          servmed_9 == '09'| servmed_12 == '12') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020, 1, 0)]
member[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09' ) & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020 , 1, 0)]

## extract insurance type: 4 categories 
member <- member[, insurance := ifelse(inst_1 == "1" | inst_5 == "5"| inst_2 == "2" | inst_3 == "3"| inst_4 == "4", "IMSS/ISSSTE/PEMEX",
                  ifelse(pop_insabi == "1" & atemed == "2" , "SP/INSABI", 
                         ifelse(atemed == "2" & pop_insabi == "2", "No coverage",
                                ifelse(inst_6 == "6", "Other",
                                       "NA"
                                ))))]


member[, private_coverage := ifelse(insurance == "IMSS/ISSSTE/PEMEX", 1, 0) ]
member[, public_coverage := ifelse(insurance == "SP/INSABI", 1, 0) ]
member[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
member[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

## extract level of education information 
member[age > 25, share_below_post_secondary := ifelse(nivelaprob <4, 1, 0)]

## TODO: check if this necessary and fix or delete
## agg <- data.table(read.dta13(file.path(indir "concentradohogar2020.dta")))
## merge(member, agg[ , c('upm', 'est_dis', )])

save.dta13(member, file.path (outdir, "MEX_ENIGH_IND_2020.dta"))

## 2018
# read in hh member file for for average age, share female, share below post-secondary, number of visits, number of
# private visits, and type of health insurance information
member <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2018_SOCIODEMOGRAPHIC_HH_MEMBERS_Y2019M10D31.DTA"), convert.factors=FALSE))

## extract share female 
member[, is_female := ifelse(sexo == 2, 1, 0)]

## extract average age 
member[, age := edad]


## count number of visits (max of 1)
member[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020 , 1, 0)]
member[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020 , 1, 0)]

## individual insurance status 
member <- member[, insurance := 
           ifelse(inst_1 == "1" | inst_5 == "5"| inst_2 == "2" | inst_3 == "3"| inst_4 == "4", "IMSS/ISSSTE/PEMEX",
                  ifelse((segpop == "1" & atemed == "2"), "SP/INSABI", 
                         ifelse((atemed == "2" & segpop == "2"), "No coverage", 
                                ifelse(inst_6 == "6", "Other",
                                       
                                       "NA"
                                ))))]


member[, private_coverage := ifelse(insurance == "IMSS/ISSSTE/PEMEX", 1, 0) ]
member[, public_coverage := ifelse(insurance == "SP/INSABI", 1, 0) ]
member[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
member[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

## extract level of education information 
member[age > 25, share_below_post_secondary := ifelse(nivelaprob <4, 1, 0)]
save.dta13(member, file.path (outdir, "MEX_ENIGH_IND_2018.dta"))

## 2016
# read in hh member file for for average age, share female, share below post-secondary, number of visits, number of
# private visits, and type of health insurance information
member <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2016_SOCIODEMO_HH_MEMBERS_Y2018M09D19.DTA"), convert.factors=FALSE))

## extract share female 
member[, is_female := ifelse(sexo == 2, 1, 0)]

## extract average age 
member[, age := edad]

## count number of visits (max of 1)
member[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020 , 1, 0)]
member[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020 , 1, 0)]

## extract insurance type: 4 categories 
member <- member[, insurance := ifelse(inst_1 == "1" | inst_5 == "5"| inst_2 == "2" | inst_3 == "3"| inst_4 == "4", "IMSS/ISSSTE/PEMEX",
                                       ifelse((segpop == "1" & atemed == "2") , "SP/INSABI", 
                                              ifelse((atemed == "2" & segpop == "2"), "No coverage",
                                                     ifelse(inst_6 == "6", "Other",
                                                            "NA"
                                                     ))))]


member[, private_coverage := ifelse(insurance == "IMSS/ISSSTE/PEMEX", 1, 0) ]
member[, public_coverage := ifelse(insurance == "SP/INSABI", 1, 0) ]
member[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
member[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

## extract level of education information 
member[age > 25, share_below_post_secondary := ifelse(nivelaprob <4, 1, 0)]
save.dta13(member, file.path (outdir, "MEX_ENIGH_IND_2016.dta"))

## 2014
# read in hh member file for for average age, share female, share below post-secondary, number of visits, number of
# private visits, and type of health insurance information
member <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2014_NC_POP_Y2015M09D08.DTA"), convert.factors=FALSE))

## extract share female 
member[, is_female := ifelse(sexo == 2, 1, 0)]

## extract average age 
member[, age := edad]

## count number of visits (max of 1)
member[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                                servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                                servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020 , 1, 0)]
member[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09') & !is.na(prob_mes) & !is.na(prob_anio) & prob_anio == 2020 , 1, 0)]

## extract insurance type: 4 categories 
member <- member[, insurance := ifelse(inst_1 == "1" | inst_5 == "5"| inst_2 == "2" | inst_3 == "3"| inst_4 == "4", "IMSS/ISSSTE/PEMEX",
                                       ifelse((segpop == "1" & atemed == "2") , "SP/INSABI", 
                                              ifelse((atemed == "2" & segpop == "2"), "No coverage",
                                                     ifelse(inst_5 == "5", "Other",
                                                            "NA"
                                                     ))))]


member[, private_coverage := ifelse(insurance == "IMSS/ISSSTE/PEMEX", 1, 0) ]
member[, public_coverage := ifelse(insurance == "SP/INSABI", 1, 0) ]
member[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
member[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

## extract level of education information 
member[age > 25, share_below_post_secondary := ifelse(nivelaprob <4, 1, 0)]
save.dta13(member, file.path (outdir, "MEX_ENIGH_IND_2014.dta"))

## 2012
# read in hh member file for for average age, share female, share below post-secondary, number of visits, number of
# private visits, and type of health insurance information
member <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2012_NC_POPULATION_Y2015M02D26.DTA"), convert.factors=FALSE))

## extract share female 
member[, is_female := ifelse(sexo == 2, 1, 0)]

## extract average age 
member[, age := edad]

## NOTE: dropping this prep because the question isn't comparable to 2014 onward 
# member[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
#                                 servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
#                                 servmed_9 == '09') , 1, 0)]
# member[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09'), 1, 0)]

## extract insurance type: 4 categories 
member <- member[, insurance := ifelse(inst_1 == "1" | inst_5 == "5"| inst_2 == "2" | inst_3 == "3"| inst_4 == "4", "IMSS/ISSSTE/PEMEX",
                                       ifelse((segpop == "1" & atemed == "2") , "SP/INSABI", 
                                              ifelse((atemed == "2" & segpop == "2"), "No coverage",
                                                     ifelse(inst_5 == "5", "Other",
                                                            "NA"
                                                     ))))]


member[, private_coverage := ifelse(insurance == "IMSS/ISSSTE/PEMEX", 1, 0) ]
member[, public_coverage := ifelse(insurance == "SP/INSABI", 1, 0) ]
member[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
member[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

## extract level of education information 
member[age > 25, share_below_post_secondary := ifelse(nivelaprob <4, 1, 0)]
save.dta13(member, file.path (outdir, "MEX_ENIGH_IND_2012.dta"))

## 2010
# read in hh member file for for average age, share female, share below post-secondary, number of visits, number of
# private visits, and type of health insurance information
member <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2010_SOCIODEMOGRAPHICS_Y2013M03D29.DTA"), convert.factors=FALSE))

## extract share female 
member[, is_female := ifelse(sexo == 2, 1, 0)]

## extract average age 
member[, age := edad]

## NOTE: dropping this prep because the question isn't comparable to 2014 onward 
# member[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
#                                 servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
#                                 servmed_9 == '09') , 1, 0)]
# member[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09') , 1, 0)]

## extract insurance type: 4 categories 
member <- member[, insurance := ifelse(inst_1 == "1" | inst_5 == "5"| inst_2 == "2" | inst_3 == "3"| inst_4 == "4", "IMSS/ISSSTE/PEMEX",
                                       ifelse((segpop == "1" & atemed == "2") , "SP/INSABI", 
                                              ifelse((atemed == "2" & segpop == "2"), "No coverage",
                                                     ifelse(inst_5 == "5", "Other",
                                                            "NA"
                                                     ))))]


member[, private_coverage := ifelse(insurance == "IMSS/ISSSTE/PEMEX", 1, 0) ]
member[, public_coverage := ifelse(insurance == "SP/INSABI", 1, 0) ]
member[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
member[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

## extract level of education information 
member[age > 25, share_below_post_secondary := ifelse(nivelaprob <4, 1, 0)]
save.dta13(member, file.path (outdir, "MEX_ENIGH_IND_2010.dta"))

## 2008
# read in hh member file for for average age, share female, share below post-secondary, number of visits, number of
# private visits, and type of health insurance information
member <- data.table(read.dta13(file.path(indir, "MEX_ENIGH_2008_SOCIODEMOGRAPHICS.DTA"), convert.factors=FALSE))

## extract share female 
member[, is_female := ifelse(sexo == 2, 1, 0)]

## extract average age 
member[, age := edad]

## NOTE: dropping this prep because the question isn't comparable to 2014 onward 
# member[, any_visit := ifelse(ate_sal8 == 1, 1, 0)]
# member[, any_visit := ifelse(is.na(any_visit), 0, any_visit)]

## extract insurance type: 4 categories 
## NOTE: changed format of ifelse statement because it was getting stuck on the first line ...
member <- member[, insurance := ifelse(inst_1 == 1 |  inst_2 == 2 | inst_3 == 3| inst_4 == 4, "IMSS/ISSSTE/PEMEX")]
member <- member[, insurance := ifelse(segpop == 1, "SP/INSABI", insurance)]
member <- member[, insurance := ifelse(atemed == 2 & segpop == 2, "No coverage", insurance)]
member <- member[inst_5 == 5, insurance := "Other"]

member[, private_coverage := ifelse(insurance == "IMSS/ISSSTE/PEMEX", 1, 0) ]
member[, public_coverage := ifelse(insurance == "SP/INSABI", 1, 0) ]
member[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
member[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

## extract level of education information 
member[age > 25, share_below_post_secondary := ifelse(nivel <4, 1, 0)]
save.dta13(member, file.path (outdir, "MEX_ENIGH_IND_2008.dta"))

## 2006
## question about utilization (servmed) and insurance affiliation (inst) were not in 2006 questionnaire 