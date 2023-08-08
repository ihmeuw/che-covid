################################################################################
## Author: USER                                                               ##
## Date: 10/27/21                                                             ##
## Purpose: Preprocess PERU ENAHO surveys to combine to individual level      ##
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
indir <-  file.path("FILEPATH)
##outdir <-  file.path("FILEPATH)

## 2020 
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2020_ANNUAL_MODULE_02_ENAHO01_2020_200_Y2021M07D23.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2020_ANNUAL_MODULE_03_ENAHO01A_2020_300_Y2021M07D23.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2020_ANNUAL_MODULE_04_ENAHO01A_2020_400_Y2021M07D23.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 
## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                            ifelse(p4198 == 1, "Other", 
                                   ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                          "No coverage"
                                   )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
roster <- roster[,c("is_female", "age","aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(roster)[12] <- "factor07" # these are the same 
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
ed_roster <- merge(ed, roster, by =c("aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH, "PER_ENAHO_IND_2020.dta"))

## design <-  svydesign(ids = ~nconglome, strata = ~estrato, weights = ~factor07, data = ed_health_roster, variables = ~age+year_id, nest = T)
## means <- svyby(~age, ~year_id, design, svymean, na.rm =T) 

## 2019 
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2019_01_200_Y2020M07D27.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2019_01A_300_Y2020M07D27.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2019_01A_400_Y2020M07D27.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]
# extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]


# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
roster <- roster[,c("is_female", "age","aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(roster)[12] <- "factor07" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]

ed_roster <- merge(ed, roster, by =c("aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH, "PER_ENAHO_IND_2019.dta"))

## 2018
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2018_200_Y2019M06D19.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2018_300_Y2019M06D19.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2018_400_Y2019M06D19.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]


# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
roster <- roster[,c("is_female", "age","aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(roster)[12] <- "factor07" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]

ed_roster <- merge(ed, roster, by =c("aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH, "PER_ENAHO_IND_2018.dta"))

## 2017 (only one quarter available)                                                  

## 2016
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2016_ANNUAL_HH_ROSTER_200_Y2017M09D08.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2016_ANNUAL_EDU_300_Y2017M09D08.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2016_HEALTH_400_Y2017M09D08.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]


# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
roster <- roster[,c("is_female", "age","aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(roster)[12] <- "factor07" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]

ed_roster <- merge(ed, roster, by =c("aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("aÑo","mes","conglome","nconglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH, "PER_ENAHO_IND_2016.dta"))

## 2015
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2015_ANNUAL_HH_ROSTER_200_Y2016M09D19.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2015_ANNUAL_EDU_300_Y2016M09D19.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2015_ANNUAL_HEALTH_400_Y2016M09D19.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]


# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
roster <- roster[,c("is_female", "age","aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(roster)[11] <- "factor07" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]

ed_roster <- merge(ed, roster, by =c("aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH,"PER_ENAHO_IND_2015.dta"))

## 2014
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2014_ANNUAL_HH_ROSTER_200_Y2016M09D19.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2014_ANNUAL_EDU_300_Y2016M09D19.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2014_ANNUAL_HEALTH_400_Y2016M09D19.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]


# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
roster <- roster[,c("is_female", "age","aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(roster)[11] <- "factor07" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]

ed_roster <- merge(ed, roster, by =c("aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH, "PER_ENAHO_IND_2014.dta"))

## 2012
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2012_ANNUAL_324_MOD_02_01_200_Y2013M07D12.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2012_ANNUAL_324_MOD_03_01A_300_Y2013M07D12.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2012_ANNUAL_324_MOD_04_01A_400_Y2013M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 
## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]


# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(ed)[11] <- "factor07" # these are the same
roster <- roster[,c("is_female", "age","aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(roster)[11] <- "factor07" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]

ed_roster <- merge(ed, roster, by =c("aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH, "PER_ENAHO_IND_2012.dta"))

## 2011
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2011_ANNUAL_291_MOD_02_01_200_Y2013M07D12_Y2013M07D12.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2011_ANNUAL_291_MOD_03_01A_300_Y2013M07D12_Y2013M07D12.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2011_ANNUAL_291_MOD_04_01A_400_Y2013M07D12_Y2013M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]
## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]


# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
roster <- roster[,c("is_female", "age","aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(roster)[11] <- "factor07" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]

ed_roster <- merge(ed, roster, by =c("aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("aÑo","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH,"PER_ENAHO_IND_2011.dta"))

## 2010
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2010_HH_ROSTER_200_Y2011M07D12.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2010_EDU_1_300_Y2011M07D12.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2010_HEALTH_400_Y2011M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## change column names to lowercase
colnames(roster) <- tolower(colnames(roster))
colnames(ed) <- tolower(colnames(ed))
colnames(health) <- tolower(colnames(health))

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]


# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
roster <- roster[,c("is_female", "age","año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob07","codperso" )]
colnames(roster)[11] <- "factor07" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso" )]
colnames(health)[7] <- "año"

ed_roster <- merge(ed, roster, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor07","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH,"PER_ENAHO_IND_2010.dta"))

## 2009
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2009_HH_ROSTER_200_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2009_EDU_1_300_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2009_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## change column names to lowercase
colnames(roster) <- tolower(colnames(roster))
colnames(ed) <- tolower(colnames(ed))
colnames(health) <- tolower(colnames(health))

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]


# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]
roster <- roster[,c("is_female", "age","año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob","codperso" )]
colnames(roster)[11] <- "factor" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]

ed_roster <- merge(ed, roster, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH,"PER_ENAHO_IND_2009.dta"))

## 2008
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2008_ANNUAL_HH_ROSTER_200_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2008_ANNUAL_EDU_1_300_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2008_ANNUAL_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## change column names to lowercase
colnames(roster) <- tolower(colnames(roster))
colnames(ed) <- tolower(colnames(ed))
colnames(health) <- tolower(colnames(health))

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 
## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]
roster <- roster[,c("is_female", "age","año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob","codperso" )]
colnames(roster)[11] <- "factor" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]

ed_roster <- merge(ed, roster, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH, "PER_ENAHO_IND_2008.dta"))

## 2007
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2007_ANNUAL_HH_ROSTER_200_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2007_ANNUAL_EDU_300_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2007_ANNUAL_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## change column names to lowercase
colnames(roster) <- tolower(colnames(roster))
colnames(ed) <- tolower(colnames(ed))
colnames(health) <- tolower(colnames(health))

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]
colnames(ed)[3] <- "año"
roster <- roster[,c("is_female", "age","año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob","codperso" )]
colnames(roster)[11] <- "factor" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]

ed_roster <- merge(ed, roster, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH,"PER_ENAHO_IND_2007.dta"))

## 2006
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2006_HH_ROSTER_200_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2006_EDU_300_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2006_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## change column names to lowercase
colnames(roster) <- tolower(colnames(roster))
colnames(ed) <- tolower(colnames(ed))
colnames(health) <- tolower(colnames(health))

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]
colnames(ed)[3] <- "año"
roster <- roster[,c("is_female", "age","ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob","codperso" )]
colnames(roster)[3] <- "año"
colnames(roster)[11] <- "factor" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]
colnames(health)[7] <- "año"

ed_roster <- merge(ed, roster, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH,"PER_ENAHO_IND_2006.dta"))

## 2005
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2005_HH_ROSTER_200_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2005_EDU_300_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2005_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## change column names to lowercase
colnames(roster) <- tolower(colnames(roster))
colnames(ed) <- tolower(colnames(ed))
colnames(health) <- tolower(colnames(health))

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]
colnames(ed)[3] <- "año"
roster <- roster[,c("is_female", "age","ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob","codperso" )]
colnames(roster)[3] <- "año"
colnames(roster)[11] <- "factor" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]
colnames(health)[7] <- "año"

ed_roster <- merge(ed, roster, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH,"peru_enaho/PER_ENAHO_IND_2005.dta"))

## 2004
# read in hh roster for average age and share female information
roster <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2004_HH_ROSTER_200_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in ed file for share below post-secondary information
ed <- data.table(read.dta13(file.path(indir, 'PER_NATIONAL_HH_SURVEY_ENAHO_2004_EDU_300_Y2011M03D09.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)
# read in health dataset for visits, private visits, and insurance type information
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2004_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

## change column names to lowercase
colnames(roster) <- tolower(colnames(roster))
colnames(ed) <- tolower(colnames(ed))
colnames(health) <- tolower(colnames(health))

## extract share female and age
roster[, is_female := ifelse(p207 == 2, 1, 0)] 
roster[, age := p208a] 

## extract age and education data
ed[, age := p208a] 
ed[, share_below_post_secondary := ifelse((p301a <7 & age > 25), 1, 
                                          ifelse((p301a >=7 & age > 25), 0, NA))] 

## extract if a person had a visit or a private visit in the past 4 weeks 

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## extract type of health insurance
health[, insurance := ifelse(p4192 == 1 |p4193 == 1 | p4196 == 1 | p4197== 1, "Private", 
                             ifelse(p4198 == 1, "Other", 
                                    ifelse(p4191 == 1 |p4195 == 1 | p4194 == 1, "Public", 
                                           "No coverage"
                                    )))]
health[, private_coverage := ifelse(insurance == "Private", 1, 0) ]
health[, public_coverage := ifelse(insurance == "Public", 1, 0) ]
health[, other_coverage := ifelse(insurance == "Other", 1, 0) ]
health[, no_coverage := ifelse(insurance == "No coverage", 1, 0) ]

# combine ed, roster, and health files
ed <- ed[,c("age", "share_below_post_secondary","ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]
colnames(ed)[3] <- "año"
roster <- roster[,c("is_female", "age","ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "facpob","codperso" )]
colnames(roster)[3] <- "año"
colnames(roster)[11] <- "factor" # these are the same
health <- health[,c("private_coverage", "public_coverage", "other_coverage", "no_coverage", "any_visit","is_private", "ano","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso" )]
colnames(health)[7] <- "año"

ed_roster <- merge(ed, roster, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso", "age"), all = T )
ed_health_roster <- merge(ed_roster, health, by =c("año","mes","conglome","vivienda","hogar","dominio","estrato","ubigeo", "factor","codperso"), all = T )
colnames(ed_health_roster)[1] <- 'year_id'
save.dta13(ed_health_roster, filepath(FILEPATH,"PER_ENAHO_IND_2004.dta"))

