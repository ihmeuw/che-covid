################################################################################
## Author: USER                                                               ##
## Date: 10/27/21                                                             ##
## Purpose: Extract visit level information from Peru                         ##
################################################################################
## clear memory
rm(list=ls())
library(vctrs, lib.loc='FILEPATH')

## runtime configuration
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/home/j'
  h <- file.path('/homes', Sys.getenv('USER'))
} else {
  j <- 'J:/'
  h <- 'H:/'
}

## libraries
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, dplyr)

## directories
indir <-  file.path("FILEPATH")
outdir <-  file.path("FILEPATH")

## 2020 
## read dataset
health <- data.table(read.dta13(file.path(indir, 'PER_ENAHO_2020_ANNUAL_MODULE_04_ENAHO01A_2020_400_Y2021M07D23.DTA'), convert.factors=FALSE), stringsAsFactors = FALSE)

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))

health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]

#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4034, p4035, p4036)
#Group 2 (private): consultorio médico particular, clínica particular, (p4033, p4038,p4039)
#Group 3 (other): FFAA/policia nacional, pharmacy/drugstore, at home, other (p4037,p40310, p40311,p40313)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## private pharmacy visit
health[, is_private_pharmacy := ifelse(p40310 ==1, 1, 0) ]
health[, is_private_pharmacy := ifelse(is.na(is_private_pharmacy), 0, is_private_pharmacy) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "is_private_pharmacy", "nconglome", "is_public", "estrato", "factor07", "year_id", "mes", "year_month")], filepath(FILEPATH,"PER_ENAHO_VISITS_2020_updated.csv"))

## 2019
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2019_01A_400_Y2020M07D27.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]

#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4034, p4035, p4036)
#Group 2 (private): consultorio médico particular, clínica particular, (p4033, p4038,p4039)
#Group 3 (other): FFAA/policia nacional, pharmacy/drugstore, at home, other (p4037,p40310, p40311,p40313)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## private pharmacy visit
health[, is_private_pharmacy := ifelse(p40310 ==1, 1, 0) ]
health[, is_private_pharmacy := ifelse(is.na(is_private_pharmacy), 0, is_private_pharmacy) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private","is_public", "is_private_pharmacy", "nconglome", "estrato", "factor07", "year_id","mes", "year_month")], filpath(FILEPATH,"PER_ENAHO_VISITS_2019_updated.csv"))

## 2018
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2018_400_Y2019M06D19.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]

#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4034, p4035, p4036)
#Group 2 (private): consultorio médico particular, clínica particular, (p4033, p4038,p4039)
#Group 3 (other): FFAA/policia nacional, pharmacy/drugstore, at home, other (p4037,p40310, p40311,p40313)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "is_public", "nconglome", "estrato", "factor07", "year_id","mes", "year_month")], filpath(FILEPATH,"PER_ENAHO_VISITS_2018_updated.csv"))

## 2017 (only one quarter available)                                                  

## 2016
## read dataset
health <- data.table(read.dta13(file.path(indir, "ANNUAL/PER_NATIONAL_HH_SURVEY_ENAHO_2016_HEALTH_400_Y2017M09D08.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]

#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4034, p4035, p4036)
#Group 2 (private): consultorio médico particular, clínica particular, (p4033, p4038,p4039)
#Group 3 (other): FFAA/policia nacional, pharmacy/drugstore, at home, other (p4037,p40310, p40311,p40313)

### any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]


health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private","is_public", "nconglome", "estrato", "factor07", "year_id")], filepath(FILEPATH,"PER_ENAHO_VISITS_2016_updated.csv"))

## 2015
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2015_ANNUAL_HEALTH_400_Y2016M09D19.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]

#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4034, p4035, p4036)
#Group 2 (private): consultorio médico particular, clínica particular, (p4033, p4038,p4039)
#Group 3 (other): FFAA/policia nacional, pharmacy/drugstore, at home, other (p4037,p40310, p40311,p40313)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "is_public", "conglome", "estrato", "factor07", "year_id")], filepath(FILEPATH,"PER_ENAHO_VISITS_2015_updated.csv"))

## 2014
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2014_ANNUAL_HEALTH_400_Y2016M09D19.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]

#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]


health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "is_public", "conglome", "estrato", "factor07", "year_id")], filepath(FILEPATH,"PER_ENAHO_VISITS_2014_updated.csv"))

## 2012
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2012_ANNUAL_324_MOD_04_01A_400_Y2013M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]


#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "is_public", "conglome", "estrato", "factor07", "year_id")], filepath(FILEPATH,"PER_ENAHO_VISITS_2012_updated.csv"))

## 2011
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_ENAHO_2011_ANNUAL_291_MOD_04_01A_400_Y2013M07D12_Y2013M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]


#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "is_public", "conglome", "estrato", "factor07", "year_id")], filepath(FILEPATH,"PER_ENAHO_VISITS_2011_updated.csv"))

## 2010
## read dataset
health <- data.table(read.dta13(file.path(indir, "2010_ANNUAL/PER_NATIONAL_HH_SURVEY_ENAHO_2010_HEALTH_400_Y2011M07D12.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
colnames(health) <- tolower(colnames(health))
colnames(health)[1] <- "aÑo"

  
#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]


#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "conglome", "is_public", "estrato", "factor07", "year_id")], filepath(FILEPATH,"PER_ENAHO_VISITS_2010_updated.csv"))

## 2009
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2009_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
colnames(health) <- tolower(colnames(health))
colnames(health)[1] <- "aÑo"

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]


#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private","is_public", "conglome", "estrato", "factor", "year_id")], filepath(FILEPATH,"PER_ENAHO_VISITS_2009_updated.csv"))

## 2008
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2008_ANNUAL_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
colnames(health) <- tolower(colnames(health))
colnames(health)[1] <- "aÑo"

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]


#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)
## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "is_public", "conglome", "estrato", "factor", "year_id")], filepath(FILEPATH,"PER_ENAHO_VISITS_2008_updated.csv"))

## 2007
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2007_ANNUAL_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
colnames(health) <- tolower(colnames(health))
colnames(health)[1] <- "aÑo"

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]


#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "is_public", "conglome", "estrato", "factor", "year_id")], filepath(FILEPATH,"PER_ENAHO_VISITS_2007_updated.csv"))

## 2006
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2006_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
colnames(health) <- tolower(colnames(health))
colnames(health)[1] <- "aÑo"

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]

#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]


health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private", "is_public", "conglome", "estrato", "factor", "year_id", "year_month", "mes")], filepath(FILEPATH, "PER_ENAHO_VISITS_2006_updated.csv"))

## 2005
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2005_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
colnames(health) <- tolower(colnames(health))
colnames(health)[1] <- "aÑo"

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]


#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]


health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private",  "is_public", "conglome", "estrato", "factor", "year_id", "year_month", "mes")], filepath(FILEPATH,"PER_ENAHO_VISITS_2005_updated.csv"))

## 2004
## read dataset
health <- data.table(read.dta13(file.path(indir, "PER_NATIONAL_HH_SURVEY_ENAHO_2004_HEALTH_400_Y2011M03D09.DTA"), convert.factors=FALSE), stringsAsFactors = FALSE)
colnames(health) <- tolower(colnames(health))
colnames(health)[1] <- "aÑo"

#combine month and year 
health[, mes := as.numeric(mes)]
health[, aÑo := as.numeric(aÑo)]

health <- health %>%
  filter(!is.na(mes))%>%
  filter(!is.na(aÑo))
health <- setDT(health)
health[, year_month := paste0(aÑo, "-", mes)]

#public/private site of care variables: p4031- p40314
#Group 1 (public): CLAS, MINSA,MINSA, ESSALUD, MINSA, ESSALUD, (p4031, p4032, p4033, p4034, p4035, p4036,p4037)
#Group 2 (private): consultorio médico particular, clínica particular, (p4038,p4039,p40310)

## any visit
health[, any_visit := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1 | p4038 == 1 | p4039 ==1| p40310 ==1, 1, 0) ]
health[, any_visit := ifelse(is.na(any_visit), 0, any_visit) ]

## private visit
health[, is_private := ifelse(p4038 == 1 | p4039 == 1| p40310 ==1, 1, 0) ]
health[, is_private := ifelse(is.na(is_private), 0, is_private) ]

## public visit
health[, is_public := ifelse(p4031 == 1 | p4032 == 1| p4033 ==1| p4034 == 1| p4035 ==1| p4036 == 1 |  p4037 == 1, 1, 0) ]
health[, is_public := ifelse(is.na(is_public), 0, is_public) ]

health[, year_id := aÑo]

write.csv(health[, c("any_visit", "is_private","is_public", "conglome", "estrato", "factor", "year_id", "year_month", "mes")], filepath(FILEPATH,"PER_ENAHO_VISITS_2004_updated.csv"))
