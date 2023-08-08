#####################################################################
## Author: User                                    ##
## Description: Prep Mexico data for utilization analyses          ##
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
################################################################################
## MEXICO ENIGH 2018                                                         ##
################################################################################
## directories

## directories
indir <-  'FILEPATH'
outdir <- 'FILEPATH'

## read 2018 dataset
visits <-  fread(file.path(indir, "MEX_ENIGH_VISITS_2018.csv"))

visits[, prob_mes := as.numeric(prob_mes)]
visits[, prob_anio := as.numeric(prob_anio)]

visits <- visits %>%
  filter(!is.na(prob_mes))%>%
  filter(!is.na(prob_anio))
visits <- visits[prob_anio == 2018]


any_visit_month <- visits[, .(any_visit_month = mean(any_visit)), by = "prob_mes"]
private_visit_month <- visits[, .(private_visit_month = mean(is_private)), by = "prob_mes"]
private_pharmacy_visit_month <- visits[, .(private_pharmacy_visit_month = mean(is_private_pharmacy)), by = "prob_mes"]
public_visit_month <- visits[, .(public_visit_month = mean(is_public)), by = "prob_mes"]

# calculate private share of visits
had_visit <- visits[any_visit == 1]
private_share_month <- had_visit[, .(private_share_month = mean(is_private)), by = "prob_mes" ]
private_pharm_share_month <- had_visit[, .(private_pharm_share_month = mean(is_private_pharmacy)), by = "prob_mes" ]

## calculate private share of visits, excluding pharmacy
had_visit[, is_private_not_pharm := ifelse(is_private & !is_private_pharmacy, 1, 0)]
private_not_pharm_share_month <- had_visit[, .(private_not_pharm_share_month = mean(is_private_not_pharm)), by = "prob_mes" ]

# merge
visit_counts <- merge(any_visit_month, private_visit_month, by = "prob_mes")
visit_counts <- merge(visit_counts, private_share_month, by = "prob_mes")
visit_counts <- merge(visit_counts, public_visit_month, by = "prob_mes")
visit_counts <- merge(visit_counts, private_pharmacy_visit_month, by = "prob_mes")
visit_counts <- merge(visit_counts, private_pharm_share_month, by = "prob_mes")
visit_counts <- merge(visit_counts, private_not_pharm_share_month, by = "prob_mes")

visit_counts[, month := prob_mes]
visit_counts[, year_id := 2018]
visit_counts[, year_month := paste0(year_id, "-", month)]


write.csv(visit_counts, file.path(outdir, 'mexico_utilization_2018.csv'))


################################################################################
## MEXICO ENIGH 2020                                                          ##
################################################################################
## read 2020 dataset
visits <-  fread(file.path(indir, "MEX_ENIGH_VISITS_2020.csv"))
visits[, prob_mes := as.numeric(prob_mes)]
visits[, prob_anio := as.numeric(prob_anio)]

visits <- visits %>%
  filter(!is.na(prob_mes))%>%
  filter(!is.na(prob_anio))
visits <- visits[prob_anio == 2020]


any_visit_month <- visits[, .(any_visit_month = mean(any_visit)), by = "prob_mes"]
private_visit_month <- visits[, .(private_visit_month = mean(is_private)), by = "prob_mes"]
private_pharmacy_visit_month <- visits[, .(private_pharmacy_visit_month = mean(is_private_pharmacy)), by = "prob_mes"]
public_visit_month <- visits[, .(public_visit_month = mean(is_public)), by = "prob_mes"]

# calculate private share of visits
had_visit <- visits[any_visit == 1]
private_share_month <- had_visit[, .(private_share_month = mean(is_private)), by = "prob_mes" ]
private_pharm_share_month <- had_visit[, .(private_pharm_share_month = mean(is_private_pharmacy)), by = "prob_mes" ]

## calculate private share of visits, excluding pharmacy
had_visit[, is_private_not_pharm := ifelse(is_private & !is_private_pharmacy, 1, 0)]
private_not_pharm_share_month <- had_visit[, .(private_not_pharm_share_month = mean(is_private_not_pharm)), by = "prob_mes" ]

#merge
visit_counts <- merge(any_visit_month, private_visit_month, by = "prob_mes")
visit_counts <- merge(visit_counts, private_share_month, by = "prob_mes")
visit_counts <- merge(visit_counts, public_visit_month, by = "prob_mes")
visit_counts <- merge(visit_counts, private_pharmacy_visit_month, by = "prob_mes")
visit_counts <- merge(visit_counts, private_pharm_share_month, by = "prob_mes")
visit_counts <- merge(visit_counts, private_not_pharm_share_month, by = "prob_mes")

visit_counts[, month := prob_mes]
visit_counts[, year_id := 2020]
visit_counts[, year_month := paste0(year_id, "-", month)]


write.csv(visit_counts, file.path(outdir, 'mexico_utilization_2020.csv'))
