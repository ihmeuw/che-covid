#####################################################################
## Author: User                                  ##
## Description: Prep Peru data for utilization analyses            ##
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
## PERU ENAHO 2019                                                        ##
################################################################################
## directory 
dir <- 'FILEPATH/peru_enaho/'

## read datasets
visits <- fread(file.path(dir, "PER_ENAHO_VISITS_2019_updated.csv"))

# group by month 
any_visit_month <- visits[, .(any_visit_month = mean(any_visit)), by = "mes"]
private_visit_month <- visits[, .(private_visit_month = mean(is_private)), by = "mes"]
private_visit_pharmacy_month <- visits[, .(private_pharmacy_visit_month = mean(is_private_pharmacy)), by = "mes"]
public_visit_month <- visits[, .(public_visit_month = mean(is_public)), by = "mes"]


# calculate private share of visits
had_visit <- visits[any_visit == 1]
private_share_month <- had_visit[, .(private_share_month = mean(is_private)), by = "mes" ]

# calculate private pharmacy share of visits
private_pharm_share_month <- had_visit[, .(private_pharm_share_month = mean(is_private_pharmacy)), by = "mes" ]

## calculate private share of visits, excluding pharmacy
had_visit[, is_private_not_pharm := ifelse(is_private & !is_private_pharmacy, 1, 0)]
private_not_pharm_share_month <- had_visit[, .(private_not_pharm_share_month = mean(is_private_not_pharm)), by = "mes" ]


# merge
visit_counts <- merge(any_visit_month, private_visit_month, by = "mes")
visit_counts <- merge(visit_counts, private_share_month, by = "mes")
visit_counts <- merge(visit_counts, public_visit_month, by = "mes")
visit_counts <- merge(visit_counts, private_visit_pharmacy_month, by = "mes")
visit_counts <- merge(visit_counts, private_pharm_share_month, by = "mes")
visit_counts <- merge(visit_counts, private_not_pharm_share_month, by = "mes")

visit_counts[, month := mes]
visit_counts[, year_id := 2019]
visit_counts[, year_month := paste0(year_id, "-", month)]

write.csv(visit_counts, "FILEPATH/peru_utilization_2019_updated.csv")

################################################################################
## PERU ENAHO 2020                                                        ##
################################################################################
## directory 
dir <- 'FILEPATH/'

## read datasets
visits <- fread(file.path(dir, "PER_ENAHO_VISITS_2020_updated.csv"))

# group by month 
any_visit_month <- visits[, .(any_visit_month = mean(any_visit)), by = "mes"]
private_visit_month <- visits[, .(private_visit_month = mean(is_private)), by = "mes"]
private_visit_pharmacy_month <- visits[, .(private_pharmacy_visit_month = mean(is_private_pharmacy)), by = "mes"]
public_visit_month <- visits[, .(public_visit_month = mean(is_public)), by = "mes"]

# calculate private share of visits
had_visit <- visits[any_visit == 1]
private_share_month <- had_visit[, .(private_share_month = mean(is_private)), by = "mes" ]

# calculate private pharmacy share of visits
private_pharm_share_month <- had_visit[, .(private_pharm_share_month = mean(is_private_pharmacy)), by = "mes" ]

## calculate private share of visits, excluding pharmacy
had_visit[, is_private_not_pharm := ifelse(is_private & !is_private_pharmacy, 1, 0)]
private_not_pharm_share_month <- had_visit[, .(private_not_pharm_share_month = mean(is_private_not_pharm)), by = "mes" ]

# merge

visit_counts <- merge(any_visit_month, private_visit_month, by = "mes")
visit_counts <- merge(visit_counts, private_share_month, by = "mes")
visit_counts <- merge(visit_counts, public_visit_month, by = "mes")
visit_counts <- merge(visit_counts, private_visit_pharmacy_month, by = "mes")
visit_counts <- merge(visit_counts, private_pharm_share_month, by = "mes")
visit_counts <- merge(visit_counts, private_not_pharm_share_month, by = "mes")


visit_counts[, month := mes]
visit_counts[, year_id := 2020]
visit_counts[, year_month := paste0(year_id, "-", month)]

write.csv(visit_counts, "FILEPATH/peru_utilization_2020_updated.csv")
