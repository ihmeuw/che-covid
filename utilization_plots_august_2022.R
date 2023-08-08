#####################################################################
## Author: User                                           ##
## Description: Utilization plots for CHE 2020 paper ##
##                                                                  ##
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
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2, gridExtra, dplyr,grid)

## directories 
indir <- 'FILEPATH/'
outdir <- 'FILEPATH'

#read in Mexico utilization data 
indiv_private_2018 <- fread('FILEPATH/mexico_utilization_2018.csv')
indiv_private_2018[,month := factor(indiv_private_2018$year_month, levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
indiv_private_2020 <- fread('FILEPATH/mexico_utilization_2020.csv')
indiv_private_2020[,month := factor(indiv_private_2020$year_month, levels = c("2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
indiv_private_18_20 <- rbind(indiv_private_2020, indiv_private_2018)

indiv_private_18_20[,year_month := factor(indiv_private_18_20$year_month, 
                                          levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12",
                                                     "2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"),
                                          labels = c("January 2018", "February 2018", "March 2018", "April 2018", "May 2018", "June 2018", "July 2018", "August 2018", 
                                                     "September 2018", "October 2018", "November 2018", "December 2018", "January 2020", "February 2020", "March 2020",
                                                     "April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020", "October 2020", "November 2020", 
                                                     "December 2020"))]
indiv_private_18_20$year_month_check <- as.double(indiv_private_18_20$year_month)

#make oct -dec na due to missing survey data
indiv_private_18_20 <- indiv_private_18_20 %>%
  mutate(any_visit_month = ifelse((year_month_check %in% c(10,11,12) |year_month_check %in% c(22,23,24) ), NA, any_visit_month))
indiv_private_18_20$month_check <- as.double(indiv_private_18_20$month)
indiv_private_18_20$year <- as.factor(indiv_private_18_20$year_id)


#read in prepped peru data
visits_19 <- fread("FILEPATH/peru_utilization_2019_updated.csv")
visits_19[,month := factor(visits_19$year_month, levels = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
visits_20 <- fread("FILEPATH/peru_utilization_2020_updated.csv")
visits_20[,month := factor(visits_20$year_month, levels = c("2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
visits_19_20 <- rbind(visits_19, visits_20)
visits_19_20[,year_month := factor(visits_19_20$year_month, 
                                   levels = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12",
                                              "2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"),
                                   labels = c("January 2019", "February 2019", "March 2019", "April 2019", "May 2019", "June 2019", "July 2019", "August 2019", 
                                              "September 2019", "October 2019", "November 2019", "December 2019", "January 2020", "February 2020", "March 2020",
                                              "April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020", "October 2020", "November 2020", 
                                              "December 2020"))]
visits_19_20$year_month_check <- as.double(visits_19_20$year_month)
visits_19_20$month_check <- as.double(visits_19_20$month)
visits_19_20$year <- as.factor(visits_19_20$year_id)


## convert to difference space 
# Mexico
indiv_private_18_20_diff <- indiv_private_18_20[, -c("private_visit_month", "private_share_month", "V1")]
indiv_private_18_20_diff <- indiv_private_18_20_diff[order(month,year)]
indiv_private_18_20_diff[, any_visit_month_diff := any_visit_month - lag(any_visit_month), by ="month"]

#Peru

visits_19_20_diff <- visits_19_20[, -c("private_visit_month", "private_share_month", "V1")]
visits_19_20_diff <- visits_19_20_diff[order(month,year)]
visits_19_20_diff[, any_visit_month_diff := any_visit_month - lag(any_visit_month), by ="month"]

## convert to difference relative space 
# Mexico
indiv_private_18_20_diff[, any_visit_month_diff_relative := (any_visit_month / lag(any_visit_month))-1, by ="month"]

#Peru
visits_19_20_diff[, any_visit_month_diff_relative  := (any_visit_month /lag(any_visit_month))-1, by ="month"]



# make facet wrap instead of grid arrange so that can add panel labels
combo_df_peru <- visits_19_20_diff %>% 
  mutate(ihme_loc_id = "PER")

combo_df_mex <- indiv_private_18_20_diff
combo_df_mex <- combo_df_mex %>% 
  mutate(ihme_loc_id = "MEX")
combo_df <- rbind(combo_df_mex[,-c(1)], combo_df_peru[,-c(1)])

combo_df$ihme_loc_id <- factor(combo_df$ihme_loc_id, levels = c("MEX", "PER"), labels = c("Mexico ENIGH 2020 vs. 2018", "Peru ENAHO 2020 vs. 2019"))


## extract coefficient of april onwards from graph in order to connect lines
model_peru <- lm(any_visit_month_diff_relative ~ month_check, data = combo_df[month_check >3 & ihme_loc_id == "Peru ENAHO 2020 vs. 2019"])
model_mex <- lm(any_visit_month_diff_relative ~ month_check, data = combo_df[month_check>3 & ihme_loc_id == "Mexico ENIGH 2020 vs. 2018"])

combo_df[, intercept := ifelse(ihme_loc_id == "Peru ENAHO 2019 and 2020", -0.75003,  -0.0907762 )]
combo_df[, slope := ifelse(ihme_loc_id == "Peru ENAHO 2019 and 2020", 0.03461, -0.0008125   )]

# create vector for manual legend
colors <- c("Observed" = "black", "Fitted" = "#e7298a")

total_visits_share_indiv_month_overlaid_difference_relative  <- ggplot(data = combo_df)+
  facet_wrap(~ihme_loc_id, scales = 'free', ncol =1)+
  geom_point(aes(x = month_check, y = any_visit_month_diff_relative, group = year, color = "Observed"), size = 2) +
  geom_line(aes(x = month_check, y = any_visit_month_diff_relative, group = year, color = "Observed"), size = 1.5) +
  scale_linetype_manual(values=c("solid", "longdash"))+
  scale_x_continuous(breaks = c(seq(1,12)), labels = c("January", "February", "March", "April", "May", "June", "July", "August", 
                                                     "September", "October", "November", "December"))+
  geom_smooth(data = combo_df[month_check %in% c(seq(1,3))], aes(x = month_check, y = any_visit_month_diff_relative,color = "Fitted"), linetype = "solid",method = "lm", se = FALSE,  size =1.5  ) +
  geom_smooth(data = combo_df[month_check %in% c(seq(4,12))], aes(x = month_check, y = any_visit_month_diff_relative,color = "Fitted"), linetype = "solid",method = "lm", se = FALSE,  size =1.5  ) +
  geom_vline(xintercept = 3.5, linetype = "dotdash")+
  theme_bw()+
  scale_y_continuous(labels = scales::percent) +
  labs(linetype = 'Year', title = 'Percent difference in proportion of people who had a \n visit by month', y = 'Percent difference in proportion of people who had a visit', x = 'Month',
       color = "Data type") +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank(),
        text = element_text(size=22, color= "black"), 
        axis.text.x =  element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black"),
        legend.key.width=unit(5, 'lines'))+
  scale_color_manual(values = colors)

pdf(file.path(outdir, "total_visits_month_overlaid_share_indiv_difference_relative_CB_regression_update_nov_2022.pdf" ), width = 10, height = 10)
print(total_visits_share_indiv_month_overlaid_difference_relative)
dev.off()



