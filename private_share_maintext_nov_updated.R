#####################################################################
## Author: User                                           ##
## Description: Private sector utilization plots for CHE 2020 paper ##
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
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2, gridExtra, RColorBrewer,dplyr,grid, sys)

## directories 
indir <- 'FILEPATH/data/'
outdir <- 'FILEPATH/che20/'

# read in prepped Peru private sector data
visits_19 <- fread("FILEPATH/peru_utilization_2019_updated.csv")
visits_19[,month := factor(visits_19$year_month, levels = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12"), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))]

visits_20 <- fread("FILEPATH/peru_utilization_2020_updated.csv")
visits_20[,month := factor(visits_20$year_month, levels = c("2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))]

visits_19_20 <- rbind(visits_19, visits_20)

visits_19_20[,year_month := factor(visits_19_20$year_month, 
                                             levels = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12",
                                                        "2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"),
                                             labels = c("Jan 2019", "Feb 2019", "Mar 2019", "Apr 2019", "May 2019", "Jun 2019", "Jul 2019", "Aug 2019", 
                                                        "Sept 2019", "Oct 2019", "Nov 2019", "Dec 2019", "Jan 2020", "Feb 2020", "Mar 2020",
                                                        "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sept 2020", "Oct 2020", "Nov 2020", 
                                                        "Dec 2020"))]
visits_19_20$year_month_check <- as.double(visits_19_20$year_month)

visits_19_20$month_check <- as.double(visits_19_20$month)

#read in Mexico utilization data 
indiv_private_2018 <- fread('FILEPATH/mexico_utilization_2018.csv')
indiv_private_2018[,month := factor(indiv_private_2018$year_month, levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
indiv_private_2020 <- fread('FILEPATH/mexico_utilization_2020.csv')
indiv_private_2020[,month := factor(indiv_private_2020$year_month, levels = c("2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))]
indiv_private_18_20 <- rbind(indiv_private_2020, indiv_private_2018)


df_2019 <- data.table(year_month = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12"), 
                        year_id = 2019 )

df_2019[,month := factor(df_2019$year_month, levels = c("2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12"), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))]
indiv_private_18_20_predict <- rbind(df_2019, indiv_private_18_20, fill = T)

indiv_private_18_20[,year_month := factor(indiv_private_18_20$year_month, 
                                          levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12",
                                                     "2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"),
                                          labels = c("Jan 2018", "Feb 2018", "Mar 2018", "Apr 2018", "May 2018", "Jun 2018", "Jul 2018", "Aug 2018", 
                                                     "Sept 2018", "Oct 2018", "Nov 2018", "Dec 2018", "Jan 2020", "Feb 2020", "Mar 2020",
                                                     "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sept 2020", "Oct 2020", "Nov 2020", 
                                                     "Dec 2020"))]
indiv_private_18_20$year_month_check <- as.double(indiv_private_18_20$year_month)

#repeat for predict df
indiv_private_18_20_predict[,year_month := factor(indiv_private_18_20_predict$year_month, 
                                                  levels = c("2018-1", "2018-2", "2018-3", "2018-4", "2018-5" ,"2018-6" ,"2018-7", "2018-8", "2018-9" ,"2018-10" ,"2018-11" ,"2018-12",
                                                             "2019-1", "2019-2", "2019-3", "2019-4", "2019-5" ,"2019-6" ,"2019-7", "2019-8", "2019-9" ,"2019-10" ,"2019-11" ,"2019-12",
                                                             "2020-1", "2020-2", "2020-3", "2020-4", "2020-5" ,"2020-6" ,"2020-7", "2020-8", "2020-9" ,"2020-10" ,"2020-11" ,"2020-12"),
                                                  labels = c("Jan 2018", "Feb 2018", "Mar 2018", "Apr 2018", "May 2018", "Jun 2018", "Jul 2018", "Aug 2018", 
                                                             "Sept 2018", "Oct 2018", "Nov 2018", "Dec 2018","Jan 2019", "Feb 2019", 
                                                             "Mar 2019", "Apr 2019", "May 2019", "Jun 2019", "Jul 2019", "Aug 2019", 
                                                             "Sept 2019", "Oct 2019", "Nov 2019", "Dec 2019", "Jan 2020", "Feb 2020", "Mar 2020",
                                                             "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sept 2020", "Oct 2020", "Nov 2020", 
                                                             "Dec 2020"))]
indiv_private_18_20_predict$year_month_check <- as.double(indiv_private_18_20_predict$year_month)

#make dec na due to missing survey data
indiv_private_18_20 <- indiv_private_18_20 %>%
  mutate(private_share_month = ifelse((year_month_check==12 |year_month_check==24), NA, private_share_month))

#make dec na due to missing survey data (predict df)
indiv_private_18_20_predict <- indiv_private_18_20_predict %>%
  mutate(private_share_month = ifelse((year_month_check==12 |year_month_check==24 | year_month_check==36), NA, private_share_month))

#Extend 2018 line to beginning of 2020 by insurance type and then all insurance
model_18 <- lm(private_share_month ~ year_month_check, data = indiv_private_18_20_predict[year_month_check <13])

# predict 2019 values to compare with estimates
indiv_private_18_20_predict_20 <- copy(indiv_private_18_20_predict)
indiv_private_18_20_predict_20 <- indiv_private_18_20_predict_20 %>%
  mutate(private_share_month = ifelse(year_id==2019, NA, private_share_month))
indiv_private_18_20_predict_20<- indiv_private_18_20_predict_20 %>% 
  mutate(private_share_month = ifelse(is.na(private_share_month), predict(model_18, .), private_share_month))

indiv_private_18_20_predict_20$year_id <- as.factor(indiv_private_18_20_predict_20$year_id)
indiv_private_18_20_predict$year_id <- as.factor(indiv_private_18_20_predict$year_id)

indiv_private_18_20$month_check <- as.double(indiv_private_18_20$month)
indiv_private_18_20$year_id <- as.factor(indiv_private_18_20$year_id)

#PLOT PERU
visits_19_20[, year_month_date := ifelse(year_month == "Jan 2019", as.Date("2019-01-01"),
                                         ifelse(year_month == "Feb 2019", as.Date("2019-02-01"),
                                                ifelse(year_month == "Mar 2019", as.Date("2019-03-01"),
                                                       ifelse(year_month == "Apr 2019", as.Date("2019-04-01"),
                                                              ifelse(year_month == "May 2019", as.Date("2019-05-01"),
                                                                     ifelse(year_month == "Jun 2019", as.Date("2019-06-01"),
                                                                            ifelse(year_month == "Jul 2019", as.Date("2019-07-01"),
                                                                                   ifelse(year_month == "Aug 2019", as.Date("2019-08-01"),
                                                                                          ifelse(year_month == "Sept 2019", as.Date("2019-09-01"),
                                                                                                 ifelse(year_month == "Oct 2019", as.Date("2019-10-01"),
                                                                                                        ifelse(year_month == "Nov 2019", as.Date("2019-11-01"),
                                                                                                               ifelse(year_month == "Dec 2019", as.Date("2019-12-01"),
                                                                                                                     ifelse(year_month == "Jan 2020", as.Date("2020-01-01"),
                                                                                                                            ifelse(year_month == "Feb 2020", as.Date("2020-02-01"),
                                                                                                                                   ifelse(year_month == "Mar 2020", as.Date("2020-03-01"),
                                                                                                                                          ifelse(year_month == "Apr 2020", as.Date("2020-04-01"),
                                                                                                                                                 ifelse(year_month == "May 2020", as.Date("2020-05-01"),
                                                                                                                                                        ifelse(year_month == "Jun 2020", as.Date("2020-06-01"),
                                                                                                                                                               ifelse(year_month == "Jul 2020", as.Date("2020-07-01"),
                                                                                                                                                                      ifelse(year_month == "Aug 2020", as.Date("2020-08-01"),
                                                                                                                                                                             ifelse(year_month == "Sept 2020", as.Date("2020-09-01"),
                                                                                                                                                                                    ifelse(year_month == "Oct 2020", as.Date("2020-10-01"),
                                                                                                                                                                                           ifelse(year_month == "Nov 2020", as.Date("2020-11-01"),
                                                                                                                                                                                                  ifelse(year_month == "Dec 2020", as.Date("2020-12-01"),NA))))))))))))))))))))))))]


visits_19_20<-setDT(visits_19_20)
visits_19_20[, year_month_as_date := ifelse(year_month == "Jan 2019", as.Date("2019-01-01"),NA)]
visits_19_20[, march_binary := ifelse(year_month_check > 15,1,0)]
per_share_private <- lm(private_share_month ~  year_month_check + march_binary + march_binary*year_month_check, data = visits_19_20)

## extract coefficient of april onwards from graph in order to connect lines
model_peru <- lm(private_share_month ~ year_month_check, data = visits_19_20[year_month_check <=15] )
model_peru <- lm(private_share_month ~ year_month_check, data = visits_19_20[year_month_check >15] )

visits_19_20[, intercept :=  0.831457   ]
visits_19_20[, slope := -0.008342    ]

                                         
# Share visits in private sector, all insurance types, Peru, not monthas.Date()# Share visits in private sector, all insurance types, Peru, not month overlaid
visits_19_20$year_id <- factor(visits_19_20$year_id, levels = c(2019, 2020))
private_share_all_insurance_peru_not_overlaid <- ggplot(data =  visits_19_20)+
  scale_color_manual(values = c("#1b9e77", "#d95f02"))+
  geom_point(aes(x = year_month_check, y = private_share_month, color = year_id, group = 1), size = 2) +
  geom_line(aes(x = year_month_check, y = private_share_month, color = year_id, group = 1), size = 1.5) +
   geom_smooth(data = visits_19_20[year_month_check <=15],aes(x = year_month_check, y = private_share_month),linetype="solid", method = "lm", size = 1.5,se = FALSE,color = "black") +
  geom_smooth(data = visits_19_20[year_month_check >15], aes(x = year_month_check, y = private_share_month),linetype = "solid",method = "lm", size = 1.5,se = FALSE,color = "black") +
  geom_vline(xintercept = 15.5, linetype = "dotdash")+
  theme_bw()+
  scale_x_continuous(breaks = seq(min(visits_19_20$year_month_check), max(visits_19_20$year_month_check), by = 2),
                     labels = c("Jan 2019","Mar 2019", "May 2019", "Jul 2019", "Sep 2019", "Nov 2019", "Jan 2020",
                                "Mar 2020", "May 2020", "Jul 2020", "Sep 2020", "Nov 2020"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0.4,0.7), breaks = c(0.40,0.45,0.50,0.55,0.60,0.65,0.70)) +
  guides(color = "none")+
  labs(color = "Year", y = 'Share of visits in private sector', x = 'Month', title = "Peru ENAHO") +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(size = 14, hjust = 0.5),
        text = element_text(size=18, color= "black"), 
        axis.text.x =  element_text(angle = 45, vjust =1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black"),
        legend.position = "right",
        legend.key.width=unit(5, 'lines'))

# PLOT MEXICO


brewer.pal(n=6,"Spectral")
indiv_private_18_20_private_share_predict_20 <- setDT(indiv_private_18_20_predict_20)
indiv_private_18_20_private_share_predict_20[, predicted := ifelse(year_id == 2019, "predicted", "not predicted")]
# Share visits in private sector, all insurance types, Mexico, months not overlaid,with prediction for 2019
indiv_private_18_20_private_share_predict_20$year_id <- as.integer(indiv_private_18_20_private_share_predict_20$year_id)

colors <- c("2020" = "#d95f02", "2019" = "#1b9e77", "2018" = "#7570b3", "Fitted" = "Black")
line_type <- c("Predicted" = "londash", "Observed" = "solid","Observed" = "solid")
indiv_private_18_20_private_share_predict_20[, category := ifelse(year_month_check >11 & year_month_check <25, "Predicted", "Observed")]
private_share_all_insurance_mexico_not_overlaid_legend<- ggplot(data = indiv_private_18_20_private_share_predict_20, aes(x = year_month_check, y = private_share_month))+
  geom_line(data = indiv_private_18_20_private_share_predict_20[year_month_check <13], aes(x = year_month_check, y = private_share_month, color = "2018", linetype=category),size = 1.5) +
  geom_line(data = indiv_private_18_20_private_share_predict_20[year_month_check >11 & year_month_check <25], aes(x = year_month_check, y = private_share_month, color = "2019", linetype=category),size = 1.5) +
  geom_line(data = indiv_private_18_20_private_share_predict_20[year_month_check >23], aes(x = year_month_check, y = private_share_month,color = "2020", linetype=category),size = 1.5) +
  scale_color_manual(values = colors)+
  geom_smooth(data = indiv_private_18_20_private_share_predict_20[year_month_check %in% seq(1,12)],aes(x = year_month_check, y = private_share_month, color = "Fitted"), size = 1.5,method = "lm", se = FALSE) +
  geom_vline(xintercept = 27.5, linetype = "dotdash")+
  scale_x_continuous(breaks = seq(min(indiv_private_18_20_private_share_predict_20$year_month_check), max(indiv_private_18_20_private_share_predict_20$year_month_check), by = 2),
                     labels = c("Jan 2018","Mar 2018", "May 2018", "Jul 2018", "Sep 2018", "Nov 2018", "Jan 2019",
                                "Mar 2019", "May 2019", "Jul 2019", "Sep 2019", "Nov 2019", "Jan 2020",
                               "Mar 2020", "May 2020", "Jul 2020", "Sep 2020", "Nov 2020"))+
  scale_linetype_manual(values = c("solid", "longdash" ),labels = c("Observed", "Predicted from 2018 values"))+
  theme_bw()+
  scale_y_continuous(labels = scales::percent, limits = c(0.4,0.72), breaks = c(0.40,0.45,0.50,0.55,0.60,0.65,0.70)) +
  labs(color = "Year",  y = 'Share of visits in private sector', x = 'Month',linetype = "Data type") +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size=18),
        axis.text.x =  element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y =  element_text( color = "black"),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.key.width=unit(5, 'lines'))

## extract coefficient of april onwards from graph in order to connect lines
model_mexico <- lm(private_share_month ~ year_month_check, data = indiv_private_18_20_private_share_predict_20[year_month_check >27] )

indiv_private_18_20_private_share_predict_20[, intercept :=   0.592951  ]
indiv_private_18_20_private_share_predict_20[, slope :=0.001844    ]

#make dec na due to missing survey data
indiv_private_18_20_private_share_predict_20 <- indiv_private_18_20_private_share_predict_20 %>%
  mutate(private_share_month = ifelse((year_month_check==12  | year_month_check == 36), NA, private_share_month))


private_share_all_insurance_mexico_not_overlaid<- ggplot(data = indiv_private_18_20_private_share_predict_20, aes(x = year_month_check, y = private_share_month))+
  geom_point(data = indiv_private_18_20_private_share_predict_20[year_id != 2], aes(x = year_month_check, y = private_share_month, color = as.character(year_id)),size = 2) +
  geom_line(data = indiv_private_18_20_private_share_predict_20[year_month_check <13], aes(x = year_month_check, y = private_share_month, color = as.character(year_id)), size = 1.5) +
  geom_line(data = indiv_private_18_20_private_share_predict_20[year_month_check >11 & year_month_check <25], aes(x = year_month_check, y = private_share_month, color = as.character(year_id)), linetype="longdash",size = 1.5) +
  geom_line(data = indiv_private_18_20_private_share_predict_20[year_month_check >23], aes(x = year_month_check, y = private_share_month, color = as.character(year_id)), linetype="solid",size = 1.5) +
   scale_color_manual(values = c("#7570b3","#1b9e77", "#d95f02"))+
  geom_smooth(data = indiv_private_18_20_private_share_predict_20[year_month_check %in% seq(1,12)],aes(x = year_month_check, y = private_share_month), size = 1.5,method = "lm", se = FALSE, color = "black", linetype = "solid") +
  geom_smooth(data = indiv_private_18_20_private_share_predict_20[year_month_check >27], aes(x = year_month_check, y = private_share_month), size = 1.5, linetype = "solid",method = "lm", se = FALSE,color = "black") +
  geom_vline(xintercept = 27.5, linetype = "dotdash")+
  geom_segment(aes(x = 25, xend = 27, y = 0.5512617, yend = 0.5512617 + 0.01210237), size =1.5,color = "black",linetype = "solid" ) +
  scale_x_continuous(breaks = seq(min(indiv_private_18_20_private_share_predict_20$year_month_check), max(indiv_private_18_20_private_share_predict_20$year_month_check), by = 2),
                     labels = c("Jan 2018","Mar 2018", "May 2018", "Jul 2018", "Sep 2018", "Nov 2018", "Jan 2019",
                                "Mar 2019", "May 2019", "Jul 2019", "Sep 2019", "Nov 2019", "Jan 2020",
                                "Mar 2020", "May 2020", "Jul 2020", "Sep 2020", "Nov 2020"))+
  scale_linetype_manual(values = c("solid", "solid"),labels = c("2019, predicted from 2018 values", "2018 and 2020, observed"))+
  theme_bw()+
  guides(linetype = "none")+
  guides(color = "none")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0.4,0.7), breaks = c(0.40,0.45,0.50,0.55,0.60,0.65,0.70)) +
  labs(color = "Year",  y = 'Share of visits in private sector', x = 'Month',linetype = "Year", title = "Mexico ENIGH") +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
       # panel.grid = element_blank(),
        text = element_text(size=18),
        axis.text.x =  element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y =  element_text( color = "black"),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.key.width=unit(5, 'lines'))



## Combine corresponding Peru and Mexico plots into panel ##

# Share visits in private sector, all insurance, months not overlaid, with prediction for 2019 for Mexico 
legend <- cowplot::get_legend(private_share_all_insurance_mexico_not_overlaid_legend)
grid.newpage()
grid.draw(legend)
pdf(file.path(outdir, "private_share_all_insurance_not_overlaid_nov_2022.pdf" ), width = 25, height = 10)
private_share_all_insurance_not_overlaid <- grid.arrange(private_share_all_insurance_mexico_not_overlaid,private_share_all_insurance_peru_not_overlaid,legend, nrow =1, 
                                                         top = textGrob("Share of visits in the private sector",gp=gpar(fontsize=22),x = 0.04, hjust = 0),widths=c(3,3,1))
print(private_share_all_insurance_not_overlaid)
dev.off()


