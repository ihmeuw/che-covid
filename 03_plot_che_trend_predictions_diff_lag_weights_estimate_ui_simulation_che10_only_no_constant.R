#####################################################################
## Author: User                                           ##
## Description: Figure 1 che trend plots: predicts from simulation: CHE 10 only, ensemble with constant and no constant  ##
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
pacman::p_load(data.table, openxlsx, haven, readstata13, readxl, ggplot2, gridExtra, dplyr, grid)

## directories 
indir <- 'FILEPATH'
outdir <- 'FILEPATH'

adjust_factor <- 10000

## read in prepped data
che10_5years_plot_df <- fread(file.path(indir, 'che10_plot_df_sep_2022_model2.csv'))[ihme_loc_id !="MEX"]
che25_5years_plot_df <- fread(file.path(indir, 'che25_plot_df_sep_2022_model2.csv'))[ihme_loc_id !="MEX"]

## filter out 2004 from Mexico
che10_5years_plot_df_mex <- fread(file.path(indir, 'che10_plot_df_sep_2022_model2.csv'))[ihme_loc_id == "MEX" & year_id > 2004]
che25_5years_plot_df_mex <- fread(file.path(indir, 'che25_plot_df_sep_2022_model2.csv'))[ihme_loc_id == "MEX" & year_id > 2004]

# combine 
che10_5years_plot_df <- rbind(che10_5years_plot_df, che10_5years_plot_df_mex)
che25_5years_plot_df <- rbind(che25_5years_plot_df, che25_5years_plot_df_mex)

## read in simulation predictions
che10_sim_predictions <- fread('FILEPATH/che10_predict_stratified_unweighted_median_no_constant.csv')

che25_sim_predictions <- fread('FILEPATH/che25_predict_stratified_unweighted_median_no_constant.csv')


#divide by 10000
 adjust_cols <- c("lower", "upper", "che10","che10_pct")
 che10_sim_predictions[, (adjust_cols) := lapply(.SD,  function(x) x/adjust_factor), .SDcols = adjust_cols]
# 
 adjust_cols <- c("lower", "upper", "che25","che25_pct")
 che25_sim_predictions[, (adjust_cols) := lapply(.SD,  function(x) x/adjust_factor), .SDcols = adjust_cols]
 
 #divide by 10000
 adjust_cols <- c("che10","lag_che")
 che10_draws[, (adjust_cols) := lapply(.SD,  function(x) x/adjust_factor), .SDcols = adjust_cols]
 # 
 adjust_cols <- c("che25","lag_che")
 che25_draws[, (adjust_cols) := lapply(.SD,  function(x) x/adjust_factor), .SDcols = adjust_cols]
 
## filter out old predictions
che10_5years_plot_df_estimate <- che10_5years_plot_df[year_id == 2020 & predict != "Prediction"]
che10_5years_plot_df_predictions_old <- che10_5years_plot_df[year_id != 2020]

che10_5years_plot_df <- rbind(che10_5years_plot_df_predictions_old, che10_5years_plot_df_estimate)
che10_5years_plot_df <- rbind(che10_5years_plot_df, che10_sim_predictions)

che25_5years_plot_df_estimate <- che25_5years_plot_df[year_id == 2020 & predict != "Prediction"]
che25_5years_plot_df_predictions_old <- che25_5years_plot_df[year_id != 2020]

che25_5years_plot_df <- rbind(che25_5years_plot_df_predictions_old, che25_5years_plot_df_estimate)
che25_5years_plot_df <- rbind(che25_5years_plot_df, che25_sim_predictions)

colnames(che10_5years_plot_df)[4:5] <- c("upper_10", "lower_10")
colnames(che25_5years_plot_df)[4:5] <- c("upper_25", "lower_25")

## read in data based UI
estimate_ui <-fread('/FILEPATH/che5years_plot_df_estimates_ui_model2.csv')

che5years_plot_df <- merge(che25_5years_plot_df, che10_5years_plot_df, by = c("year_id", "ihme_loc_id", "predict"))

che5years_plot_df_uncertainty <- che5years_plot_df[, c("year_id", "ihme_loc_id", "predict", "upper_10", "lower_10", "upper_25", "lower_25")]
che5years_plot_df_uncertainty <- che5years_plot_df_uncertainty[year_id == 2020 & predict == "Prediction"]

che5years_plot_df <- merge(che25_5years_plot_df, che10_5years_plot_df, by = c("year_id", "ihme_loc_id", "predict")) [,-c("upper_10", "lower_10", "upper_25", "lower_25")]

## plot: regression with past 5 data points
data_dash_blr_peru <- che5years_plot_df[year_id >2018 & ihme_loc_id %in% c("BLR", "PER")]
data_dash_blr_peru <- data_dash_blr_peru %>% 
  mutate(che25_pct = ifelse(year_id ==2020 & predict == "Estimate from surveys or HEFPI", NA, che25_pct)) %>% 
  mutate(che10_pct = ifelse(year_id ==2020 & predict == "Estimate from surveys or HEFPI", NA, che10_pct))

data_dash_mex <- che5years_plot_df[year_id >2017 & ihme_loc_id == "MEX"]
data_dash_mex <- data_dash_mex %>% 
  mutate(che25_pct = ifelse(year_id ==2020 & predict == "Estimate from surveys or HEFPI", NA, che25_pct)) %>% 
  mutate(che10_pct = ifelse(year_id ==2020 & predict == "Estimate from surveys or HEFPI", NA, che10_pct))

data_dash_vnm <- che5years_plot_df[year_id >2017 & ihme_loc_id == "VNM"]
data_dash_vnm <- data_dash_vnm %>% 
  mutate(che25_pct = ifelse(year_id ==2020 & predict == "Estimate from surveys or HEFPI", NA, che25_pct)) %>% 
  mutate(che10_pct = ifelse(year_id ==2020 & predict == "Estimate from surveys or HEFPI", NA, che10_pct))

data_dash_rus <- che5years_plot_df[year_id >2013 & ihme_loc_id == "RUS"]
data_dash_rus <- data_dash_rus %>% 
  mutate(che25_pct = ifelse(year_id ==2020 & predict == "Estimate from surveys or HEFPI", NA, che25_pct)) %>% 
  mutate(che10_pct = ifelse(year_id ==2020 & predict == "Estimate from surveys or HEFPI", NA, che10_pct))

che5years_plot_df_long <- che5years_plot_df[,-c("che25", "che10")]
che5years_plot_df_long <- melt(che5years_plot_df_long, id.vars=c("year_id", "ihme_loc_id","predict" ))

data_dash <- rbind(data_dash_blr_peru, data_dash_mex, data_dash_rus, data_dash_vnm)
data_dash <- na.omit(data_dash)

data_dash <- data_dash[,-c("che25", "che10")]


data_dash_long <- melt(data_dash, id.vars=c("year_id", "ihme_loc_id","predict" ))

che5years_plot_df_uncertainty_10 <- che5years_plot_df_uncertainty[, c("year_id", "ihme_loc_id","predict", "upper_10", "lower_10" )]
che5years_plot_df_uncertainty_10[, variable := "10%"]
colnames(che5years_plot_df_uncertainty_10) <- c("year_id", "ihme_loc_id","predict", "upper", "lower", "variable" )

che5years_plot_df_uncertainty_25 <- che5years_plot_df_uncertainty[, c("year_id", "ihme_loc_id","predict", "upper_25", "lower_25" )]
che5years_plot_df_uncertainty_25[, variable := "25%"]
colnames(che5years_plot_df_uncertainty_25) <- c("year_id", "ihme_loc_id","predict", "upper", "lower", "variable" )

che5years_plot_df_uncertainty <-  rbind(che5years_plot_df_uncertainty_10, che5years_plot_df_uncertainty_25)

che5years_plot_df_long[,variable := ifelse(variable == "che25_pct", "25%", "10%")]
data_dash_long[,variable := ifelse(variable == "che25_pct", "25%", "10%")]
data_dash_long <- merge(data_dash_long, che5years_plot_df_uncertainty, by = c("ihme_loc_id", "variable", "year_id", "predict" ), all = T)


# create fake variable "row"
che5years_plot_df_long_row <- che5years_plot_df_long %>% mutate(row = ifelse(variable== "25%", 1, 2))
# specify the maximum height for each row
che5years_plot_df_long_row<-che5years_plot_df_long_row %>%
  group_by(row) %>%
  mutate(max_y = ifelse(row == 1, 5, 15)) %>%
  ungroup()

che5years_plot_df_long_row <- setDT(che5years_plot_df_long_row)
data_dash_long[, upper := upper *100]
data_dash_long[, lower := lower *100]

data_dash_long[, upper := ifelse(is.na(upper), value, upper)]
data_dash_long[, lower := ifelse(is.na(lower), value, lower)]

data_dash_long$variable <- factor(data_dash_long$variable, levels = c("10%", "25%"))
dummy <- che5years_plot_df_long_row[1:5]
dummy$ihme_loc_id <- c("MEX", "PER", "BLR", "VNM", "RUS")
dummy$value <- c(NA, NA, NA, NA, NA)
dummy_2 <- copy(dummy)
dummy_2$variable <- rep("10%", 5)

dummy <- rbind (dummy, dummy_2)
dummy$year_id <- rep(2022, 10)
che5years_plot_df_long_row <- rbind(che5years_plot_df_long_row, dummy)
che5years_plot_df_long_row[, location_name := ifelse(ihme_loc_id == "MEX", "Mexico",
                                                     ifelse(ihme_loc_id == "PER", "Peru",
                                                            ifelse(ihme_loc_id == "BLR", "Belarus",
                                                                   ifelse(ihme_loc_id == "VNM", "Vietnam",
                                                                          ifelse(ihme_loc_id == "RUS", "Russia", NA)))))]
data_dash_long[, location_name := ifelse(ihme_loc_id == "MEX", "Mexico",
                                                     ifelse(ihme_loc_id == "PER", "Peru",
                                                            ifelse(ihme_loc_id == "BLR", "Belarus",
                                                                   ifelse(ihme_loc_id == "VNM", "Vietnam",
                                                                          ifelse(ihme_loc_id == "RUS", "Russia", NA)))))]
# prepare estimate UI df for plotting 
estimate_ui_plot_che10 <- estimate_ui[, c("year_id","ihme_loc_id","predict","che10_pct",  "lower_10", "upper_10")]
estimate_ui_plot_che10[, variable := "10%"]
setnames(estimate_ui_plot_che10, old = c('che10_pct',  "lower_10", "upper_10"), 
         new = c('value', 'lower', 'upper'))

estimate_ui_plot_che25 <- estimate_ui[, c("year_id","ihme_loc_id","predict","che25_pct",  "lower_25", "upper_25")]
estimate_ui_plot_che25[, variable := "25%"]
setnames(estimate_ui_plot_che25, old = c('che25_pct',  "lower_25", "upper_25"), 
         new = c('value', 'lower', 'upper'))

estimate_ui_plot <- rbind(estimate_ui_plot_che25, estimate_ui_plot_che10)

che5years_plot_df_long_row_ui <- merge(che5years_plot_df_long_row, estimate_ui_plot, by = c("year_id","ihme_loc_id","predict", "variable", "value"))

che5years_plot_10 <- ggplot()+
  facet_wrap(~location_name, nrow=5, scales = 'free')+
  geom_line(data = che5years_plot_df_long_row[year_id >2003 & predict == "Estimate from surveys or HEFPI" & variable == "10%"], aes(x = year_id, y = value, group = variable), color = "#FC8D59") +
  geom_point(data = che5years_plot_df_long_row[year_id >2003 & variable == "10%"], aes(x = year_id, y = value, color = predict,  shape = predict), size = 1) +
  geom_ribbon(data=data_dash_long[variable == "10%"],aes(x = year_id, ymin = lower, ymax = upper),  color = "#737373",fill = "grey", alpha = 0.6) +
  geom_ribbon(data=che5years_plot_df_long_row_ui[year_id >2003 &variable == "10%"],aes(x = year_id, ymin = lower*100, ymax = upper*100), fill = "#FC8D59", color = "#fec7ad", alpha = 0.25, linetype = 'dotted') +
  geom_line(data = data_dash_long[variable == "10%"], aes(x = year_id, y = value, group = variable),linetype = 'twodash', color = "black") +
  
  theme_bw()+
  scale_color_manual(name = "", values = c("#FC8D59",  "black"), labels = c("CHE 10% estimate from surveys or HEFPI", 
                                                                                "Prediction"))+
  scale_shape_manual(name = "", values = c(16,17), labels = c("CHE 10% estimate from surveys or HEFPI", 
                                                                  "Prediction"))+
  labs(shape = "Type of estimate", color = "Threshold", y = "CHE 10%" ) +
  scale_x_continuous( breaks = seq(2004,2022, by = 2))+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank(),
        text = element_text(size=12), 
        axis.text.x =  element_text(angle = 90, vjust = 0.5, hjust=1, color = "black"),
        axis.text.y =  element_text(color = "black"),
       # axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        aspect.ratio = 1,
        strip.text.y = element_blank()
  )+
  guides(color=guide_legend(nrow=2,byrow=TRUE))

che5years_plot_df_long_row[,predict := ifelse(variable == "25%" & predict == "Estimate from surveys or HEFPI", "25 predict", predict) ]
legend_plot_color <- ggplot()+
  facet_wrap(~location_name+variable, nrow=1)+
  geom_line(data = che5years_plot_df_long_row[year_id >2009 & predict == "Estimate from surveys or HEFPI"& variable == "10%"], aes(x = year_id, y = value, group = variable),color = "#99D594") +
  geom_point(data = che5years_plot_df_long_row[year_id >2009 & variable == "10%"], aes(x = year_id, y = value,color = predict,shape = predict), size = 1) +
  geom_ribbon(data=data_dash_long,aes(x = year_id, ymin = lower, ymax = upper), color = "#737373", fill = "grey", alpha = 0.6) +
  geom_ribbon(data=che5years_plot_df_long_row_ui[year_id >2009& variable == "10%" ],aes(x = year_id, ymin = lower*100, ymax = upper*100), fill = "#99D594", color = "#A4E09F", alpha = 0.25, linetype = 'dotted') +
  geom_line(data = data_dash_long, aes(x = year_id, y = value, group = variable), linetype = "twodash", color = "black") +
  theme_bw()+
  scale_color_manual(name = "Data", values = c("#FC8D59",  "black"), labels = c("CHE 10% estimate from surveys or HEFPI", 
                                                                                          "Prediction"))+
  scale_shape_manual(name = "Data", values = c(16,17), labels = c("CHE 10% estimate from surveys or HEFPI", 
                                                                     "Prediction"))+
  geom_blank(data =che5years_plot_df_long_row[variable == "25%"],  aes( y=max_y)) +
  geom_blank(data =che5years_plot_df_long_row[variable == "25%"],  aes( y=0)) +
  scale_y_continuous( breaks = seq(0,5, by = 1))+
  scale_x_continuous( breaks = seq(2010,2022, by = 2))+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank(),
        text = element_text(size=12), 
        axis.text.x =  element_text(angle = 90, vjust = 0.5, hjust=1, color = "black"),
        axis.text.y =  element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        #legend.position = "none",
        aspect.ratio = 1,
        strip.text.y = element_blank()
  )

## check overlap of prediction and data UI 
che5years_plot_df_long_row_ui_check <- copy(che5years_plot_df_long_row_ui)
adjust_cols <- c("lower", "upper")
che5years_plot_df_long_row_ui_check[, (adjust_cols) := lapply(.SD,  function(x) x*100), .SDcols = adjust_cols]
setnames(che5years_plot_df_long_row_ui_check, old = c("lower", "upper", "value"), new = c("lower_estimate", "upper_estimate", "mean_estimate"))


check_che10 <- merge(data_dash_long[variable == "10%" & year_id == 2020 & predict == "Prediction", c("variable", "year_id", "ihme_loc_id", "upper", "lower", "value", "location_name")], 
                     che5years_plot_df_long_row_ui_check[variable == "10%" & year_id == 2020 & predict == "Estimate from surveys or HEFPI",  c("variable", "year_id", "ihme_loc_id","upper_estimate", "lower_estimate", "mean_estimate",
                                                                                                                                               "location_name")],
                     by = c("variable", "year_id", "ihme_loc_id", "location_name"))

check_che10[, sig := ifelse(upper < lower_estimate | lower > upper_estimate, 1, 0 )]

check_che25 <- merge(data_dash_long[variable == "25%" & year_id == 2020 & predict == "Prediction", c("variable", "year_id", "ihme_loc_id", "upper", "lower", "value", "location_name")], 
                     che5years_plot_df_long_row_ui_check[variable == "25%" & year_id == 2020 & predict == "Estimate from surveys or HEFPI",  c("variable", "year_id", "ihme_loc_id","upper_estimate", "lower_estimate", "mean_estimate",
                                                                                                                                               "location_name")],
                     by = c("variable", "year_id", "ihme_loc_id", "location_name"))

check_che25[, sig := ifelse(upper < lower_estimate | lower > upper_estimate, 1, 0 )]

# output nicely for appendix
check_che10_output <- check_che10[, .( prediction = paste0(sprintf('%.1f', round(`value`, 1)), "\n (", sprintf('%.1f',round(`lower`, 1)), ' to ',  sprintf('%.1f',round(`upper`, 1)), ")"),
                                       estimate = paste0(sprintf('%.1f',round(mean_estimate, 1)), "\n (", sprintf('%.1f',round(lower_estimate, 1)), ' to ',  sprintf('%.1f',round(upper_estimate, 1)), ")")), by = c("ihme_loc_id")]

check_che25_output <- check_che25[, .( prediction = paste0(sprintf('%.1f', round(`value`, 1)), "\n (", sprintf('%.1f',round(`lower`, 1)), ' to ',  sprintf('%.1f',round(`upper`, 1)), ")"),
                                       estimate = paste0(sprintf('%.1f',round(mean_estimate, 1)), "\n (", sprintf('%.1f',round(lower_estimate, 1)), ' to ',  sprintf('%.1f',round(upper_estimate, 1)), ")")), by = c("ihme_loc_id")]

legend_color <- cowplot::get_legend(legend_plot_color)


x <- grid.arrange(che5years_plot_10, top = textGrob("Forecasted vs. observed values of CHE 10%") )

ggsave(file.path(outdir, "ensemble_level_diff_all_data_che10_constant_no_constant.pdf"),x, height =15, width = 15)



