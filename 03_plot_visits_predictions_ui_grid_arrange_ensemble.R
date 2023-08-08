
################################################################################
## Author: User                                                      ##
## Date: 12/5/2022                                                     ##
## Purpose: Plot visit predictions and past trends in Peru in Mexico                           ##
################################################################################
## clear memory
rm(list=ls())
library(vctrs, lib.loc='FILEPATH/4.1.3.4')
## runtime configuration
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/home/j'
  h <- file.path('/homes', Sys.getenv('USER'))
} else {
  j <- 'J:/'
  h <- 'H:/'
}

## libraries
pacman::p_load(data.table, haven,readstata13, readxl, ggplot2,dplyr,survey, broom, stringr, gridExtra, gtable,ragg, cowplot, RColorBrewer)

## directories
dir <- 'FILEPATH'

# read in che drivers predictions
predict_df <- fread(file.path(dir, 'visits_plotting_predictions.csv'))
predict_df[, ihme_loc_id := "PER"]
setnames(predict_df,old = c("mean_value", "lower_value", "upper_value"), new = c("value", "lower", "upper"))

# read in means for earlier years
means_df <- fread(file.path(dir, "che_drivers_means.csv"))[, -c("se")]

## extract previous year values for dashed prediction line
means_df_previous_per <- means_df[year_id == 2019 & ihme_loc_id == "PER"]
means_df_previous_mex <- means_df[year_id == 2018 & ihme_loc_id == "MEX"]

means_df_previous <- rbind(means_df_previous_per,means_df_previous_mex )

# merge previous year onto 2020 for plotting
predict_df <- rbind(means_df_previous, predict_df, fill = T)

# take out upper and lower for 2019/2018 point in predict df
predict_df[, lower := ifelse(year_id ==2020, lower, value)]
predict_df[, upper := ifelse(year_id ==2020, upper, value)]

## add country labels
predict_df[, country := ifelse(ihme_loc_id == "PER", "Peru", "Mexico")]
means_df[, country := ifelse(ihme_loc_id == "PER", "Peru", "Mexico")]

## plot by variable
colors <- brewer.pal(6, "Spectral")
any_visit_plot <- ggplot()+
  facet_wrap(~factor(country), nrow = 1)+
  geom_point(data = means_df[variable == "any_visit"], aes(x = year_id, y = value), color = "#99D594", size = 1.5, shape = 16) +
  geom_line(data = means_df[variable == "any_visit"], aes(x = year_id, y = value, group = variable),color = "#99D594") +
  geom_point(data = predict_df[year_id ==2020 & variable == "any_visit"], aes(x = year_id, y = value), color = "black", size = 1.5, shape = 17) +
  geom_line(data = predict_df[variable == "any_visit"], aes(x = year_id, y = value),linetype = 'twodash', color = "black") +
  geom_ribbon(data=predict_df[variable == "any_visit"],aes(x = year_id, ymin = lower, ymax = upper), fill = "grey", color = "#737373",  alpha = 0.6) +
  geom_ribbon(data=means_df[variable == "any_visit"],aes(x = year_id, ymin = lower, ymax = upper),fill = "#99D594", color = "#A4E09F",  alpha = 0.25, linetype = 'dotted') +
  theme_bw()+
  ggtitle('Share of individuals with any visit')+
  
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark = ".", accuracy = 0.01))+
  scale_x_continuous( breaks = seq(2000,2022, by = 2))+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank(),
        text = element_text(size=8), 
        axis.text.x =  element_text(angle = 90, vjust = 0.5, hjust=1, color = "black"),
        axis.text.y =  element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        aspect.ratio = 1,
        strip.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,-0.5,0.1,-0.5), "cm")
  )

private_visits_plot <- ggplot()+
  facet_wrap(~factor(country), nrow = 1)+
  geom_point(data = means_df[variable == "is_private"], aes(x = year_id, y = value), color = "#d95f02", size = 1.5, shape = 16) +
  geom_line(data = means_df[variable == "is_private"], aes(x = year_id, y = value, group = variable),color = "#d95f02") +
  geom_point(data = predict_df[year_id ==2020 & variable == "is_private"], aes(x = year_id, y = value), color = "black", size = 1.5, shape = 17) +
  geom_line(data = predict_df[variable == "is_private"], aes(x = year_id, y = value),linetype = 'twodash', color = "black") +
  geom_ribbon(data=predict_df[variable == "is_private"],aes(x = year_id, ymin = lower, ymax = upper), fill = "grey", color = "#737373", alpha = 0.6) +
  geom_ribbon(data=means_df[variable == "is_private"],aes(x = year_id, ymin = lower, ymax = upper), fill = "#FC8D59", color = "#fec7ad", alpha = 0.25, linetype = 'dotted') +
  theme_bw()+
  ggtitle('Share of individuals with a private visit')+
  
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark = ".", accuracy = 0.01))+
  scale_x_continuous( breaks = seq(2000,2022, by = 2))+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank(),
        text = element_text(size=8), 
        axis.text.x =  element_text(angle = 90, vjust = 0.5, hjust=1, color = "black"),
        axis.text.y =  element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        aspect.ratio = 1,
        strip.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,-0.5,0.1,-0.5), "cm")
  )


private_share_plot <- ggplot()+
  facet_wrap(~factor(country), nrow = 1)+
  geom_point(data = means_df[variable == "share_private"], aes(x = year_id, y = value), color = "#7570b3", size = 1.5, shape = 16) +
  geom_line(data = means_df[variable == "share_private"], aes(x = year_id, y = value, group = variable),color = "#7570b3") +
  geom_point(data = predict_df[year_id ==2020 & variable == "share_private"], aes(x = year_id, y = value), color = "black", size = 1.5, shape = 17) +
  geom_line(data = predict_df[variable == "share_private"], aes(x = year_id, y = value),linetype = 'twodash', color = "black") +
  geom_ribbon(data=predict_df[variable == "share_private"],aes(x = year_id, ymin = lower, ymax = upper), fill = "grey", color = "#737373", alpha = 0.6) +
  geom_ribbon(data=means_df[variable == "share_private"],aes(x = year_id, ymin = lower, ymax = upper),fill = "#7570b3", color = "#a3a0cc", alpha = 0.25, linetype = 'dotted') +
  theme_bw()+
  ggtitle('Share of visits in the private sector')+
  
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark = ".", accuracy = 0.01))+
  scale_x_continuous( breaks = seq(2000,2022, by = 2))+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank(),
        text = element_text(size=8), 
        axis.text.x =  element_text(angle = 90, vjust = 0.5, hjust=1, color = "black"),
        axis.text.y =  element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        aspect.ratio = 1,
        strip.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,-0.5,0.1,-0.5), "cm")
  )
shapes <- c("Observation" = 16, "Prediction" = 17)
means_df[, plot_variable := variable]
means_df[, point_shape := "Observation"]
predict_df[, plot_variable := "Prediction"]
predict_df[, point_shape := "Prediction"]

plot_df <- rbind(means_df, predict_df, fill = T)
legend_plot_color <- ggplot()+
  geom_point(data = plot_df[ihme_loc_id == "PER" & variable %in% c("share_private", "is_private", "any_visit")], aes(x = year_id, y = value, group = variable, color = plot_variable, shape = plot_variable), size = 1.5) +
  theme_bw()+
  ggtitle('Share of individuals with any insurance')+
  scale_shape_manual(name = "Data", values = c(16,16,16,17),
                     labels = c("Share of visits in \nthe private sector",
                                'Share of individuals \nwith a private visit',
                                'Share of individuals \nwith any visit',
                                "Prediction"))+
  scale_color_manual(name = "Data",
                     values = c("#7570b3", "#d95f02","#99D594", "black"),
                     labels = c("Share of visits in \nthe private sector",
                                'Share of individuals \nwith a private visit',
                                'Share of individuals \nwith any visit',
                                "Prediction"))
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark = ".", accuracy = 0.01))+
  scale_x_continuous( breaks = seq(2000,2022, by = 2))+
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid = element_blank(),
        text = element_text(size=8), 
        axis.text.x =  element_text(angle = 90, vjust = 0.5, hjust=1, color = "black"),
        axis.text.y =  element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        aspect.ratio = 1,
        strip.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,-0.5,0.1,-0.5), "cm")
  )

x <- grid.arrange(any_visit_plot, private_visits_plot, private_share_plot,  nrow = 3, ncol = 1)
legend_color <- cowplot::get_legend(legend_plot_color)
y <- grid.arrange(x, legend_color, ncol =1, heights = c(5,1))

ggsave(file.path(dir, "visits_predictions.tiff"),y, height = 12, width = 12)
