#####################################################################
## Name: User                                               ##
## Purpose: Construct figure describing covid regressions in Peru  ##
## and Mexico                                                      ##
## Date: 09/02/2021                                                ##
#####################################################################
## clean environment 
rm(list = ls())

# runtime configuration
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/home/j/'
  h <- paste0('/homes/', Sys.getenv('USER'), '/')
  k <- '/ihme/cc_resources/libraries'
} else {
  j <- 'J:/'
  h <- 'H:/'
  k <- 'K:/libraries'
}

## libraries 
pacman::p_load(data.table, ggplot2, ggpubr)

## read regression results 
peru <- fread('FILEPATH/lpm_thresh25_per_frp_mods2023-02-24.csv')
mex <- fread('FILEPATH/lpm_thresh25_mex_frp_mods2023-02-24.csv')

## update variable naming 
peru[, term := ifelse(term == 'log_death_rate_4m' | term == 'log_death_rate_1m', 'log_death_rate', term)]

## subset to covid deaths coefficient 
peru <- peru[term == 'log_death_rate'][, location_name := 'Peru']
mex <- mex[term == 'log_death_rate'][, location_name := 'Mexico']

df <- rbind(peru[, !c('V1')], mex[, !c('V1')])

df[, lower := estimate-(std.error*1.96)]
df[, upper := estimate+(std.error*1.96)]

df[, label := ifelse(label == 'Model 1 predicting log OOP', 'OOP health \nspending',
              ifelse(label == 'Model 2 predicting CHE10', 'Catastrophic health \nexpenditure (10%)',
                            ifelse(label == 'Model 3 predicting CHE25', 'Catastrophic health \nexpenditure (25%)',
                                   ifelse(label == 'Model 4 predicting any visit', 'Share of individuals with a healthcare visit',
                                          ifelse(label == 'Model 5 predicting share of private visit',  'Share of healthcare visits in the private sector', 
                                                 ifelse(label == 'Model 6 predicting private visit', "Share of individuals with a private healthcare visit", NA))))))]



p1 <- ggplot(data = df[label == 'OOP health \nspending'], aes(x = location_name, y = estimate, fill = location_name)) + 
  geom_hline(yintercept = 0) + 
  geom_point(shape = 21, size = 3.5, alpha = 1, color = "transparent",  aes(fill = location_name)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = location_name), width=0, size = 0.75) + # y error bar
  scale_fill_manual(values=c('#FC8D59','#99D594'))+
  scale_color_manual(values=c('#FC8D59','#99D594'))+
  labs(x = '', y = 'OOP health spending (2021 PPP)', title = 'Association between COVID-19 deaths and \n OOP health spending') + 
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(size = 14),
        panel.grid.major.x = element_blank(), 
        axis.text = element_text(size = 12), 
        legend.position='none',
        plot.title = element_text(hjust = 0.5, size = 13))

p2 <- ggplot(data = df[label == 'Catastrophic health \nexpenditure (10%)'], aes(x = location_name, y = estimate, fill = location_name)) + 
  geom_hline(yintercept = 0) + 
  geom_point(shape = 21, size = 3.5, alpha = 1, color = "transparent",  aes(fill = location_name)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = location_name), width=0, size = 0.75) + 
  scale_fill_manual(values=c('#FC8D59','#99D594'))+
  scale_color_manual(values=c('#FC8D59','#99D594'))+
  labs(x = '', y = 'Catastrophic health expenditure (10%)', title = 'Association between COVID-19 deaths and \n catastrophic health expenditure (10%)') + 
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(size = 14),
        panel.grid.major.x = element_blank(), 
        axis.text = element_text(size = 12), 
        legend.position='none',
        plot.title = element_text(hjust = 0.5, size = 13))

p3 <- ggplot(data = df[label == 'Catastrophic health \nexpenditure (25%)'], aes(x = location_name, y = estimate, fill = location_name)) + 
  geom_hline(yintercept = 0) + 
  geom_point(shape = 21, size = 3.5, alpha = 1, color = "transparent",  aes(fill = location_name)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = location_name), width=0, size = 0.75) + 
  scale_fill_manual(values=c('#FC8D59','#99D594'))+
  scale_color_manual(values=c('#FC8D59','#99D594'))+
  labs(x = '', y = 'Catastrophic health expenditure (25%)', title = 'Association between COVID-19 deaths and \n catastrophic health expenditure (25%)') + 
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(size = 14),
        panel.grid.major.x = element_blank(), 
        axis.text = element_text(size = 12), 
        legend.position='none',
        plot.title = element_text(hjust = 0.5, size = 13))

p4 <- ggplot(data = df[label == 'Share of individuals with a healthcare visit'], aes(x = location_name, y = estimate, fill = location_name)) + 
  geom_hline(yintercept = 0) + 
  geom_point(shape = 21, size = 3.5, alpha = 1, color = "transparent",  aes(fill = location_name)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = location_name), width=0, size = 0.75) + 
  scale_fill_manual(values=c('#FC8D59','#99D594'))+
  scale_color_manual(values=c('#FC8D59','#99D594'))+
  labs(x = '', y = 'Share of individuals with a healthcare visit', title = 'Association between COVID-19 deaths and \n share of individuals with a healthcare visit') + 
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(size = 14),
        panel.grid.major.x = element_blank(), 
        axis.text = element_text(size = 12), 
        legend.position='none',
        plot.title = element_text(hjust = 0.5, size = 13))

p5 <- ggplot(data = df[label == 'Share of healthcare visits in the private sector'], aes(x = location_name, y = estimate, fill = location_name)) + 
  geom_hline(yintercept = 0) + 
  geom_point(shape = 21, size = 3.5, alpha = 1, color = "transparent",  aes(fill = location_name)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = location_name), width=0, size = 0.75) + 
  scale_fill_manual(values=c('#FC8D59','#99D594'))+
  scale_color_manual(values=c('#FC8D59','#99D594'))+
  labs(x = '', y = 'Share of individuals with a healthcare visit in the private sector', title = 'Association between COVID-19 deaths and \n share of individuals with a healthcare visit in the private sector') + 
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(size = 14),
        panel.grid.major.x = element_blank(), 
        axis.text = element_text(size = 12), 
        legend.position='none',
        plot.title = element_text(hjust = 0.5, size = 13))

p6 <- ggplot(data = df[label == 'Share of individuals with a private healthcare visit'], aes(x = location_name, y = estimate, fill = location_name)) + 
  geom_hline(yintercept = 0) + 
  geom_point(shape = 21, size = 3.5, alpha = 1, color = "transparent",  aes(fill = location_name)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color = location_name), width=0, size = 0.75) + 
  scale_fill_manual(values=c('#FC8D59','#99D594'))+
  scale_color_manual(values=c('#FC8D59','#99D594'))+
  labs(x = '', y = 'Share of healthcare visits in the private sector', title = 'Association between COVID-19 deaths and \n share of healthcare visits in the private sector') + 
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(size = 14),
        panel.grid.major.x = element_blank(), 
        axis.text = element_text(size = 12), 
        legend.position='none',
        plot.title = element_text(hjust = 0.5, size = 13))

pdf('FILEPATH//lpm_thresh20_per_mex_covid_death.pdf', height = 10, width = 10*1.618, pointsize = 2.5)
grid.arrange(p1, p2, p3,  p4, p5,  p6, nrow = 2, ncol = 3)
dev.off()

