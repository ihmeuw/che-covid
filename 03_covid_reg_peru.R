#####################################################################
## Name: User                                              ##
## Purpose: Compare Mexico CHE data and COVID-19 data in peru      ##
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
pacman::p_load(data.table, tidyr, ggplot2, readstata13, broom, survey)

## directory
dir <- 'FILEPATH'

## read in prepped covid data from Peru 
covid <- fread(file.path(dir, 'FILEPATH/data/peru_covid_summary.csv'))

## read in extracted PERU data from 2020 
peru <- as.data.table(read.dta13(file.path(dir, 'FILEPATH/PER_ENAHO_2020.dta')))
indiv <- data.table(read.dta13(file.path(dir, 'FILEPATH/PER_ENAHO_IND_2020.dta'), convert.factors=FALSE), stringsAsFactors = FALSE)

## drop interviews conducted prior to April 2020 
nrow(peru) - nrow(peru[!(mes %in% c('01', '02', '03')), ]) ## Note: check dropped households from before COVID-19 period (April 2020 - December 2020)
peru <- peru[!(mes %in% c('01', '02', '03')), ] 

nrow(indiv) - nrow(indiv[!(mes %in% c('01', '02', '03')), ]) ## Note: check dropped individuals from before COVID-19 period (April 2020 - December 2020)
indiv <- indiv[!(mes %in% c('01', '02', '03')), ] 

## subset data based on number of households in sample by geography
## pull districts with less than 6 households to stabilize regressions results 
peru_dist <- peru[, .N, by = 'ubigeo']
locs <- peru_dist[N >= 20] 
length(unique(peru$ubigeo))-nrow(locs) ## districts dropped

## drop household observations in sparse districts
nrow(peru)-nrow(peru[ubigeo %in% locs$ubigeo]) ## households dropped
peru <- peru[ubigeo %in% locs$ubigeo] 

## drop individual observations in sparse districts
nrow(indiv)-nrow(indiv[ubigeo %in% locs$ubigeo] ) ## individuals dropped
indiv <- indiv[ubigeo %in% locs$ubigeo] 

## calculate measures of interest at household level
## calculate proportion of health expenditure relative to consumption
peru[, health_exp_prop := total_health/consumption_expenditure]

## calculate che indicators at household level 
peru[, che10 := ifelse(health_exp_prop > 0.1, 1, 0)]
peru[, che25 := ifelse(health_exp_prop > 0.25, 1, 0)]

## divide consumption and health expenditure by the number of household members
peru[, total_health := total_health/mieperho]
peru[, consumption_expenditure := consumption_expenditure/mieperho]

## convert municipality id to integer
peru[, ubigeo := as.integer(ubigeo)]
peru[, year_id := as.integer(year_id)]
peru[, mes := as.integer(mes)]

indiv[, ubigeo := as.integer(ubigeo)]
indiv[, year_id := as.integer(year_id)]
indiv[, mes := as.integer(mes)]

## calculate measures of interest at visit level
## share of private visits 
visit <- indiv 
visit <- visit[any_visit == 1]
visit[, share_private_visits := is_private/any_visit]

## attach covid data
nrow(peru)-nrow( merge(peru, covid, by.x = c('ubigeo', 'mes'), 
                       by.y = c('UBIGEO', 'month')) ) ## district-months dropped because there is no covid data available
peru <- merge(peru, covid, by.x = c('ubigeo', 'mes'), 
           by.y = c('UBIGEO', 'month'))

## set survey design 
hh_design <- svydesign(ids = ~nconglome, strata = ~estrato, weights = ~factor07, 
                    variables = ~total_health+che10+che25+consumption_expenditure+rural+insurance+ubigeo+mes+year_id+death_rate_1m+death_rate_4m+case_rate_1m+case_rate_4m+hosp_rate_1m+hosp_rate_4m, 
                    data = peru, nest = T)
ind_design <- svydesign(ids = ~nconglome, strata = ~estrato, weights = ~factor07, 
                       variables = ~share_below_post_secondary+age+any_visit+is_private+ubigeo+mes+year_id, 
                       data = indiv, nest = T) 
vis_design <- svydesign(ids = ~nconglome, strata = ~estrato, weights = ~factor07, 
                        variables = ~share_private_visits+ubigeo+mes+year_id, 
                        data = visit, nest = T)

## calculate survey weighted means for household and individual level data 
hh_peru <- as.data.table(svyby(~total_health+
                                che10+
                                che25+
                                consumption_expenditure+
                                rural+
                                insurance+
                                year_id+
                                death_rate_1m+
                                death_rate_4m+
                                hosp_rate_1m+
                                hosp_rate_4m+
                                case_rate_1m+
                                case_rate_4m, ~ubigeo, hh_design, svymean))
ind_peru <- as.data.table(svyby(~age+
                                 any_visit+
                                 is_private, ~ubigeo, ind_design, svymean, na.rm = T))
ed_peru <- as.data.table(svyby(~share_below_post_secondary, ~ubigeo, ind_design, svymean, na.rm = T))
vis_peru <- as.data.table(svyby(~share_private_visits, ~ubigeo, vis_design, svymean))

## attach district covid data 
df <- merge(hh_peru[, c('ubigeo', 
                  'total_health', 
                  'che10', 
                  'che25', 
                  'consumption_expenditure', 
                  'rural', 
                  'insurance', 
                  'death_rate_1m', 
                  'death_rate_4m', 
                  'case_rate_1m', 
                  'case_rate_4m', 
                  'hosp_rate_1m', 
                  'hosp_rate_4m')], 
      ind_peru[, c('ubigeo', 
                   'age', 
                   'any_visit', 
                   'is_private')], by = c('ubigeo'))
df <- merge(df, 
            ed_peru[, c('ubigeo', 
                         'share_below_post_secondary')], by = c('ubigeo'))
df <- merge(df, 
            vis_peru[, c('ubigeo', 
                         'share_private_visits')], by = c('ubigeo'))

write.csv(df, 'FILEPATH/per_frp_deaths_data.csv')

## attach population for plotting
peru_dist[, ubigeo := as.numeric(ubigeo)]
df <- merge(df, peru_dist, by = 'ubigeo')

pdf('~/per_districts_comp.pdf', height = 4.15, width = 7.5, pointsize = 6.5)
ggplot(data = df, aes(x = N, y = pop)) + 
  geom_point() + 
  labs(x = "Number of households", y = "Population") + 
  annotate("text", x = 400, y = 1200000, label = paste0("r = ", round(cor(df$N, df$pop), 3))) + 
  theme_bw()

ggplot(data = df, aes(x = N, y = log(total_health))) + 
  geom_point() + 
  labs(x = "Number of households", y = "OOP expenditure (log)") + 
  annotate("text", x = 400, y = 7.5, label = paste0("r = ", round(cor(df$N, df$total_health), 3))) + 
  theme_bw()

ggplot(data = df, aes(x = N, y = log(consumption_expenditure))) + 
  geom_point() + 
  labs(x = "Number of households", y = "Consumption expenditure (log)") + 
  annotate("text", x = 400, y = 10.5, label = paste0("r = ", round(cor(df$N, df$consumption_expenditure), 3))) + 
  theme_bw()

ggplot(data = df, aes(x = N, y = share_below_post_secondary)) + 
  geom_point() + 
  labs(x = "Number of households", y = "Share of indiviudals with less than post-secondary education") + 
  annotate("text", x = 400, y = 1, label = paste0("r = ", round(cor(df$N, df$share_below_post_secondary), 3))) + 
  theme_bw()

ggplot(data = df, aes(x = N, y = age)) + 
  geom_point() + 
  labs(x = "Number of households", y = "Share of indiviudals with less than post-secondary education") + 
  annotate("text", x = 400, y = 45, label = paste0("r = ", round(cor(df$N, df$age), 3))) + 
  theme_bw()

ggplot(data = df, aes(x = N, y = rural)) + 
  geom_point() + 
  labs(x = "Number of households", y = "Share rural") + 
  annotate("text", x = 400, y = 1, label = paste0("r = ", round(cor(df$N, df$rural), 3))) + 
  theme_bw()

ggplot(data = df, aes(x = N, y = insurance)) + 
  geom_point() + 
  labs(x = "Number of households", y = "Share of indiviudals with any insurance") + 
  annotate("text", x = 400, y = 1, label = paste0("r = ", round(cor(df$N, df$insurance), 3))) + 
  theme_bw()

ggplot(data = df, aes(x = N, y = log(death_rate_1m))) + 
  geom_point() + 
  labs(x = "Number of households", y = "COVID-19 death rate (log)") + 
  annotate("text", x = 400, y = 1, label = paste0("r = ", round(cor(df$N, df$death_rate_1m), 3))) + 
  theme_bw()

ggplot(data = df, aes(x = N, y = log(death_rate_4m))) + 
  geom_point() + 
  labs(x = "Number of households", y = "COVID-19 death rate (log)") + 
  annotate("text", x = 400, y = 2, label = paste0("r = ", round(cor(df$N, df$death_rate_4m), 3))) + 
  theme_bw()

dev.off()

## look at variables for log transformation 
hist(df$total_health, breaks = 100)
hist(df$consumption_expenditure, breaks = 100)
hist(df$death_rate_1m, breaks = 100)
hist(df$death_rate_4m, breaks = 100)

## add offset for total health expenditure and COVID-19 death rate
df[, total_health := total_health + median(total_health)*0.10]
df[, death_rate_1m := death_rate_1m + median(death_rate_1m)*0.10]
df[, death_rate_4m := death_rate_4m + median(death_rate_4m)*0.10]
df[, hosp_rate_1m := hosp_rate_1m + median(hosp_rate_1m)*0.10]
df[, hosp_rate_4m := hosp_rate_4m + median(hosp_rate_4m)*0.10]
df[, case_rate_1m := case_rate_1m + median(case_rate_1m)*0.10]
df[, case_rate_4m := case_rate_4m + median(case_rate_4m)*0.10]

## COVID-19 death regressions  
## log health outcome
m1 <- glm(log(total_health) ~ log(death_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural, data = df)
m1_df <- as.data.table(tidy(m1))

m1_hosp <- glm(log(total_health) ~ log(hosp_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural, data = df)
m1_hosp_df <- as.data.table(tidy(m1_hosp))

m1_case <- glm(log(total_health) ~ log(case_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m1_case_df <- as.data.table(tidy(m1_case))

## CHE10 outcome with demographic controls
m2 <- glm(che10 ~ log(death_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m2_df <- as.data.table(tidy(m2))

m2_hosp <- glm(che10 ~ log(hosp_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m2_hosp_df <- as.data.table(tidy(m2_hosp))

m2_case <- glm(che10 ~ log(case_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m2_case_df <- as.data.table(tidy(m2_case))

## logit CHE10 outcome with demographic controls
m3 <- glm(formula = che10 ~ log(death_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df, family = binomial(link="logit"))
m3_df <- as.data.table(tidy(m3))

## CHE25 outcome with demographic controls
m4 <- glm(che25 ~ log(death_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m4_df <- as.data.table(tidy(m4))

m4_hosp <- glm(che25 ~ log(hosp_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m4_hosp_df <- as.data.table(tidy(m4_hosp))

m4_case <- glm(che25 ~ log(case_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m4_case_df <- as.data.table(tidy(m4_case))

## logit CHE25 outcome with demographic controls
m5 <- glm(formula = che25 ~ log(death_rate_4m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df, family = binomial(link="logit"))
m5_df <- as.data.table(tidy(m5))

## Number of visits outcome with demographic controls
m6 <- glm(formula = any_visit ~ log(death_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m6_df <- as.data.table(tidy(m6))

m6_hosp <- glm(formula = any_visit ~ log(hosp_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m6_hosp_df <- as.data.table(tidy(m6_hosp))

m6_case <- glm(formula = any_visit ~ log(case_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m6_case_df <- as.data.table(tidy(m6_case))

## Number of visits outcome with demographic controls
m7 <- glm(formula = any_visit ~ log(death_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df, family = binomial(link="logit"))
m7_df <- as.data.table(tidy(m7))

## Share of private visits outcome with demographic controls
m8 <- glm(formula = share_private_visits ~ log(death_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m8_df <- as.data.table(tidy(m8))

m8_case <- glm(formula = share_private_visits ~ log(case_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m8_case_df <- as.data.table(tidy(m8_case))

m8_hosp <- glm(formula = share_private_visits ~ log(hosp_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df)
m8_hosp_df <- as.data.table(tidy(m8_hosp))

## Share of private visits outcome with demographic controls
m9 <- glm(formula = share_private_visits ~ log(death_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural , data = df, family = binomial(link="logit"))
m9_df <- as.data.table(tidy(m9))

## Number of private visits
m10 <- glm(formula = is_private ~ log(death_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural ,  data = df)
m10_df <- as.data.table(tidy(m10))

m10_case <- glm(formula = is_private ~ log(case_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural ,  data = df)
m10_case_df <- as.data.table(tidy(m10_case))

m10_hosp <- glm(formula = is_private ~ log(hosp_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural ,  data = df)
m10_hosp_df <- as.data.table(tidy(m10_hosp))

## Number of private visits
m11 <- glm(formula = is_private ~ log(death_rate_1m) + log(consumption_expenditure) + share_below_post_secondary + age + rural ,  data = df, family = binomial(link="logit"))
m11_df <- as.data.table(tidy(m11))

## export diagnostic summary
write.csv(rbind(m1_df[, label := 'Model 1 predicting log OOP with demographic covariates'][-1,], 
                m2_df[, label := 'Model 2 predicting CHE10 with demographic covariates'][-1,], 
                m3_df[, label := 'Model 3 predicting logit CHE10 with demographic covariates'][-1,], 
                m4_df[, label := 'Model 4 predicting CHE25 with demographic covariates'][-1,],  
                m5_df[, label := 'Model 5 predicting logit CHE25 with demographic covariates'][-1,], 
                m6_df[, label := 'Model 6 predicting any visit with demographic covariates'][-1,], 
                m7_df[, label := 'Model 7 predicting logit any visit with demographic covariates'][-1,], 
                m8_df[, label := 'Model 8 predicting share of private visit with demographic covariates'][-1,], 
                m9_df[, label := 'Model 9 predicting logit share of private visits with demographic covariates'][-1,], 
                m10_df[, label := 'Model 10 predicting private visit with demographic covariates'][-1,],
                m11_df[, label := 'Model 11 predicting logit private visit with demographic covariates'][-1,]), file.path('FILEPATH', paste0('per_frp_deaths_', Sys.Date(), '.csv')))

## format final summary
m1_df[, oop_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, oop_pval := round(p.value, digits = 4)]
m2_df[, che10_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che10_pval := round(p.value, digits = 4)]
m4_df[, che25_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che25_pval := round(p.value, digits = 4)]
m6_df[, any_visit_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, any_visit_pval := round(p.value, digits = 4)]
m8_df[, share_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, share_private_pval := round(p.value, digits = 4)]
m10_df[, is_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, is_private_pval := round(p.value, digits = 4)]

m1_hosp_df[, oop_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, oop_pval := round(p.value, digits = 4)]
m2_hosp_df[, che10_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che10_pval := round(p.value, digits = 4)]
m4_hosp_df[, che25_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che25_pval := round(p.value, digits = 4)]
m6_hosp_df[, any_visit_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, any_visit_pval := round(p.value, digits = 4)]
m8_hosp_df[, share_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, share_private_pval := round(p.value, digits = 4)]
m10_hosp_df[, is_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, is_private_pval := round(p.value, digits = 4)]

m1_case_df[, oop_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, oop_pval := round(p.value, digits = 4)]
m2_case_df[, che10_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che10_pval := round(p.value, digits = 4)]
m4_case_df[, che25_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che25_pval := round(p.value, digits = 4)]
m6_case_df[, any_visit_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, any_visit_pval := round(p.value, digits = 4)]
m8_case_df[, share_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, share_private_pval := round(p.value, digits = 4)]
m10_case_df[, is_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, is_private_pval := round(p.value, digits = 4)]

## combine final summary
df <- merge(m1_df[-1, c('term', 'oop_val', 'oop_pval')], m2_df[-1, c('term', 'che10_val', 'che10_pval')], by = 'term', all = T)
df <- merge(df, m4_df[-1, c('term', 'che25_val', 'che25_pval')], by = 'term', all = T)
df <- merge(df, m6_df[-1, c('term', 'any_visit_val', 'any_visit_pval')], by = 'term', all = T)
df <- merge(df, m8_df[-1, c('term', 'share_private_val', 'share_private_pval')], by = 'term', all = T)
df <- merge(df, m10_df[-1, c('term', 'is_private_val', 'is_private_pval')], by = 'term', all = T)

df_case <- merge(m1_case_df[-1, c('term', 'oop_val', 'oop_pval')], m2_case_df[-1, c('term', 'che10_val', 'che10_pval')], by = 'term', all = T)
df_case <- merge(df_case, m4_case_df[-1, c('term', 'che25_val', 'che25_pval')], by = 'term', all = T)
df_case <- merge(df_case, m6_case_df[-1, c('term', 'any_visit_val', 'any_visit_pval')], by = 'term', all = T)
df_case <- merge(df_case, m8_case_df[-1, c('term', 'share_private_val', 'share_private_pval')], by = 'term', all = T)
df_case <- merge(df_case, m10_case_df[-1, c('term', 'is_private_val', 'is_private_pval')], by = 'term', all = T)

df_hosp <- merge(m1_hosp_df[-1, c('term', 'oop_val', 'oop_pval')], m2_hosp_df[-1, c('term', 'che10_val', 'che10_pval')], by = 'term', all = T)
df_hosp <- merge(df_hosp, m4_hosp_df[-1, c('term', 'che25_val', 'che25_pval')], by = 'term', all = T)
df_hosp <- merge(df_hosp, m6_hosp_df[-1, c('term', 'any_visit_val', 'any_visit_pval')], by = 'term', all = T)
df_hosp <- merge(df_hosp, m8_hosp_df[-1, c('term', 'share_private_val', 'share_private_pval')], by = 'term', all = T)
df_hosp <- merge(df_hosp, m10_hosp_df[-1, c('term', 'is_private_val', 'is_private_pval')], by = 'term', all = T)

## re-order final dataset 
df[, term := factor(term, levels = c("log(death_rate_1m)", 
                                     "log(death_rate_4m)",
                                     "log(consumption_expenditure)", 
                                     "age", 
                                     "rural", 
                                     "share_below_post_secondary", 
                                     "insurance"))]

df <- df[order(term)]

##
n <- cbind("N", 
           length(m1$residuals),
           "",
           length(m2$residuals),
           "",
           length(m4$residuals),
           "",
           length(m6$residuals),
           "",
           length(m8$residuals),
           "",
           length(m10$residuals), 
           "") 

r2 <- cbind("R^2", 
            round(with(summary(m1), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m2), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m4), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m6), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m8), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m10), 1 - deviance/null.deviance), 3),
            "") 

colnames(n) <- colnames(df)
colnames(r2) <- colnames(df)

## 
df <- rbind(df, n, r2, fill = T)

df_case[, term := factor(term, levels = c("log(case_rate_1m)", 
                                     "log(case_rate_4m)",
                                     "log(consumption_expenditure)", 
                                     "age", 
                                     "rural", 
                                     "share_below_post_secondary", 
                                     "insurance"))]

df_case <- df_case[order(term)]

##
n_case <- cbind("N", 
           length(m1_case$residuals),
           "",
           length(m2_case$residuals),
           "",
           length(m4_case$residuals),
           "",
           length(m6_case$residuals),
           "",
           length(m8_case$residuals),
           "",
           length(m10_case$residuals), 
           "") 

r2_case <- cbind("R^2", 
            round(with(summary(m1_case), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m2_case), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m4_case), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m6_case), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m8_case), 1 - deviance/null.deviance), 3),
            "",
            round(with(summary(m10_case), 1 - deviance/null.deviance), 3),
            "") 

colnames(n_case) <- colnames(df_case)
colnames(r2_case) <- colnames(df_case)

df_case <- rbind(df_case, n_case, r2_case, fill = T)

df_hosp[, term := factor(term, levels = c("log(hosp_rate_1m)", 
                                          "log(hosp_rate_4m)",
                                          "log(consumption_expenditure)", 
                                          "age", 
                                          "rural", 
                                          "share_below_post_secondary", 
                                          "insurance"))]

df_hosp <- df_hosp[order(term)]

##
n_hosp <- cbind("N", 
                length(m1_hosp$residuals),
                "",
                length(m2_hosp$residuals),
                "",
                length(m4_hosp$residuals),
                "",
                length(m6_hosp$residuals),
                "",
                length(m8_hosp$residuals),
                "",
                length(m10_hosp$residuals), 
                "") 

r2_hosp <- cbind("R^2", 
                 round(with(summary(m1_hosp), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m2_hosp), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m4_hosp), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m6_hosp), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m8_hosp), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m10_hosp), 1 - deviance/null.deviance), 3),
                 "") 

colnames(n_hosp) <- colnames(df_hosp)
colnames(r2_hosp) <- colnames(df_hosp)

df_hosp <- rbind(df_hosp, n_hosp, r2_hosp, fill = T)

## export final summary 
write.csv(df[order(term)], file.path('FILEPATH', paste0('per_frp_deaths_final_', Sys.Date(), '.csv')))
write.csv(df_case[order(term)], file.path('FILEPATH', paste0('per_frp_cases_final_', Sys.Date(), '.csv')))
write.csv(df_hosp[order(term)], file.path('FILEPATH', paste0('per_frp_hosps_final_', Sys.Date(), '.csv')))
