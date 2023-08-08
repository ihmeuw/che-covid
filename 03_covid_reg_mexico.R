#####################################################################
## Name: User                                              ##
## Purpose: Compare Mexico CHE data and COVID-19 data              ##
## Date: 09/02/2021                                                ##
#####################################################################
## clean environment 
rm(list = ls())

# runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
  h <- paste0("/homes/", Sys.getenv("USER"), "/")
  k <- "/ihme/cc_resources/libraries"
} else {
  j <- "J:/"
  h <- "H:/"
  k <- "K:/libraries"
}

## libraries 
pacman::p_load(data.table, ggplot2, broom, haven, survey)

## read prepped covid data from Mexico municipalities
covid <- fread('FILEPATH/mexico_covid_summary.csv')

## read in extracted MEX ENIGH data from 2020 
enigh <- as.data.table(read_dta('FILEPATH/MEX_ENIGH_2020.dta'))
indiv <- as.data.table(read_dta('FILEPATH/MEX_ENIGH/MEX_ENIGH_IND_2020.dta'))

## attach municipality codes 
indiv <- merge(enigh[, c('hh_id', 'hh_id2', 'ubica_geo', 'strata1', 'strata2', 'upm', 'factor')], indiv, 
      by.x = c('hh_id', 'hh_id2'), by.y = c('folioviv', 'foliohog'))

## recode any_visit and is_private to indicate only after April 
indiv[, any_visit := ifelse((servmed_1 == '01' | servmed_2 == '02' | servmed_3 == '03' | servmed_4 == '04' | 
                              servmed_5 == '05' | servmed_6 == '06' | servmed_7 == '07' | servmed_8 == '08' | 
                              servmed_9 == '09'| servmed_12 == '12') & !is.na(prob_mes) & prob_mes %in% c('04', '05', '06', '07', '08', '09', '10', '11', '12') & prob_anio == 2020, 1, 0)]
indiv[, is_private := ifelse((servmed_8 == '08' | servmed_9 == '09') & prob_mes %in% c('04', '05', '06', '07', '08', '09', '10', '11', '12') & !is.na(prob_mes) & prob_anio == 2020 , 1, 0)]

## subset data based on number of households in sample by geography
## pull municipalities with less than 20 households to stabilize regressions results 
enigh_mun <- enigh[, .N, by = 'ubica_geo']
locs <- enigh_mun[N >= 20, ] 
length(unique(enigh$ubica_geo))-nrow(locs) ## NOTE: check number of dropped municipalities 

## drop household observations in sparse municipalities
nrow(enigh)-nrow(enigh[ubica_geo %in% locs$ubica_geo]) ## NOTE: check number of dropped households
enigh <- enigh[ubica_geo %in% locs$ubica_geo] 

## drop individual observations in sparse municipalities 
nrow(indiv)-nrow(indiv[ubica_geo %in% locs$ubica_geo]) ## NOTE: check number of dropped individuals
indiv <- indiv[ubica_geo %in% locs$ubica_geo] 

## calculate measures at household level
## calculate proportion of health expenditure
enigh[, health_exp_prop := health/consumption]

## calculate CHE
enigh[, che10 := ifelse(health_exp_prop > 0.1, 1, 0)]
enigh[, che25 := ifelse(health_exp_prop > 0.25, 1, 0)]

## divide consumption and health expenditure by the number of household members
enigh[, health := health/hhsize]
enigh[, consumption := consumption/hhsize]

## calculate measures at visit level
## Calculate share of private visits
visits <- indiv
visits <- visits[any_visit == 1,]
visits[, share_private_visits := is_private/any_visit]

## 
che10_counts <- enigh[che10 == 1, .N, by = 'ubica_geo']
setnames(che10_counts, old = c('N'), new = c('n_che10'))

che25_counts <- enigh[che25 == 1, .N, by = 'ubica_geo']
setnames(che25_counts, old = c('N'), new = c('n_che25'))

che10_no_counts <- enigh[che10 == 0, .N, by = 'ubica_geo']
setnames(che10_no_counts, old = c('N'), new = c('n_no_che10'))

che25_no_counts <- enigh[che25 == 0, .N, by = 'ubica_geo']
setnames(che25_no_counts , old = c('N'), new = c('n_no_che25'))

che_counts <- merge(che10_counts, che25_counts, by = 'ubica_geo', all = T)
che_counts <- merge(che_counts, che10_no_counts, by = 'ubica_geo', all = T)
che_counts <- merge(che_counts, che25_no_counts, by = 'ubica_geo', all = T)

che_counts[, n_che10 := ifelse(is.na(n_che10), 0, n_che10)][, n_che25 := ifelse(is.na(n_che25), 0, n_che25)]

## define survey design
hh_design <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, variables = ~health+che10+che25+consumption+rural+insurance_id+ubica_geo, data = enigh[!is.na(insurance_id)], nest = T) ## NOTE: drop 105 households with no missing insurance
ind_design <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, variables = ~share_below_post_secondary+age+any_visit+is_private+ubica_geo, data = indiv[!is.na(share_below_post_secondary)], nest = T) ## NOTE: drop 138166 individuals with no education 
vis_design <- svydesign(ids = ~strata1, strata = ~strata2+upm, weights = ~factor, variables = ~share_private_visits+ubica_geo, data = visits, nest = T) 

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
## calculate survey weighted means for household and individual level data 
hh_mex <- as.data.table(svyby(~health+
                               che10+
                               che25+
                               consumption+
                               rural+
                               insurance_id, ~ubica_geo, hh_design, svymean))

ind_mex <- as.data.table(svyby(~share_below_post_secondary+
                                 any_visit+
                                 is_private+
                                age, ~ubica_geo, ind_design, svymean))

vis_mex <- as.data.table(svyby(~share_private_visits, 
                               ~ubica_geo, vis_design, svymean))

## create municipality and state codes
hh_mex[, municipality_code := as.integer(substr(ubica_geo, 3,5))]
hh_mex[, state_id := as.integer(substr(ubica_geo, 1, 2))] 

## attach district covid data 
df <- merge(hh_mex[, c('ubica_geo', 
                       'health', 
                       'che10', 
                       'che25', 
                       'consumption', 
                       'rural', 
                       'insurance_id', 
                       'municipality_code', 
                       'state_id')], 
            ind_mex[, c('ubica_geo',
                        'any_visit', 
                        'is_private',  
                        'share_below_post_secondary', 
                        'age')], by = c('ubica_geo'))

df <- merge(df, vis_mex[, c('ubica_geo', 
                            'share_private_visits')], by = c('ubica_geo'))

df <- merge(df, covid, by.x = c('state_id', 'municipality_code'), 
                       by.y = c('state_code', 'municipal_code')) ## NOTE: drops 52 municipalities 

## add offset for total health expenditure and COVID-19 death rate
df[, health := health + median(health)*0.10]
df[, death_rate := death_rate + median(death_rate)*0.10]
df[, case_rate := case_rate + median(case_rate)*0.10]
df[, test_rate := test_rate + median(test_rate)*0.10]
df[, hosp_rate := hosp_rate + median(hosp_rate)*0.10]

## attach population for plotting
df <- merge(df, enigh_mun, by = 'ubica_geo')

##
df <- merge(df, che_counts, by = 'ubica_geo')

## CHE10 outcome with  demographic controls
m2 <- stats::glm(cbind(n_che10, n_no_che10) ~ 1 + log(death_rate) + log(consumption) + share_below_post_secondary + age + rural,
                 family = binomial,
                 data = df)
m2 <- as.data.table(tidy(m2))

m2_case <- stats::glm(cbind(n_che10, n_no_che10) ~ 1 + log(case_rate) + log(consumption) + share_below_post_secondary + age + rural,
                 family = binomial,
                 data = df)
m2_case <- as.data.table(tidy(m2_case))

m2_test <- stats::glm(cbind(n_che10, n_no_che10) ~ 1 + log(test) + log(consumption) + share_below_post_secondary + age + rural,
                      family = binomial,
                      data = df)
m2_test <- as.data.table(tidy(m2_test))



m2_test <- glm(che10 ~ log(test_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m2_test_df <- as.data.table(tidy(m2_test))

m2_hosp <- glm(che10 ~ log(hosp_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m2_hosp_df <- as.data.table(tidy(m2_hosp))




## COVID-19 death regressions  
## log health outcome with demographic controls
m1 <- glm(formula = log(health)~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m1_df <- as.data.table(tidy(m1))

m1_case <- glm(formula = log(health) ~ log(case_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m1_case_df<- as.data.table(tidy(m1_case))

m1_test <- glm(formula = log(health) ~ log(test_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m1_test_df<- as.data.table(tidy(m1_test))

m1_hosp <- glm(formula = log(health) ~ log(hosp_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m1_hosp_df<- as.data.table(tidy(m1_hosp))

## CHE10 outcome with  demographic controls
m2 <- glm(che10 ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m2_df <- as.data.table(tidy(m2))

m2_case <- glm(che10 ~ log(case_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m2_case_df <- as.data.table(tidy(m2_case))

m2_test <- glm(che10 ~ log(test_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m2_test_df <- as.data.table(tidy(m2_test))

m2_hosp <- glm(che10 ~ log(hosp_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m2_hosp_df <- as.data.table(tidy(m2_hosp))

## logit CHE10 outcome with demographic controls
m3 <- glm(formula = che10 ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df, family = binomial(link="logit"))
m3_df <- as.data.table(tidy(m3))

m3_case <- glm(che10 ~ log(case_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df, family = binomial(link="logit"))
m3_case_df <- as.data.table(tidy(m3_case))

m3_test <- glm(che10 ~ log(test_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df, family = binomial(link="logit"))
m3_test_df <- as.data.table(tidy(m3_test))

m3_hosp <- glm(che10 ~ log(hosp_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df, family = binomial(link="logit"))
m3_hosp_df <- as.data.table(tidy(m3_hosp))

## CHE25 outcome with demographic controls
m4 <- glm(che25 ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df)
m4_df <- as.data.table(tidy(m4))

m4_case <- glm(che25 ~ log(case_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df)
m4_case_df <- as.data.table(tidy(m4_case))

m4_test <- glm(che25 ~ log(test_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df)
m4_test_df <- as.data.table(tidy(m4_test))

m4_hosp <- glm(che25 ~ log(hosp_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df)
m4_hosp_df <- as.data.table(tidy(m4_hosp))

## logit CHE25 outcome with demographic controls
m5 <- glm(formula = che25 ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df, family = "binomial")
m5_df <- as.data.table(tidy(m5))

## Any visit outcome with demographic controls
m6 <- glm(formula = any_visit ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m6_df <- as.data.table(tidy(m6))

m6_case <- glm(formula = any_visit ~ log(case_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m6_case_df <- as.data.table(tidy(m6_case))

m6_test <- glm(formula = any_visit ~ log(test_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m6_test_df <- as.data.table(tidy(m6_test))

m6_hosp <- glm(formula = any_visit ~ log(hosp_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df)
m6_hosp_df <- as.data.table(tidy(m6_hosp))

## Logit any visit outcome with demographic controls 
m7 <- glm(formula = any_visit ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural , data = df, binomial(link="logit"))
m7_df <- as.data.table(tidy(m7))

## Share of private visits outcome with demographic controls
m8 <- glm(formula = share_private_visits ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural  , data = df)
m8_df <- as.data.table(tidy(m8))

m8_case <- glm(formula = share_private_visits ~ log(case_rate) + log(consumption) + share_below_post_secondary + age + rural  , data = df)
m8_case_df <- as.data.table(tidy(m8_case))

m8_test <- glm(formula = share_private_visits ~ log(test_rate) + log(consumption) + share_below_post_secondary + age + rural  , data = df)
m8_test_df <- as.data.table(tidy(m8_test))

m8_hosp <- glm(formula = share_private_visits ~ log(hosp_rate) + log(consumption) + share_below_post_secondary + age + rural  , data = df)
m8_hosp_df <- as.data.table(tidy(m8_hosp))

## Logit share of private visits outcome with demographic controls
m9 <- glm(formula = share_private_visits ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural  , data = df, binomial(link="logit"))
m9_df <- as.data.table(tidy(m9))

## private visit outcome with demographic controls
m10 <- glm(formula = is_private ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df)
m10_df <- as.data.table(tidy(m10))

m10_case <- glm(formula = is_private ~ log(case_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df)
m10_case_df <- as.data.table(tidy(m10_case))

m10_test <- glm(formula = is_private ~ log(test_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df)
m10_test_df <- as.data.table(tidy(m10_test))

m10_hosp <- glm(formula = is_private ~ log(hosp_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df)
m10_hosp_df <- as.data.table(tidy(m10_hosp))

## private visit outcome with demographic controls
m11 <- glm(formula = is_private ~ log(death_rate) + log(consumption) + share_below_post_secondary + age + rural ,  data = df, binomial(link="logit"))
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
                m11_df[, label := 'Model 11 predicting logit private visit with demographic covariates'][-1,]), file.path('/mnt/share/scratch/projects/hssa/che/data/', paste0('mex_frp_deaths_', Sys.Date(), '.csv')))

## format final summary
m1_df[, oop_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, oop_pval := round(p.value, digits = 4)]
m2_df[, che10_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che10_pval := round(p.value, digits = 4)]
m4_df[, che25_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che25_pval := round(p.value, digits = 4)]
m6_df[, any_visit_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, any_visit_pval := round(p.value, digits = 4)]
m8_df[, share_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, share_private_pval := round(p.value, digits = 4)]
m10_df[, is_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, is_private_pval := round(p.value, digits = 4)]

m1_case_df[, oop_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, oop_pval := round(p.value, digits = 4)]
m2_case_df[, che10_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che10_pval := round(p.value, digits = 4)]
m4_case_df[, che25_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che25_pval := round(p.value, digits = 4)]
m6_case_df[, any_visit_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, any_visit_pval := round(p.value, digits = 4)]
m8_case_df[, share_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, share_private_pval := round(p.value, digits = 4)]
m10_case_df[, is_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, is_private_pval := round(p.value, digits = 4)]

m1_test_df[, oop_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, oop_pval := round(p.value, digits = 4)]
m2_test_df[, che10_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che10_pval := round(p.value, digits = 4)]
m4_test_df[, che25_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che25_pval := round(p.value, digits = 4)]
m6_test_df[, any_visit_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, any_visit_pval := round(p.value, digits = 4)]
m8_test_df[, share_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, share_private_pval := round(p.value, digits = 4)]
m10_test_df[, is_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, is_private_pval := round(p.value, digits = 4)]

m1_hosp_df[, oop_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, oop_pval := round(p.value, digits = 4)]
m2_hosp_df[, che10_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che10_pval := round(p.value, digits = 4)]
m4_hosp_df[, che25_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, che25_pval := round(p.value, digits = 4)]
m6_hosp_df[, any_visit_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, any_visit_pval := round(p.value, digits = 4)]
m8_hosp_df[, share_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, share_private_pval := round(p.value, digits = 4)]
m10_hosp_df[, is_private_val := paste0(signif(estimate, digits = 3), '\n(', signif(std.error, digits = 3), ')')][, is_private_pval := round(p.value, digits = 4)]

## combine final summary
df <- merge(m1_df[-1, c('term', 'oop_val', 'oop_pval')], m2_df[-1, c('term', 'che10_val', 'che10_pval')], by = 'term')
df <- merge(df, m4_df[-1, c('term', 'che25_val', 'che25_pval')], by = 'term')
df <- merge(df, m6_df[-1, c('term', 'any_visit_val', 'any_visit_pval')], by = 'term')
df <- merge(df, m8_df[-1, c('term', 'share_private_val', 'share_private_pval')], by = 'term')
df <- merge(df, m10_df[-1, c('term', 'is_private_val', 'is_private_pval')], by = 'term')

df_case <- merge(m1_case_df[-1, c('term', 'oop_val', 'oop_pval')], m2_case_df[-1, c('term', 'che10_val', 'che10_pval')], by = 'term')
df_case <- merge(df_case, m4_case_df[-1, c('term', 'che25_val', 'che25_pval')], by = 'term')
df_case <- merge(df_case, m6_case_df[-1, c('term', 'any_visit_val', 'any_visit_pval')], by = 'term')
df_case <- merge(df_case, m8_case_df[-1, c('term', 'share_private_val', 'share_private_pval')], by = 'term')
df_case <- merge(df_case, m10_case_df[-1, c('term', 'is_private_val', 'is_private_pval')], by = 'term')

df_test <- merge(m1_test_df[-1, c('term', 'oop_val', 'oop_pval')], m2_test_df[-1, c('term', 'che10_val', 'che10_pval')], by = 'term')
df_test <- merge(df_test, m4_test_df[-1, c('term', 'che25_val', 'che25_pval')], by = 'term')
df_test <- merge(df_test, m6_test_df[-1, c('term', 'any_visit_val', 'any_visit_pval')], by = 'term')
df_test <- merge(df_test, m8_test_df[-1, c('term', 'share_private_val', 'share_private_pval')], by = 'term')
df_test <- merge(df_test, m10_test_df[-1, c('term', 'is_private_val', 'is_private_pval')], by = 'term')

df_hosp <- merge(m1_hosp_df[-1, c('term', 'oop_val', 'oop_pval')], m2_hosp_df[-1, c('term', 'che10_val', 'che10_pval')], by = 'term')
df_hosp <- merge(df_hosp, m4_hosp_df[-1, c('term', 'che25_val', 'che25_pval')], by = 'term')
df_hosp <- merge(df_hosp, m6_hosp_df[-1, c('term', 'any_visit_val', 'any_visit_pval')], by = 'term')
df_hosp <- merge(df_hosp, m8_hosp_df[-1, c('term', 'share_private_val', 'share_private_pval')], by = 'term')
df_hosp <- merge(df_hosp, m10_hosp_df[-1, c('term', 'is_private_val', 'is_private_pval')], by = 'term')


## re-order final dataset 
df[, term := factor(term, levels = c("log(death_rate)", 
                                     "log(consumption)", 
                                     "age", 
                                     "rural", 
                                     "share_below_post_secondary", 
                                     "insurance_id"))]

df <- df[order(term)]

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

df <- rbind(df, n, r2, fill = T)

df_case[, term := factor(term, levels = c("log(case_rate)", 
                                     "log(consumption)", 
                                     "age", 
                                     "rural", 
                                     "share_below_post_secondary", 
                                     "insurance_id"))]

df_case <- df_case[order(term)]

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

df_hosp[, term := factor(term, levels = c("log(hosp_rate)", 
                                          "log(consumption)", 
                                          "age", 
                                          "rural", 
                                          "share_below_post_secondary", 
                                          "insurance_id"))]

df_hosp <- df_hosp[order(term)]

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

df_test[, term := factor(term, levels = c("log(test_rate)", 
                                          "log(consumption)", 
                                          "age", 
                                          "rural", 
                                          "share_below_post_secondary", 
                                          "insurance_id"))]

df_test <- df_test[order(term)]

n_test <- cbind("N", 
                length(m1_test$residuals),
                "",
                length(m2_test$residuals),
                "",
                length(m4_test$residuals),
                "",
                length(m6_test$residuals),
                "",
                length(m8_test$residuals),
                "",
                length(m10_test$residuals), 
                "") 

r2_test <- cbind("R^2", 
                 round(with(summary(m1_test), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m2_test), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m4_test), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m6_test), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m8_test), 1 - deviance/null.deviance), 3),
                 "",
                 round(with(summary(m10_test), 1 - deviance/null.deviance), 3),
                 "") 

colnames(n_test) <- colnames(df_test)
colnames(r2_test) <- colnames(df_test)

df_test <- rbind(df_test, n_test, r2_test, fill = T)
## export final summary
write.csv(df, file.path('FILEPATH', paste0('mex_frp_deaths_final_', Sys.Date(), '.csv')))
write.csv(df_case, file.path('FILEPATH', paste0('mex_frp_cases_final_', Sys.Date(), '.csv')))
write.csv(df_test, file.path('FILEPATH', paste0('mex_frp_tests_final_', Sys.Date(), '.csv')))
write.csv(df_hosp, file.path('FILEPATH', paste0('mex_frp_hosps_final_', Sys.Date(), '.csv')))