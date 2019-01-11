## calculate inequality indicators
##################################

# this file is similar to eu28_indicators.R - the main difference is the adjustment of weights for country population proportions
# furthermore, the manual computation of the theil is skipped here



# load packages and data -----------------------------------------------------------

library(dplyr)
library(survey)
library(convey)
library(eurostat)

load("./data/silc_eu28.RData")



# imbalances in the dataset wrt number of observations per country
table(silc.h.store$hb020[which(silc.h.store$hb010==2017)])



rm(silc.d.store, silc.h.store, silc.r.store, silc.p.store)



# Subsetting? --------------------------------------------------------------
# 
# # # To get useful results we may want to subset to only positive income (at least one positive income!)
silc.p1.ppp.store <- silc.p1.ppp.store %>% filter_at(vars(equivalent_pre_tax_factor_income:equivalent_post_tax_disposable_income), any_vars(. > 0))
silc.p2.ppp.store <- silc.p2.ppp.store %>% filter_at(vars(pre_tax_factor_income:post_tax_disposable_income), any_vars(. > 0))



# get population data for Theil -------------------------------------------




pop <- get_eurostat("naida_10_pe", time_format = "num")
pop <- pop %>% filter(na_item == "POP_NC")



# loop to calculate indicators for each year ------------------------------

indicators <- array(NA, c(14, 40, 2))
#theil <- array(NA, c(40, 500, 2), dimnames = list(c(as.character(1:40)),c(as.character(1:500)),c(as.character(1:2))))

for(year in 2004:2017){
  
  
  
  indicators[year-2003, 1,] <- year
  
  
  
  
  # Creating Survey Objects -------------------------------------------------
  
  
  silc.p1.y <- silc.p1.ppp.store %>% filter(rb010 %in% year)
  silc.p2.y <- silc.p2.ppp.store %>% filter(pb010 %in% year)
  
  
  
  # adjust country weights by population proportion
  # what do we need? 
  
  # # the proportion of a country's EU SILC observations relative to all EU28 observations in a certain year (note: only for those who have positive disposable income)
  
  silc.p1.yprop <- silc.p1.y %>% filter(equivalent_post_tax_disposable_income > 0)
  silc.p2.yprop <- silc.p2.y %>% filter(post_tax_disposable_income > 0)
  
  silc.p1.nobs <- silc.p1.yprop %>% group_by(rb020) %>% summarise(nobs_c = n())
  silc.p2.nobs <- silc.p2.yprop %>% group_by(pb020) %>% summarise(nobs_c = n())
  
  silc.p1.nobs <- silc.p1.nobs %>% mutate(silc.prop = nobs_c/sum(nobs_c))
  silc.p2.nobs <- silc.p2.nobs %>% mutate(silc.prop = nobs_c/sum(nobs_c))

  # # the proportion of a country's population relative to the population of all EU28 countries
  pop.y <- pop %>% filter(time %in% year)

  pop.nobs <- pop.y %>% filter(geo %in% c(levels(as.factor(silc.p1.y$rb020))))
  pop.nobs <- pop.nobs %>% mutate(pop.prop = values/sum(values))
  
  # calculate population adjustment weights
  
  silc.p1.nobs <- silc.p1.nobs %>% mutate(w_adj = pop.nobs$pop.prop/silc.prop)
  silc.p2.nobs <- silc.p2.nobs %>% mutate(w_adj = pop.nobs$pop.prop/silc.prop)

  # adjust weights rb050 and pb040 by country
  silc.p1.y <- left_join(silc.p1.y, silc.p1.nobs %>% select(rb020, w_adj))
  silc.p2.y <- left_join(silc.p2.y, silc.p2.nobs %>% select(pb020, w_adj))
  
  silc.p1.y <- silc.p1.y %>% mutate(rb050 = rb050 * w_adj)
  silc.p2.y <- silc.p2.y %>% mutate(pb040 = pb040 * w_adj)
  

  # countries per year
  
  indicators[year-2003, 35,1] <- toString(levels(as.factor(silc.p1.y$rb020))) 
  indicators[year-2003, 36,1] <- nlevels(as.factor(silc.p1.y$rb020))
  
  indicators[year-2003, 35,2] <- toString(levels(as.factor(silc.p2.y$pb020))) 
  indicators[year-2003, 36,2] <- nlevels(as.factor(silc.p2.y$pb020))
  
  
  # svy designs
  silc.p1.svy <- svydesign(ids =  ~ id_h,
                           strata = ~db040,
                           weights = ~rb050,
                           data = silc.p1.y) %>% convey_prep()
  
  silc.p2.svy <- svydesign(ids = ~id_h,
                           strata = ~db040,
                           weights = ~pb040,
                           data = silc.p2.y) %>% convey_prep()
  
  
  # Indicators --------------------------------------------------------------
  
  
  
  # Mean Income
  #
  
  
  indicators[year-2003, 2, 1] <- svymean(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, equivalent_pre_tax_factor_income > 0))
  indicators[year-2003, 3, 1] <- svymean(~equivalent_pre_tax_national_income, subset(silc.p1.svy, equivalent_pre_tax_national_income > 0))
  indicators[year-2003, 4, 1] <- svymean(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, equivalent_post_tax_disposable_income > 0))
  indicators[year-2003, 5, 1] <- svymean(~equivalent_pre_tax_factor_income_imputed, subset(silc.p1.svy, equivalent_pre_tax_factor_income_imputed > 0))
  indicators[year-2003, 6, 1] <- svymean(~equivalent_post_tax_disposable_income_imputed, subset(silc.p1.svy, equivalent_post_tax_disposable_income_imputed > 0))
  
  indicators[year-2003, 2, 2] <- svymean(~pre_tax_factor_income, subset(silc.p2.svy, pre_tax_factor_income > 0))
  indicators[year-2003, 3, 2] <- svymean(~pre_tax_national_income, subset(silc.p2.svy, pre_tax_national_income > 0))
  indicators[year-2003, 4, 2] <- svymean(~post_tax_disposable_income, subset(silc.p2.svy, post_tax_disposable_income > 0))

  # For comparing countries
  # svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svymean)
  # svyby(~hy010, ~as.factor(db020), silc.hd.svy, svymean)
  
  # Median Income
  #
  
  indicators[year-2003, 7, 1] <- svyquantile(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, equivalent_pre_tax_factor_income > 0), quantiles = c(0.5))
  indicators[year-2003, 8, 1] <- svyquantile(~equivalent_pre_tax_national_income, subset(silc.p1.svy, equivalent_pre_tax_national_income > 0), quantiles = c(0.5))
  indicators[year-2003, 9, 1] <- svyquantile(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, equivalent_post_tax_disposable_income > 0), quantiles = c(0.5))
  indicators[year-2003, 10, 1] <- svyquantile(~equivalent_pre_tax_factor_income_imputed, subset(silc.p1.svy, equivalent_pre_tax_factor_income_imputed > 0), quantiles = c(0.5))
  indicators[year-2003, 11, 1] <- svyquantile(~equivalent_post_tax_disposable_income_imputed, subset(silc.p1.svy, equivalent_post_tax_disposable_income_imputed > 0), quantiles = c(0.5))
  
  indicators[year-2003, 7, 2] <- svyquantile(~pre_tax_factor_income, subset(silc.p2.svy, pre_tax_factor_income > 0), quantiles = c(0.5))
  indicators[year-2003, 8, 2] <- svyquantile(~pre_tax_national_income, subset(silc.p2.svy, pre_tax_national_income > 0), quantiles = c(0.5))
  indicators[year-2003, 9, 2] <- svyquantile(~post_tax_disposable_income, subset(silc.p2.svy, post_tax_disposable_income > 0), quantiles = c(0.5))
  
  # For comparing countries
  # svyby(~total.inc, ~as.factor(db020), silc.pd.svy,
  #       svyquantile, ~total.inc, quantiles = c(0.5), keep.var = FALSE)
  # svyby(~hy010, ~as.factor(db020), silc.hd.svy,
  #       svyquantile, ~hy010, quantiles = c(0.5), keep.var = FALSE)
  # 
  # # Decile Points
  # #
  # svyquantile(~total.inc, silc.pd.svy, quantiles = seq(0, 1, 0.1))
  # svyquantile(~hy010, silc.hd.svy, quantiles = seq(0, 1, 0.1))
  # # For comparing countries
  # svyby(~total.inc, ~as.factor(db020), silc.pd.svy, 
  #       svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)
  # svyby(~hy010, ~as.factor(hb020), silc.pd.svy, 
  #       svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)
  
  # Quantile Share Ratio
  #
 
  indicators[year-2003, 12, 1] <- svyqsr(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, equivalent_pre_tax_factor_income > 0), 0.2, 0.8)
  indicators[year-2003, 13, 1] <- svyqsr(~equivalent_pre_tax_national_income, subset(silc.p1.svy, equivalent_pre_tax_national_income > 0), 0.2, 0.8)
  indicators[year-2003, 14, 1] <- svyqsr(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, equivalent_post_tax_disposable_income > 0), 0.2, 0.8)
  indicators[year-2003, 15, 1] <- svyqsr(~equivalent_pre_tax_factor_income_imputed, subset(silc.p1.svy, equivalent_pre_tax_factor_income_imputed > 0), 0.2, 0.8)
  indicators[year-2003, 16, 1] <- svyqsr(~equivalent_post_tax_disposable_income_imputed, subset(silc.p1.svy, equivalent_post_tax_disposable_income_imputed > 0), 0.2, 0.8)
  
  indicators[year-2003, 12, 2] <- svyqsr(~pre_tax_factor_income, subset(silc.p2.svy, pre_tax_factor_income > 0), 0.2, 0.8)
  indicators[year-2003, 13, 2] <- svyqsr(~pre_tax_national_income, subset(silc.p2.svy, pre_tax_national_income > 0), 0.2, 0.8)
  indicators[year-2003, 14, 2] <- svyqsr(~post_tax_disposable_income, subset(silc.p2.svy, post_tax_disposable_income > 0), 0.2, 0.8)
  
  
  # For comparing countries
  # svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svyqsr, 0.2, 0.8)
  # svyby(~hy010, ~as.factor(db020), silc.hd.svy, svyqsr, 0.2, 0.8)
  
  # Top 10% Income Share
  #
  indicators[year-2003, 17, 1] <-  svytotal(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, equivalent_pre_tax_factor_income >= 
                                                                 as.numeric(svyquantile(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, equivalent_pre_tax_factor_income > 0), quantile = 0.9)))) / 
    svytotal(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, equivalent_pre_tax_factor_income > 0))
  indicators[year-2003, 18, 1] <-  svytotal(~equivalent_pre_tax_national_income, subset(silc.p1.svy, equivalent_pre_tax_national_income >= 
                                                                 as.numeric(svyquantile(~equivalent_pre_tax_national_income, subset(silc.p1.svy, equivalent_pre_tax_national_income > 0), quantile = 0.9)))) / 
    svytotal(~equivalent_pre_tax_national_income, subset(silc.p1.svy, equivalent_pre_tax_national_income > 0))
  indicators[year-2003, 19, 1] <-  svytotal(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, equivalent_post_tax_disposable_income >= 
                                                                 as.numeric(svyquantile(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, equivalent_post_tax_disposable_income > 0), quantile = 0.9)))) / 
    svytotal(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, equivalent_post_tax_disposable_income > 0))
  indicators[year-2003, 20, 1] <-  svytotal(~equivalent_pre_tax_factor_income_imputed, subset(silc.p1.svy, equivalent_pre_tax_factor_income_imputed >= 
                                                                 as.numeric(svyquantile(~equivalent_pre_tax_factor_income_imputed, subset(silc.p1.svy, equivalent_pre_tax_factor_income_imputed > 0), quantile = 0.9)))) / 
    svytotal(~equivalent_pre_tax_factor_income_imputed, subset(silc.p1.svy, equivalent_pre_tax_factor_income_imputed > 0))
  indicators[year-2003, 21, 1] <-  svytotal(~equivalent_post_tax_disposable_income_imputed, subset(silc.p1.svy, equivalent_post_tax_disposable_income_imputed >= 
                                                                 as.numeric(svyquantile(~equivalent_post_tax_disposable_income_imputed, subset(silc.p1.svy, equivalent_post_tax_disposable_income_imputed > 0), quantile = 0.9)))) / 
    svytotal(~equivalent_post_tax_disposable_income_imputed, subset(silc.p1.svy, equivalent_post_tax_disposable_income_imputed > 0))
  
  
  indicators[year-2003, 17, 2] <- svytotal(~pre_tax_factor_income, subset(silc.p2.svy, pre_tax_factor_income >= 
                                                                                       as.numeric(svyquantile(~pre_tax_factor_income, subset(silc.p2.svy, pre_tax_factor_income > 0), quantile = 0.9)))) / 
    svytotal(~pre_tax_factor_income, subset(silc.p2.svy, pre_tax_factor_income > 0))
  indicators[year-2003, 18, 2] <-  svytotal(~pre_tax_national_income, subset(silc.p2.svy, pre_tax_national_income >= 
                                                                                          as.numeric(svyquantile(~pre_tax_national_income, subset(silc.p2.svy, pre_tax_national_income > 0), quantile = 0.9)))) / 
    svytotal(~pre_tax_national_income, subset(silc.p2.svy, pre_tax_national_income > 0))
  indicators[year-2003, 19, 2] <-  svytotal(~post_tax_disposable_income, subset(silc.p2.svy, post_tax_disposable_income >= 
                                                                                             as.numeric(svyquantile(~post_tax_disposable_income, subset(silc.p2.svy, post_tax_disposable_income > 0), quantile = 0.9)))) / 
    svytotal(~post_tax_disposable_income, subset(silc.p2.svy, post_tax_disposable_income > 0))
  
  

  
  # Gini Coefficient
  #

  indicators[year-2003, 22, 1] <- svygini(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, equivalent_pre_tax_factor_income > 0))
  indicators[year-2003, 23, 1] <- svygini(~equivalent_pre_tax_national_income, subset(silc.p1.svy, equivalent_pre_tax_national_income > 0))
  indicators[year-2003, 24, 1] <- svygini(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, equivalent_post_tax_disposable_income > 0))
  indicators[year-2003, 25, 1] <- svygini(~equivalent_pre_tax_factor_income_imputed, subset(silc.p1.svy, equivalent_pre_tax_factor_income_imputed > 0))
  indicators[year-2003, 26, 1] <- svygini(~equivalent_post_tax_disposable_income_imputed, subset(silc.p1.svy, equivalent_post_tax_disposable_income_imputed > 0))
  
  indicators[year-2003, 22, 2] <- svygini(~pre_tax_factor_income, subset(silc.p2.svy, pre_tax_factor_income > 0))
  indicators[year-2003, 23, 2] <- svygini(~pre_tax_national_income, subset(silc.p2.svy, pre_tax_national_income > 0))
  indicators[year-2003, 24, 2] <- svygini(~post_tax_disposable_income, subset(silc.p2.svy, post_tax_disposable_income > 0))
  
  
  # For comparing countries
  # svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svygini)
  # svyby(~hy010, ~as.factor(db020), silc.hd.svy, svygini)
  
  # Theil Index
  #
  
  indicators[year-2003, 27, 1] <- svygei(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, equivalent_pre_tax_factor_income > 0), epsilon = 1)
  indicators[year-2003, 28, 1] <- svygei(~equivalent_pre_tax_national_income, subset(silc.p1.svy, equivalent_pre_tax_national_income > 0), epsilon = 1)
  indicators[year-2003, 29, 1] <- svygei(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, equivalent_post_tax_disposable_income > 0), epsilon = 1)
  indicators[year-2003, 30, 1] <- svygei(~equivalent_pre_tax_factor_income_imputed, subset(silc.p1.svy, equivalent_pre_tax_factor_income_imputed > 0), epsilon = 1)
  indicators[year-2003, 31, 1] <- svygei(~equivalent_post_tax_disposable_income_imputed, subset(silc.p1.svy, equivalent_post_tax_disposable_income_imputed > 0), epsilon = 1)
  
  indicators[year-2003, 27, 2] <- svygei(~pre_tax_factor_income, subset(silc.p2.svy, pre_tax_factor_income > 0), epsilon = 1)
  indicators[year-2003, 28, 2] <- svygei(~pre_tax_national_income, subset(silc.p2.svy, pre_tax_national_income > 0), epsilon = 1)
  indicators[year-2003, 29, 2] <- svygei(~post_tax_disposable_income, subset(silc.p2.svy, post_tax_disposable_income > 0), epsilon = 1)
  
#   # For comparing countries
#   # svyby(~total.inc, ~as.factor(db020), silc.pd.svy,
#   #      svygei, epsilon = 1)
#   # svyby(~hy010, ~as.factor(db020), silc.hd.svy,
#   #      svygei, epsilon = 1)
#   
#   
#   # Theil Index Decomposition
# # 
# #   svygeidec(~equivalent_pre_tax_factor_income, ~rb020,silc.p1.svy, epsilon = 1)
# #   table(silc.p1.y$rb020)
# 
# 
#   # calculate sum of total population for all countries
#   pop.y <- pop %>% filter(time %in% year)
#   pop_sum <- pop.y %>% filter(geo %in% c(levels(as.factor(silc.p1.y$rb020)))) %>% summarise(pop_sum = sum(values))
# 
# 
#   # first theil loop p1
#   
#   for(country in 1:nlevels(as.factor(silc.p1.y$rb020))){
#     # colnames
#     colnames(theil)[1+(year-2004)*13] <- paste0("countries",year)
#     colnames(theil)[2+(year-2004)*13] <- paste0("fi.theil",year)
#     colnames(theil)[3+(year-2004)*13] <- paste0("fi.mean",year)
#     colnames(theil)[4+(year-2004)*13] <- paste0("fi.econ.weight",year)
#     colnames(theil)[5+(year-2004)*13] <- paste0("fi.ineq.share",year)
#     colnames(theil)[6+(year-2004)*13] <- paste0("ni.theil",year)
#     colnames(theil)[7+(year-2004)*13] <- paste0("ni.mean",year)
#     colnames(theil)[8+(year-2004)*13] <- paste0("ni.econ.weight",year)
#     colnames(theil)[9+(year-2004)*13] <- paste0("ni.ineq.share",year)
#     colnames(theil)[10+(year-2004)*13] <- paste0("di.theil",year)
#     colnames(theil)[11+(year-2004)*13] <- paste0("di.mean",year)
#     colnames(theil)[12+(year-2004)*13] <- paste0("di.econ.weight",year)
#     colnames(theil)[13+(year-2004)*13] <- paste0("di.ineq.share",year)
#     
#     # country population
#     pop_c <- pop.y$values[pop.y$geo==levels(as.factor(silc.p1.y$rb020))[country]]
# 
#     # country
#     theil[country,1+(year-2004)*13,1] <- levels(as.factor(silc.p1.y$rb020))[country]
#     # country theil
#     theil[country,2+(year-2004)*13,1] <- svygei(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, rb020==levels(as.factor(silc.p1.y$rb020))[country] & equivalent_pre_tax_factor_income > 0), epsilon = 1)
#     # country mean
#     theil[country,3+(year-2004)*13,1] <- svymean(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, rb020==levels(as.factor(silc.p1.y$rb020))[country]& equivalent_pre_tax_factor_income > 0))
#     # country econ weight
#     theil[country,4+(year-2004)*13,1] <- (pop_c*as.numeric(theil[country,3+(year-2004)*13,1]))/(as.numeric(pop_sum)*as.numeric(indicators[year-2003, 2, 1]))
#     
#     # country ineq share
#     #theil[country,5+(year-2004)*13,1] <- (theil[country,4+(year-2004)*13,1]*theil[country,2+(year-2004)*13,1])/
#     
#     
#     # country theil
#     theil[country,6+(year-2004)*13,1] <- svygei(~equivalent_pre_tax_national_income, subset(silc.p1.svy, rb020==levels(as.factor(silc.p1.y$rb020))[country]& equivalent_pre_tax_national_income > 0), epsilon = 1)
#     # country mean
#     theil[country,7+(year-2004)*13,1] <- svymean(~equivalent_pre_tax_national_income, subset(silc.p1.svy, rb020==levels(as.factor(silc.p1.y$rb020))[country]& equivalent_pre_tax_national_income > 0))
#     # country econ weight
#     theil[country,8+(year-2004)*13,1] <- (pop_c*as.numeric(theil[country,3+(year-2004)*13,1]))/(as.numeric(pop_sum)*as.numeric(indicators[year-2003, 2, 1]))
#     
#     # country theil
#     theil[country,10+(year-2004)*13,1] <- svygei(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, rb020==levels(as.factor(silc.p1.y$rb020))[country]&equivalent_post_tax_disposable_income>0), epsilon = 1)
#     # country mean
#     theil[country,11+(year-2004)*13,1] <- svymean(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, rb020==levels(as.factor(silc.p1.y$rb020))[country]&equivalent_post_tax_disposable_income>0))
#     # country econ weight
#     theil[country,12+(year-2004)*13,1] <- (pop_c*as.numeric(theil[country,3+(year-2004)*13,1]))/(as.numeric(pop_sum)*as.numeric(indicators[year-2003, 2, 1]))
#     
#     
#     }
# 
#   
#   # first theil loop p2
#   
#   for(country in 1:nlevels(as.factor(silc.p2.y$pb020))){
#     # country population
#     pop_c <- pop.y$values[pop.y$geo==levels(as.factor(silc.p2.y$pb020))[country]]
#     
#     # country
#     theil[country,1+(year-2004)*13,2] <- levels(as.factor(silc.p2.y$pb020))[country]
#     # country theil
#     theil[country,2+(year-2004)*13,2] <- svygei(~pre_tax_factor_income, subset(silc.p2.svy, pb020==levels(as.factor(silc.p2.y$pb020))[country]&pre_tax_factor_income > 0), epsilon = 1)
#     # country mean
#     theil[country,3+(year-2004)*13,2] <- svymean(~pre_tax_factor_income, subset(silc.p2.svy, pb020==levels(as.factor(silc.p2.y$pb020))[country]&pre_tax_factor_income > 0))
#     # country econ weight
#     theil[country,4+(year-2004)*13,2] <- (pop_c*as.numeric(theil[country,3+(year-2004)*13,2]))/(as.numeric(pop_sum)*as.numeric(indicators[year-2003, 2, 2]))
#     
#     # country ineq share
#     #theil[country,5+(year-2004)*13,2] <- (theil[country,4+(year-2004)*13,2]*theil[country,2+(year-2004)*13,2])/
#     
#     
#     # country theil
#     theil[country,6+(year-2004)*13,2] <- svygei(~pre_tax_national_income, subset(silc.p2.svy, pb020==levels(as.factor(silc.p2.y$pb020))[country]& pre_tax_national_income > 0), epsilon = 1)
#     # country mean
#     theil[country,7+(year-2004)*13,2] <- svymean(~pre_tax_national_income, subset(silc.p2.svy, pb020==levels(as.factor(silc.p2.y$pb020))[country]& pre_tax_national_income > 0))
#     # country econ weight
#     theil[country,8+(year-2004)*13,2] <- (pop_c*as.numeric(theil[country,3+(year-2004)*13,2]))/(as.numeric(pop_sum)*as.numeric(indicators[year-2003, 2, 2]))
#     
#     # country theil
#     theil[country,10+(year-2004)*13,2] <- svygei(~post_tax_disposable_income, subset(silc.p2.svy, pb020==levels(as.factor(silc.p2.y$pb020))[country]&post_tax_disposable_income>0), epsilon = 1)
#     # country mean
#     theil[country,11+(year-2004)*13,2] <- svymean(~post_tax_disposable_income, subset(silc.p2.svy, pb020==levels(as.factor(silc.p2.y$pb020))[country]&post_tax_disposable_income>0))
#     # country econ weight
#     theil[country,12+(year-2004)*13,2] <- (pop_c*as.numeric(theil[country,3+(year-2004)*13,2]))/(as.numeric(pop_sum)*as.numeric(indicators[year-2003, 2, 2]))
#     
#     
#   }
#   
#  
# 
#   # calculate Theil manually
#   indicators[year-2003, 32, 1] <-  t(na.exclude(as.numeric(theil[,2+(year-2004)*13,1])))%*%na.exclude(as.numeric(theil[,4+(year-2004)*13,1])) + t(na.exclude(as.numeric(theil[,4+(year-2004)*13,1])))%*%(log(na.exclude(as.numeric(theil[,3+(year-2004)*13,1]))/as.numeric(indicators[year-2003, 2, 1])))
#   indicators[year-2003, 33, 1] <- t(na.exclude(as.numeric(theil[,6+(year-2004)*13,1])))%*%na.exclude(as.numeric(theil[,8+(year-2004)*13,1])) + t(na.exclude(as.numeric(theil[,8+(year-2004)*13,1])))%*%(log(na.exclude(as.numeric(theil[,7+(year-2004)*13,1]))/as.numeric(indicators[year-2003, 3, 1])))
#   indicators[year-2003, 34, 1] <- t(na.exclude(as.numeric(theil[,10+(year-2004)*13,1])))%*%na.exclude(as.numeric(theil[,12+(year-2004)*13,1])) + t(na.exclude(as.numeric(theil[,12+(year-2004)*13,1])))%*%(log(na.exclude(as.numeric(theil[,11+(year-2004)*13,1]))/as.numeric(indicators[year-2003, 4, 1])))
#     
#   indicators[year-2003, 32, 2] <-  t(na.exclude(as.numeric(theil[,2+(year-2004)*13,2])))%*%na.exclude(as.numeric(theil[,4+(year-2004)*13,2])) + t(na.exclude(as.numeric(theil[,4+(year-2004)*13,2])))%*%(log(na.exclude(as.numeric(theil[,3+(year-2004)*13,2]))/as.numeric(indicators[year-2003, 2, 2])))
#   indicators[year-2003, 33, 2] <- t(na.exclude(as.numeric(theil[,6+(year-2004)*13,2])))%*%na.exclude(as.numeric(theil[,8+(year-2004)*13,2])) + t(na.exclude(as.numeric(theil[,8+(year-2004)*13,2])))%*%(log(na.exclude(as.numeric(theil[,7+(year-2004)*13,2]))/as.numeric(indicators[year-2003, 3, 2])))
#   indicators[year-2003, 34, 2] <- t(na.exclude(as.numeric(theil[,10+(year-2004)*13,2])))%*%na.exclude(as.numeric(theil[,12+(year-2004)*13,2])) + t(na.exclude(as.numeric(theil[,12+(year-2004)*13,2])))%*%(log(na.exclude(as.numeric(theil[,11+(year-2004)*13,2]))/as.numeric(indicators[year-2003, 4, 2])))
#     
# 
#   # 2nd theil loop p1 - calculate ineq shares
#   
#   for(country in 1:nlevels(as.factor(silc.p1.y$rb020))){
# 
#     # country ineq share
#     theil[country,5+(year-2004)*13,1] <- (as.numeric(theil[country,4+(year-2004)*13,1])*as.numeric(theil[country,2+(year-2004)*13,1]))/as.numeric(indicators[year-2003, 32, 1])
#     
#     theil[country,9+(year-2004)*13,1] <- (as.numeric(theil[country,8+(year-2004)*13,1])*as.numeric(theil[country,6+(year-2004)*13,1]))/as.numeric(indicators[year-2003, 33, 1])
#     
#     theil[country,13+(year-2004)*13,1] <- (as.numeric(theil[country,12+(year-2004)*13,1])*as.numeric(theil[country,10+(year-2004)*13,1]))/as.numeric(indicators[year-2003, 34, 1])
#     
#   }
# 
#   
#   # 2nd theil loop p2 - calculate ineq shares
#   
#   for(country in 1:nlevels(as.factor(silc.p2.y$pb020))){
#     
#     # country ineq share
#     theil[country,5+(year-2004)*13,2] <- (as.numeric(theil[country,4+(year-2004)*13,2])*as.numeric(theil[country,2+(year-2004)*13,2]))/as.numeric(indicators[year-2003, 32, 2])
#     
#     theil[country,9+(year-2004)*13,2] <- (as.numeric(theil[country,8+(year-2004)*13,2])*as.numeric(theil[country,6+(year-2004)*13,2]))/as.numeric(indicators[year-2003, 33, 2])
#     
#     theil[country,13+(year-2004)*13,2] <- (as.numeric(theil[country,12+(year-2004)*13,2])*as.numeric(theil[country,10+(year-2004)*13,2]))/as.numeric(indicators[year-2003, 34, 2])
#     
#   }
  
}



# create dataframes with indicators for p1 and p2 and name them

indicators.p1.w_adj <- as.data.frame(indicators[,,1])
indicators.p2.w_adj <- as.data.frame(indicators[,,2])

#theil.p1 <- as.data.frame(theil[,,1])
#theil.p2 <- as.data.frame(theil[,,2])


indicators.p1.w_adj <- indicators.p1.w_adj %>% rename(year = V1, mean_eptfi = V2, mean_eptni = V3, mean_eptdi = V4, mean_eptfii = V5, mean_eptdii = V6, median_eptfi = V7, median_eptni = V8, median_eptdi = V9, median_eptfii = V10, median_eptdii = V11, qsr8020_eptfi = V12, qsr8020_eptni = V13, qsr8020_eptdi = V14, qsr8020_eptfii = V15, qsr8020_eptdii = V16, top10_eptfi = V17, top10_eptni = V18, top10_eptdi = V19, top10_eptfii = V20, top10_eptdii = V21, gini_eptfi = V22, gini_eptni = V23, gini_eptdi = V24, gini_eptfii = V25, gini_eptdii = V26, theil_eptfi = V27, theil_eptni = V28, theil_eptdi = V29, theil_eptfii = V30, theil_eptdii = V31, theil_fi_manual = V32, theil_ni_manual = V33, theil_di_manual = V34, countries = V35, ncountries = V36)

indicators.p2.w_adj <- indicators.p2.w_adj %>% rename(year = V1, mean_ptfi = V2, mean_ptni = V3, mean_ptdi = V4, mean_ptfii = V5, mean_ptdii = V6, median_ptfi = V7, median_ptni = V8, median_ptdi = V9, median_ptfii = V10, median_ptdii = V11, qsr8020_ptfi = V12, qsr8020_ptni = V13, qsr8020_ptdi = V14, qsr8020_ptfii = V15, qsr8020_ptdii = V16, top10_ptfi = V17, top10_ptni = V18, top10_ptdi = V19, top10_ptfii = V20, top10_ptdii = V21, gini_ptfi = V22, gini_ptni = V23, gini_ptdi = V24, gini_ptfii = V25, gini_ptdii = V26, theil_ptfi = V27, theil_ptni = V28, theil_ptdi = V29, theil_ptfii = V30, theil_ptdii = V31, theil_fi_manual = V32, theil_ni_manual = V33, theil_di_manual = V34, countries = V35, ncountries = V36)




# save data ---------------------------------------------------------------




save(indicators.p1.w_adj, indicators.p2.w_adj, file = "./data/eu28_indicators_w_adj.RData")
