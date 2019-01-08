## calculate inequality indicators
##################################


# load packages and data -----------------------------------------------------------

library(dplyr)
library(survey)
library(convey)

load("./data/silc_eu28.RData")



# Subsetting? --------------------------------------------------------------

# # To get useful results we may want to subset to only positive income
silc.p1.ppp.store <- silc.p1.ppp.store %>% filter_at(vars(equivalent_pre_tax_factor_income:equivalent_post_tax_disposable_income), all_vars(. > 0))
silc.p2.ppp.store <- silc.p2.ppp.store %>% filter_at(vars(pre_tax_factor_income:post_tax_disposable_income), all_vars(. > 0))



# loop to calculate indicators for each year ------------------------------

indicators <- array(NA, c(14, 40, 2))


for(year in 2004:2017){
  
  
  
  indicators[year-2003, 1,] <- year
  
  # Creating Survey Objects -------------------------------------------------
  
  
  silc.p1.y <- silc.p1.ppp.store %>% filter(rb010 %in% year)
  silc.p2.y <- silc.p2.ppp.store %>% filter(pb010 %in% year)

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
  
  
  indicators[year-2003, 2, 1] <- svymean(~equivalent_pre_tax_factor_income, silc.p1.svy)
  indicators[year-2003, 3, 1] <- svymean(~equivalent_pre_tax_national_income, silc.p1.svy)
  indicators[year-2003, 4, 1] <- svymean(~equivalent_post_tax_disposable_income, silc.p1.svy)
  indicators[year-2003, 5, 1] <- svymean(~equivalent_pre_tax_factor_income_imputed, silc.p1.svy)
  indicators[year-2003, 6, 1] <- svymean(~equivalent_post_tax_disposable_income_imputed, silc.p1.svy)
  
  indicators[year-2003, 2, 2] <- svymean(~pre_tax_factor_income, silc.p2.svy)
  indicators[year-2003, 3, 2] <- svymean(~pre_tax_national_income, silc.p2.svy)
  indicators[year-2003, 4, 2] <- svymean(~post_tax_disposable_income, silc.p2.svy)

  # For comparing countries
  # svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svymean)
  # svyby(~hy010, ~as.factor(db020), silc.hd.svy, svymean)
  
  # Median Income
  #
  
  indicators[year-2003, 7, 1] <- svyquantile(~equivalent_pre_tax_factor_income, silc.p1.svy, quantiles = c(0.5))
  indicators[year-2003, 8, 1] <- svyquantile(~equivalent_pre_tax_national_income, silc.p1.svy, quantiles = c(0.5))
  indicators[year-2003, 9, 1] <- svyquantile(~equivalent_post_tax_disposable_income, silc.p1.svy, quantiles = c(0.5))
  indicators[year-2003, 10, 1] <- svyquantile(~equivalent_pre_tax_factor_income_imputed, silc.p1.svy, quantiles = c(0.5))
  indicators[year-2003, 11, 1] <- svyquantile(~equivalent_post_tax_disposable_income_imputed, silc.p1.svy, quantiles = c(0.5))
  
  indicators[year-2003, 7, 2] <- svyquantile(~pre_tax_factor_income, silc.p2.svy, quantiles = c(0.5))
  indicators[year-2003, 8, 2] <- svyquantile(~pre_tax_national_income, silc.p2.svy, quantiles = c(0.5))
  indicators[year-2003, 9, 2] <- svyquantile(~post_tax_disposable_income, silc.p2.svy, quantiles = c(0.5))
  
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
 
  indicators[year-2003, 12, 1] <- svyqsr(~equivalent_pre_tax_factor_income, silc.p1.svy, 0.2, 0.8)
  indicators[year-2003, 13, 1] <- svyqsr(~equivalent_pre_tax_national_income, silc.p1.svy, 0.2, 0.8)
  indicators[year-2003, 14, 1] <- svyqsr(~equivalent_post_tax_disposable_income, silc.p1.svy, 0.2, 0.8)
  indicators[year-2003, 15, 1] <- svyqsr(~equivalent_pre_tax_factor_income_imputed, silc.p1.svy, 0.2, 0.8)
  indicators[year-2003, 16, 1] <- svyqsr(~equivalent_post_tax_disposable_income_imputed, silc.p1.svy, 0.2, 0.8)
  
  indicators[year-2003, 12, 2] <- svyqsr(~pre_tax_factor_income, silc.p2.svy, 0.2, 0.8)
  indicators[year-2003, 13, 2] <- svyqsr(~pre_tax_national_income, silc.p2.svy, 0.2, 0.8)
  indicators[year-2003, 14, 2] <- svyqsr(~post_tax_disposable_income, silc.p2.svy, 0.2, 0.8)
  
  
  # For comparing countries
  # svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svyqsr, 0.2, 0.8)
  # svyby(~hy010, ~as.factor(db020), silc.hd.svy, svyqsr, 0.2, 0.8)
  
  # Top 10% Income Share
  #
  indicators[year-2003, 17, 1] <-  svytotal(~equivalent_pre_tax_factor_income, subset(silc.p1.svy, equivalent_pre_tax_factor_income >= 
                                                                 as.numeric(svyquantile(~equivalent_pre_tax_factor_income, silc.p1.svy, quantile = 0.9)))) / 
    svytotal(~equivalent_pre_tax_factor_income, silc.p1.svy)
  indicators[year-2003, 18, 1] <-  svytotal(~equivalent_pre_tax_national_income, subset(silc.p1.svy, equivalent_pre_tax_national_income >= 
                                                                 as.numeric(svyquantile(~equivalent_pre_tax_national_income, silc.p1.svy, quantile = 0.9)))) / 
    svytotal(~equivalent_pre_tax_national_income, silc.p1.svy)
  indicators[year-2003, 19, 1] <-  svytotal(~equivalent_post_tax_disposable_income, subset(silc.p1.svy, equivalent_post_tax_disposable_income >= 
                                                                 as.numeric(svyquantile(~equivalent_post_tax_disposable_income, silc.p1.svy, quantile = 0.9)))) / 
    svytotal(~equivalent_post_tax_disposable_income, silc.p1.svy)
  indicators[year-2003, 20, 1] <-  svytotal(~equivalent_pre_tax_factor_income_imputed, subset(silc.p1.svy, equivalent_pre_tax_factor_income_imputed >= 
                                                                 as.numeric(svyquantile(~equivalent_pre_tax_factor_income_imputed, silc.p1.svy, quantile = 0.9)))) / 
    svytotal(~equivalent_pre_tax_factor_income_imputed, silc.p1.svy)
  indicators[year-2003, 21, 1] <-  svytotal(~equivalent_post_tax_disposable_income_imputed, subset(silc.p1.svy, equivalent_post_tax_disposable_income_imputed >= 
                                                                 as.numeric(svyquantile(~equivalent_post_tax_disposable_income_imputed, silc.p1.svy, quantile = 0.9)))) / 
    svytotal(~equivalent_post_tax_disposable_income_imputed, silc.p1.svy)
  
  
  indicators[year-2003, 17, 2] <- svytotal(~pre_tax_factor_income, subset(silc.p2.svy, pre_tax_factor_income >= 
                                                                                       as.numeric(svyquantile(~pre_tax_factor_income, silc.p2.svy, quantile = 0.9)))) / 
    svytotal(~pre_tax_factor_income, silc.p2.svy)
  indicators[year-2003, 18, 2] <-  svytotal(~pre_tax_national_income, subset(silc.p2.svy, pre_tax_national_income >= 
                                                                                          as.numeric(svyquantile(~pre_tax_national_income, silc.p2.svy, quantile = 0.9)))) / 
    svytotal(~pre_tax_national_income, silc.p2.svy)
  indicators[year-2003, 19, 2] <-  svytotal(~post_tax_disposable_income, subset(silc.p2.svy, post_tax_disposable_income >= 
                                                                                             as.numeric(svyquantile(~post_tax_disposable_income, silc.p2.svy, quantile = 0.9)))) / 
    svytotal(~post_tax_disposable_income, silc.p2.svy)
  
  

  
  # Gini Coefficient
  #

  indicators[year-2003, 22, 1] <- svygini(~equivalent_pre_tax_factor_income, silc.p1.svy)
  indicators[year-2003, 23, 1] <- svygini(~equivalent_pre_tax_national_income, silc.p1.svy)
  indicators[year-2003, 24, 1] <- svygini(~equivalent_post_tax_disposable_income, silc.p1.svy)
  indicators[year-2003, 25, 1] <- svygini(~equivalent_pre_tax_factor_income_imputed, silc.p1.svy)
  indicators[year-2003, 26, 1] <- svygini(~equivalent_post_tax_disposable_income_imputed, silc.p1.svy)
  
  indicators[year-2003, 22, 2] <- svygini(~pre_tax_factor_income, silc.p2.svy)
  indicators[year-2003, 23, 2] <- svygini(~pre_tax_national_income, silc.p2.svy)
  indicators[year-2003, 24, 2] <- svygini(~post_tax_disposable_income, silc.p2.svy)
  
  
  # For comparing countries
  # svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svygini)
  # svyby(~hy010, ~as.factor(db020), silc.hd.svy, svygini)
  
  # Theil Index
  #
  
  indicators[year-2003, 27, 1] <- svygei(~equivalent_pre_tax_factor_income, silc.p1.svy, epsilon = 1)
  indicators[year-2003, 28, 1] <- svygei(~equivalent_pre_tax_national_income, silc.p1.svy, epsilon = 1)
  indicators[year-2003, 29, 1] <- svygei(~equivalent_post_tax_disposable_income, silc.p1.svy, epsilon = 1)
  indicators[year-2003, 30, 1] <- svygei(~equivalent_pre_tax_factor_income_imputed, silc.p1.svy, epsilon = 1)
  indicators[year-2003, 31, 1] <- svygei(~equivalent_post_tax_disposable_income_imputed, silc.p1.svy, epsilon = 1)
  
  indicators[year-2003, 27, 2] <- svygei(~pre_tax_factor_income, silc.p2.svy, epsilon = 1)
  indicators[year-2003, 28, 2] <- svygei(~pre_tax_national_income, silc.p2.svy, epsilon = 1)
  indicators[year-2003, 29, 2] <- svygei(~post_tax_disposable_income, silc.p2.svy, epsilon = 1)
  
  # For comparing countries
  # svyby(~total.inc, ~as.factor(db020), silc.pd.svy,
  #      svygei, epsilon = 1)
  # svyby(~hy010, ~as.factor(db020), silc.hd.svy,
  #      svygei, epsilon = 1)
  
  
}



# create dataframes with indicators for p1 and p2 and name them

indicators.p1 <- as.data.frame(indicators[,,1])
indicators.p2 <- as.data.frame(indicators[,,2])

indicators.p1 <- indicators.p1 %>% rename(year = V1, mean_eptfi = V2, mean_eptni = V3, mean_eptdi = V4, mean_eptfii = V5, mean_eptdii = V6, median_eptfi = V7, median_eptni = V8, median_eptdi = V9, median_eptfii = V10, median_eptdii = V11, qsr8020_eptfi = V12, qsr8020_eptni = V13, qsr8020_eptdi = V14, qsr8020_eptfii = V15, qsr8020_eptdii = V16, top10_eptfi = V17, top10_eptni = V18, top10_eptdi = V19, top10_eptfii = V20, top10_eptdii = V21, gini_eptfi = V22, gini_eptni = V23, gini_eptdi = V24, gini_eptfii = V25, gini_eptdii = V26, theil_eptfi = V27, theil_eptni = V28, theil_eptdi = V29, theil_eptfii = V30, theil_eptdii = V31)

indicators.p2 <- indicators.p2 %>% rename(year = V1, mean_eptfi = V2, mean_eptni = V3, mean_eptdi = V4, mean_eptfii = V5, mean_eptdii = V6, median_eptfi = V7, median_eptni = V8, median_eptdi = V9, median_eptfii = V10, median_eptdii = V11, qsr8020_eptfi = V12, qsr8020_eptni = V13, qsr8020_eptdi = V14, qsr8020_eptfii = V15, qsr8020_eptdii = V16, top10_eptfi = V17, top10_eptni = V18, top10_eptdi = V19, top10_eptfii = V20, top10_eptdii = V21, gini_eptfi = V22, gini_eptni = V23, gini_eptdi = V24, gini_eptfii = V25, gini_eptdii = V26, theil_eptfi = V27, theil_eptni = V28, theil_eptdi = V29, theil_eptfii = V30, theil_eptdii = V31)


