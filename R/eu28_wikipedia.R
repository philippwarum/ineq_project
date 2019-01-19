## EU28 - Coding for Wikipedia
##############################



# load packages -----------------------------------------------------------

library(dplyr)
library(eurostat)
library(reshape2)
library(plyr)

load("./data/silc_eu28.RData")

# get data from EUROSTAT --------------------------------------------------
# get indicators on countrylevel

names <- c("pov_th", "pov", "quants", "qsr", "gini", 
           "inc", "hlth", "pov_reg")
codes <- c("ilc_li01", "ilc_li02", "ilc_di01", "ilc_di11", "ilc_di12", 
           "ilc_di03", "ilc_lk11", "ilc_li41")

df <- ldply(data)

#filter indicators

gini.EU <- filter(df, indic_il =="GINI_HND", time >= "2016")
median.EU <- filter(df, indic_il =="MED_E", time >= "2016", unit=="EUR", sex=="T", age=="TOTAL")
S80_20.EU <- filter(df, indic_il =="S80_S20", time >="2016", sex=="T", age=="TOTAL")
mean.EU <- filter(df, indic_il=="MEI_E", time >="2016",unit=="EUR", sex=="T", age=="TOTAL")



# get data
gini <- get_eurostat(id = "ilc_di12", time_format = "num")

ginid <- dcast(gini, geo ~ time, value.var = c("values"))
ginieu28 <- ginid %>% filter(geo %in% c(levels(as.factor(silc.p1.ppp.store$rb020[which(silc.p1.ppp.store$rb010==2016)]))))


# # check if the EU data is the average (for gini) - what is eurostat showing us there?
# for hy020: W/O PPP we are in the 40s with ppp we are around 34 (already excluding <=0, otherwise higher still)
#average_ginieu28 <- ginieu28 %>% summarise_all(mean, na.rm=TRUE)
# 
# pop <- get_eurostat("naida_10_pe", time_format = "num")
# pop <- pop %>% filter(na_item == "POP_NC")
# popd <- pop %>% dcast(geo ~ time, value.var = c("values"))
# 
# popeu28 <- popd %>% filter(geo %in% c(levels(as.factor(silc.p1.ppp.store$rb020[which(silc.p1.ppp.store$rb010==2016)]))))
# popeu28_w <- popeu28 %>% mutate_at(vars(contains("1975"):contains("2017")),funs(./sum(.)))
# 
# gini_wavg_2016 <- t(ginieu28[,23])%*%popeu28_w[,43]
# gini_wavg_econw_2016 <- t(ginieu28[,23]) %*% na.exclude(as.numeric(as.character(theil.p1$di.econ.weight2016)))
# 
# 
# # check silc gini
# 
# dfhy2016 <- silc.p1.ppp.store %>% filter(rb010 %in% 2016 & equivalent_hy020>0)
# 
# # count(dfhy2016$equivalent_hy020[which(dfhy2016$equivalent_hy020==0)])
# # sum(dfhy2016$equivalent_hy020<0)
# # dfhy2016$equivalent_hy020[which(dfhy2016$equivalent_hy020<0)] <- 0
# 
# 
# hy2016svy <- svydesign(ids =  ~ id_h,
#                          strata = ~db040,
#                          weights = ~rb050,
#                          data = dfhy2016) %>% convey_prep()
# gini_hy2016 <- svygini(~equivalent_hy020, hy2016svy)
# gini_hy2016
