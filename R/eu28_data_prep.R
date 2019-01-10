## Prepare EU28 data
####################



# load packages and data -----------------------------------------------------------

library(dplyr)
library(eurostat)

load("./data/silc_eu28.RData")




# basic data manipulations ------------------------------------------------

# drop Switzerland (CH), Iceland (IS), Norway (NO) and Serbia (RS)
silc.p.store <- silc.p.store %>% filter(pb020 != "CH" & pb020 != "IS" & pb020 != "NO" & pb020 != "RS")
silc.h.store <- silc.h.store %>% filter(hb020 != "CH" & hb020 != "IS" & hb020 != "NO" & hb020 != "RS")
silc.d.store <- silc.d.store %>% filter(db020 != "CH" & db020 != "IS" & db020 != "NO" & db020 != "RS")
silc.r.store <- silc.r.store %>% filter(rb020 != "CH" & rb020 != "IS" & rb020 != "NO" & rb020 != "RS")


# Create unique IDs for merging
silc.p.store <- silc.p.store %>% mutate(id_h = paste0(pb010, pb020, px030))
silc.h.store <- silc.h.store %>% mutate(id_h = paste0(hb010, hb020, hb030))
silc.d.store <- silc.d.store %>% mutate(id_h = paste0(db010, db020, db030))
silc.r.store <- silc.r.store %>% mutate(id_h = paste0(rb010, rb020, rx030))

# replace NAs by 0
silc.p.store[is.na(silc.p.store)] <- 0
silc.h.store[is.na(silc.h.store)] <- 0
silc.d.store[is.na(silc.d.store)] <- 0
silc.r.store[is.na(silc.r.store)] <- 0

# Rename GR to EL from 2004 to 2007
silc.p.store$pb020[silc.p.store$pb020=="GR"] <- "EL"
silc.h.store$hb020[silc.h.store$hb020=="GR"] <- "EL"
silc.d.store$db020[silc.d.store$db020=="GR"] <- "EL"
silc.r.store$rb020[silc.r.store$rb020=="GR"] <- "EL"


# Can we even compute meaningful statistics for the EU28?
table(silc.p1.ppp.store$rb020[silc.p1.ppp.store$rb010==2017])


# income aggregation ------------------------------------------------------


# Merge the datasets
silc.pd.store <- left_join(silc.p.store, silc.d.store)
silc.hd.store <- left_join(silc.h.store, silc.d.store)

# P1 (Eurostat): Gesamte Bevölkerung & equal sharing of resources within household. 

# add up individual incomes within households
silc.p.store_sum <- silc.p.store %>% group_by(id_h) %>% summarise_if(is.numeric, sum)
silc.hdp.store <- left_join(silc.hd.store, silc.p.store_sum)

# summing up individual components
# 1. Pre-tax factor income (Canberra: primary income)
silc.hdp.store <- silc.hdp.store %>% mutate(pre_tax_factor_income = py010g + py021g + py050g + hy110g + hy040g + hy090g + py080g)

# 2. Pre-tax national income
silc.hdp.store <- silc.hdp.store %>% mutate(pre_tax_national_income = pre_tax_factor_income + py090g + py100g)

# 3. Post-tax disposable income
silc.hdp.store <- silc.hdp.store %>% mutate(post_tax_disposable_income = pre_tax_national_income + py110g + py120g + py130g + py140g + hy050g + hy060g + hy070g + hy080g - hy120g - hy130g - hy140g)

# drop HH for which hx050 == 0
table(silc.h.store$hb020[which(silc.h.store$hx050==0)])
silc.hdp.store <- silc.hdp.store %>% filter(hx050!=0)

# calculate equivalent incomes
silc.hdp.store <- silc.hdp.store %>% mutate(equivalent_pre_tax_factor_income = if_else(hx050==0, 0, pre_tax_factor_income/hx050) , 
                                            equivalent_pre_tax_national_income = if_else(hx050==0, 0, pre_tax_national_income/hx050), 
                                            equivalent_post_tax_disposable_income = if_else(hx050==0, 0, post_tax_disposable_income/hx050))

# there are a number of observations where the silc aggregates are available but the components are missing
sum(silc.hdp.store$hy020>0 & silc.hdp.store$post_tax_disposable_income==0)
sum(silc.hdp.store$hy010>0 & silc.hdp.store$pre_tax_factor_income==0)

# we therefore also compute imputed equivalent incomes where we add hy020 and hy010 if they are available and the components are missing
silc.hdp.store <- silc.hdp.store %>% mutate(equivalent_pre_tax_factor_income_imputed = if_else(silc.hdp.store$hy010>0 & silc.hdp.store$pre_tax_factor_income==0, hy010/hx050, equivalent_pre_tax_factor_income), 
                                            equivalent_post_tax_disposable_income_imputed = if_else(silc.hdp.store$hy020>0 & silc.hdp.store$post_tax_disposable_income==0, hy020/hx050, equivalent_post_tax_disposable_income))

# merge with personal register data
silc.rhdp.store <- left_join(silc.r.store, silc.hdp.store %>% select(id_h, hx010, db040, pre_tax_factor_income:equivalent_post_tax_disposable_income_imputed))

# p1 dataset
silc.rhdp.p1.store <- silc.rhdp.store %>% select(-rx030, -(pre_tax_factor_income:post_tax_disposable_income))
rm(silc.hdp.store, silc.p.store_sum, silc.rhdp.store, silc.hd.store)


# P2 (wid.world):Nur Personen >= 20 Jahre & partial sharing of resources 
silc.p.age <- silc.p.store %>% select(id_h, pb010, pb140) %>% mutate(age = pb010 - pb140, twentyandabove = if_else(age>=20, 1, 0))
silc.h.age <- silc.p.age %>% group_by(id_h) %>% summarise(twentyandabove_sum = sum(twentyandabove))

silc.pdh.store <- left_join(silc.pd.store, silc.h.store)
silc.pdh.age.store <- left_join(silc.pdh.store, silc.h.age)

silc.pdh.age.store <- silc.pdh.age.store %>% mutate(age = pb010 - pb140) %>% filter(age>=20)
silc.pdh.age.store <- silc.pdh.age.store %>% mutate_at(vars(hy110g:hy140g), funs(. / twentyandabove_sum))

# summing up individual components
# 1. Pre-tax factor income (Canberra: primary income)
silc.pdh.age.store <- silc.pdh.age.store %>% mutate(pre_tax_factor_income = py010g + py021g + py050g + hy110g + hy040g + hy090g + py080g)

# 2. Pre-tax national income
silc.pdh.age.store <- silc.pdh.age.store %>% mutate(pre_tax_national_income = pre_tax_factor_income + py090g + py100g)

# 3. Post-tax disposable income
silc.pdh.age.store <- silc.pdh.age.store %>% mutate(post_tax_disposable_income = pre_tax_national_income + py110g + py120g + py130g + py140g + hy050g + hy060g + hy070g + hy080g - hy120g - hy130g - hy140g)

# p2 dataset
silc.pdh.p2.store <- silc.pdh.age.store %>% select(pb010:pb040, pb150, px010, id_h, db040, age:post_tax_disposable_income)
rm(silc.pdh.age.store, silc.pdh.store, silc.pd.store, silc.p.age, silc.h.age)

# cleaning up the datasets ------------------------------------------------

# check for and remove NAs
sum(is.na(silc.rhdp.p1.store))
silc.rhdp.p1.store[which(is.na(silc.rhdp.p1.store)),]
silc.rhdp.p1.store <- na.exclude(silc.rhdp.p1.store)

sum(is.na(silc.pdh.p2.store))
silc.pdh.p2.store[which(is.na(silc.pdh.p2.store)),]
silc.pdh.p2.store <- na.exclude((silc.pdh.p2.store))

# remove zero disposable income people?
#silc.rhdp.p1.store <- silc.rhdp.p1.store %>% filter(equivalent_post_tax_disposable_income_imputed>0)
#silc.pdh.p2.store <- silc.pdh.p2.store %>% filter(post_tax_disposable_income>0)





# ppp adjustment ----------------------------------------------------------

# get data
ppp <- get_eurostat(id = "prc_ppp_ind", time_format = "num")

# filter relevant values
ppp <- ppp %>% filter(aggreg == "A01" &
                        na_item == "PPP_EU28")

# merge data
silc.rhdp.p1.store <- left_join(silc.rhdp.p1.store, ppp, c("rb010"="time", "rb020"="geo"))
silc.pdh.p2.store <- left_join(silc.pdh.p2.store, ppp, by = c("pb010" = "time", "pb020" = "geo"))
silc.rhdp.p1.store <- silc.rhdp.p1.store %>% select(-na_item, -aggreg) %>% rename("ppp" = "values")
silc.pdh.p2.store <- silc.pdh.p2.store %>% select(-na_item, -aggreg) %>% rename("ppp" = "values")

# exchange rate addition
# some exchange rate values are missing in 2015, 2016, 2017
table(silc.p.store$pb010[which(silc.p.store$px010==0)])

# get exchange rates from eurostat
exc <- get_eurostat(id="tec00033", time_format = "num")
exc <- exc %>% mutate(country = substr(currency, 1, 2))

# rename GB to UK
exc$country[exc$country=="GB"] <- "UK"

# add exchange rates to datasets
silc.pdh.p2.store <- left_join(silc.pdh.p2.store, exc, by = c("pb010" = "time", "pb020" = "country"))
silc.pdh.p2.store <- silc.pdh.p2.store %>% select(-statinfo, -unit, -currency) %>% rename("xr" = "values")
silc.rhdp.p1.store <- left_join(silc.rhdp.p1.store, exc, by = c("rb010"="time", "rb020"="country"))
silc.rhdp.p1.store <- silc.rhdp.p1.store %>% select(-statinfo, -unit, -currency) %>% rename("xr" = "values")

# calculate ppp adjusted incomes
silc.p1.ppp.store <- silc.rhdp.p1.store %>% mutate_at(vars(equivalent_pre_tax_factor_income:equivalent_post_tax_disposable_income_imputed), funs(if_else(hx010==0, . * xr / ppp, . * hx010 / ppp)))
rm(silc.rhdp.p1.store)

silc.p2.ppp.store <- silc.pdh.p2.store %>% mutate_at(vars(pre_tax_factor_income:post_tax_disposable_income), funs(if_else(px010==0, . * xr / ppp, . * px010 / ppp)))
rm(silc.pdh.p2.store)

# save data ---------------------------------------------------------------




save(silc.p.store, silc.h.store, silc.d.store, silc.r.store, silc.p1.ppp.store, silc.p2.ppp.store, file = "./data/silc_eu28.RData")
