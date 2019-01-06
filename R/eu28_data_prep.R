## Prepare EU28 data
####################




load("./data/silc_eu28.RData")


# basic data manipulations ------------------------------------------------

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

# Merge the datasets
silc.pd.store <- left_join(silc.p.store, silc.d.store)
silc.hd.store <- left_join(silc.h.store, silc.d.store)


# income aggregation ------------------------------------------------------

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

# calculate equivalent incomes
silc.hdp.store <- silc.hdp.store %>% mutate(equivalent_pre_tax_factor_income = if_else(hx050==0, 0, pre_tax_factor_income/hx050) , equivalent_pre_tax_national_income = if_else(hx050==0, 0, pre_tax_national_income/hx050), equivalent_post_tax_disposable_income = if_else(hx050==0, 0, post_tax_disposable_income/hx050))

sum(silc.hdp.store$hy020>0 & silc.hdp.store$post_tax_disposable_income==0)

