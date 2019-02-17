# # construct eu28 distribution graph
# 
# library(dplyr)
# library(plyr)
# 
# load("./data/silc_eu28.RData")
# rm(silc.d.store, silc.h.store, silc.p.store, silc.r.store, silc.p2.ppp.store)
# 
# silc_2016 <- silc.p1.ppp.store %>% filter(rb010 %in% 2016)
# silc_2016 <- silc_2016 %>% select(id_h, rb020, rb050, equivalent_pre_tax_factor_income, equivalent_post_tax_disposable_income)
# silc_2016$rb050 <- round(silc_2016$rb050, digits = 0)
# 
# list_store2 <- vector("list", length = 28)
# 
# 
# for(country in 20:nlevels(as.factor(silc_2016$rb020))){
#   
#   
#   silc_2016c <- silc_2016 %>% filter(rb020 %in% levels(as.factor(silc_2016$rb020))[country])
#   silc_2016_expanded <- silc_2016c[rep(seq.int(1,nrow(silc_2016c)), silc_2016c$rb050),1:5]
#   
#   
#   list_store2[[country]] <- silc_2016_expanded
# }
# 
# save(list_store2, file = "./data/list_store2.RData")
# 
# 
# 
# load("./data/list_store2.RData")
# 
# list_small <- vector("list", length = 28)
# 
# for(country in 22:28){
#   
#   country_silc <- list_store2[[country]]
#   random_sample <- country_silc[runif(nrow(country_silc)/1000, min = 1, max = nrow(country_silc)),]
#   list_small[[country]] <- random_sample
#   
# }
# 
# df <- ldply(list_small, data.frame)
# 
# save(df, file = "./data/sample_distribution.RData")
