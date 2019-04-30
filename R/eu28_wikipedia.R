## EU28 - Coding for Wikipedia
##############################

#servus

# load packages -----------------------------------------------------------

library(dplyr)
library(eurostat)
library(reshape2)
library(plyr)
library(ggplot2)
library(svglite)

#load("./data/silc_eu28.RData")

# get data from EUROSTAT --------------------------------------------------
# get indicators on countrylevel

names <- c("pov_th", "pov", "quants", "qsr", "gini", 
           "inc", "hlth", "pov_reg")
codes <- c("ilc_li01", "ilc_li02", "ilc_di01", "ilc_di11", "ilc_di12",
           "ilc_di03", "ilc_lk11", "ilc_li41")
data <- lapply(codes, get_eurostat, stringsAsFactors = FALSE, time_format = "num")

df <- ldply(data)

#filter indicators

ginigraph_data <- df %>% filter(indic_il %in% "GINI_HND" & time %in% c("2008","2017"))

s8020graph_data <- df %>% filter(indic_il %in% "S80_S20" & time %in% c("2008", "2017") & sex%in%"T" & age %in% "TOTAL")
gini.EU <- filter(df, indic_il =="GINI_HND", time >= "2016")
median.EU <- filter(df, indic_il =="MED_E", time >= "2016", unit=="EUR", sex=="T", age=="TOTAL")
S80_20.EU <- filter(df, indic_il =="S80_S20", time >="2016", sex=="T", age=="TOTAL")
mean.EU <- filter(df, indic_il=="MEI_E", time >="2016",unit=="EUR", sex=="T", age=="TOTAL")


indicators.cl <- bind_cols(gini.EU, median.EU, mean.EU, S80_20.EU)
colnames(indicators.cl)[colnames(indicators.cl)=="values"] <- "gini"
colnames(indicators.cl)[colnames(indicators.cl)=="values1"] <- "median" 
colnames(indicators.cl)[colnames(indicators.cl)=="values2"] <- "mean"
colnames(indicators.cl)[colnames(indicators.cl)=="values3"] <- "S80_20"

indicators.cl <- select(indicators.cl, geo, time, gini, median, mean, S80_20)

indicators.cl <- filter(indicators.cl, geo%in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
                                                 "EE", "EL", "ES","FI", "FR", "HR", 
                                                 "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                                                 "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK", "EU28"))

indicators.wiki <- filter(indicators.cl, time %in% 2017)

indicators.eu <- indicators.wiki %>% filter(geo %in% "EU28")
indicators.wiki <- indicators.wiki %>% filter(geo!="EU28")
indicators.wiki <- rbind(indicators.eu, indicators.wiki)

write.csv2(indicators.wiki, file = "indicators.wiki.csv",row.names=FALSE)

# gini time development graph ---------------------------------------------

ginigraph_data <- ginigraph_data %>% select(geo, time, values)

ginigraph_data <- ginigraph_data %>% filter(geo%in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
                                                 "EE", "EL", "ES","FI", "FR", "HR", 
                                                 "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                                                 "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK", "EU27"))

gini_cro2010 <- df %>% filter(indic_il %in% "GINI_HND" & time %in% c("2010") & geo %in% "HR")
gini_cro2010 <- gini_cro2010 %>% select(geo, time, values)
ginigraph_data <- rbind(ginigraph_data, gini_cro2010)
ginigraph_data <- ginigraph_data %>% group_by(time) %>% arrange(geo)
ginigraph_data8 <- ginigraph_data %>% filter(time %in% c("2008","2010"))
ginigraph_data16 <- ginigraph_data %>% filter(time %in% c("2017"))
ginigraph_data <- left_join(ginigraph_data8, ginigraph_data16, by = "geo")

gini_eu <- ginigraph_data %>% filter(geo %in% "EU27")
ginigraph_data <- ginigraph_data %>% filter(geo!="EU27")
ginigraph_data <- rbind(gini_eu, ginigraph_data)
ginigraph_data$geo[ginigraph_data$geo=="EU27"] <- "EU"
ginigraph_data$geo <- factor(ginigraph_data$geo, levels = unique(ginigraph_data$geo))


p <- ggplot(ginigraph_data) +
  geom_segment( aes(x=factor(geo, level=geo), xend=factor(geo, level=geo), y=values.x, yend=values.y), color="blue",  arrow = arrow(angle = 30, length = unit(0.12, "inches"), ends = "last", type = "open")) +
  xlab("") + ylab("Gini Koeffizienten (0-100)\n")  + scale_y_continuous(breaks = pretty(ginigraph_data$values.y, n=15)) + labs(title="Entwicklung der Gini Koeffizienten der EU Länder, verfügbare Einkommen (2008 und 2017)", caption = "Datenquelle: Eurostat,  Anmerkungen: EU bezieht sich auf EU27 in 2008 und EU28\n in 2017; für Kroatien werden Werte aus 2010 und 2017 herangezogen")

p

ggsave(file='graphs/EU_gini.svg',height=4,width=7)


# s8020 time development graph --------------------------------------------


s8020graph_data <- s8020graph_data %>% select(geo, time, values)

s8020graph_data <- s8020graph_data %>% filter(geo%in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
                                                      "EE", "EL", "ES","FI", "FR", "HR", 
                                                      "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                                                      "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK", "EU27"))

s8020_cro2010 <- df %>% filter(time %in% c("2010") & geo %in% "HR" & indic_il %in% "S80_S20"  & sex%in%"T" & age %in% "TOTAL")
s8020_cro2010 <- s8020_cro2010 %>% select(geo, time, values)
s8020graph_data <- rbind(s8020graph_data, s8020_cro2010)
s8020graph_data <- s8020graph_data %>% group_by(time) %>% arrange(geo)
s8020graph_data8 <- s8020graph_data %>% filter(time %in% c("2008","2010"))
s8020graph_data16 <- s8020graph_data %>% filter(time %in% c("2017"))
s8020graph_data <- left_join(s8020graph_data8, s8020graph_data16, by = "geo")

s8020_eu <- s8020graph_data %>% filter(geo %in% "EU27")
s8020graph_data <- s8020graph_data %>% filter(geo!="EU27")
s8020graph_data <- rbind(s8020_eu, s8020graph_data)
s8020graph_data$geo[s8020graph_data$geo=="EU27"] <- "EU"
s8020graph_data$geo <- factor(s8020graph_data$geo, levels = unique(s8020graph_data$geo))


p <- ggplot(s8020graph_data) +
  geom_segment( aes(x=factor(geo, level=geo), xend=factor(geo, level=geo), y=values.x, yend=values.y), color="blue",  arrow = arrow(angle = 30, length = unit(0.12, "inches"), ends = "last", type = "open")) +
  xlab("") + ylab("S80/S20 Verhältnisse\n")  + scale_y_continuous(breaks = pretty(s8020graph_data$values.y, n=15)) + labs(title="Entwicklung der S80/S20 Verhältnisse der EU Länder, verfügbare Einkommen (2008 und 2017)", caption = "Datenquelle: Eurostat,  Anmerkungen: EU bezieht sich auf EU27 in 2008 und EU28\n in 2017; für Kroatien werden Werte aus 2010 und 2017 herangezogen")
p

ggsave(file='graphs/EU_s8020.svg',height=4,width=7)



# gini time series --------------------------------------------------------

gini <- get_eurostat(id = "ilc_di12", time_format = "num")
gini_wop <- get_eurostat(id = "ilc_di12b", time_format = "num")
gini_wp <- get_eurostat(id = "ilc_di12c", time_format = "num")

gini$variable <- "disposable income"
gini_wop$variable <- "di before transfers (without pensions)"
gini_wp$variable <- "di before transfers (but with pensions)"

hehe <- rbind(gini, gini_wop)
hehe <- filter(hehe, time < 2018, time > 2004)

hehe <- hehe %>% filter(geo%in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
                                  "EE", "EL", "ES","FI", "FR", "HR", 
                                  "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                                  "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK", "EU27", "EU28"))

#merge eu27 and eu28

hehe[919,] <- c("GINI_HND","EU28",2005,30.6,"disposable income")
hehe[920,] <- c("GINI_HND","EU28",2006,30.3,"disposable income")
hehe[921,] <- c("GINI_HND","EU28",2007,30.6,"disposable income")
hehe[922,] <- c("GINI_HND","EU28",2008,31.0,"disposable income")
hehe[923,] <- c("GINI_HND","EU28",2009,30.6,"disposable income")

hehe[924,] <- c("GINI_HND","EU28",2005,49.7,"di before transfers (without pensions)")
hehe[925,] <- c("GINI_HND","EU28",2006,50.2,"di before transfers (without pensions)")
hehe[926,] <- c("GINI_HND","EU28",2007,49.6,"di before transfers (without pensions)")
hehe[927,] <- c("GINI_HND","EU28",2008,49.6,"di before transfers (without pensions)")
hehe[928,] <- c("GINI_HND","EU28",2009,49.6,"di before transfers (without pensions)")


eu <- hehe %>% filter(geo%in% c("EU28"))

hehe <- hehe %>% filter(geo%in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
                                  "EE", "EL", "ES","FI", "FR", "HR", 
                                  "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                                  "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"))
#hehe$EU <- ifelse(hehe$geo=="EU28", 1,0)
#hehe$EU <- as.factor(hehe$EU)

hehe$time <- as.integer(hehe$time)
hehe$values <- as.numeric(hehe$values)
hehe$cv <- as.factor(paste0(hehe$geo, "_", hehe$variable))


hehe <- arrange(hehe, time)
hehe$time <- paste0(hehe$time,"-01-01")
hehe$time <- as.Date(hehe$time)

eu$time <- paste0(eu$time,"-01-01")
eu$time <- as.Date(eu$time)

eu$values <- as.numeric(eu$values)
eu$variable <- as.factor(eu$variable)

hehe$EU4 <- ifelse(hehe$geo=="DE"| hehe$geo=="FR"| 
                     hehe$geo=="IT"|
                     hehe$geo=="UK", 1, 0)
hehe$variable_eu4 <- paste0(hehe$variable, "_", hehe$EU4)
hehe$variable_eu4 <- as.factor(hehe$variable_eu4)
hehe$EU4 <- as.factor(hehe$EU4)

ginid <- dcast(gini, geo ~ time, value.var = c("values"))
# ginid_wop <- dcast(gini_wop, geo ~ time, value.var = c("values"))
RColorBrewer::display.brewer.all()

bla <- ggplot() + geom_point(data=hehe, aes(x=time, y=values, colour=variable_eu4, alpha=EU4))+ geom_line(data=hehe, aes(x=time, y=values, colour=variable_eu4, group=cv, alpha=EU4), size=0.8) + geom_line(data=eu, aes(x=time, y=values, group=variable), colour="yellow", size=1)+ geom_point(data=eu, aes(x=time, y=values, group=variable, fill="EU Durchschnitt"), colour="yellow") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + labs(x="", y="Gini Koeffizienten (0-100)", title="Entwicklung der Gini Koeffizienten der EU Länder, verfügbare Einkommen (2005 bis 2017)", caption = "Datenquelle: Eurostat,  Anmerkungen: Beim EU Durchschnitt der von Eurostat berechnet wird, handelt es\n sich um einen gewichteten Durchschnitt der von 2005-2009 27 und danach 28 Länder einbezieht.") + scale_colour_brewer(palette = "Paired", labels=c("Verfügbare Einkommen ohne Sozialtransfers und Pensionen (Rest EU)", "Verfügbare Einkommen ohne Sozialtransfers und Pensionen (DE, FR, IT, UK)", "Verfügbare Einkommen (Rest EU)", "Verfügbare Einkommen (DE, FR, IT, UK)")) + scale_alpha_discrete(range = c(0.3, 0.9)) + theme(legend.position = "bottom", legend.box = "horizontal", legend.direction = "vertical", legend.box.just = "left", legend.box.margin = margin(-25,0,-10,0), legend.justification = "left", plot.caption = element_text(size=8)) + guides(fill = guide_legend(title = NULL, label.theme = element_text(size=8)), alpha="none", colour = guide_legend(title = "Einkommensvariable", title.theme = element_text(size=10), label.theme = element_text(size = 8))) 

bla

ggsave(file='graphs/EU_gini_ts.svg',height=5,width=8)

#legend wrap?

#cross graph?


# gini time series - EU15 only? --------------------------------------------------------
#largest?
#15.92+12.98+11.98+12.67 = ~53%


gini <- get_eurostat(id = "ilc_di12", time_format = "num")
gini_wop <- get_eurostat(id = "ilc_di12b", time_format = "num")
gini_wp <- get_eurostat(id = "ilc_di12c", time_format = "num")

gini$variable <- "disposable income"
gini_wop$variable <- "di before transfers (without pensions)"
gini_wp$variable <- "di before transfers (but with pensions)"

hehe <- rbind(gini, gini_wop)
hehe <- filter(hehe, time < 2018, time > 2004)

hehe <- hehe %>% filter(geo%in% c("DE", "FR", 
                                  "IT", 
                                  "UK", "EU27", "EU28"))

#merge eu27 and eu28

hehe[919,] <- c("GINI_HND","EU28",2005,30.6,"disposable income")
hehe[920,] <- c("GINI_HND","EU28",2006,30.3,"disposable income")
hehe[921,] <- c("GINI_HND","EU28",2007,30.6,"disposable income")
hehe[922,] <- c("GINI_HND","EU28",2008,31.0,"disposable income")
hehe[923,] <- c("GINI_HND","EU28",2009,30.6,"disposable income")

hehe[924,] <- c("GINI_HND","EU28",2005,49.7,"di before transfers (without pensions)")
hehe[925,] <- c("GINI_HND","EU28",2006,50.2,"di before transfers (without pensions)")
hehe[926,] <- c("GINI_HND","EU28",2007,49.6,"di before transfers (without pensions)")
hehe[927,] <- c("GINI_HND","EU28",2008,49.6,"di before transfers (without pensions)")
hehe[928,] <- c("GINI_HND","EU28",2009,49.6,"di before transfers (without pensions)")


eu <- hehe %>% filter(geo%in% c("EU28"))

hehe <- hehe %>% filter(geo%in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
                                  "EE", "EL", "ES","FI", "FR", "HR", 
                                  "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                                  "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK"))
#hehe$EU <- ifelse(hehe$geo=="EU28", 1,0)
#hehe$EU <- as.factor(hehe$EU)

hehe$time <- as.integer(hehe$time)
hehe$values <- as.numeric(hehe$values)
hehe$cv <- as.factor(paste0(hehe$geo, "_", hehe$variable))


hehe <- arrange(hehe, time)
hehe$time <- paste0(hehe$time,"-01-01")
hehe$time <- as.Date(hehe$time)

eu$time <- paste0(eu$time,"-01-01")
eu$time <- as.Date(eu$time)

eu$values <- as.numeric(eu$values)
eu$variable <- as.factor(eu$variable)

# ginid <- dcast(gini, geo ~ time, value.var = c("values"))
# ginid_wop <- dcast(gini_wop, geo ~ time, value.var = c("values"))


bla <- ggplot() + geom_point(data=hehe, aes(x=time, y=values, colour=variable), alpha=0.5)+ geom_line(data=hehe, aes(x=time, y=values, colour=variable, group=cv), alpha=0.3) + geom_line(data=eu, aes(x=time, y=values, group=variable), colour="blue")+ geom_point(data=eu, aes(x=time, y=values, group=variable, fill="EU"), colour="blue") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + labs(x="") 

bla





# checking eurostat calculations ------------------------------------------



# get data
gini <- get_eurostat(id = "ilc_di12", time_format = "num")

ginid <- dcast(gini, geo ~ time, value.var = c("values"))
ginieu28 <- ginid %>% filter(geo %in% c(levels(as.factor(silc.p1.ppp.store$rb020[which(silc.p1.ppp.store$rb010==2016)]))))

# 
# # # check if the EU data is the average (for gini) - what is eurostat showing us there?
# # for hy020: W/O PPP we are in the 40s with ppp we are around 34 (already excluding <=0, otherwise higher still)
average_ginieu28 <- ginieu28 %>% summarise_all(mean, na.rm=TRUE)

pop <- get_eurostat("naida_10_pe", time_format = "num")
pop <- pop %>% filter(na_item == "POP_NC")
popd <- pop %>% dcast(geo ~ time, value.var = c("values"))

popeu28 <- popd %>% filter(geo %in% c(levels(as.factor(silc.p1.ppp.store$rb020[which(silc.p1.ppp.store$rb010==2016)]))))
popeu28_w <- popeu28 %>% mutate_at(vars(contains("1975"):contains("2017")),funs(./sum(.)))

gini_wavg_2016 <- array(NA, c(1,4))

for(i in 20:23){
  for(j in 40:43){
    gini_wavg_2016[1,i-19] <- t(ginieu28[,i])%*%popeu28_w[,j]
    
    
  }
  
}

gini_wavg_2016 <- t(ginieu28[,23])%*%popeu28_w[,43]
gini_wavg_econw_2016 <- t(ginieu28[,23]) %*% na.exclude(as.numeric(as.character(theil.p1$di.econ.weight2016)))


# check silc gini

dfhy2016 <- silc.p1.ppp.store %>% filter(rb010 %in% 2016 & equivalent_hy020>0)

# count(dfhy2016$equivalent_hy020[which(dfhy2016$equivalent_hy020==0)])
# sum(dfhy2016$equivalent_hy020<0)
# dfhy2016$equivalent_hy020[which(dfhy2016$equivalent_hy020<0)] <- 0
# 
# 
# hy2016svy <- svydesign(ids =  ~ id_h,
#                          strata = ~db040,
#                          weights = ~rb050,
#                          data = dfhy2016) %>% convey_prep()
# gini_hy2016 <- svygini(~equivalent_hy020, hy2016svy)
# gini_hy2016





# also check the mean
mean <- get_eurostat(id = "ilc_di03", time_format = "num")
mean <- mean %>% filter(indic_il %in% "MEI_E" & age %in% "TOTAL" & sex %in% "T" & unit %in% "EUR")
meand <- dcast(mean, fun.aggregate = NULL, geo ~ time, value.var = c("values"))
meaneu28 <- meand %>% filter(geo %in% c(levels(as.factor(silc.p1.ppp.store$rb020[which(silc.p1.ppp.store$rb010==2016)]))))

# 
# # # check if the EU data is the average (for mean) - what is eurostat showing us there?
# 400 € off for pop weigthed
average_meaneu28 <- meaneu28 %>% summarise_all(mean, na.rm=TRUE)

pop <- get_eurostat("naida_10_pe", time_format = "num")
pop <- pop %>% filter(na_item == "POP_NC")
popd <- pop %>% dcast(geo ~ time, value.var = c("values"))

popeu28 <- popd %>% filter(geo %in% c(levels(as.factor(silc.p1.ppp.store$rb020[which(silc.p1.ppp.store$rb010==2016)]))))
popeu28_w <- popeu28 %>% mutate_at(vars(contains("1975"):contains("2017")),funs(./sum(.)))

mean_wavg_2016 <- array(NA, c(1,4))

for(i in 19:22){
  for(j in 40:43){
    mean_wavg_2016[1,i-18] <- t(meaneu28[,i])%*%popeu28_w[,j]
    
    
  }
  
}
