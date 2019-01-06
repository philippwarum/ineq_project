## Download and store EU28 data
###############################

# Download Data ------------------------------------------------------------

silc.p.store <- NULL
silc.h.store <- NULL
silc.d.store <- NULL
silc.r.store <- NULL

silc.p <- tbl(pg, "pp") %>%
  filter(pb010 %in% c(2004, 2005, 2006)) %>%
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, px010, px030, py080g, py090g, py100g, py110g, py120g, py130g, py140g) %>%
  collect(n = Inf)

silc.p.store <- silc.p %>% rename(py021g = py020g)

for(p in 7:17){
  silc.p <- tbl(pg, paste0("c",formatC(p,width = 2, format = "d", flag = "0"), "p")) %>%
    select(pb010, pb020, pb030, pb040, pb150, py010g, py021g, py050g, py050n, px010, px030, py080g, py090g, py100g, py110g, py120g, py130g, py140g) %>%
    collect(n = Inf)
  silc.p.store <- rbind(silc.p.store, silc.p)
}


for(h in 4:17){
  silc.h <- tbl(pg, paste0("c",formatC(h,width = 2, format = "d", flag = "0"), "h")) %>%
    select(hb010, hb020, hb030, hy010, hy020, hx010, hx050, hy110g, hy040g, hy090g, hy050g, hy060g, hy070g, hy080g, hy120g, hy130g, hy140g) %>%
    collect(n = Inf)
  silc.h.store <- rbind(silc.h.store, silc.h)
}


for(d in 4:17){
  silc.d <- tbl(pg, paste0("c",formatC(d,width = 2, format = "d", flag = "0"), "d")) %>%
    select(db010, db020, db030, db040, db090) %>%
    collect(n = Inf)
  silc.d.store <- rbind(silc.d.store, silc.d)
}


for(r in 4:17){
  silc.r <- tbl(pg, paste0("c",formatC(r,width = 2, format = "d", flag = "0"), "r")) %>% 
    select(rb010, rb020, rb030, rb050, rx030) %>%
    collect(n = Inf)
  silc.r.store <- rbind(silc.r.store, silc.r)
}




# save dataset --------------------------------------------

save(silc.p.store, silc.h.store, silc.d.store, silc.r.store, file = "./data/silc_eu28.RData")
