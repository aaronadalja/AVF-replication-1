#######################################################################################
### R replication code for                                                          ###
### Preharvest food safety and conservation challenges facing US produce growers:   ###
### Results from a national survey                                                  ###
#######################################################################################

# DATE: June 2022
# Editor: Aaron Adalja

rm(list = ls())
objects()
options(error=recover, scipen=999)

# Any package that is required by the script below is given here:----
# Check to see if packages are installed, if not install.
inst_pkgs = load_pkgs =  c("data.table", "tidyverse", "magrittr", "tidyselect", "stringr", "texreg", "xtable", "systemfit", "knitr", "lubridate", "mfx", "plm", "mhurdle", "censReg", "survival", "margins", "kableExtra", "bife", "gridExtra", "gtools", "lme4", "ggeffects", "openxlsx", "glmm", "MCMCglmm", "BaylorEdPsych", "patchwork", "sf")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages---
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

wd <- "~/AVF replication 1/"
setwd(wd)

raw<-read.csv("./data/survey_04.20.2020.csv")
epa<-read.csv("./data/epa_region.csv")

# EPA Regions
epa %<>% left_join(data.frame(state.abb=c(state.abb,"DC"),state.name=c(state.name,"District of Columbia")))
merged2 <- raw %>% left_join(epa, by=c("state"="state.name"))

# Clean up revenue & expenses
merged2$rev.est[merged2$rev.est==0] <- NA
merged2$exp.est[merged2$exp.est==0] <- NA

# Revenue Categories
merged2$rev_cat <- ifelse(is.na(merged2$rev.est),NA,ifelse(merged2$rev.est<25000,"<25K",ifelse(merged2$rev.est<250000,"25-250K",ifelse(merged2$rev.est<500000,"250-500K",ifelse(merged2$rev.est<1e6,"500K-1M",ifelse(merged2$rev.est<5e6,"1-5M","5M+"))))))
merged2$rev_cat <- factor(merged2$rev_cat, levels = c("<25K", "25-250K", "250-500K", "500K-1M", "1-5M", "5M+"))
merged2$rev_cat_abbr <- factor(merged2$rev_cat, levels = c("<25K", "25-250K", "250-500K", "500K-1M", "1-5M", "5M+"), labels = c("<25K", "25-500K", "25-500K", ">500K", ">500K", ">500K"))

# Organic
merged2$org <- factor(merged2$org.v.conv, levels=levels(merged2$org.v.conv)[2:4], labels=c("Both", "Organic", "Conventional"))
merged2$org_abbr <- factor(merged2$org, levels=levels(merged2$org)[1:3], labels=c("Some", "Some", "None"))

# All the Summary Variables 
merged2 %<>% mutate(fvacreage.pct = (acre.infield.veg.herb + acre.infield.fruit)/acre.sum,
                    veg.pct = acre.infield.veg.herb / acre.sum, fruit.pct = acre.infield.fruit / acre.sum,
                    cereal.pct = acre.infield.cerealbeans / acre.sum, gh.pct = acre.gh / acre.sum, pasture.pct = acre.pasture / acre.sum,
                    grass.pct = acre.grass / acre.sum, wild.pct = acre.wild / acre.sum, fallow.pct = acre.fallow / acre.sum, other.pct = acre.other / acre.sum,
                    fvacreage.quartile = factor(ifelse(fvacreage.pct < 0.25, 1, ifelse(fvacreage.pct < 0.5, 2, ifelse(fvacreage.pct < 0.75, 3, 4))),
                                                levels = as.character(1:4), labels = c("[0%,25%)", "[25%,50%)", "[50,75%)", "[75%,100%)")),
                    acre_hhi = (veg.pct*100)^2 + (fruit.pct*100)^2 + (cereal.pct*100)^2 + (gh.pct*100)^2 + (pasture.pct*100)^2 +
                      (grass.pct*100)^2 + (wild.pct*100)^2 + (fallow.pct*100)^2 + (other.pct*100)^2,
                    dist_hhi = sale.directcons^2 + sale.wholesale^2 + sale.grocery^2 + sale.massmerch^2 + sale.process^2 + sale.other^2 + sale.foodserv^2,
                    directsales.quartile = factor(ifelse(sale.directcons < 25, 1, ifelse(sale.directcons < 50, 2, ifelse(sale.directcons < 75, 3, 4))),
                                                  levels = as.character(1:4), labels = c("[0%,25%)", "[25%,50%)", "[50,75%)", "[75%,100%)")),
                    lscape.wild = factor(border.noncrop, levels=c(levels(border.noncrop)[2],levels(border.noncrop)[5],levels(border.noncrop)[3]),
                                         labels = c("Most", "Some", "None")),
                    lscape.graze = factor(border.livestock, levels=c(levels(border.livestock)[2],levels(border.livestock)[5],levels(border.livestock)[3]),
                                         labels = c("Most", "Some", "None")),
                    emp.train = (train.attend=="Yes"), fs.plan = (fsplan == "Yes"), fs.consult = (fsplan.consult == "Yes"),
                    fs.insure = (insure=="Yes"), fs.audit = (audit=="Yes"), fs.loss = (losses.fs=="Yes"),
                    prac.watertest = (!is.na(fspract.watertest.treat) & (fspract.watertest.treat != "")),
                    prac.soil = (!is.na(fspract.bsaao.treat) & (fspract.bsaao.treat != "")),
                    prac.monwild = (!is.na(fspract.wild.monitor) & (fspract.wild.monitor != "")),
                    prac.deter = (!is.na(fspract.deter.animals) & (fspract.deter.animals != "")),
                    prac.hygiene = (!is.na(fspract.hygeine) & (fspract.hygeine != "")),
                    prac.emp = (!is.na(fspract.worker.train) & (fspract.worker.train != "")),
                    prac.sani = (!is.na(fspract.sanitize) & (fspract.sanitize != "")),
                    prac.rec = (!is.na(fspract.fsrecords) & (fspract.fsrecords != "")),
                    acre.rej = (!is.na(loss.acre.animals) & (loss.acre.animals > 0)) | (!is.na(loss.acre.water) & (loss.acre.water > 0)) | (!is.na(loss.noncrop.acre) & (loss.noncrop.acre > 0)),
                    fs.ins.payout = (!is.na(as.numeric(loss.insurance.payout)) & (as.numeric(loss.insurance.payout) > 0)),
                    fs.oth.comp = (!is.na(as.numeric(loss.other.payout)) & (as.numeric(loss.other.payout) > 0)),
                    water.any.stand = (!is.na(water.stand) & (water.stand != "")) | (!is.na(water.notouch.stand) & (water.notouch.stand != "")),
                    water.any.flow = (!is.na(water.flow) & (water.flow != "")) | (!is.na(water.notouch.flow) & (water.notouch.flow != "")),
                    water.any.ground = (!is.na(water.ground) & (water.ground != "")) | (!is.na(water.notouch.ground) & (water.notouch.ground != "")),
                    water.any.public = (!is.na(water.public) & (water.public != "")) | (!is.na(water.notouch.public) & (water.notouch.public != "")),
                    water.any.other = (!is.na(filler.20) & (filler.20 != "")) | (!is.na(filler.21) & (filler.21 != "")),
                    water.touch.stand = (!is.na(water.stand) & (water.stand != "")),
                    water.touch.flow = (!is.na(water.flow) & (water.flow != "")),
                    water.touch.ground = (!is.na(water.ground) & (water.ground != "")),
                    water.touch.public = (!is.na(water.public) & (water.public != "")),
                    water.touch.other = (!is.na(filler.21) & (filler.21 != "")),
                    test.freq = factor(water.freq.test, levels = c(levels(water.freq.test)[c(2,5)], levels(water.freq.test)[c(6,8)], 
                                                                   levels(water.freq.test)[4]), labels=c("freq", "freq", "occ", "occ", "never")),
                    treat.freq = factor(water.treat, levels = c(levels(water.treat)[2], levels(water.treat)[4], 
                                                                levels(water.treat)[5]), labels=c("never","always", "sometimes")),
                    hab.clearbuffer = wlrisk.clearbuffer=="Yes", hab.expbuffer = wlrisk.expbuffer=="Yes", hab.lowriskcrop = wlrisk.lowriskcrop=="Yes",
                    hab.fallow = wlrisk.fallow=="Yes", hab.corridor = wlrisk.corridors=="Yes", hab.drain = wlrisk.drain=="Yes", 
                    hab.clearvegwater = wlrisk.remove.nc.water=="Yes", hab.clearvegnear = wlrisk.remove.nc.fieldedge=="Yes",
                    det.deerfence = wl.manage.deerfence=="Yes", det.plasticfence = wl.manage.plasticfence=="Yes", det.waterfence = wl.manage.water.fence=="Yes",
                    det.mechtrap = wl.manage.mechanicaltraps=="Yes", det.poison = wl.manage.poison=="Yes", det.hunt = wl.manage.hunt=="Yes",
                    det.deter = wl.manage.deter=="Yes", 
                    buf.dirt = (!is.na(buffer.dirt) & buffer.dirt != ""),
                    buf.grass = (!is.na(buffer.mowedgrass) & (buffer.mowedgrass != "")),
                    buf.lrcrop = (!is.na(buffer.lowriskcrop) & (buffer.lowriskcrop != "")),
                    buf.ncv = (!is.na(buffer.nc) & (buffer.nc != "")),
                    buf.nouse = (!is.na(buffer.notused) & (buffer.notused != "")),
                    hr.bird = risk.birds=="High Risk", hr.rodent = risk.rodents=="High Risk", hr.deer = risk.cervid=="High Risk", hr.rept = risk.rept=="High Risk",
                    hr.pet = risk.pets=="High Risk", hr.livestock = risk.livestock=="High Risk", hr.draft = risk.draft=="High Risk",
                    wl.con.auditor = (!is.na(concern.wl.auditor) & (concern.wl.auditor != "")),
                    wl.con.buyer = (!is.na(concern.wl.buyer) & (concern.wl.buyer != "")),
                    wl.con.insp = (!is.na(concern.wl.inspector) & (concern.wl.inspector != "")),
                    wl.con.consult = (!is.na(concern.wl.consult) & (concern.wl.consult != "")),
                    wl.con.fstrain = (!is.na(concern.wl.fstrainer) & (concern.wl.fstrainer != "")),
                    wl.con.ext = (!is.na(concern.wl.extension) & (concern.wl.extension != "")),
                    wl.con.worker = (!is.na(concern.wl.worker) & (concern.wl.worker != "")),
                    wl.con.none = (!is.na(concern.wl.none) & (concern.wl.none != "")),
                    nc.con.auditor = (!is.na(concern.nc.auditor) & (concern.nc.auditor != "")),
                    nc.con.buyer = (!is.na(concern.nc.buyer) & (concern.nc.buyer != "")),
                    nc.con.insp = (!is.na(concern.nc.inspector) & (concern.nc.inspector != "")),
                    nc.con.consult = (!is.na(concern.nc.consult) & (concern.nc.consult != "")),
                    nc.con.fstrain = (!is.na(concern.nc.fstrain) & (concern.nc.fstrain != "")),
                    nc.con.ext = (!is.na(concern.nc.extension) & (concern.nc.extension != "")),
                    nc.con.none = (!is.na(concern.nc.none) & (concern.nc.none != "")),
                    audit.pt.wl = (!is.na(audit.lost.pts.wl) & (audit.lost.pts.wl != "")),
                    audit.pt.water = (!is.na(audit.lost.pt.water) & (audit.lost.pt.water != "")),
                    audit.pt.soil = (!is.na(audit.lost.pt.soil) & (audit.lost.pt.soil != "")),
                    audit.pt.clean = (!is.na(audit.lost.pt.clean) & (audit.lost.pt.clean != "")),
                    audit.pt.worker = (!is.na(audit.lost.pt.worker) & (audit.lost.pt.worker != "")),
                    audit.pt.none = (!is.na(audit.lost.pt.none) & (audit.lost.pt.none != "")),                    
                    con.hedge = (!is.na(cons.hedges) & (cons.hedges != "")),
                    con.nat = (!is.na(cons.natural) & (cons.natural != "")),
                    con.buf = (!is.na(cons.riparian) & (cons.riparian != "")),
                    con.time = (!is.na(cons.timing) & (cons.timing != "")),
                    con.house = (!is.na(cons.houses) & (cons.houses != "")),
                    con.flower = (!is.na(cons.poll) & (cons.poll != "")),
                    con.none = (!is.na(cons.none) & (cons.none != "")),
                    cons.prog = (cons.incentive.yn=="Yes"), cons.self = (cons.self.yn=="Yes"), 
                    cons.impair = (cost.fsimpair.yn=="Yes"), cons.runoff = (water.runoff.nc=="Yes")
)

#Convert Logical to 0/1
cols <- sapply(merged2, is.logical)
merged2[,cols] <- lapply(merged2[,cols], as.integer)

## Table 1 - Farm characteristics
tbl1 <- bind_rows(
  # All farms
merged2 %>% group_by(rev_cat) %>% summarize(resp=n()) %>% mutate(type="All Farms", level="All") %>% 
  dplyr::select(type,level,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0), 
  # Organic status
merged2 %>% group_by(org, rev_cat) %>% summarize(resp=n()) %>% mutate(type="Organic Status") %>% 
  dplyr::select(type,org,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0) %>% 
  rename(level=org),
  # Geographic Region
merged2 %>% group_by(region_epa, rev_cat) %>% summarize(resp=n()) %>% mutate(type="Region") %>% 
  dplyr::select(type,region_epa,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0) %>% 
  rename(level=region_epa),
  # Average FV Acreage
merged2 %>% group_by(rev_cat) %>% summarize(resp=mean(fvacreage.pct, na.rm=T)) %>% mutate(type="Prop. FV Acreage", level="All") %>% 
  dplyr::select(type,level,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0), 
  # Average Crop HHI
merged2 %>% group_by(rev_cat) %>% summarize(resp=mean(acre_hhi, na.rm=T)) %>% mutate(type="Avg. Acreage HHI", level="All") %>% 
  dplyr::select(type,level,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0), 
# Average Distribution Channel HHI
merged2 %>% group_by(rev_cat) %>% summarize(resp=mean(dist_hhi, na.rm=T)) %>% mutate(type="Avg. Distribution HHI", level="All") %>% 
  dplyr::select(type,level,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0), 
# Landscape Wildlife Habitat
merged2 %>% group_by(lscape.wild, rev_cat) %>% summarize(resp=n()) %>% mutate(type="Landscape Wildlife Habitat") %>% 
  dplyr::select(type,lscape.wild,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0) %>% 
  rename(level=lscape.wild),
# Landscape Grazing Land
merged2 %>% group_by(lscape.graze, rev_cat) %>% summarize(resp=n()) %>% mutate(type="Landscape Grazing Land") %>% 
  dplyr::select(type,lscape.graze,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0) %>% 
  rename(level=lscape.graze),
# Produce Surface Water
merged2 %>% mutate(surface=pmax(water.touch.stand, water.touch.flow), nosurface=(surface==0 & !is.na(other.pna))) %>%
                     group_by(rev_cat) %>% summarize(resp=sum(surface, na.rm=T)) %>% mutate(type="Surface Water", level="All") %>% 
  dplyr::select(type,level,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0),
# Produce Other Water
# Produce Surface Water
merged2 %>% mutate(surface=pmax(water.touch.stand, water.touch.flow), nosurface=(surface==0 & !is.na(other.pna))) %>%
  group_by(rev_cat) %>% summarize(resp=sum(nosurface, na.rm=T)) %>% mutate(type="Other Water", level="All") %>% 
  dplyr::select(type,level,rev_cat,resp) %>% pivot_wider(names_from = rev_cat, values_from = resp, values_fill = 0)
) %>% mutate(all=rowSums(.[3:9]))
write_csv(tbl1,"./results/tbl1.csv")

merged2 %>% summarize(mean(dist_hhi, na.rm=T))
merged2 %>% summarize(mean(acre_hhi, na.rm=T))
merged2 %>% summarize(mean(fvacreage.pct, na.rm=T))
sum(merged2$other.pna=="Source used", na.rm=T)

## Table 2 - Food safety program organization
tbl2 <- bind_rows(
  # All Farms
    merged2 %>%  summarize(N=n(), empt.train = mean(emp.train, na.rm=T), fs.plan=mean(fs.plan, na.rm=T), fs.consult=mean(fs.consult, na.rm=T),
                           fs.insure=mean(fs.insure, na.rm=T), fs.audit=mean(fs.audit, na.rm=T), fs.loss=mean(fs.loss, na.rm=T)) %>%
      mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Revenue Category
  merged2 %>% group_by(rev_cat) %>% summarize(N=n(), empt.train = mean(emp.train, na.rm=T), fs.plan=mean(fs.plan, na.rm=T), fs.consult=mean(fs.consult, na.rm=T),
                                              fs.insure=mean(fs.insure, na.rm=T), fs.audit=mean(fs.audit, na.rm=T), fs.loss=mean(fs.loss, na.rm=T)) %>%
    mutate(type="Revenue Category") %>% dplyr::select(type,rev_cat,everything()) %>% rename(level=rev_cat), 
  # Organic status
  merged2 %>% group_by(org_abbr) %>% summarize(N=n(), empt.train = mean(emp.train, na.rm=T), fs.plan=mean(fs.plan, na.rm=T), fs.consult=mean(fs.consult, na.rm=T),
                                          fs.insure=mean(fs.insure, na.rm=T), fs.audit=mean(fs.audit, na.rm=T), fs.loss=mean(fs.loss, na.rm=T)) %>%
    mutate(type="Organic Status") %>% dplyr::select(type,org_abbr,everything()) %>% rename(level=org_abbr), 
  # Geographic Region
  merged2 %>% group_by(region_epa) %>% summarize(N=n(), empt.train = mean(emp.train, na.rm=T), fs.plan=mean(fs.plan, na.rm=T), fs.consult=mean(fs.consult, na.rm=T),
                                                 fs.insure=mean(fs.insure, na.rm=T), fs.audit=mean(fs.audit, na.rm=T), fs.loss=mean(fs.loss, na.rm=T)) %>%
    mutate(type="Geographic Region") %>% dplyr::select(type,region_epa,everything()) %>% rename(level=region_epa), 
  # Crop Diversity
  merged2 %>% group_by(fvacreage.quartile) %>% summarize(N=n(), empt.train = mean(emp.train, na.rm=T), fs.plan=mean(fs.plan, na.rm=T), fs.consult=mean(fs.consult, na.rm=T),
                                                         fs.insure=mean(fs.insure, na.rm=T), fs.audit=mean(fs.audit, na.rm=T), fs.loss=mean(fs.loss, na.rm=T)) %>%
    mutate(type="FV Acreage Proportion") %>% dplyr::select(type,fvacreage.quartile,everything()) %>% rename(level=fvacreage.quartile), 
  # Direct Sales Proportion
  merged2 %>% group_by(directsales.quartile) %>% summarize(N=n(), empt.train = mean(emp.train, na.rm=T), fs.plan=mean(fs.plan, na.rm=T), fs.consult=mean(fs.consult, na.rm=T),
                                                           fs.insure=mean(fs.insure, na.rm=T), fs.audit=mean(fs.audit, na.rm=T), fs.loss=mean(fs.loss, na.rm=T)) %>%
    mutate(type="Direct Sales Proportion") %>% dplyr::select(type,directsales.quartile,everything()) %>% rename(level=directsales.quartile) 
)
write_csv(tbl2,"./results/tbl2.csv")

## Table 3 - Food safety practices
tbl3 <- bind_rows(
  # All Farms
  merged2 %>%  summarize(N=n(), prac.watertest=mean(prac.watertest, na.rm=T), prac.soil=mean(prac.soil, na.rm=T), 
                         prac.monwild=mean(prac.monwild, na.rm=T), prac.deter=mean(prac.deter, na.rm=T),
                         prac.hygiene=mean(prac.hygiene, na.rm=T), prac.emp=mean(prac.emp, na.rm=T),
                         prac.sani=mean(prac.sani, na.rm=T), prac.rec=mean(prac.rec, na.rm=T)) %>%
    mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Revenue Category
  merged2 %>% group_by(rev_cat) %>% summarize(N=n(), prac.watertest=mean(prac.watertest, na.rm=T), prac.soil=mean(prac.soil, na.rm=T), 
                                              prac.monwild=mean(prac.monwild, na.rm=T), prac.deter=mean(prac.deter, na.rm=T),
                                              prac.hygiene=mean(prac.hygiene, na.rm=T), prac.emp=mean(prac.emp, na.rm=T),
                                              prac.sani=mean(prac.sani, na.rm=T), prac.rec=mean(prac.rec, na.rm=T)) %>%
    mutate(type="Revenue Category") %>% dplyr::select(type,rev_cat,everything()) %>% rename(level=rev_cat), 
  # Organic Status
  merged2 %>% group_by(org_abbr) %>% summarize(N=n(), prac.watertest=mean(prac.watertest, na.rm=T), prac.soil=mean(prac.soil, na.rm=T), 
                                                   prac.monwild=mean(prac.monwild, na.rm=T), prac.deter=mean(prac.deter, na.rm=T),
                                                   prac.hygiene=mean(prac.hygiene, na.rm=T), prac.emp=mean(prac.emp, na.rm=T),
                                                   prac.sani=mean(prac.sani, na.rm=T), prac.rec=mean(prac.rec, na.rm=T)) %>%
    mutate(type="Organic Status") %>% dplyr::select(type,org_abbr,everything()) %>% rename(level=org_abbr), 
  # Landscape Characteristics Wildlife
  merged2 %>% group_by(lscape.wild) %>% summarize(N=n(), prac.watertest=mean(prac.watertest, na.rm=T), prac.soil=mean(prac.soil, na.rm=T), 
                                               prac.monwild=mean(prac.monwild, na.rm=T), prac.deter=mean(prac.deter, na.rm=T),
                                               prac.hygiene=mean(prac.hygiene, na.rm=T), prac.emp=mean(prac.emp, na.rm=T),
                                               prac.sani=mean(prac.sani, na.rm=T), prac.rec=mean(prac.rec, na.rm=T)) %>%
    mutate(type="Landscape Wildlife Habitat") %>% dplyr::select(type,lscape.wild,everything()) %>% rename(level=lscape.wild), 
  # Landscape Characteristics Grazing
  merged2 %>% group_by(lscape.graze) %>% summarize(N=n(), prac.watertest=mean(prac.watertest, na.rm=T), prac.soil=mean(prac.soil, na.rm=T), 
                                                  prac.monwild=mean(prac.monwild, na.rm=T), prac.deter=mean(prac.deter, na.rm=T),
                                                  prac.hygiene=mean(prac.hygiene, na.rm=T), prac.emp=mean(prac.emp, na.rm=T),
                                                  prac.sani=mean(prac.sani, na.rm=T), prac.rec=mean(prac.rec, na.rm=T)) %>%
    mutate(type="Landscape Grazing") %>% dplyr::select(type,lscape.graze,everything()) %>% rename(level=lscape.graze), 
)  
write_csv(tbl3,"./results/tbl3.csv")

## Appendix Table A1 - Water Management
tblA1 <- bind_rows(
  # All Farms
  merged2 %>% mutate(surface=pmax(water.touch.stand, water.touch.flow), surface_nt=(surface==0 & pmax(water.any.stand, water.any.flow)==1)*1, 
                     nosurface=(surface==0 & surface_nt==0 & !is.na(other.pna))*1) %>% 
    summarize(surface=sum(surface, na.rm=T), surface_nt=sum(surface_nt, na.rm=T), nosurface=sum(nosurface, na.rm=T)) %>%
    mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Testing Frequency
  merged2 %>% mutate(surface=pmax(water.touch.stand, water.touch.flow), surface_nt=(surface==0 & pmax(water.any.stand, water.any.flow)==1)*1, 
                     nosurface=(surface==0 & surface_nt==0 & !is.na(other.pna))*1) %>% 
    group_by(test.freq) %>% summarize(surface=sum(surface, na.rm=T), surface_nt=sum(surface_nt, na.rm=T), nosurface=sum(nosurface, na.rm=T)) %>%
    mutate(type="Testing Frequency") %>% dplyr::select(type,test.freq,everything()) %>% rename(level=test.freq),
  # Treatment Frequency
  merged2 %>% mutate(surface=pmax(water.touch.stand, water.touch.flow), surface_nt=(surface==0 & pmax(water.any.stand, water.any.flow)==1)*1, 
                     nosurface=(surface==0 & surface_nt==0 & !is.na(other.pna))*1) %>% 
    group_by(treat.freq) %>% summarize(surface=sum(surface, na.rm=T), surface_nt=sum(surface_nt, na.rm=T), nosurface=sum(nosurface, na.rm=T)) %>%
    mutate(type="Treatment Frequency") %>% dplyr::select(type,treat.freq,everything()) %>% rename(level=treat.freq)
) %>% pivot_longer(cols=c(surface,surface_nt,nosurface), names_to="var", values_to="value") %>% pivot_wider(names_from=c(type, level))
write_csv(tblA1,"./results/tblA1.csv")


## Table 4 - Buffers
tbl4 <- bind_rows(
  # All Farms
  merged2 %>% summarize(N=n(), buf.dirt=mean(buf.dirt, na.rm=T), buf.grass=mean(buf.grass, na.rm=T), 
                        buf.lrcrop=mean(buf.lrcrop, na.rm=T), buf.ncv=mean(buf.ncv, na.rm=T),
                        buf.nouse=mean(buf.nouse, na.rm=T)) %>%
    mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Revenue Category
  merged2 %>% group_by(rev_cat) %>% summarize(N=n(), buf.dirt=mean(buf.dirt, na.rm=T), buf.grass=mean(buf.grass, na.rm=T), 
                                                   buf.lrcrop=mean(buf.lrcrop, na.rm=T), buf.ncv=mean(buf.ncv, na.rm=T),
                                                   buf.nouse=mean(buf.nouse, na.rm=T)) %>%
    mutate(type="Revenue Category") %>% dplyr::select(type,rev_cat,everything()) %>% rename(level=rev_cat), 
  # Organic Status
  merged2 %>% group_by(org_abbr) %>% summarize(N=n(), buf.dirt=mean(buf.dirt, na.rm=T), buf.grass=mean(buf.grass, na.rm=T), 
                                               buf.lrcrop=mean(buf.lrcrop, na.rm=T), buf.ncv=mean(buf.ncv, na.rm=T),
                                               buf.nouse=mean(buf.nouse, na.rm=T)) %>%
    mutate(type="Organic Status") %>% dplyr::select(type,org_abbr,everything()) %>% rename(level=org_abbr), 
  # Landscape Characteristics Wildlife
  merged2 %>% group_by(lscape.wild) %>% summarize(N=n(), buf.dirt=mean(buf.dirt, na.rm=T), buf.grass=mean(buf.grass, na.rm=T), 
                                                  buf.lrcrop=mean(buf.lrcrop, na.rm=T), buf.ncv=mean(buf.ncv, na.rm=T),
                                                  buf.nouse=mean(buf.nouse, na.rm=T)) %>%
    mutate(type="Landscape Wildlife Habitat") %>% dplyr::select(type,lscape.wild,everything()) %>% rename(level=lscape.wild), 
  # Landscape Characteristics Grazing
  merged2 %>% group_by(lscape.graze) %>% summarize(N=n(), buf.dirt=mean(buf.dirt, na.rm=T), buf.grass=mean(buf.grass, na.rm=T), 
                                                   buf.lrcrop=mean(buf.lrcrop, na.rm=T), buf.ncv=mean(buf.ncv, na.rm=T),
                                                   buf.nouse=mean(buf.nouse, na.rm=T)) %>%
    mutate(type="Landscape Grazing") %>% dplyr::select(type,lscape.graze,everything()) %>% rename(level=lscape.graze), 
)  
write_csv(tbl4,"./results/tbl4.csv")

## Table 5 - Wildlife Habitat Control
tbl5 <- bind_rows(
  # All Farms
  merged2 %>% summarize(N=n(), hab.clearbuffer=mean(hab.clearbuffer, na.rm=T), hab.expbuffer=mean(hab.expbuffer, na.rm=T), 
                        hab.lowriskcrop=mean(hab.lowriskcrop, na.rm=T), hab.fallow=mean(hab.fallow, na.rm=T),
                        hab.corridor=mean(hab.corridor, na.rm=T), hab.drain=mean(hab.drain, na.rm=T), 
                        hab.clearvegwater=mean(hab.clearvegwater, na.rm=T), hab.clearvegnear=mean(hab.clearvegnear, na.rm=T)) %>%
    mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Revenue Category
  merged2 %>% group_by(rev_cat) %>% summarize(N=n(), hab.clearbuffer=mean(hab.clearbuffer, na.rm=T), hab.expbuffer=mean(hab.expbuffer, na.rm=T), 
                                                   hab.lowriskcrop=mean(hab.lowriskcrop, na.rm=T), hab.fallow=mean(hab.fallow, na.rm=T),
                                                   hab.corridor=mean(hab.corridor, na.rm=T), hab.drain=mean(hab.drain, na.rm=T), 
                                                   hab.clearvegwater=mean(hab.clearvegwater, na.rm=T), hab.clearvegnear=mean(hab.clearvegnear, na.rm=T)) %>%
    mutate(type="Revenue Category") %>% dplyr::select(type,rev_cat,everything()) %>% rename(level=rev_cat), 
  # Organic Status
  merged2 %>% group_by(org_abbr) %>% summarize(N=n(), hab.clearbuffer=mean(hab.clearbuffer, na.rm=T), hab.expbuffer=mean(hab.expbuffer, na.rm=T), 
                                               hab.lowriskcrop=mean(hab.lowriskcrop, na.rm=T), hab.fallow=mean(hab.fallow, na.rm=T),
                                               hab.corridor=mean(hab.corridor, na.rm=T), hab.drain=mean(hab.drain, na.rm=T), 
                                               hab.clearvegwater=mean(hab.clearvegwater, na.rm=T), hab.clearvegnear=mean(hab.clearvegnear, na.rm=T)) %>%
    mutate(type="Organic Status") %>% dplyr::select(type,org_abbr,everything()) %>% rename(level=org_abbr), 
  # Landscape Characteristics Wildlife
  merged2 %>% group_by(lscape.wild) %>% summarize(N=n(), hab.clearbuffer=mean(hab.clearbuffer, na.rm=T), hab.expbuffer=mean(hab.expbuffer, na.rm=T), 
                                                  hab.lowriskcrop=mean(hab.lowriskcrop, na.rm=T), hab.fallow=mean(hab.fallow, na.rm=T),
                                                  hab.corridor=mean(hab.corridor, na.rm=T), hab.drain=mean(hab.drain, na.rm=T), 
                                                  hab.clearvegwater=mean(hab.clearvegwater, na.rm=T), hab.clearvegnear=mean(hab.clearvegnear, na.rm=T)) %>%
    mutate(type="Landscape Wildlife Habitat") %>% dplyr::select(type,lscape.wild,everything()) %>% rename(level=lscape.wild), 
  # Landscape Characteristics Grazing
  merged2 %>% group_by(lscape.graze) %>% summarize(N=n(), hab.clearbuffer=mean(hab.clearbuffer, na.rm=T), hab.expbuffer=mean(hab.expbuffer, na.rm=T), 
                                                   hab.lowriskcrop=mean(hab.lowriskcrop, na.rm=T), hab.fallow=mean(hab.fallow, na.rm=T),
                                                   hab.corridor=mean(hab.corridor, na.rm=T), hab.drain=mean(hab.drain, na.rm=T), 
                                                   hab.clearvegwater=mean(hab.clearvegwater, na.rm=T), hab.clearvegnear=mean(hab.clearvegnear, na.rm=T)) %>%
    mutate(type="Landscape Grazing") %>% dplyr::select(type,lscape.graze,everything()) %>% rename(level=lscape.graze), 
)  
write_csv(tbl5,"./results/tbl5.csv")

## Table 6 - Wildlife Deterrence
tbl6 <- bind_rows(
  # All Farms
  merged2 %>% summarize(N=n(), det.deerfence=mean(det.deerfence, na.rm=T), det.plasticfence=mean(det.plasticfence, na.rm=T), 
                        det.waterfence=mean(det.waterfence, na.rm=T), det.mechtrap=mean(det.mechtrap, na.rm=T),
                        det.poison=mean(det.poison, na.rm=T), det.hunt=mean(det.hunt, na.rm=T), 
                        det.deter=mean(det.deter, na.rm=T)) %>%
    mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Revenue Category
  merged2 %>% group_by(rev_cat) %>% summarize(N=n(), det.deerfence=mean(det.deerfence, na.rm=T), det.plasticfence=mean(det.plasticfence, na.rm=T), 
                                                   det.waterfence=mean(det.waterfence, na.rm=T), det.mechtrap=mean(det.mechtrap, na.rm=T),
                                                   det.poison=mean(det.poison, na.rm=T), det.hunt=mean(det.hunt, na.rm=T), 
                                                   det.deter=mean(det.deter, na.rm=T)) %>%
    mutate(type="Revenue Category") %>% dplyr::select(type,rev_cat,everything()) %>% rename(level=rev_cat), 
  # Organic Status
  merged2 %>% group_by(org_abbr) %>% summarize(N=n(), det.deerfence=mean(det.deerfence, na.rm=T), det.plasticfence=mean(det.plasticfence, na.rm=T), 
                                               det.waterfence=mean(det.waterfence, na.rm=T), det.mechtrap=mean(det.mechtrap, na.rm=T),
                                               det.poison=mean(det.poison, na.rm=T), det.hunt=mean(det.hunt, na.rm=T), 
                                               det.deter=mean(det.deter, na.rm=T)) %>%
    mutate(type="Organic Status") %>% dplyr::select(type,org_abbr,everything()) %>% rename(level=org_abbr), 
  # Landscape Characteristics Wildlife
  merged2 %>% group_by(lscape.wild) %>% summarize(N=n(), det.deerfence=mean(det.deerfence, na.rm=T), det.plasticfence=mean(det.plasticfence, na.rm=T), 
                                                  det.waterfence=mean(det.waterfence, na.rm=T), det.mechtrap=mean(det.mechtrap, na.rm=T),
                                                  det.poison=mean(det.poison, na.rm=T), det.hunt=mean(det.hunt, na.rm=T), 
                                                  det.deter=mean(det.deter, na.rm=T)) %>%
    mutate(type="Landscape Wildlife Habitat") %>% dplyr::select(type,lscape.wild,everything()) %>% rename(level=lscape.wild), 
  # Landscape Characteristics Grazing
  merged2 %>% group_by(lscape.graze) %>% summarize(N=n(), det.deerfence=mean(det.deerfence, na.rm=T), det.plasticfence=mean(det.plasticfence, na.rm=T), 
                                                   det.waterfence=mean(det.waterfence, na.rm=T), det.mechtrap=mean(det.mechtrap, na.rm=T),
                                                   det.poison=mean(det.poison, na.rm=T), det.hunt=mean(det.hunt, na.rm=T), 
                                                   det.deter=mean(det.deter, na.rm=T)) %>%
    mutate(type="Landscape Grazing") %>% dplyr::select(type,lscape.graze,everything()) %>% rename(level=lscape.graze), 
)  
write_csv(tbl6,"./results/tbl6.csv")

## Appendix Table A2 - Self-Reported Risk Assessment
tblA2 <- bind_rows(
  # All Farms
  merged2 %>% summarize(N=n(), hr.bird = mean(hr.bird, na.rm=T), hr.rodent=mean(hr.rodent, na.rm=T), hr.deer=mean(hr.deer, na.rm=T),
                        hr.rept=mean(hr.rept, na.rm=T), hr.pet=mean(hr.pet, na.rm=T), hr.livestock=mean(hr.livestock, na.rm=T),
                        hr.draft=mean(hr.draft, na.rm=T)) %>%
    mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Revenue Category
  merged2 %>% group_by(rev_cat) %>% summarize(N=n(), hr.bird = mean(hr.bird, na.rm=T), hr.rodent=mean(hr.rodent, na.rm=T), hr.deer=mean(hr.deer, na.rm=T),
                                              hr.rept=mean(hr.rept, na.rm=T), hr.pet=mean(hr.pet, na.rm=T), hr.livestock=mean(hr.livestock, na.rm=T),
                                              hr.draft=mean(hr.draft, na.rm=T)) %>%
    mutate(type="Revenue Category") %>% dplyr::select(type,rev_cat,everything()) %>% rename(level=rev_cat), 
  # Organic status
  merged2 %>% group_by(org_abbr) %>% summarize(N=n(), hr.bird = mean(hr.bird, na.rm=T), hr.rodent=mean(hr.rodent, na.rm=T), hr.deer=mean(hr.deer, na.rm=T),
                                          hr.rept=mean(hr.rept, na.rm=T), hr.pet=mean(hr.pet, na.rm=T), hr.livestock=mean(hr.livestock, na.rm=T),
                                          hr.draft=mean(hr.draft, na.rm=T)) %>%
    mutate(type="Organic Status") %>% dplyr::select(type,org_abbr,everything()) %>% rename(level=org_abbr), 
   # Crop Diversity
  merged2 %>% group_by(fvacreage.quartile) %>% summarize(N=n(), hr.bird = mean(hr.bird, na.rm=T), hr.rodent=mean(hr.rodent, na.rm=T), hr.deer=mean(hr.deer, na.rm=T),
                                                         hr.rept=mean(hr.rept, na.rm=T), hr.pet=mean(hr.pet, na.rm=T), hr.livestock=mean(hr.livestock, na.rm=T),
                                                         hr.draft=mean(hr.draft, na.rm=T)) %>%
    mutate(type="FV Acreage Proportion") %>% dplyr::select(type,fvacreage.quartile,everything()) %>% rename(level=fvacreage.quartile), 
  # Landscape Characteristics Wildlife
  merged2 %>% group_by(lscape.wild) %>% summarize(N=n(), hr.bird = mean(hr.bird, na.rm=T), hr.rodent=mean(hr.rodent, na.rm=T), hr.deer=mean(hr.deer, na.rm=T),
                                                  hr.rept=mean(hr.rept, na.rm=T), hr.pet=mean(hr.pet, na.rm=T), hr.livestock=mean(hr.livestock, na.rm=T),
                                                  hr.draft=mean(hr.draft, na.rm=T)) %>%
    mutate(type="Landscape Wildlife Habitat") %>% dplyr::select(type,lscape.wild,everything()) %>% rename(level=lscape.wild), 
  # Landscape Characteristics Grazing
  merged2 %>% group_by(lscape.graze) %>% summarize(N=n(), hr.bird = mean(hr.bird, na.rm=T), hr.rodent=mean(hr.rodent, na.rm=T), hr.deer=mean(hr.deer, na.rm=T),
                                                    hr.rept=mean(hr.rept, na.rm=T), hr.pet=mean(hr.pet, na.rm=T), hr.livestock=mean(hr.livestock, na.rm=T),
                                                    hr.draft=mean(hr.draft, na.rm=T)) %>%
    mutate(type="Landscape Grazing") %>% dplyr::select(type,lscape.graze,everything()) %>% rename(level=lscape.graze), 
  )
write_csv(tblA2,"./results/tblA2.csv")

## Table 7 - External Input to Risk Assessment
# Combined and Collapsed
  merged3 <- merged2 %>% mutate(buyer=pmax(wl.con.buyer, nc.con.buyer), auditor=pmax(wl.con.auditor, wl.con.insp, nc.con.auditor, nc.con.insp),
                                advisor=pmax(wl.con.consult, wl.con.fstrain, wl.con.ext, nc.con.consult, nc.con.ext, nc.con.fstrain), 
                                noone=pmin(wl.con.none, nc.con.none))
  tbl7 <- bind_rows(
  # All Farms
  merged3 %>% summarize(N=n(), buyer=mean(buyer, na.rm=T), auditor=mean(auditor, na.rm=T), advisor=mean(advisor, na.rm=T), none=mean(noone, na.rm=T)) %>%
    mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Revenue Category
  merged3 %>% group_by(rev_cat) %>% summarize(N=n(), buyer=mean(buyer, na.rm=T), auditor=mean(auditor, na.rm=T), advisor=mean(advisor, na.rm=T), none=mean(noone, na.rm=T)) %>%
    mutate(type="Revenue Category") %>% dplyr::select(type,rev_cat,everything()) %>% rename(level=rev_cat), 
  # Organic status
  merged3 %>% group_by(org_abbr) %>% summarize(N=n(), buyer=mean(buyer, na.rm=T), auditor=mean(auditor, na.rm=T), advisor=mean(advisor, na.rm=T), none=mean(noone, na.rm=T)) %>%
    mutate(type="Organic Status") %>% dplyr::select(type,org_abbr,everything()) %>% rename(level=org_abbr), 
  # Crop Diversity
  merged3 %>% group_by(fvacreage.quartile) %>% summarize(N=n(), buyer=mean(buyer, na.rm=T), auditor=mean(auditor, na.rm=T), advisor=mean(advisor, na.rm=T), none=mean(noone, na.rm=T)) %>%
    mutate(type="FV Acreage Proportion") %>% dplyr::select(type,fvacreage.quartile,everything()) %>% rename(level=fvacreage.quartile), 
  # Direct Sales Proportion
  merged3 %>% group_by(directsales.quartile) %>% summarize(N=n(), buyer=mean(buyer, na.rm=T), auditor=mean(auditor, na.rm=T), advisor=mean(advisor, na.rm=T), none=mean(noone, na.rm=T)) %>%
    mutate(type="Direct Sales Proportion") %>% dplyr::select(type,directsales.quartile,everything()) %>% rename(level=directsales.quartile) 
)
write_csv(tbl7,"./results/tbl7.csv")
  

## Table 8 - Conservation Program Participation
tbl8 <- bind_rows(
  # All Farms
  merged2 %>% summarize(N=n(), cons.prog = mean(cons.prog, na.rm=T), cons.self=mean(cons.self, na.rm=T), cons.runoff=mean(cons.runoff, na.rm=T),
                        cons.impair=mean(cons.impair, na.rm=T)) %>%
    mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Revenue Category
  merged2 %>% group_by(rev_cat) %>% summarize(N=n(), cons.prog = mean(cons.prog, na.rm=T), cons.self=mean(cons.self, na.rm=T), cons.runoff=mean(cons.runoff, na.rm=T),
                                              cons.impair=mean(cons.impair, na.rm=T)) %>%
    mutate(type="Revenue Category") %>% dplyr::select(type,rev_cat,everything()) %>% rename(level=rev_cat), 
  # Organic status
  merged2 %>% group_by(org_abbr) %>% summarize(N=n(), cons.prog = mean(cons.prog, na.rm=T), cons.self=mean(cons.self, na.rm=T), cons.runoff=mean(cons.runoff, na.rm=T),
                                          cons.impair=mean(cons.impair, na.rm=T)) %>%
    mutate(type="Organic Status") %>% dplyr::select(type,org_abbr,everything()) %>% rename(level=org_abbr), 
  # Landscape Characteristics Wildlife
  merged2 %>% group_by(lscape.wild) %>% summarize(N=n(), cons.prog = mean(cons.prog, na.rm=T), cons.self=mean(cons.self, na.rm=T), cons.runoff=mean(cons.runoff, na.rm=T),
                                                  cons.impair=mean(cons.impair, na.rm=T)) %>%
    mutate(type="Landscape Wildlife Habitat") %>% dplyr::select(type,lscape.wild,everything()) %>% rename(level=lscape.wild), 
  # Landscape Characteristics Grazing
  merged2 %>% group_by(lscape.graze) %>% summarize(N=n(), cons.prog = mean(cons.prog, na.rm=T), cons.self=mean(cons.self, na.rm=T), cons.runoff=mean(cons.runoff, na.rm=T),
                                                   cons.impair=mean(cons.impair, na.rm=T)) %>%
    mutate(type="Landscape Grazing") %>% dplyr::select(type,lscape.graze,everything()) %>% rename(level=lscape.graze), 
)
write_csv(tbl8,"./results/tbl8.csv")

# Table 9 - Conservation Practices
tbl9 <- bind_rows(
  # All Farms
  merged2 %>% summarize(N=n(), con.hedge=mean(con.hedge, na.rm=T), con.nat=mean(con.nat, na.rm=T), 
                        con.buf=mean(con.buf, na.rm=T), con.time=mean(con.time, na.rm=T),
                        con.house=mean(con.house, na.rm=T), con.flower=mean(con.flower, na.rm=T), 
                        con.none=mean(con.none, na.rm=T)) %>%
    mutate(type="All Farms", level="All") %>% dplyr::select(type,level,everything()),
  # Revenue Category
  merged2 %>% group_by(rev_cat) %>% summarize(N=n(), con.hedge=mean(con.hedge, na.rm=T), con.nat=mean(con.nat, na.rm=T), 
                                                   con.buf=mean(con.buf, na.rm=T), con.time=mean(con.time, na.rm=T),
                                                   con.house=mean(con.house, na.rm=T), con.flower=mean(con.flower, na.rm=T), 
                                                   con.none=mean(con.none, na.rm=T)) %>%
    mutate(type="Revenue Category") %>% dplyr::select(type,rev_cat,everything()) %>% rename(level=rev_cat), 
  # Organic Status
  merged2 %>% group_by(org_abbr) %>% summarize(N=n(), con.hedge=mean(con.hedge, na.rm=T), con.nat=mean(con.nat, na.rm=T), 
                                               con.buf=mean(con.buf, na.rm=T), con.time=mean(con.time, na.rm=T),
                                               con.house=mean(con.house, na.rm=T), con.flower=mean(con.flower, na.rm=T), 
                                               con.none=mean(con.none, na.rm=T)) %>%
    mutate(type="Organic Status") %>% dplyr::select(type,org_abbr,everything()) %>% rename(level=org_abbr), 
  # Landscape Characteristics Wildlife
  merged2 %>% group_by(lscape.wild) %>% summarize(N=n(), con.hedge=mean(con.hedge, na.rm=T), con.nat=mean(con.nat, na.rm=T), 
                                                  con.buf=mean(con.buf, na.rm=T), con.time=mean(con.time, na.rm=T),
                                                  con.house=mean(con.house, na.rm=T), con.flower=mean(con.flower, na.rm=T), 
                                                  con.none=mean(con.none, na.rm=T)) %>%
    mutate(type="Landscape Wildlife Habitat") %>% dplyr::select(type,lscape.wild,everything()) %>% rename(level=lscape.wild), 
  # Landscape Characteristics Grazing
  merged2 %>% group_by(lscape.graze) %>% summarize(N=n(), con.hedge=mean(con.hedge, na.rm=T), con.nat=mean(con.nat, na.rm=T), 
                                                   con.buf=mean(con.buf, na.rm=T), con.time=mean(con.time, na.rm=T),
                                                   con.house=mean(con.house, na.rm=T), con.flower=mean(con.flower, na.rm=T), 
                                                   con.none=mean(con.none, na.rm=T)) %>%
    mutate(type="Landscape Grazing") %>% dplyr::select(type,lscape.graze,everything()) %>% rename(level=lscape.graze), 
)  
write_csv(tbl9,"./results/tbl9.csv")

tables <- list("tbl1"=tbl1, "tbl2"=tbl2, "tbl3"=tbl3, "tbl4"=tbl4, "tbl5"=tbl5,
               "tbl6"=tbl6, "tbl7"=tbl7, "tbl8"=tbl8, "tbl9"=tbl9, "tblA1"=tblA1, "tblA2"=tblA2)
write.xlsx(tables, file = "./results/tables.xlsx")


##  PROBIT MODELS
modeldata <- merged2 %>% mutate(org_abbr = relevel(org_abbr, ref="None"), 
                                rev_cat_abbr = relevel(rev_cat_abbr, ref="<25K"),
                                lscape.wild = relevel(lscape.wild, ref="None"),
                                fvacreage.pct = fvacreage.pct*100) %>% 
  drop_na(org_abbr, rev_cat_abbr, fvacreage.pct, lscape.wild, region_epa, sale.directcons)
rhsprob <- " ~ org_abbr + rev_cat_abbr + fvacreage.pct + sale.directcons + lscape.wild"

# Food Safety Practices
prac <- c("prac.watertest", "prac.soil","prac.monwild", "prac.deter", "prac.hygiene", "prac.emp", "prac.sani", "prac.rec")
fs.probmodels <- lapply(prac, function(x) probitmfx(as.formula(paste(x,rhsprob, sep="")), data=modeldata, robust=T, atmean=F))

# Buffers
buf <- c("buf.dirt", "buf.grass", "buf.lrcrop", "buf.ncv", "buf.nouse")
buf.probmodels <- lapply(buf, function(x) probitmfx(as.formula(paste(x,rhsprob, sep="")), data=modeldata, robust=T, atmean=F))

# Willife Habitat Control
hab <- c("hab.clearbuffer", "hab.expbuffer", "hab.lowriskcrop", "hab.fallow", "hab.corridor", "hab.drain", "hab.clearvegwater", "hab.clearvegnear")
hab.probmodels <- lapply(hab, function(x) probitmfx(as.formula(paste(x,rhsprob, sep="")), data=modeldata, robust=T, atmean=F))

# Wildlife Direct Deterence
det <- c("det.deerfence", "det.plasticfence", "det.waterfence", "det.mechtrap", "det.poison", "det.hunt", "det.deter")
det.probmodels <- lapply(det, function(x) probitmfx(as.formula(paste(x,rhsprob, sep="")), data=modeldata, robust=T, atmean=F))

# Conservation Practices
con <- c("con.hedge", "con.nat", "con.buf", "con.time", "con.house", "con.flower", "con.none")
con.probmodels <- lapply(con, function(x) probitmfx(as.formula(paste(x,rhsprob, sep="")), data=modeldata, robust=T, atmean=F))

capture.output(lapply(fs.probmodels, function(x) summary(x$fit)), file="./results/probit/fs.models.txt")
capture.output(lapply(buf.probmodels, function(x) summary(x$fit)), file="./results/probit/buf.models.txt")
capture.output(lapply(hab.probmodels, function(x) summary(x$fit)), file="./results/probit/hab.models.txt")
capture.output(lapply(det.probmodels, function(x) summary(x$fit)), file="./results/probit/det.models.txt")
capture.output(lapply(con.probmodels, function(x) summary(x$fit)), file="./results/probit/con.models.txt")

capture.output(fs.probmodels, file="./results/probit/fs.mfx.txt")
capture.output(buf.probmodels, file="./results/probit/buf.mfx.txt")
capture.output(hab.probmodels, file="./results/probit/hab.mfx.txt")
capture.output(det.probmodels, file="./results/probit/det.mfx.txt")
capture.output(con.probmodels, file="./results/probit/con.mfx.txt")

## CREATE A TABLE OF SIGNIFICANT PROBIT MODELS ##

sigcoef <- function(model, pval=0.05) {
  tmp <- summary(model$fit)
  tmpcoef <- tmp$coefficients[,1][-1]
  tmppval <- tmp$coefficients[,4][-1]
  cols <- names(tmpcoef)
  tmpdvar <- attr(tmp$terms, "variables")[[2]]
  sign <- ifelse(tmppval <= pval, ifelse(tmpcoef > 0, "(+)", "(-)"), " ")
  
  tmp.df <- data.frame(rbind(c(as.character(tmpdvar),sign)))
  colnames(tmp.df) <- c("depvar",cols)
  return(tmp.df)
}

sig.fs<- rbindlist(lapply(fs.probmodels, sigcoef))
sig.buf <- rbindlist(lapply(buf.probmodels, sigcoef))
sig.hab <- rbindlist(lapply(hab.probmodels, sigcoef))
sig.det <- rbindlist(lapply(det.probmodels, sigcoef))
sig.con <- rbindlist(lapply(con.probmodels, sigcoef))

sig.tbl <- rbind(sig.fs, sig.buf, sig.hab, sig.det, sig.con)
write.csv(sig.tbl, "./results/sig_tbl.csv", row.names=F)

## Tables of Coefficient Estimates ##
htmlreg(lapply(fs.probmodels, function(x) x$fit), file="./results/probit tables/tblA3.doc", digits=4, caption.above=T, stars = c(0.01, 0.05, 0.1),
        caption = "",
               custom.model.names = prac,
         custom.coef.names = c("Intercept", "Some Organic (0/1)", "Revenue $25K to $500K", "Revenue > $500K", "Produce Acreage Percent (0 to 1)", "DTC Sales Percentage (0 to 100)", "Bordering Noncrop Vegetation = Most", "Bordering Noncrop Vegetation = Some"),
        
        custom.note = "Note: Robust standard errors are reported in parentheses. Asterisk (*), double asterisk (**), and triple asterisk (***) 
                      indicate significance at the 10, 5 and 1 percent level, respectively."
)

htmlreg(lapply(buf.probmodels, function(x) x$fit), file="./results/probit tables/tblA4.doc", digits=4, caption.above=T, stars = c(0.01, 0.05, 0.1),
        caption = "",
        custom.model.names = buf,
        custom.coef.names = c("Intercept", "Some Organic (0/1)", "Revenue $25K to $500K", "Revenue > $500K", "Produce Acreage Percent (0 to 1)", "DTC Sales Percentage (0 to 100)", "Bordering Noncrop Vegetation = Most", "Bordering Noncrop Vegetation = Some"),
        
        custom.note = "Note: Robust standard errors are reported in parentheses. Asterisk (*), double asterisk (**), and triple asterisk (***) 
                      indicate significance at the 10, 5 and 1 percent level, respectively."
)

htmlreg(lapply(hab.probmodels, function(x) x$fit), file="./results/probit tables/tblA5.doc", digits=4, caption.above=T, stars = c(0.01, 0.05, 0.1),
        caption = "",
        custom.model.names = hab,
        custom.coef.names = c("Intercept", "Some Organic (0/1)", "Revenue $25K to $500K", "Revenue > $500K", "Produce Acreage Percent (0 to 1)", "DTC Sales Percentage (0 to 100)", "Bordering Noncrop Vegetation = Most", "Bordering Noncrop Vegetation = Some"),
        
        custom.note = "Note: Robust standard errors are reported in parentheses. Asterisk (*), double asterisk (**), and triple asterisk (***) 
                      indicate significance at the 10, 5 and 1 percent level, respectively."
)

htmlreg(lapply(det.probmodels, function(x) x$fit), file="./results/probit tables/tblA6.doc", digits=4, caption.above=T, stars = c(0.01, 0.05, 0.1),
        caption = "",
        custom.model.names = det,
        custom.coef.names = c("Intercept", "Some Organic (0/1)", "Revenue $25K to $500K", "Revenue > $500K", "Produce Acreage Percent (0 to 1)", "DTC Sales Percentage (0 to 100)", "Bordering Noncrop Vegetation = Most", "Bordering Noncrop Vegetation = Some"),
        
        custom.note = "Note: Robust standard errors are reported in parentheses. Asterisk (*), double asterisk (**), and triple asterisk (***) 
                      indicate significance at the 10, 5 and 1 percent level, respectively."
)

htmlreg(lapply(con.probmodels, function(x) x$fit), file="./results/probit tables/tblA7.doc", digits=4, caption.above=T, stars = c(0.01, 0.05, 0.1),
        caption = "",
        custom.model.names = con,
        custom.coef.names = c("Intercept", "Some Organic (0/1)", "Revenue $25K to $500K", "Revenue > $500K", "Produce Acreage Percent (0 to 1)", "DTC Sales Percentage (0 to 100)", "Bordering Noncrop Vegetation = Most", "Bordering Noncrop Vegetation = Some"),
        
        custom.note = "Note: Robust standard errors are reported in parentheses. Asterisk (*), double asterisk (**), and triple asterisk (***) 
                      indicate significance at the 10, 5 and 1 percent level, respectively."
)


## FIGURE 1 - RESPONSES BY EPA REGION MAP ##
eparegion<-st_read("./data/Region_EPA_Modified.shp")
merged2 %>% group_by(region_epa) %>% summarize(freq=n())
totalaggregate<-table(total$region_epa_mod)%>%as.data.frame()
colnames(totalaggregate)<-c("NAME", "freq")

totalaggregate<-as.data.frame(totalaggregate)
totalaggregate$NAME<-as.factor(totalaggregate$NAME)
eparegion<-merge(eparegion, totalaggregate, by.x="Mod_EPA_Re",by.y="NAME", all.y=TRUE, all.x=TRUE)

ggplot()+geom_sf(data=eparegion, aes(fill=freq))+geom_sf(data=state, fill=NA, color="black", aes())+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")+theme_bw()+theme(legend.title = element_blank())


## PEARSON CHI-SQUARED TESTS ##
cut1 <- c("rev_cat", "org_abbr", "region_epa", "fvacreage.quartile", "directsales.quartile")
cut2 <- c("rev_cat", "org_abbr", "lscape.wild", "lscape.graze")
cut3 <- c("rev_cat", "org_abbr", "fvacreage.quartile", "lscape.wild", "lscape.graze")
cut4 <- c("rev_cat", "org_abbr", "fvacreage.quartile", "directsales.quartile")
cut <- list(cut1, cut2, cut2, cut2, cut2, cut3, cut4, cut2, cut2)
resp <- list(c("emp.train", "fs.plan", "fs.consult", "fs.insure", "fs.audit", "fs.loss"), 
          c("prac.watertest", "prac.soil", "prac.monwild", "prac.deter", "prac.hygiene", "prac.emp", "prac.sani", "prac.rec"), 
          c("buf.dirt", "buf.grass", "buf.lrcrop", "buf.ncv", "buf.nouse"), 
          c("hab.clearbuffer", "hab.expbuffer", "hab.lowriskcrop", "hab.fallow", "hab.corridor", "hab.drain", "hab.clearvegwater", "hab.clearvegnear"),
          c("det.deerfence", "det.plasticfence", "det.waterfence", "det.mechtrap", "det.poison", "det.hunt", "det.deter"), 
          c("hr.bird", "hr.rodent", "hr.deer", "hr.rept", "hr.pet", "hr.livestock", "hr.draft"), 
          c("buyer", "auditor", "advisor", "noone"),     #merged3
          c("cons.prog", "cons.self", "cons.impair"),
          c("cons.runoff", "con.hedge", "con.nat", "con.buf", "con.time", "con.house", "con.flower", "con.none"))

chi_list <- vector(mode="list", length=9)
chi_list_tbl <- vector(mode="list", length=9)
for (i in 1:9) {
  chi_list[[i]] <- lapply(cut[[i]], function(x) lapply(resp[[i]], function(y) chisq.test(x=merged3[[x]], y=merged3[[y]])))
  chi_list_tbl[[i]] <- data.frame(cut=rep(cut[[i]], each=length(resp[[i]])), resp=rep(resp[[i]], times=length(cut[[i]])),
                                  stat=unlist(lapply(chi_list[[i]], function(x) lapply(x, function(y) y$statistic)),recursive = T),
                                  df=unlist(lapply(chi_list[[i]], function(x) lapply(x, function(y) y$parameter)),recursive = T),
                                  pval=unlist(lapply(chi_list[[i]], function(x) lapply(x, function(y) y$p.value)),recursive = T)) %>% 
    mutate(sig=ifelse(pval<=0.05,"*",""))
}          
chi_sq_tbl <- rbindlist(chi_list_tbl)
write.csv(chi_sq_tbl, "./results/chi_tbl.csv", row.names=F)


## FRUIT AND VEGETABLE ACREAGE C.I.'s
# Average FV Acreage
merged2 %>% filter(!is.na(fvacreage.pct)) %>% summarize(n=n(), mu=mean(fvacreage.pct), sigma=sd(fvacreage.pct)) %>% 
  mutate(se=sigma/sqrt(n), ciMult=qt(0.95/2 + .5, n-1), ci = se*ciMult, upper=mu+ci, lower=mu-ci)