# checking out several compounds tested in 20160921
# to see how the outlier points affect the result

# script to run MEA acute lvl 0 data
library(tcpl)
library(data.table)
library(RMySQL)
library(openxlsx)
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

# REVIEW REGISTRATION>UPDATE --------------------------------------------------------------------------------

acids <- tcplLoadAcid(fld = "asid", val = 20)
acids[grepl("MEA_dev",acnm)] # oh dear, empty! must register these


# REGISTER NEW ACID/AEID --------------------------------------------------------------------------------

tcplLoadAsid(fld = "asid", val = 20)
tcplLoadAid(fld = "asid", val = 20)
# asid aid        anm
# 1:   20 685 NHEERL_MEA

# identify new acnm
new_acnm <- setdiff(unique(mea_nfa_lvl0$acnm), tcplLoadAcid(fld = "acnm", val = unique(mea_nfa_lvl0$acnm))$acnm)
new_acnm <- grep("(firing_rate_mean)|(active_electrodes_number)|(LDH)",new_acnm,val=T) # limiting to just 4 endpoints fro now

new.acid.info <- as.data.table(list(asid = rep(20,length(new_acnm)), aid = rep(685,length(new_acnm)), acnm = new_acnm))
tcplConfList() # sbox confrimed
for (i in 1:nrow(new.acid.info)){
  #Assay Component Registration
  tcplRegister(what="acid", flds=list(aid= new.acid.info$aid[i], 
                                      acnm = new.acid.info$acnm[i]))
  
  # acsn registreation
  acid <- tcplLoadAcid(fld="acnm",val= new.acid.info$acnm[i])$acid
  tcplRegister(what="acsn", flds=list(acid= acid, acsn = new.acid.info$acnm[i]))
  
  # aeid registration
  tcplRegister(what = "aeid", flds = list(acid = rep(acid,2), 
                                          aenm = paste0(new.acid.info$acnm[i],c("_up","_dn")), 
                                          normalized_data_type = rep("percent_activity",2)))
}

# check it out
tcplLoadAcid(fld = "acnm", val = new.acid.info$acnm, add.fld = "acsn")
# yep, all there!
tcplLoadAeid(fld = "acnm", val = new.acid.info$acnm, add.fld = c("normalized_data_type"))
# looks great!


# PREPARE MC0 ------------------------------------------------------
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/")
load("lvl0_snapshots/mea_nfa_lvl0_2020-11-10.RData")

# Assign acid's
acid.acnm <- tcplLoadAcid(fld = "acnm", val = unique(mea_nfa_lvl0$acnm))
mea_nfa_lvl0 <- merge(mea_nfa_lvl0, acid.acnm, by = "acnm") # will drop the endpoints I have not registered
mea_nfa_lvl0[, acnm := NULL]

# extract the apid I am interested in
check_spids <- mea_nfa_lvl0[grepl("20160921",apid) & wllt == "t", unique(spid)]
use_apid <- mea_nfa_lvl0[spid %in% check_spids, unique(apid)]
mea_nfa_lvl0 <- mea_nfa_lvl0[apid %in% use_apid]

# register spids
tcplLoadChem(field = "spid", val = check_spids) # empty :(
spidmap <- as.data.table(read.xlsx("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/EPA_12088_EPA-Shafer_96misc_75ul_20160826_key.xlsx", sheet = 1))
spidmap <- spidmap[EPA_SAMPLE_ID %in% check_spids]
setnames(spidmap, old = "EPA_SAMPLE_ID", new = "spid")
spidmap[, casn := sub("'","",sub("'","",casrn))]

# see if any of htese spids are already register in the chemical table
chem.map <- tcplLoadChem(field = "casn", val = spidmap$casn, include.spid = F) # all 12 present
chem.map

# register the spids
tcplRegister(what = "spid",
             flds = merge(spidmap[ , list(spid, casn)],
                          chem.map[ , list(casn, chid)],
                          by = "casn")[ , list(spid, chid)])
# TRUE!

mc0 <- mea_nfa_lvl0
nrow(mc0) # 2160

# close any connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConfList() # confirming db
tcplWriteLvl0(mc0, type = "mc") # Nov 11, 2020 5:26pm
# Completed delete cascade for 10 ids (87.92 secs)
# [1] TRUE


# REGISTER/CONFIRM METHODS -----------------------------------------
acids <- unique(mea_nfa_lvl0$acid)
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# lvl 2
tcplMthdLoad(lvl=2, id = acids) # totally empty
# tcplMthdClear(lvl=2L, id = acids, type = "mc") # this is what froze for 3 hrs last time!
tcplMthdAssign(lvl = 2L, id = acids, mthd_id = 1, ordr = c(1), type = "mc") # all none for lvl 2
tcplMthdLoad(lvl=2, id = acids) # yay, all 45 are none!

# lvl 3
dn.mea <- aeid.info[grepl("_dn",aenm), aeid]
tcplMthdLoad(lvl=3, id = dn.mea)
tcplMthdList(lvl=3L, "mc")
# tcplMthdClear(lvl=3, id = dn.mea, type = "mc")
tcplMthdAssign(lvl = 3, id = dn.mea, mthd_id = c(11, 32, 5), ordr = c(1:3), type = "mc")
tcplMthdLoad(lvl=3, id = dn.mea)
# aeid                mthd mthd_id ordr
# 1: 3263 bval.apid.nwlls.med      11    1
# 2: 3263           pval.zero      32    2
# 3: 3263             resp.pc       5    3

up.mea <- aeid.info[grepl("_up",aenm), aeid]
tcplMthdLoad(lvl=3, id = up.mea)
tcplMthdAssign(lvl = 3, id = up.mea, mthd_id = c(11, 32, 5, 6), ordr = c(1:4), type = "mc")
# aeid                mthd mthd_id ordr
# 1: 3262 bval.apid.nwlls.med      11    1
# 2: 3262           pval.zero      32    2
# 3: 3262             resp.pc       5    3
# 4: 3262       resp.multneg1       6    4

# lvl 4
tcplMthdLoad(lvl=4L, id = aeid.info$aeid)
# tcplMthdClear(lvl=4L, id = aeid.info$aeid, type = "mc")
aeid.new <- setdiff(aeid.info$aeid, tcplMthdLoad(lvl=4L, id = aeid.info$aeid)[, unique(aeid)])
tcplMthdAssign(lvl=4L, id = aeid.new, mthd_id = c(2), ordr = c(1), type = "mc")
tcplMthdLoad(lvl=4L, id = aeid.info$aeid)
# bmad.aeid.lowconc.nwells

# lvl 5
tcplMthdLoad(lvl=5L, id = aeid.info$aeid)
# tcplMthdClear(lvl=5L, id = aeid.info$aeid, type = "mc")
aeid.new <- setdiff(aeid.info$aeid, tcplMthdLoad(lvl=5L, id = aeid.info$aeid)[, unique(aeid)])
tcplMthdAssign(lvl=5L, id = aeid.new, mthd_id = c(1), ordr = c(1), type = "mc")
# bmad3

# lvl 6
tcplMthdLoad(lvl=6L, id = aeid.info$aeid)
lvl6_mthds <- tcplMthdList(lvl = 6L, type = "mc")
tcplMthdAssign(lvl = 6L, id = aeid.info[!grepl("(LDH)|(AB)",aenm),aeid], mthd_id = lvl6_mthds[mc6_mthd != "viability.gnls",mc6_mthd_id], ordr = c(1:length(mthd_id)), type = "mc")
tcplMthdAssign(lvl = 6L, id = aeid.info[grepl("(LDH)|(AB)",aenm),aeid], mthd_id = lvl6_mthds$mc6_mthd_id, ordr = c(1:length(mthd_id)), type = "mc")
tcplMthdLoad(lvl=6L, id = aeid.info$aeid)

# view all 
for (i in 3:6) {
  print(tcplMthdLoad(lvl = i, id = aeid.info$aeid))
}


# ----------------------------------- tcpl run with hack function
rm(list = ls())
library(tcpl)
library(RMySQL)
library(data.table)

assay.list <- tcplLoadAcid(fld = "asid", val= 20)[grepl("dev",acnm) , ]

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source("C:/Users/Acarpe01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R", echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList()

# Nov 10, 5:41 pm 
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

# Writing level 6 complete. (0.17 secs)
# 
# 
# Total processing time: 13.9 mins 

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")


# Save data and plots ----------------------------

# save the data, see some plots
aeids <- tcplLoadAeid(fld = "acid", val = unique(assay.list$acid))
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/plots")
for (i in aeids$aeid){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   odir=getwd())
}

# oh oops.. I forgot that a tiny dataset will affect the bmad...

# I aborted this auqeyre bc I thought I made a mistake, then re-ran adn got this message
mc1 <- dbGetQuery(con, paste0("SELEct m0id, acid, cndx, repi FROM mc1 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
# Error in .local(conn, statement, ...) : 
#   connection with pending rows, close resultSet before continuing

# I aborted a query before it could complete
dbClearResult(dbListResults(con)[[1]]) # TRUE
dbListResults(con)[[1]]
# Error in dbListResults(con)[[1]] : subscript out of bounds
# cool, now I don't have any pending results?

# con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))
# mc3 <- dbGetQuery(con, paste0("SELEct m0id, aeid, acid, bval, pval, logc, resp FROM mc3 WHERE aeid IN (",paste0(aeids$aeid,collapse=","),")"))
# setDT(mc3)
# mc1 <- dbGetQuery(con, paste0("SELEct m0id, acid, cndx, repi FROM mc1 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
# mc3 <- merge(mc3, mc1, all.x = T)
# mc2 <- dbGetQuery(con, paste0("SELEct m0id, acid, cval FROM mc2 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
# mc3 <- merge(mc3, mc2, all.x = T)
# mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = unique(assay.list$acid), type = "mc")
# mc3 <- merge(mc3, mc0[, .(m0id, spid, acid, apid, rowi, coli, wllt, wllq, conc, srcf)], all.x = T)
# mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=aeids$aeid, type = "mc"))
# mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=assay.list$aeid, type = "mc")
# mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), flag_ids = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
# mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)
# mc3_mthds <- tcplMthdLoad(lvl = 3L, id = assay.list$aeid, "mc")
# mc4_mthds <- tcplMthdLoad(lvl = 4L, id = assay.list$aeid, "mc")
# mc6_mthds <- tcplMthdLoad(lvl = 6L, id = assay.list$aeid, "mc")
# setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
# save(mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, file = paste0("sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData"))
# dbDisconnect(con)
# rm(list = c("mc5","mc6","mc6_collapsed"))
