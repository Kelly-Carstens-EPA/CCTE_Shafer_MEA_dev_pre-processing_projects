# checking out several compounds tested in 20160921
# to see how the outlier points affect the result
# 11/11/2020 - keeping all of the wllt="n" points so that I have a realistic bmad and coff
# The guiding question:
# How does TCPL respond to extreme outliers?
# Then I will investigate the next question - should I remove this really bad plate
# (asking the first q in hopes of just addressing my fears/level of concern for teh future)

# script to run MEA acute lvl 0 data
library(tcpl)
library(data.table)
library(RMySQL)
library(openxlsx)
tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

# REVIEW REGISTRATION>UPDATE --------------------------------------------------------------------------------

acids <- tcplLoadAcid(fld = "asid", val = 20)
acids[grepl("MEA_dev",acnm)]
# asid acid                                               acnm
# 1:   20 2961         CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12
# 2:   20 2962               CCTE_Shafer_MEA_dev_firing_rate_mean
# 3:   20 2963 CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12
# 4:   20 2964       CCTE_Shafer_MEA_dev_active_electrodes_number
# 5:   20 2965                            CCTE_Shafer_MEA_dev_LDH

# REGISTER NEW ACID/AEID --------------------------------------------------------------------------------

# PREPARE MC0 ------------------------------------------------------
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/")
load("lvl0_snapshots/mea_nfa_lvl0_2020-11-10.RData")

# Assign acid's
acid.acnm <- tcplLoadAcid(fld = "acnm", val = unique(mea_nfa_lvl0$acnm))
mea_nfa_lvl0 <- merge(mea_nfa_lvl0, acid.acnm, by = "acnm") # will drop the endpoints I have not registered
mea_nfa_lvl0[, acnm := NULL]

# extract the apid I am interested in (plus all points where wllt == "n")
check_spids <- mea_nfa_lvl0[grepl("20160921",apid) & wllt == "t", unique(spid)]
use_apid <- mea_nfa_lvl0[spid %in% check_spids, unique(apid)]
mea_nfa_lvl0 <- mea_nfa_lvl0[apid %in% use_apid | wllt == "n"]

# register spids
tcplLoadChem(field = "spid", val = check_spids) # 12 spids loaded
setdiff(unique(mea_nfa_lvl0$spid), tcplLoadChem(field = "spid", val = check_spids)$spid) # "DMSO"         "Water"        "DMSO/Ethanol" "Ethanol"

mc0 <- mea_nfa_lvl0
nrow(mc0) # 9508

mc0[, .N, by = .(apid, acid)][N != 48, unique(N)] # 6 12  8 Ah yes, the few plates from Brown2014 have diff control well arrangement

# close any connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

tcplConfList() # confirming db
tcplWriteLvl0(mc0, type = "mc") # Nov 11, 2020 9:37pm
# Completed delete cascade for 10 ids (90.87 secs)
# [1] TRUE


# REGISTER/CONFIRM METHODS -----------------------------------------
acids <- unique(mea_nfa_lvl0$acid)
aeid.info <- tcplLoadAeid(fld = "acid", val = acids)

# remove the LDH_up endpoint methods
# actually, I'm not sure how to do that.

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

# Nov 11, 9:44pm 
tcplRun(slvl = 1L, elvl = 6L, id = unique(assay.list$acid), type = "mc")

# Total processing time: 20.85 mins 

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")


# Save data and plots ----------------------------

# save the data, see some plots
aeids <- tcplLoadAeid(fld = "acid", val = unique(assay.list$acid))
for (i in aeids$aeid){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   odir=file.path(getwd(),"sbox_dat/plots"))
}

con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='sbox_invitrodb_v3_2',host = Sys.getenv('INVITRODB_HOST_RO'))

# get mc3, add spid, apid, rowi, coli, etc from mc0, add cndx, repi, cval from mc1, add cval from mc2
mc3 <- dbGetQuery(con, paste0("SELEct m0id, aeid, acid, bval, pval, logc, resp FROM mc3 WHERE aeid IN (",paste0(aeids$aeid,collapse=","),")"))
setDT(mc3)
mc1 <- dbGetQuery(con, paste0("SELEct m0id, acid, cndx, repi FROM mc1 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
mc3 <- merge(mc3, mc1, all.x = T)
mc2 <- dbGetQuery(con, paste0("SELEct m0id, acid, cval FROM mc2 WHERE acid IN (",paste0(unique(assay.list$acid),collapse=","),")"))
mc3 <- merge(mc3, mc2, all.x = T)
mc0 <- tcplLoadData(lvl = 0L, fld = "acid", val = unique(assay.list$acid), type = "mc")
mc3 <- merge(mc3, mc0[, .(m0id, spid, acid, apid, rowi, coli, wllt, wllq, conc, srcf)], all.x = T)

# get mc5_mc6 data
mc5 <- tcplPrepOtpt(tcplLoadData(lvl = 5L, fld = "aeid", val=aeids$aeid, type = "mc"))
mc6 <- tcplLoadData(lvl = 6L, fld = "aeid", val=assay.list$aeid, type = "mc")
mc6_collapsed <- mc6[, .(flags = paste0(unique(flag), collapse = ","), flag_length = length(unique(flag)), flag_ids = paste0(unique(mc6_mthd_id), collapse = ",")), by = c("spid","aeid")]
mc5_mc6 <- merge(mc5, mc6_collapsed, by = c("spid","aeid"), all.x = T)

# get the methods
mc3_mthds <- tcplMthdLoad(lvl = 3L, id = assay.list$aeid, "mc")
mc4_mthds <- tcplMthdLoad(lvl = 4L, id = assay.list$aeid, "mc")
mc6_mthds <- tcplMthdLoad(lvl = 6L, id = assay.list$aeid, "mc")

# save the data
sbox_run_summary <- paste0("Testing the impact of extreme outliers from 20190621, esp in MFR DIV 12. Keeping all wllt=='n' points.")
save(sbox_run_summary, mc3_mthds, mc4_mthds, mc6_mthds, mc3, mc5_mc6, 
     file = file.path(getwd(),paste0("sbox_dat/sbox_dat_",as.character.Date(Sys.Date()),".RData")))
dbDisconnect(con)
rm(list = c("mc5","mc6","mc6_collapsed"))
