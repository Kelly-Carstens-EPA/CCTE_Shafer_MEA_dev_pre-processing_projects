# Checking out some things for Tim
# As he addresses comments from reviewers of the manuscript
# Feb 3, 2022

library(tcpl)
library(data.table)
tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb')

# Question 1: GF Ammonium - has it been pipelined?
gfa.chem.tb <- tcplLoadChem(field = 'chnm', val = 'Glufosinate-ammonium', include.spid = T)
gfa.chem.tb # all have same DTXSID
mc5 <- tcplLoadData(lvl = 5L, fld = 'spid', val = gfa.chem.tb$spid, type = 'mc')
mc5 <- tcplPrepOtpt(mc5)
mc6 <- tcplLoadData(lvl = 6L, fld = 'spid', val = gfa.chem.tb$spid, type = 'mc')
mc5_mc6 <- merge(mc5, mc6[, .(mc6_mthd_id = paste0(unique(mc6_mthd_id),collapse=','),
                              flags = paste0(unique(flag),collapse=',')), by = .(spid, aeid)], by = c('spid','aeid'), all = T)
mc5[, .N, by = .(aenm)]

# Tim wants to know if the "EFSA" data has been pipelined
# Because there was 1 hit for the NPC3 assay
# looks like EFSA = IUF

# Any hits?
mc5_mc6[grepl('IUF',aenm), .N, by = .(hitc)]
# hitc  N
# 1:    0 29
# no hits in IUF assays. 
# Confirm that NPc3 assays is present
mc5_mc6[grepl('NPC3',aenm), .N, by = .(hitc, aenm, flags)]
# hitc                                       aenm flags N
# 1:    0 IUF_NPC3_neuronal_differentiation_120hr_up  <NA> 1
# 2:    0 IUF_NPC3_neuronal_differentiation_120hr_dn  <NA> 1
# no hit, no flags

# Any hits in any other assay?
mc5_mc6[, .N, by = .(hitc)]
# hitc   N
# 1:    0 190
# 2:    1   9
mc5_mc6[hitc == 1, .(aenm, flags)]
mc5_mc6[hitc == 1, .N, by = .(grepl('MEA_acute',aenm))]
# true for all 9
# not sure if Tim is interested in that

# Plot of NPC endpoints!
m4ids <- mc5_mc6[grepl('IUF_NPC',aenm)][order(spid, aeid),unique(m4id)]
length(m4ids)
pdf(file = 'DNTGF2019/glufosinate-ammonium_IUF_NPC_tcpl_dose_response_2022-02-03.pdf', width = 14, height = 7)
for (m4idi in m4ids) {
  tcplPlotM4ID(m4idi, lvl = 6L)
}
graphics.off()

# Update 5:37 PM
# Okay, so it looks like invitrodb has changed as of this morning!
mc5_mc6[grepl('IUF',aenm), .N, by = .(hitc)]
# hitc  N
# 1:    0 28
# 2:    1  1
mc5_mc6[hitc == 1, .N, by = .(aenm)]
# just the same 9 acute endpints, pluts
# IUF_NPC2a_radial_glia_migration_72hr_dn

# Rechecking 2/4/2022, now that Kelly has said it is officially changed
# still the same as yesterday?
mc5_mc6[grepl('IUF',aenm), .N, by = .(hitc)]
# hitc  N
# 1:    0 28
# 2:    1  1
mc5_mc6[hitc == 1, .N, by = .(aenm)]
# same!
# okay, but should i update the fig?
# when was the last change made?
mc5_mc6[aenm == 'IUF_NPC2a_radial_glia_migration_72hr_dn', unique(aeid)] # 2938
library(RMySQL)
con <- dbConnect(drv = MySQL(),user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), dbname = 'invitrodb')
mc5 <- dbGetQuery(con, 'SELECT * FROM mc5 WHERE aeid = 2938;')
unique(mc5$modified_date)
# [1] "2022-02-03 14:04:51"
# okay, cool, so this hasn't changed sine 5pm yesterda
dbDisconnect(con)



# Loperamide --------------------------------------------------------------
# Tim wants to compare the AC50s in the cyto assays with teh other NFA hits
lop.chem.tb <- tcplLoadChem(field = 'dsstox_substance_id', val = 'DTXSID00880006', include.spid = T)
mc5 <- tcplLoadData(lvl = 5L, fld = 'spid', val = lop.chem.tb$spid, type = 'mc')
mc5 <- tcplPrepOtpt(mc5)
mc6 <- tcplLoadData(lvl = 6L, fld = 'spid', val = lop.chem.tb$spid, type = 'mc')
mc5_mc6 <- merge(mc5, mc6[, .(mc6_mthd_id = paste0(unique(mc6_mthd_id),collapse=','),
                              flags = paste0(unique(flag),collapse=',')), by = .(spid, aeid)], by = c('spid','aeid'), all = T)

# Want to pull dose-response curves for Loperamide for the NFA
names(mc5)
m4ids <- mc5_mc6[hitc == 1 & grepl('MEA_dev',aenm) & !grepl('DIV12',aenm)][order(spid, aeid),unique(m4id)]
# not that there are 3 different spids here
pdf(file = 'DNTGF2019/loperamide_MEA_dev_hits_2022-02-03.pdf', width = 14, height = 7)
for (m4idi in m4ids) {
  tcplPlotM4ID(m4idi, lvl = 6L)
}
graphics.off()

# Just curious, is lop a hit for any other relevant endpoints?
mc5_mc6[, .N, by = .(hitc)]
mc5_mc6[hitc == 1, .N, by = .(aenm)]
# quite a few dev, HCI, and acute

# Does this apply to the spid of interest?
mc5_mc6[spid == 'EX000411' & hitc == 1, .N, by = .(aenm)]
# yep, some HCI hits

# I guess I'll save the AC50s as well?
ac50.tb <- mc5_mc6[grepl('MEA_dev',aenm) & !grepl('DIV12',aenm), .(spid,aeid,chid,casn,chnm,dsstox_substance_id,code,aenm,
                       max_med,max_med_conc,logc_max,logc_min,hitc,modl,fitc,coff,actp,
                       modl_ga,mc6_mthd_id,flags)]
ac50.tb[, ac50_log10 := ifelse(hitc == 1, modl_ga, NA_real_)]
ac50.tb[, ac50_uM := ifelse(hitc == 1, 10^modl_ga, NA_real_)]
ac50.tb <- ac50.tb[grepl('MEA_dev',aenm) & !grepl('DIV12',aenm), .(spid,aeid,chid,casn,chnm,dsstox_substance_id,code,aenm,
                                                                   max_med,max_med_conc,logc_max,logc_min,hitc,modl,fitc,coff,actp,
                                                                   modl_ga,ac50_log10,ac50_uM,mc6_mthd_id,flags)]
write.csv(ac50.tb, file = 'DNTGF2019/loperamide_MEA_dev_mc5_mc6_2022-02-03.csv', row.names = F)
