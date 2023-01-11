#-----------------------------------------------------------------------------------#
# SAving updated suggested sc2 methds
# 5/20/2021
#-----------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------#
# loading libraries
#-----------------------------------------------------------------------------------#

rm(list = ls())

library(data.table)
library(stringi)
library(openxlsx)
library(tcpl)

setwd("C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_nfa/SPS_endpoint_investigation/")

load('Snapshot_Jan-14-2021/SPS Hit Call Analysis/SPS_PFAS2019_normalized_dat_2021-01-25.RData')
load('Snapshot_Jan-14-2021/SPS Hit Call Analysis/endset_tables_2021-01-25.RData')

#-----------------------------------------------------------------------------------#
# Prep dat
#-----------------------------------------------------------------------------------#
# create coff.tb with coff's developed by mc analysis
for (i in 1:length(estl_unique)) {
  # renaming to match aenm as appears in sc data (inter_network_spike_interval is the correct version)
  estl_unique[[i]]$aenm <- stri_replace_all_fixed(estl_unique[[i]]$aenm, pattern = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean',
                                                  replacement = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean')
}
coff.tb <- estl_unique[[1]]
for (i in 2:length(estl_unique)) {
  coff.tb <- merge(coff.tb, estl_unique[[i]], by = c('aenm','aeid'), all = T, suffixes = c('',paste0('.',i)))
}
coff.tb[, nonexchangeable_endpoint := as.numeric(!(is.na(coff) | is.na(coff.2) | is.na(coff.3)))]

# Confirmation that the coffs for the nonexchangeable endpoints are the same with every table
coff.tb[nonexchangeable_endpoint == 1 & coff != coff.2] # empty
coff.tb[nonexchangeable_endpoint == 1 & coff != coff.3] # empty
coff.tb[nonexchangeable_endpoint == 1 & coff.2 != coff.3] # empty

# Merge all into 1 set of coffs
coff.tb[is.na(coff) & !is.na(coff.2), coff := coff.2]
coff.tb[is.na(coff) & !is.na(coff.3), coff := coff.3]
coff.tb[, `:=`(coff.2 = NULL, coff.3 = NULL)]


# Create table with coffs and bmad's together
bmad.coff <- merge(dat[, .(bmad = unique(bmad)), by = .(aenm)], coff.tb[, .(aenm, coff.current = coff, nonexchangeable_endpoint)], by = c('aenm'), all.y = T)
bmad.coff[, `:=`(coff.bmad1.5 = bmad*1.5, coff.bmad2 = bmad*2, coff.bmad3 = bmad*3, coff.bmad5 = bmad*5, coff.bmad6 = bmad*6, coff.bmad10 = bmad*10,
                 coff.pc20 = 20, coff.pc25 = 25, coff.pc30 = 30, coff.pc88 = 88)]
bmad.coff.long <- melt(bmad.coff, id.vars = c('aenm','nonexchangeable_endpoint','bmad','coff.current'), variable.name = 'sc2_mthd', variable.factor = F, value.name = 'coff_value')
bmad.coff.long[, coff_value_per_bmad := coff_value/bmad]
bmad.coff.long[, coff_current_per_bmad := coff.current/bmad]
bmad.coff.long[, sc2_mthd_value_dist_to_coff_current := signif(abs(coff_value - coff.current),5), by = .(aenm)]

# Get the max_med's for each test chemical
max_med.tb <- merge(dat[wllt == 't', .(max_med = (unique(max_med))), by = .(aenm, bmad, spid)], coff.tb, all.y = T, by = 'aenm') # dropping aenm in dat not in coff.tb
max_med.tb[, cur_hitc := as.numeric(max_med >= coff)]
max_med.tb[, any_hit := as.numeric(sum(cur_hitc)>0), by = .(spid)]
max_med.tb[, max_med_per_bmad := max_med/bmad]

# Determine the number of added positives with each potential coff
max_med.tb.coffs <- merge(max_med.tb, bmad.coff, by = c('aenm','bmad','nonexchangeable_endpoint'), all = T) # adding in all potential coffs
max_med.tb.coffs[, .(length(unique(spid))), by = .(aenm)] # 131, but 125 for some div12 endpoints
max_med.tb.coffs[, (paste0('hitc_',grep('coff\\.',names(max_med.tb.coffs),val=T))) := lapply(.SD, function(x) as.numeric(max_med >= x)), .SDcols = grep('coff\\.',names(max_med.tb.coffs),val=T)]
max_med.tb.coffs[, (paste0('num_add_pos_',grep('^coff\\.',names(max_med.tb.coffs),val=T))) := lapply(.SD, function(x) sum(x[any_hit == 0])), .SDcols = grep('hitc_coff\\.',names(max_med.tb.coffs),val=T), by = .(aenm)]
num_added_pos.tb <- unique(melt(max_med.tb.coffs, id.vars = c('aenm','bmad','nonexchangeable_endpoint'), measure.vars = grep('num_add_pos_coff\\.',names(max_med.tb.coffs),val=T),
                                variable.factor = F, variable.name = 'sc2_mthd', value.name = 'num_add_pos'))
num_added_pos.tb[, sc2_mthd := sub('num_add_pos_','',sc2_mthd)]
# 0 added hits -> best option
# <0 added hits (i.e., removed hits) -> second best option
# >0 added hits -> worst option. Sort these after the worst possible scenario of removing all hits

# Is there at least 1 method that will result in 0 hits for sc negatives?
num_added_pos.tb[, .(sum(num_add_pos == 0)), by = .(aenm)][V1 == 0]
# empty -> yes

# Add the num_added_pos.tb
setdiff(num_added_pos.tb$sc2_mthd, bmad.coff.long$sc2_mthd) # "coff.current"
setdiff(bmad.coff.long$sc2_mthd, num_added_pos.tb$sc2_mthd) # empty
bmad.coff.long <- merge(bmad.coff.long, num_added_pos.tb[sc2_mthd != 'coff.current'], by = c('aenm','bmad','nonexchangeable_endpoint','sc2_mthd'), all = T)

# Select the winning method
setkey(bmad.coff.long, sc2_mthd_value_dist_to_coff_current)
bmad.coff.long[, sc2_mthd_selection := sc2_mthd[num_add_pos == 0][1], by = .(aenm)]
bmad.coff.long[, sc2_mthd_selection_value_per_bmad := coff_value_per_bmad[sc2_mthd == sc2_mthd_selection], by = .(aenm)]
bmad.coff.long[sc2_mthd == sc2_mthd_selection, unique(.SD), .SDcols = c('aenm','bmad','sc2_mthd_selection','sc2_mthd_selection_value_per_bmad','num_add_pos','sc2_mthd_value_dist_to_coff_current')]


# -----------------------------------------------------------------------------------#
# Illustrate situation re inter network spike interval
# -----------------------------------------------------------------------------------#
tcplLoadAcid(fld = 'acnm', val = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean', add.fld = c('aenm','aeid'))
# acnm acid                                                              aenm aeid
# 1: CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean 2483 CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean_up 2519
# 2: CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean 2483 CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean_dn 2518


# -----------------------------------------------------------------------------------#
# Create table containing the desired sc methods
# -----------------------------------------------------------------------------------#
# updated 5/19/2021 -> switching resp.multneg1 to up, not dn
# update 5/20/201 -> adding methods for all endpoints for each endpoint (required for tcplRun)
tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_MY'), pass = Sys.getenv('INVITRODB_PASS_MY'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb')

acnms <- unique(sub('_[a-z]{2}$','',unique(coff.tb$aenm)))
all.mea.aenms <- paste0(rep(acnms[!grepl('(LDH)|(AB)',acnms)],each=2),c('_up','_dn'))
aenms <- union(unique(coff.tb$aenm), all.mea.aenms)
sort(aenms)

# looks like lvl1 methods are by aeid
sc1_mthd_options <- tcplMthdList(lvl = 1L, type = 'sc')
up.lvl1 <- data.table(aenm = rep(grep('_up',aenms,val=T), each = 4),
                      mthd = c('bval.apid.nwlls.med','pval.zero','resp.pc','resp.multneg1'),
                      ordr = c(1:4))
dn.lvl1 <- data.table(aenm = rep(grep('_dn',aenms,val=T), each = 3),
                      mthd = c('bval.apid.nwlls.med','pval.zero','resp.pc'),
                      ordr = c(1:3))
sc1_mthds <- rbind(up.lvl1, dn.lvl1)
sc1_mthds <- merge(sc1_mthds, sc1_mthd_options[, .(mthd = sc1_mthd, mthd_id = sc1_mthd_id)], by = 'mthd', all.x = T)
sc1_mthds[is.na(mthd_id)] # empty, cool
setkey(sc1_mthds, aenm, ordr)

# Level 2 methods
sc2_mthd_options <- tcplMthdList(lvl = 2L, type = 'sc')
# bmad ow nwells for all
sc2_mthds <- data.table(aenm = aenms,
                        mthd = 'ow_bmad_nwells')
# get the winning sc2 method for the 13 core endpoints
sc2_mthds <- rbind(sc2_mthds, bmad.coff.long[sc2_mthd == sc2_mthd_selection, .(aenm, mthd = sub('coff\\.','',sc2_mthd))])
# Assign bmad6 for all others
sc2_mthds <- rbind(sc2_mthds, data.table(aenm = setdiff(aenms, bmad.coff.long$aenm), mthd = 'bmad6'))
sc2_mthds[, ordr := 1:.N, by = .(aenm)]
sc2_mthds <- merge(sc2_mthds, sc2_mthd_options[, .(mthd = sc2_mthd, mthd_id = sc2_mthd_id)], by = 'mthd', all.x = T)
sc2_mthds[is.na(mthd_id)]
# sc2_mthd                                                 aenm lvl ordr sc2_mthd_id
# 1:  bmad1.5    CCTE_Shafer_MEA_dev_bursting_electrodes_number_dn   2    2          NA
# 2:  bmad1.5        CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12_dn   2    2          NA
# 3:     pc30                            CCTE_Shafer_MEA_dev_AB_dn   2    2          NA
# 4:     pc30                           CCTE_Shafer_MEA_dev_LDH_dn   2    2          NA
# 5:     pc30 CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12_up   2    2          NA
# this is expected for these methods
setkey(sc2_mthds, aenm, ordr)

# I'm going to replace teh aenm with the aeid to prevent confusion/endpoints getting dropped with the change in name for inter network spike interval
tcpl_aenms <- stri_replace_all_fixed(aenms, pattern = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean',
                                     replacement = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean')
aeid.tb <- tcplLoadAeid(fld = 'aenm', val = tcpl_aenms)
aeid.tb[, my_aenm := stri_replace_all_fixed(aenm, pattern = 'CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean',
                                            replacement = 'CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean')]

aeid.tb[aenm != my_aenm]
#                                                                 aenm aeid                                                  my_aenm
# 1: CCTE_Shafer_MEA_dev_per_network_spike_interspike_interval_mean_up 2519 CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_up

sc1_mthds <- merge(aeid.tb, sc1_mthds, by.x = 'my_aenm', by.y = 'aenm')
sc1_mthds # looks okay
sc1_mthds[, c('my_aenm','aenm') := NULL]
sc2_mthds <- merge(aeid.tb, sc2_mthds, by.x = 'my_aenm', by.y = 'aenm')
sc2_mthds # looks okay
sc2_mthds[, c('my_aenm','aenm','mthd_id') := NULL] # removing the mthd_id, since it woudl be NA for soem rows...

save(sc1_mthds, sc2_mthds, file = paste0('configuring_for_tcpl/mea_nfa_sc_proposed_methods_2021-05-20.RData'))
