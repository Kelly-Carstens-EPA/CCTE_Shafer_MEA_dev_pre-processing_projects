# saving lvl0_snapshots
rm(list = ls())
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl")
source("supplemental_scripts/get_latest_dat.R")


# 11/12/2020 ----------------------------------------------------------------------------
save_lvl0_snapshot()
# Loading longfile.Rdata for...
# Brown2014 
# DNTGF2019 
# Frank2017 
# NTP91 
# OPP2015 
# PFAS2018 
# ToxCast2016 
# 
# Removing apid where median of controls on DIV 12 is < 10 spikes per min < 2 active electrodes...
# Setting wllq:=0 these apid: 20160921_MW1160-23 
# apid                             V2       rval
# 1: 20160921_MW1160-23         firing_rate_mean_DIV12  0.0000000
# 2: 20160921_MW1160-23         firing_rate_mean_DIV12  0.0000000
# 3: 20160921_MW1160-23         firing_rate_mean_DIV12  0.1911127
# 4: 20160921_MW1160-23         firing_rate_mean_DIV12  0.0000000
# 5: 20160921_MW1160-23         firing_rate_mean_DIV12  0.9486744
# 6: 20160921_MW1160-23         firing_rate_mean_DIV12  2.5734933
# 7: 20160921_MW1160-23 active_electrodes_number_DIV12  0.0000000
# 8: 20160921_MW1160-23 active_electrodes_number_DIV12  0.0000000
# 9: 20160921_MW1160-23 active_electrodes_number_DIV12  1.0000000
# 10: 20160921_MW1160-23 active_electrodes_number_DIV12  0.0000000
# 11: 20160921_MW1160-23 active_electrodes_number_DIV12 15.0000000
# 12: 20160921_MW1160-23 active_electrodes_number_DIV12 16.0000000
# Number of points where wllq==1 and rval is NA for AUC, LDH, or AB endpoints: 0 
# Number of points where wllq==1 and rval is NA for DIV endpoints: 127126 
# Renaming 'acsn' to 'acnm'...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_extra_cols_2020-11-12.RData has been saved.
# Restricting to only AUC, DIV12, and LDH/AB endpoints...
# Settting wllq==0 where rval is NA...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_2020-11-12.RData has been saved.

# load('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_2020-11-12.RData')
# dat1 <- mea_nfa_lvl0
# load('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_2020-11-12 - Copy.RData')
# all.equal(dat1, mea_nfa_lvl0) # TRUE! Will delete newly made file.
# just checkign since I just change save_lvl0() to keep the DIV5-9 endpoints for the first file save.


# 5/5/2021 ---------------------------------------------------------------------------
# (added 'PFAS2019' to the default datasettitles for get_latest_dat)
save_lvl0_snapshot()
# Loading longfile.Rdata for...
# Brown2014 
# DNTGF2019 
# Frank2017 
# NTP91 
# OPP2015 
# PFAS2018 
# PFAS2019 
# ToxCast2016 
# 
# Some conc's are NA for the following spids:
# Media
# Removing apid where median of controls on DIV 12 is < 10 spikes per min or < 2 active electrodes...
# Setting wllq:=0 for these apid: 20160921_MW1160-23 
#                   apid                             V2       rval
#  1: 20160921_MW1160-23         firing_rate_mean_DIV12  0.0000000
#  2: 20160921_MW1160-23         firing_rate_mean_DIV12  0.0000000
#  3: 20160921_MW1160-23         firing_rate_mean_DIV12  0.1911127
#  4: 20160921_MW1160-23         firing_rate_mean_DIV12  0.0000000
#  5: 20160921_MW1160-23         firing_rate_mean_DIV12  0.9486744
#  6: 20160921_MW1160-23         firing_rate_mean_DIV12  2.5734933
#  7: 20160921_MW1160-23 active_electrodes_number_DIV12  0.0000000
#  8: 20160921_MW1160-23 active_electrodes_number_DIV12  0.0000000
#  9: 20160921_MW1160-23 active_electrodes_number_DIV12  1.0000000
# 10: 20160921_MW1160-23 active_electrodes_number_DIV12  0.0000000
# 11: 20160921_MW1160-23 active_electrodes_number_DIV12 15.0000000
# 12: 20160921_MW1160-23 active_electrodes_number_DIV12 16.0000000
# Number of points where wllq==1 and rval is NA for AUC, LDH, or AB endpoints: 0 
# Number of points where wllq==1 and rval is NA for DIV endpoints: 138409 
# Renaming 'acsn' to 'acnm'...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_extra_cols_2021-05-05.RData has been saved.
# Restricting to only AUC, DIV12, and LDH/AB endpoints...
# Settting wllq==0 where rval is NA...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_2021-05-05.RData has been saved.


# 5/18/2021 ---------------------------------------------------------------------------
# (saving first SPS lvl0 snapshot)
save_lvl0_snapshot_sps(dataset_titles = 'SPS_PFAS2019')
# Loading longfile.Rdata for...
# SPS_PFAS2019 
# 
# Some conc's are NA for the following spids:
# Media
# Removing apid where median of controls on DIV 12 is < 10 spikes per min or < 2 active electrodes...
# Setting wllq:=0 for these apid:  
# Empty data.table (0 rows and 3 cols): apid,V2,rval
# Number of points where wllq==1 and rval is NA for AUC, LDH, or AB endpoints: 0 
# Number of points where wllq==1 and rval is NA for DIV endpoints: 6884 
# Restricting to the following acsn's...
# CCTE_Shafer_MEA_dev_AB
# CCTE_Shafer_MEA_dev_bursting_electrodes_number
# CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12
# CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean
# CCTE_Shafer_MEA_dev_LDH
# CCTE_Shafer_MEA_dev_mutual_information_norm
# CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12
# CCTE_Shafer_MEA_dev_network_spike_peak
# CCTE_Shafer_MEA_dev_network_spike_peak_DIV12
# CCTE_Shafer_MEA_dev_per_burst_interspike_interval_DIV12
# CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12
# CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_DIV12
# Renaming 'acsn' to 'acnm'...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_sc0_extra_cols_2021-05-18.RData has been saved.
# Settting wllq==0 where rval is NA...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_sc0_2021-05-18.RData has been saved.


# 6/9/2021 ---------------------------------------------------------------------------
# (saving first SPS lvl0 snapshot)
save_lvl0_snapshot_sps(dataset_titles = 'SPS_PFAS2019')
# Loading required package: stringi
# Loading longfile.Rdata for...
# SPS_PFAS2019 
# 
# Some conc's are NA for the following spids:
# Media
# Removing apid where median of controls on DIV 12 is < 10 spikes per min or < 2 active electrodes...
# Setting wllq:=0 for these apid:  
# Empty data.table (0 rows and 3 cols): apid,V2,rval
# Number of points where wllq==1 and rval is NA for AUC, LDH, or AB endpoints: 0 
# Number of points where wllq==1 and rval is NA for DIV endpoints: 6884 
# Restricting to the following acsn's...
# CCTE_Shafer_MEA_dev_AB
# CCTE_Shafer_MEA_dev_bursting_electrodes_number
# CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12
# CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean
# CCTE_Shafer_MEA_dev_LDH
# CCTE_Shafer_MEA_dev_mutual_information_norm
# CCTE_Shafer_MEA_dev_mutual_information_norm_DIV12
# CCTE_Shafer_MEA_dev_network_spike_peak
# CCTE_Shafer_MEA_dev_network_spike_peak_DIV12
# CCTE_Shafer_MEA_dev_per_burst_interspike_interval_DIV12
# CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12
# CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_DIV12
# Renaming 'acsn' to 'acnm'...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_sc0_extra_cols_2021-06-09.RData has been saved.
# Settting wllq==0 where rval is NA...
# L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_sc0_2021-06-09.RData has been saved.



# 9/15/2022 ---------------------------------------------------------------
# saving the additional endpoints for the mea dev sc
library(stringi)
library(data.table)

mea_nfa_sc0 <- get_latest_dat(dataset_titles = 'SPS_PFAS2019')

cat("Removing apid where median of controls on DIV 12 is < 10 spikes per min or < 2 active electrodes...\n")
remove_apid <- mea_nfa_sc0[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn), .(bval = median(rval, na.rm = T)), by = .(apid)][bval < 10/60, unique(apid)]
remove_apid <- union(remove_apid,
                     mea_nfa_sc0[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn), .(bval = median(rval, na.rm = T)), by = .(apid)][bval < 2, unique(apid)])
cat("Setting wllq:=0 for these apid:",remove_apid,"\n")
print(mea_nfa_sc0[apid %in% remove_apid & (grepl("active_electrodes_number_DIV12",acsn) | grepl("firing_rate_mean_DIV12",acsn)) & wllt == "n", .(apid, sub("CCTE_Shafer_MEA_dev_","",acsn), rval)])
mea_nfa_sc0[apid %in% remove_apid, `:=`(wllq = 0,
                                        wllq_notes = paste0(wllq_notes,"; Median MFR < 10 spikes per min or nAE < 2 on DIV12"))]

cat("Number of points where wllq==1 and rval is NA for AUC, LDH, or AB endpoints:",mea_nfa_sc0[!grepl("_DIV",acsn) & is.na(rval) & wllq == 1, .N],"\n")
cat("Number of points where wllq==1 and rval is NA for DIV endpoints:",mea_nfa_sc0[grepl("_DIV",acsn) & is.na(rval) & wllq == 1, .N],"\n")

# Restrict to the endpoints NOT already released
mea_nfa_sc0_cur <- mea_nfa_sc0
load('lvl0_snapshots/mea_nfa_sc0_2021-06-09.RData')
mea_nfa_sc0_cur <- mea_nfa_sc0_cur[!(acsn %in% mea_nfa_sc0$acnm)]

# Confirm this endpoint is named correctly
mea_nfa_sc0_cur[grepl('per_network_spike_interspike_interval', acsn), .N, by = .(acsn)] # not present
mea_nfa_sc0_cur[grepl('inter_network_spike_interval_mean', acsn), .N, by = .(acsn)]
# acsn   N
# 1: CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV12 480
# 2:  CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV5 480
# 3:  CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV7 480
# 4:  CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_DIV9 480
# Okay, so just the DIV endpoints
# Was this endpoint included in the list of endpoints that was already peiplined?
mea_nfa_sc0[grepl('per_network_spike_interspike_interval', acnm), .N, by = .(acnm)]
mea_nfa_sc0[grepl('inter_network_spike_interval_mean', acnm), .N, by = .(acnm)]
#                                                     acnm   N
# 1: CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean 480
# hmm... okay then
# I dont' think I need to do anything here, since not including it now
mea_nfa_sc0 <- mea_nfa_sc0_cur
rm(mea_nfa_sc0_cur)

cat("Renaming 'acsn' to 'acnm'...\n")
setnames(mea_nfa_sc0, old = "acsn", new = "acnm", skip_absent = TRUE)

# save snapshot with all columns
setkey(mea_nfa_sc0, NULL) # remove keys I inadvertantly added, to make the file smaller
save(mea_nfa_sc0, file = file.path("lvl0_snapshots",paste0("mea_nfa_sc0_additional_endpoints_extra_cols_",as.character.Date(Sys.Date()),".RData")))
cat(file.path("lvl0_snapshots",paste0("mea_nfa_sc0_additional_endpoints_extra_cols_",as.character.Date(Sys.Date()),".RData")),"has been saved.\n")

# save snapshot with just TCPL mc0 cols
cat("Settting wllq==0 where rval is NA...\n")
mea_nfa_sc0[is.na(rval), wllq := 0]

usecols <- c("acnm","spid","apid","rowi","coli","wllt","wllq","conc","rval","srcf")
mea_nfa_sc0 <- mea_nfa_sc0[, .SD, .SDcols = usecols]
setkey(mea_nfa_sc0, NULL) # remove keys I inadvertantly added, to make the file smaller
save(mea_nfa_sc0, file = file.path("lvl0_snapshots",paste0("mea_nfa_sc0_additional_endpoints_",as.character.Date(Sys.Date()),".RData")))

cat(file.path("lvl0_snapshots",paste0("mea_nfa_sc0_additional_endpoints_",as.character.Date(Sys.Date()),".RData")),"has been saved.\n")
# (note: this includes both all DIV endpoints and the AUC endpoints that were not released as part of June 2021)
