#-----------------------------------------------------------------------------------#
# Confirming that my mistake of 'intersect' instead of 'union' in save_lvl0_snapshot function
# did not cause me to miss some plates that should be removed
# June 30, 2021
#-----------------------------------------------------------------------------------#
library(data.table)

# checking out the level 0 data...
load('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_extra_cols_2021-05-05.RData')

# Get a sense of the type of notes I'm looking at...
View(mea_nfa_lvl0[, .N, by = .(wllq_notes)][order(-N)])
# Yep, all look like notes derived from lab notebook/discussions with others, 
# nothing programmatic other than MFR <10 or AE < 2

# Any NA for MFR or AE on DIV12?
mea_nfa_lvl0[wllq == 1 & wllt == 'n' & acnm %in% c('CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12','CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12'), .N, by = .(is.na(rval))]
# is.na    N
# 1: FALSE 3324
# phew!
# But going forward, I will include na.rm = T when I calculate the median of controls

# Are there any apid where either the MFR < 10 OR the nAE < 2 (but not both)?
# note that the one plate where both is true has already been removed (wllq set to 0)
mea_nfa_lvl0[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acnm), .(bval = median(rval)), by = .(apid)][bval < 10/60]
# empty
mea_nfa_lvl0[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acnm), .(bval = median(rval)), by = .(apid)][, min(bval*60)]
# 23.85
mea_nfa_lvl0[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acnm), .(bval = median(rval)), by = .(apid)][bval < 2]
# also empty
mea_nfa_lvl0[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acnm), .(bval = median(rval)), by = .(apid)][, min(bval)]
# 8

# PHew... so there aren't any plates where only the nAE < 2 OR MFR <10
# Honestly htat makes snese... this was a very low threshold


# I'm going to check this for the sc as well:
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl")
source("supplemental_scripts/get_latest_dat.R")

sc0 <- get_latest_dat(dataset_titles = 'SPS_PFAS2019')
# Loading longfile.Rdata for...
# SPS_PFAS2019 
# 
# Some conc's are NA for the following spids:
# Media
sc0[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn), .(bval = median(rval)), by = .(apid)][bval < 10/60]
# empty
sc0[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn), .(bval = median(rval)), by = .(apid)][, min(bval*60)]
# 23.21
sc0[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn), .(bval = median(rval)), by = .(apid)][bval < 2]
# also empty
sc0[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn), .(bval = median(rval)), by = .(apid)][, min(bval)]
# 8.5.

# cool, so no additional plates should have been removed here either.

# I will update the code in case there are any plates that only meet one of these criteria going forward,
# but I don't have to change any of the existing data
