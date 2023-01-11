# ------------------------------------------------------------------------ #
# using tcplfit2 to assign preliminary curve fits
# and estimate if the modl_ga will be less than the lowest conc tested
# 
# July 28, 2022
# Update after have repeated samples that needed to be repeated
# as determined in 2022-05-26
# carpenter.amy@epa.gov
# ------------------------------------------------------------------------ # 

library(data.table)
library(ggplot2)
library(tcplfit2)
library(openxlsx)

# Load mc3-like data
load('DNT_NTP2021/check_for_chem_to_rescreen/data/DNT_NTP2021_preliminary_in_house_normalized_values_2022-07-27.RData')
cat(description)
# An mc3-like table containing resp values along with bmads and coff estimated from pooled current invitrodb and this dataset's response values.
# Created from DNT_NTP2021_longfile.RData.
# Created with apply_in_house_tcpl-like_processing_2022-07-27.R

# Restrict to AUC up and AB and LDH (since I used bidirectional fitting, up endpoints can capture both up and dn response)
use.aenms <- dat2[!grepl('DIV',aenm) & (grepl('up',aenm) | grepl('(LDH)|(AB)',aenm)), unique(aenm)]
length(use.aenms) # 19, good
dat2 <- dat2[aenm %in% use.aenms]

# Source tcplfit2 diy scripts
source('DNT_NTP2021/check_for_chem_to_rescreen/diy_tcplfit2_functions_2022-05-12.R')



# Run tcplfit2 ------------------------------------------------------------

## Prepare
dat.by.spid.aenm <- create_dat.by.spid.aenm(dat2)

## tcplfit2
# Remove controls, where only 2 conc tested
use.dat <- dat.by.spid.aenm[nconc > 1]
rm(dat.by.spid.aenm)
rm(dat2)
mc4.list <- my_tcplfit2(use.dat)
# Processing 19 assay endpoints...
# 2022-07-28 00:54:34 CCTE_Shafer_MEA_dev_AB_dn complete
# 2022-07-28 00:54:55 CCTE_Shafer_MEA_dev_LDH_dn complete
# 2022-07-28 00:55:14 CCTE_Shafer_MEA_dev_active_electrodes_number_up complete
# 2022-07-28 00:55:34 CCTE_Shafer_MEA_dev_burst_duration_mean_up complete
# 2022-07-28 00:55:54 CCTE_Shafer_MEA_dev_burst_rate_up complete
# 2022-07-28 00:56:15 CCTE_Shafer_MEA_dev_bursting_electrodes_number_up complete
# 2022-07-28 00:56:36 CCTE_Shafer_MEA_dev_correlation_coefficient_mean_up complete
# 2022-07-28 00:56:55 CCTE_Shafer_MEA_dev_firing_rate_mean_up complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
                   # f() values at end points not of opposite sign
# 2022-07-28 00:57:16 CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_up complete
# 2022-07-28 00:57:37 CCTE_Shafer_MEA_dev_interburst_interval_mean_up complete
# 2022-07-28 00:57:58 CCTE_Shafer_MEA_dev_mutual_information_norm_up complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
#                    f() values at end points not of opposite sign
# 2022-07-28 00:58:18 CCTE_Shafer_MEA_dev_network_spike_duration_std_up complete
# 2022-07-28 00:58:39 CCTE_Shafer_MEA_dev_network_spike_number_up complete
# 2022-07-28 00:59:02 CCTE_Shafer_MEA_dev_network_spike_peak_up complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
#                    f() values at end points not of opposite sign
#                  Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
#                                     f() values at end points not of opposite sign
# 2022-07-28 00:59:22 CCTE_Shafer_MEA_dev_per_burst_interspike_interval_up complete
# 2022-07-28 00:59:43 CCTE_Shafer_MEA_dev_per_burst_spike_percent_up complete
# 2022-07-28 01:00:04 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_up complete
# 2022-07-28 01:00:28 CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_up complete
# 2022-07-28 01:00:48 CCTE_Shafer_MEA_dev_spike_duration_mean_up complete
length(mc4.list) # 2223
nrow(use.dat) # 2223

# these error messages are occuring when there is an issue with the gnls model
# (which is a known issue). The ac50_loss, but no the ac50, will be NA in these cases.
# And the rest of the models will still be fit to the data

rm(list = setdiff(ls(),c('dat2','mc4.list','use.dat','my_tcplhit2')))


## tcplhit2
mc5 <- my_tcplhit2(use.dat, mc4.list)
# (in the past when I ran this, tere were warning messages such as )
# Warning messages:
#   1: In uniroot(bmdobj, bmdrange, fname = fname, bmr = bmr,  ... :
#                   NA/Inf replaced by maximum positive value
#                 2: In uniroot(bmdobj, bmdrange, fname = fname, bmr = bmr,  ... :
#                                 NA/Inf replaced by maximum positive value
# ...

# Confirm that no cases where there was an issue with the gnls model had the gnls as the best fit
mc5[fit_method == 'gnls' & is.na(ac50_loss)] # empty, good



## Save the output ---------------------------------------------------------

setkey(mc5, NULL)
description <- 'Preliminary tcplfit2 output and AC50 estimates for DNT NTP 2021 data.
For AUC up and LDH, AB endpoints only.
Created with DNT_NTP_2021_determin_samples_to_rescreen_2022-07-27.R'
save(mc4.list, mc5, description, file = file.path('DNT_NTP2021/check_for_chem_to_rescreen/data/DNT_NTP2021_tcplfit2_exploratory_ac50_estimates_2022-07-27.RData'))

