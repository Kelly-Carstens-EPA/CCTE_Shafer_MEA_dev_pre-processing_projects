# ------------------------------------------------------------------------ #
# using tcplfit2 to assign preliminary curve fits
# and estimate if the modl_ga will be less than the lowest conc tested
# 
# May 16, 2022
# carpenter.amy@epa.gov
# ------------------------------------------------------------------------ # 

library(data.table)
library(ggplot2)
library(tcplfit2)
library(openxlsx)

# Load mc3-like data
load('DNT_NTP2021/check_for_chem_to_rescreen/data/DNT_NTP2021_preliminary_in_house_normalized_values_2022-05-17.RData')
cat(description)
# An mc3-like table containing resp values along with current invitrodb cutoffs.
# Created from DNT_NTP2021_preliminary_longfile.RData.
# Created with apply_in_house_tcpl-like_processing_2022-05-17.R

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
mc4.list <- my_tcplfit2(use.dat)
# Processing 19 assay endpoints...
# 2022-05-17 17:55:56 CCTE_Shafer_MEA_dev_AB_dn complete
# 2022-05-17 17:56:20 CCTE_Shafer_MEA_dev_LDH_dn complete
# 2022-05-17 17:56:48 CCTE_Shafer_MEA_dev_active_electrodes_number_up complete
# 2022-05-17 17:57:13 CCTE_Shafer_MEA_dev_burst_duration_mean_up complete
# 2022-05-17 17:57:36 CCTE_Shafer_MEA_dev_burst_rate_up complete
# 2022-05-17 17:57:58 CCTE_Shafer_MEA_dev_bursting_electrodes_number_up complete
# 2022-05-17 17:58:20 CCTE_Shafer_MEA_dev_correlation_coefficient_mean_up complete
# 2022-05-17 17:58:43 CCTE_Shafer_MEA_dev_firing_rate_mean_up complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
# f() values at end points not of opposite sign
# 2022-05-17 17:59:14 CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_up complete
# 2022-05-17 17:59:14 CCTE_Shafer_MEA_dev_inter_network_spike_interval_mean_up complete
# 2022-05-17 17:59:39 CCTE_Shafer_MEA_dev_interburst_interval_mean_up complete
# 2022-05-17 18:00:04 CCTE_Shafer_MEA_dev_mutual_information_norm_up complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
# f() values at end points not of opposite sign
# 2022-05-17 18:00:28 CCTE_Shafer_MEA_dev_network_spike_duration_std_up complete
# 2022-05-17 18:00:55 CCTE_Shafer_MEA_dev_network_spike_number_up complete
# 2022-05-17 18:01:22 CCTE_Shafer_MEA_dev_network_spike_peak_up complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
# f() values at end points not of opposite sign
# 2022-05-17 18:01:51 CCTE_Shafer_MEA_dev_per_burst_interspike_interval_up complete
# 2022-05-17 18:02:16 CCTE_Shafer_MEA_dev_per_burst_spike_percent_up complete
# 2022-05-17 18:02:42 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_up complete
# 2022-05-17 18:03:11 CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_up complete
# 2022-05-17 18:03:35 CCTE_Shafer_MEA_dev_spike_duration_mean_up complete
# Warning message:
#   In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation
length(mc4.list) # 2147
nrow(use.dat) # 2147

# these error messages are occuring when there is an issue with the gnls model
# (which is a known issue). The ac50_loss, but no the ac50, will be NA in these cases.
# And the rest of the models will still be fit to the data

rm(list = setdiff(ls(),c('dat2','mc4.list','use.dat','my_tcplhit2')))


## tcplhit2
mc5 <- my_tcplhit2(use.dat, mc4.list)
# There were 50 or more warnings (use warnings() to see the first 50)
# warnings()
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
Created with DNT_NTP_2021_determin_samples_to_rescreen_2022-05-17.R'
save(mc4.list, mc5, description, file = file.path('DNT_NTP2021/check_for_chem_to_rescreen/data/DNT_NTP2021_tcplfit2_exploratory_ac50_estimates_2022-05-17.RData'))



# Note where AC50 is below lowest conc tested -------------------------------

# Note that if gnls was the model winner and so the curve has an ac50_loss value,
# the ac50 is always more potent than the ac50_loss
mc5[, .N, by = .(gnls_model_won = fit_method == 'gnls',
                 ac50 < ac50_loss)]
#    gnls_model_won ac50    N
# 1:          FALSE   NA 2112
# 2:           TRUE TRUE   35
mc5[, ac50_below_minconc := as.numeric(ac50 < 10^logc_min)]
mc5[, ac50_below_minconc_and_hit := as.numeric(hitcall >= 0.5 & ac50 < 10^logc_min)]

mc5[ac50_below_minconc_and_hit == 1, .N, by = .(spid)]



# Note where noise is relatively high -------------------------------------

# Let's try noting where the noise is in the upper say 95%-ile for a given endpoint AND noise > coff
mc5[, upper_95th_rmse_by_aenm := quantile(rmse, probs = 0.95), by = .(aenm)]
mc5[, exceptional_noise := as.numeric(rmse > coff & rmse > upper_95th_rmse_by_aenm)]


# Save table to inform decision -------------------------------------------

tb1 <- mc5[, .(num_assay_components = .N,
               num_hitcall_above_0.5 = sum(hitcall >= 0.5),
               num_ac50_below_minconc_and_hit = sum(ac50_below_minconc_and_hit),
               num_noisy_endpoints = sum(exceptional_noise)), by = .(spid)]
tb1 <- tb1[order(-num_ac50_below_minconc_and_hit, -num_noisy_endpoints)]
View(tb1)

# Merge in wllq info from Seline's notes
# (only concerned about notes seline added that did not result in outright removal of data points right now)
wllq.notes.tb <- dat2[wllq == 1, .(wllq_notes = paste0(sort(unique(wllq_notes)),collapse = ";")), by = .(spid)]

tb1 <- merge(tb1, wllq.notes.tb, by = 'spid', all.x = T)
tb1 <- tb1[order(-num_ac50_below_minconc_and_hit, -num_noisy_endpoints, -wllq_notes)]

View(tb1)

definitions.tb <- data.table(column = c('spid' ,'num_assay_components' ,'num_hitcall_above_0.5' ,'num_ac50_below_minconc_and_hit' ,'num_noisy_endpoints' ,'wllq_notes'),
                             definition = c('Sample ID','Total number of assay component (2 cytotoxicity, plus 17 AUC endpoints fit bidirectionally)',
                                             'Number of assay components with tcplfit2 hit call >= 50% (tcplfit2 assigns continuous hit call estimates)',
                                             'Number of assay components with hit call >= 50% and AC50 < lowest conc tested',
                                             'Number of assay components with winning model RMSE above the cutoff and the 95%-ile for the given assay component',
                                             'Well quality notes added by lab technicians'))

wb <- createWorkbook()
addWorksheet(wb, sheet = 'definitions')
writeData(wb, sheet = 1, definitions.tb)
addWorksheet(wb, sheet = 'tcplfit2 summary')
writeData(wb, sheet = 2, tb1)
saveWorkbook(wb, file = 'DNT_NTP2021/check_for_chem_to_rescreen/DNT_NTP2021_MEA_NFA_tcplfit2_summary_to_aid_rescreen_determinations_2022-05-17.xlsx',
             overwrite = T)



# Supportive plots --------------------------------------------------------

# For substances that have any hits with ac50 below min conc tested - I want to see all hits for that substance

# For samples that have some noisy endpoints - pull all endpoints

# For samples that have some note added by seline but we aren't retesting - see all curves

# Or make it simple - just pull all curves for substances that are flagged for any of these reasons
plot.spids <- tb1[num_ac50_below_minconc_and_hit > 0 | num_noisy_endpoints > 0 | wllq_notes != '', unique(spid)]
length(plot.spids) # 40

# Adding some additional columsn for concRespPlot
mc5[, assay := aenm]
mc5[, name := spid]
mc5[, .N, by = .(rmse)][N > 1] # only 0 is ever repeated...
# and these cases don't matter, bc they won't be near the 95th percentile
mc5[, rmse_percentile := rank(rmse, ties.method = 'random')/.N, by = .(aenm)]

pdf(file = 'DNT_NTP2021/check_for_chem_to_rescreen/figs/DNT_NTP2021_tcplfit2_plots_to_aid_rescreen_determinations_2022-05-17.pdf',
    width = 8, height = 10)
par(mfrow = c(4, 2), mar = c(2.5,2,2.5,1))
for (spidi in plot.spids) {
  for (aenmi in unique(mc5$aenm)) {
    res <- mc5[spid == spidi & aenm == aenmi]
    concRespPlot(res)  
    abline(v = res$ac50, col = 'red', lty = ifelse(res$hitcall >= 0.5, 'solid','dashed'))
    
    # Add text for RMSE
    plotrange = c(0.001,100)
    xplot = 10^(seq(log10(plotrange[1]), log10(plotrange[2]), length.out = 8))[-8]
    text(x = xplot[7], 
         y = 120*0.95, # default ymax
         labels = 'RMSE (%ile)',
         pos = 4)
    text(x = xplot[7], 
         y = 120*0.95-(120-(-120))*0.05, # default ymax
         labels = paste0(signif(res$rmse,2), ' (',signif(res$rmse_percentile*100,2),'%)'),
         pos = 4)

  }
  # to make each page just have 1 spid
  plot.new()
  plot.new()
  plot.new()
  plot.new()
  plot.new()
}
graphics.off()


# My recommendations ------------------------------------------------------

# What should be retested:
# - If AC50s for > 25% of the endpoints (say 5 or more), then rescreen
# - If Ac50s below min conc tested for 1-4 endpoints - let's look at the curves
# - If very noisy in say 5 or more endpoints - retest. Otherwise no


# Just 1 endpoint wtih exceptional noise - not a big concern

# 4 endpoints with exceptional noise - nice to retest, but we live with this all the time

# it happens that ll of the cases with 7-10 endpoints with exceptional noise also have a lot of hits
# Looking at the curves, I'm confiden that there is activity at the higher conc's tested.
# So even if there is noise, we can be confident that there is some activity...?

# My other concern - part of the "treatment effect" might just be to increase the variability!
# So we don't know if a repeat woudl be better

# CONCLUSION
# Okahy, so I could go through these one by one... or just say...
# My thought is - let's retest the 7 samples where >= 5 of the endpoints have ac50 estimate < min conc tested
# Would be nice to retest the 9 substances with >= 5 endpoints with exceptional noise,
# Particularly 4 that have <= 5 hits, so there is less confidence in the hits.
# But we live with noise all the time, so I think it's not entirely necessary
# There are 4 additional substances that Seline flagged to retest that look totally fine to me.
# But maybe I missed something, so we can touch base again.
# Also, I'm not considering cases where there was precipitate - I'm assuming Seline is handling that?

load('DNT_NTP2021/output/DNT_NTP2021_preliminary_longfile.RData')
cat(description)
dat[grepl('precipitate',wllq_notes), .N, by = .(treatment, wllq_notes)]
dat[grepl('precipitate',wllq_notes), .N, by = .(treatment)]
# treatment    N
# 1:  7126 A12 1827
# 2:   7126 B2 1827
# 3:  7126 B11 1827
# Repeat these 3 as well

# So 7 to repeat at lower conc
# 3 to repeat because of precipitate
# 4-9 that would be nice to repeat because of noise, but we can decide if that necessary

# create code product to go with this. Anything need to be updated?
# writeup summary of methods

# re-run if necessary
