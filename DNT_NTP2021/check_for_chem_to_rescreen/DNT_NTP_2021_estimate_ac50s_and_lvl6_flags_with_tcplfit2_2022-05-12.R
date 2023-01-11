# ------------------------------------------------------------------------ #
# using tcplfit2 to assign preliminary curve fits
# and estimate if the modl_ga will be less than the lowest conc tested
# 
# May 12, 2022
# ------------------------------------------------------------------------ # 

# To do:
# check = See if there is additional data to add, since Seline's update
# Estimate the AC50s with tcplfit2
# estimate the flags
# see what would be nice to re-run! (or necessary)
# make this streamlined?

library(tcpl) # (I don't think I need the new version of tcpl. t
# the mc6_mthds function in tcpl_v3 has not been edited since the last edit in th master branch)
library(tcplfit2)
library(data.table)
library(stringi)
library(ggplot2)

# Source tcplfit2 diy scripts
source('L:/Lab/NHEERL_MEA/Carpenter_Amy/data_requests/personal/R/diy_tcplfit2_functions_2022-05-12.R')


# Load data ---------------------------------------------------------------

load(file.path('DNT_NTP2021','check_for_chem_to_rescreen','DNT_NTP2021_preliminary_in_house_normalized_values_2022-05-11.RData'))
cat(description)
# An mc3-like table containing resp values along with current invitrodb cutoffs.
# Created from DNT_NTP2021_preliminary_longfile.RData.
# Created with apply_in_house_tcpl-like_processing_2022-05-11.R



# Run the "pipeline"! -----------------------------------------------------



## Goal: -------------------------------------------------------------------

## Prepare
dat.by.spid.aenm <- create_dat.by.spid.aenm(dat2)

## tcplfit2
use.dat <- dat.by.spid.aenm[nconc > 1]
mc4.list <- my_tcplfit2(use.dat)
# this take a minute... but understandably!!

## tcplhit2
mc5 <- my_tcplhit2(use.dat, mc4.list)

## Apply mc6 flags
mc5_mc6 <- my_mc6(c5)



# What actually happend ---------------------------------------------------

## Prepare
dat.by.spid.aenm <- create_dat.by.spid.aenm(dat2)

## tcplfit2
use.dat <- dat.by.spid.aenm[nconc > 1]
mc4.list <- my_tcplfit2(use.dat)
# Error in if ((rmad <- mad(resp)) > 0) log(rmad) else log(1e-32) : 
#   missing value where TRUE/FALSE needed
# In addition: Warning messages:
#   1: In tcplfit2::tcplfit2_core(unlist(dati$concentration_unlogged),  :
#                                   all response values are 0: add epsilon (1e-6) to all response elements.
#                                 Response Range: 0,0
#                                 2: In tcplfit2::tcplfit2_core(unlist(dati$concentration_unlogged),  :
#                                                                 all response values are 0: add epsilon (1e-6) to all response elements.
#                                                               Response Range: 0,0
#                                                               3: In tcplfit2::tcplfit2_core(unlist(dati$concentration_unlogged),  :
#                                                                                               all response values are 0: add epsilon (1e-6) to all response elements.
#                                                                                             Response Range: 0,0
#                                                                                             4: In tcplfit2::tcplfit2_core(unlist(dati$concentration_unlogged),  :

# I think I know how to investigate the warnings
resp.range.tb <- dat2[, .(resp_range = max(resp) - min(resp), max_abs_resp = max(abs(resp))),by = .(spid, aenm)]
resp.range.tb[, summary(max_abs_resp)]
resp.range.tb[max_abs_resp == 0]
# spid                                                    aenm resp_range max_abs_resp
# 1: 7126 A9   CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12_dn          0            0
# 2: 7126 B3   CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12_dn          0            0
# 3: 7126 A9   CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12_up          0            0
# 4: 7126 B3   CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12_up          0            0
# 5: 7126 B3 CCTE_Shafer_MEA_dev_bursting_electrodes_number_DIV12_dn          0            0
# 6: 7126 B3 CCTE_Shafer_MEA_dev_bursting_electrodes_number_DIV12_up          0            0
# ah... let's just not even run these endpoints!!

# I just realized... we're not even going to run this in tcpl!!
# We're just going to send the raw data and have them re-run it!!
# So it's really going to be on them,
# and I don't have to send them ac50 estimates!!

# And I told them the DIV 12 endpionts are "preliminary", so they can do with them as they will
# I don't have to make sure that they can fit a curve
rm(resp.range.tb)

use.dat <- dat.by.spid.aenm[nconc > 1 & !grepl('DIV12',aenm)]
mc4.list <- my_tcplfit2(use.dat)
# Processing 34 assay endpoints...
# 2022-05-12 17:04:01 CCTE_Shafer_MEA_dev_AB_dn complete
# 2022-05-12 17:04:24 CCTE_Shafer_MEA_dev_LDH_dn complete
# 2022-05-12 17:04:55 CCTE_Shafer_MEA_dev_active_electrodes_number_dn complete
# 2022-05-12 17:05:19 CCTE_Shafer_MEA_dev_active_electrodes_number_up complete
# 2022-05-12 17:05:53 CCTE_Shafer_MEA_dev_burst_duration_mean_dn complete
# 2022-05-12 17:06:24 CCTE_Shafer_MEA_dev_burst_duration_mean_up complete
# 2022-05-12 17:06:44 CCTE_Shafer_MEA_dev_burst_rate_dn complete
# 2022-05-12 17:07:05 CCTE_Shafer_MEA_dev_burst_rate_up complete
# 2022-05-12 17:07:28 CCTE_Shafer_MEA_dev_bursting_electrodes_number_dn complete
# 2022-05-12 17:07:48 CCTE_Shafer_MEA_dev_bursting_electrodes_number_up complete
# 2022-05-12 17:08:10 CCTE_Shafer_MEA_dev_correlation_coefficient_mean_dn complete
# 2022-05-12 17:08:32 CCTE_Shafer_MEA_dev_correlation_coefficient_mean_up complete
# 2022-05-12 17:08:55 CCTE_Shafer_MEA_dev_firing_rate_mean_dn complete
# 2022-05-12 17:09:17 CCTE_Shafer_MEA_dev_firing_rate_mean_up complete
# 2022-05-12 17:09:37 CCTE_Shafer_MEA_dev_interburst_interval_mean_dn complete
# 2022-05-12 17:09:58 CCTE_Shafer_MEA_dev_interburst_interval_mean_up complete
# 2022-05-12 17:10:20 CCTE_Shafer_MEA_dev_mutual_information_norm_dn complete
# 2022-05-12 17:10:45 CCTE_Shafer_MEA_dev_mutual_information_norm_up complete
# 2022-05-12 17:11:14 CCTE_Shafer_MEA_dev_network_spike_duration_std_dn complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
# f() values at end points not of opposite sign
# 2022-05-12 17:11:47 CCTE_Shafer_MEA_dev_network_spike_duration_std_up complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
#                                     f() values at end points not of opposite sign
#                                   2022-05-12 17:12:16 CCTE_Shafer_MEA_dev_network_spike_number_dn complete
#                                   2022-05-12 17:12:53 CCTE_Shafer_MEA_dev_network_spike_number_up complete
#                                   2022-05-12 17:13:17 CCTE_Shafer_MEA_dev_network_spike_peak_dn complete
#                                   2022-05-12 17:13:43 CCTE_Shafer_MEA_dev_network_spike_peak_up complete
#                                   2022-05-12 17:14:05 CCTE_Shafer_MEA_dev_per_burst_interspike_interval_dn complete
#                                   Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
#                                                      f() values at end points not of opposite sign
#                                                    2022-05-12 17:14:29 CCTE_Shafer_MEA_dev_per_burst_interspike_interval_up complete
#                                                    2022-05-12 17:14:49 CCTE_Shafer_MEA_dev_per_burst_spike_percent_dn complete
#                                                    2022-05-12 17:15:09 CCTE_Shafer_MEA_dev_per_burst_spike_percent_up complete
#                                                    2022-05-12 17:15:30 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_dn complete
#                                                    2022-05-12 17:15:49 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_up complete
#                                                    2022-05-12 17:16:12 CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_dn complete
#                                                    2022-05-12 17:16:34 CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_up complete
#                                                    2022-05-12 17:17:04 CCTE_Shafer_MEA_dev_spike_duration_mean_dn complete
#                                                    2022-05-12 17:17:30 CCTE_Shafer_MEA_dev_spike_duration_mean_up complete
length(mc4.list) # 3842
nrow(use.dat) # 3842, cool!


# What's teh cause of this error?
test.dat <- use.dat[aenm == 'CCTE_Shafer_MEA_dev_network_spike_duration_std_up']
# debugonce(my_tcplfit2)
test.mc4.list <- my_tcplfit2(test.dat)
# Processing 1 assay endpoints...
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
# f() values at end points not of opposite sign
# 2022-05-16 09:11:20 CCTE_Shafer_MEA_dev_network_spike_duration_std_up complete
length(test.mc4.list) #113
nrow(test.dat) # 113
# So, I guess my_tcplfit2 did return a value for every row... there was just an error somwhere along

# What if it's caused by the gnls function?
test.mc4.list <- my_tcplfit2(test.dat, fitmodels = setdiff(eval(formals(my_tcplfit2)$fitmodels), 'gnls'))
# Processing 1 assay endpoints...
# 2022-05-16 09:14:55 CCTE_Shafer_MEA_dev_network_spike_duration_std_up complete
# ah ha, no error this time!

# I know there are issues with teh gnls model
# I'm fine with leavign that to others to solve for now
# But would I be missing some potentially sensitive modl_ga's if I leave out the gnls model?

# This is important for the question at hand. 

# Wait... why am I fitting in both the up and dn direction??
# I should just fit 1 set of aenm's!!

# Would make the most sense to fit the up
# Unfortunatley, that's what's giving me trouble right now

# Could I just run the the gnls, and assume that if gnls doesn't fit, it wasn't meant to be?
# Would a result for another model be returned?
res <- list()
for (i in 1:nrow(test.dat)) {
  res <- c(res, my_tcplfit2(test.dat[i]))
}


length(res)

res[[53]]$gnls

gnls.success <- unlist(lapply(res, function(x) x$gnls$success))
table(gnls.success) # all 1 ??

gnls.ac50.vals <- unlist(lapply(res, function(x) x$gnls$ac50))
gnls.ac50_loss.vals <- unlist(lapply(res, function(x) x$gnls$ac50_loss))

sum(is.na(gnls.ac50.vals)) # none 0??
sum(is.na(gnls.ac50_loss.vals)) # just 1 case...
which(is.na(gnls.ac50_loss.vals)) # 53 -> so this is the special case

# Let's see a dose-response curve

plotdat <- data.table('concentration_unlogged' = test.dat[53, unlist(concentration_unlogged)],
                      'response' = test.dat[53, unlist(response)],
                      'coff' = test.dat[53, coff])
library(ggplot2)
ggplot(plotdat, aes(x = concentration_unlogged, y = response)) +
  geom_point(pch = 19)+
  geom_hline(yintercept = 0)+
  scale_x_log10()
  
# Okay, this is rather messy (at the highest conc tested, 2/3 replicates are around -70, the other is above 200!)
# So even if a gnls model did fit this data - it woudl be really messy
# and I wouldn't worry too much if there was a really low modl_ga, because if that were the case, I think the
# model would have been doing something off!

# So I'm okay with just running the the gnls model, and if the gnls model doesn't converge,
# we'll just based the AC50 off of whatever model does win, and not wory about the possibility of another
# "true" low ac50 based on a gnls fit


# Running it again --------------------------------------------------------

rm(list = setdiff(ls(),c('dat2','my_tcplfit2','create_dat.by.spid.aenm','my_tcplhit2')))

## Prepare
dat.by.spid.aenm <- create_dat.by.spid.aenm(dat2)

## tcplfit2
# Remove controls, where only 2 conc tested
use.dat <- dat.by.spid.aenm[nconc > 1]
# Remove DIV12 endpoints, since I'm less interested in optimizing for these right now
# And some spid have a response range of 0 for the DIV12 ABE and BEN 
use.dat <- use.dat[!grepl('DIV12',aenm)]
# Keeping both up and dn for now, so that can compare
# Even though in the future I'd want to only run up
rm(dat.by.spid.aenm)
mc4.list <- my_tcplfit2(use.dat)
# Processing 34 assay endpoints...
# 2022-05-16 09:53:33 CCTE_Shafer_MEA_dev_AB_dn complete
# 2022-05-16 09:54:41 CCTE_Shafer_MEA_dev_LDH_dn complete
# 2022-05-16 09:55:50 CCTE_Shafer_MEA_dev_active_electrodes_number_dn complete
# 2022-05-16 09:56:53 CCTE_Shafer_MEA_dev_active_electrodes_number_up complete
# 2022-05-16 09:58:43 CCTE_Shafer_MEA_dev_burst_duration_mean_dn complete
# 2022-05-16 10:00:23 CCTE_Shafer_MEA_dev_burst_duration_mean_up complete
# 2022-05-16 10:01:26 CCTE_Shafer_MEA_dev_burst_rate_dn complete
# 2022-05-16 10:02:27 CCTE_Shafer_MEA_dev_burst_rate_up complete
# 2022-05-16 10:03:54 CCTE_Shafer_MEA_dev_bursting_electrodes_number_dn complete
# 2022-05-16 10:04:50 CCTE_Shafer_MEA_dev_bursting_electrodes_number_up complete
# 2022-05-16 10:05:47 CCTE_Shafer_MEA_dev_correlation_coefficient_mean_dn complete
# 2022-05-16 10:06:46 CCTE_Shafer_MEA_dev_correlation_coefficient_mean_up complete
# 2022-05-16 10:07:31 CCTE_Shafer_MEA_dev_firing_rate_mean_dn complete
# 2022-05-16 10:08:01 CCTE_Shafer_MEA_dev_firing_rate_mean_up complete
# 2022-05-16 10:08:29 CCTE_Shafer_MEA_dev_interburst_interval_mean_dn complete
# 2022-05-16 10:08:58 CCTE_Shafer_MEA_dev_interburst_interval_mean_up complete
# 2022-05-16 10:09:21 CCTE_Shafer_MEA_dev_mutual_information_norm_dn complete
# 2022-05-16 10:09:49 CCTE_Shafer_MEA_dev_mutual_information_norm_up complete
# 2022-05-16 10:10:35 CCTE_Shafer_MEA_dev_network_spike_duration_std_dn complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
# f() values at end points not of opposite sign
# 2022-05-16 10:11:09 CCTE_Shafer_MEA_dev_network_spike_duration_std_up complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
# f() values at end points not of opposite sign
# 2022-05-16 10:11:49 CCTE_Shafer_MEA_dev_network_spike_number_dn complete
# 2022-05-16 10:12:25 CCTE_Shafer_MEA_dev_network_spike_number_up complete
# 2022-05-16 10:13:00 CCTE_Shafer_MEA_dev_network_spike_peak_dn complete
# 2022-05-16 10:13:41 CCTE_Shafer_MEA_dev_network_spike_peak_up complete
# 2022-05-16 10:14:08 CCTE_Shafer_MEA_dev_per_burst_interspike_interval_dn complete
# Error in uniroot(acgnlsobj, c(toploc, 1e+05), y = y, tp = tp, ga = ga,  : 
# f() values at end points not of opposite sign
# 2022-05-16 10:14:41 CCTE_Shafer_MEA_dev_per_burst_interspike_interval_up complete
# 2022-05-16 10:16:36 CCTE_Shafer_MEA_dev_per_burst_spike_percent_dn complete
# 2022-05-16 10:17:08 CCTE_Shafer_MEA_dev_per_burst_spike_percent_up complete
# 2022-05-16 10:17:43 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_dn complete
# 2022-05-16 10:18:09 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_up complete
# 2022-05-16 10:18:37 CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_dn complete
# 2022-05-16 10:19:18 CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_up complete
# 2022-05-16 10:19:48 CCTE_Shafer_MEA_dev_spike_duration_mean_dn complete
# 2022-05-16 10:20:18 CCTE_Shafer_MEA_dev_spike_duration_mean_up complete
length(mc4.list) # 3842
nrow(use.dat) # 3842, cool!

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


## Save the output ---------------------------------------------------------

setkey(mc5, NULL)
description <- 'Preliminary tcplfit2 output and AC50 estimates for DNT NTP 2021 data.
Ran in both up and dn direction to compare AC50 values.
Created with DNT_NTP_2021_estimate_ac50s_and_lvl6_flags_with_tcplfit2_2022-05-12.R'
save(mc4.list, mc5, description, file = file.path('DNT_NTP2021/check_for_chem_to_rescreen/data/DNT_NTP2021_tcplfit2_exploratory_ac50_estimates_2022-05-16.RData'))


# How does it look? -------------------------------------------------------

rm(mc4.list, dat2)

## Quick comparison of up and dn, see if anything is drastically di --------

head(mc5)
mc5[, aenm_dir := ifelse(grepl('_up$',aenm),'up','dn')]
mc5[, acnm := stri_replace(aenm, regex = '_[^_]{2}$', replacement = '')]
mc5[, ac50_log10 := log10(ac50)]
mc5[, ac50_loss_log10 := log10(ac50_loss)]

# is the ac0 loss always larger than the ac50?
mc5[, summary(ac50_loss - ac50)]
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.031    0.340    7.563  466.622   18.377 9739.648     3778
# Yes - so for the sake of checking if the ac50 is less than the lowest conc tested,
# the ac50_loss is irrelevant

# Compare the actual ac50's from the aenm's in the up and dn direction - should be diff models, but overall very similar ac50s
ac50.diff <- mc5[, .(diff_val = ac50[aenm_dir == 'dn'] - ac50[aenm_dir == 'up'],
                     mean_val = (ac50[aenm_dir == 'dn'] + ac50[aenm_dir == 'up'])*0.5), by = .(acnm, spid)]

ac50.diff[, diff_perc := diff_val/abs(mean_val)*100]
p <- ggplot(ac50.diff, aes(x = diff_perc)) +
  geom_histogram()+
  ggtitle('DNT NT 2021 Distribution of % different in tcplfit2 AC50 values\nwhen fitting up vs dn endpoints resp values
% diff = (AC50_dn - AC50 up)/|mean(AC50_dn, AC50_up)|*100%')
ggsave(plot = p, filename = file.path('DNT_NTP2021/check_for_chem_to_rescreen/figs/DNT_NTP2021_tcplfit2_ac50_dn_vs_up_endpoint_results.png'),
                            width = 6, height = 5)
# good, vast majority centered around 0!!

# how much with +/- 10%?
ac50.diff[, summary(diff_perc)]
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -199.92003   -0.00076    0.00000   -3.43698    0.00040  199.59724
npts <- nrow(ac50.diff)
ac50.diff[diff_perc < -0.1, .(perc_points = .N/npts)]
# perc_points
# 1:  0.08130531
ac50.diff[diff_perc > 0.1, .(perc_points = .N/npts)]
# perc_points
# 1:  0.05973451
ac50.diff[abs(diff_perc) < 0.1, .(perc_points = .N/npts)]
# perc_points
# 1:   0.8589602
ac50.diff[abs(diff_perc) < 0.15, .(perc_points = .N/npts)]
# perc_points
# 1:   0.8661504
ac50.diff[abs(diff_perc) < 0.2, .(perc_points = .N/npts)]
# perc_points
# 1:   0.8761062
# you have that middle section, then the rest if pretty spread out


# Maybe this would make more sense to talk bout as a fold change
ac50.fc <- mc5[, .(ac50_dn = ac50[aenm_dir == 'dn'], ac50_up = ac50[aenm_dir == 'up']), by = .(acnm, spid)]
ac50.fc[, fc := ac50_dn/ac50_up]
ac50.fc[, summary(fc)]
# actually, I think thsi makes more sense when the values are logged


ac50.diff[abs(diff_perc) > 0.1, .N, by = .(spid)][order(-N)]
# of the 74 affected spid, 7 are affected in 9 or mroe assay components (out of 15)

# REally, I just want to get my eyes on some curves, makesure it looks reasonable

# options to get curves
# - plot conc response? Or did that only plot values
# - zach's function
# - 
concRespPlot()
?concRespPlot
# nah, need output from concRespCore

# Actually, let's do it
ac50.diff[spid == '7126 F8' & abs(diff_perc) > 0.1]
plotting.data <- use.dat[spid == '7126 F8' & aenm == 'CCTE_Shafer_MEA_dev_firing_rate_mean_up']
row <- list(conc = plotting.data[, unlist(concentration_unlogged)],
            resp = plotting.data[, unlist(response)],
            cutoff = plotting.data[, unique(coff)],
            onesd = plotting.data[, unique(osd)],
            name = plotting.data[, unique(spid)],
            assay = plotting.data[, unique(aenm)])
res <- concRespCore(row,fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3",
                                      "exp4", "exp5"),conthits = T, do.plot=T)
concRespPlot(res) # whaa...? Hit call 0...?

# What does mc5 say hitcall should be?
mc5[spid == '7126 F8' & acnm == 'CCTE_Shafer_MEA_dev_firing_rate_mean', .(spid, aenm_dir, ac50, fit_method, hitcall, coff, cutoff)]
# spid aenm_dir        ac50 fit_method hitcall     coff   cutoff
# 1: 7126 F8       dn 0.010321535       exp5       1 72.16093 72.16093
# 2: 7126 F8       up 0.004699298       exp5       1 72.16093 72.16093

# Why is every response vaue -100??
plotting.data[, unlist(response)]
# [1] -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100 -100

# Let's get soem additional info
plotting.data <- use.dat[spid == '7126 F8' & aenm == 'CCTE_Shafer_MEA_dev_burst_duration_mean_up']
plotting.data[, unlist(response)] # also all -100...
lapply(1:nrow(use.dat[spid == '7126 F8']),function(i) use.dat[spid == '7126 F8'][i, unique(unlist(response))])
# only the first 2 endpoints are different
use.dat[spid == '7126 F8'][1:2]
# ah, AB and LDH
# ah - this substance is already on Seline's list of substances to repeat!!
# So I believe that is it super potent!!


# Wha taobut some others?
ac50.diff[abs(diff_perc) > 0.1, .N, by = .(spid)][order(-N)]
# the top 5 are all on Seline's list of repeats
# I would liek to know - if I had used only the up or only the dn, would I have missed the AC50 being realy low for these 5?
mc5[, ac50_below_minconc := as.numeric(log10(ac50) < logc_min)]
flag.tb <- dcast(mc5, spid + acnm ~ aenm_dir, value.var = 'ac50_below_minconc')
flag.tb[, .N, by = .(dn, up)]
#    dn up    N
# 1:  0 NA  209
# 2:  0  0 1641
# 3:  1  1  158
# 4:  1  0    7
# 5:  0  1    2
# 6:  1 NA   17
# only 9 cases woudl have disagreed!
# more cases where dn is below, up is not

# are they borderline, or drastic difference?
check.rows <- flag.tb[dn != up, .(spid, acnm)]
setkey(mc5, spid, acnm)
mc5[.(check.rows), .(spid, aenm, ac50_log10 = log10(ac50), logc_min)][order(spid, aenm, ac50_log10)]
# spid                                                       aenm ac50_log10   logc_min
# 1: 7126 B10     CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_dn -1.9999995 -1.0000000
# 2: 7126 B10     CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_up -0.3840194 -1.0000000
# 3:  7126 B3                    CCTE_Shafer_MEA_dev_firing_rate_mean_dn -2.3010299 -1.3010300
# 4:  7126 B3                    CCTE_Shafer_MEA_dev_firing_rate_mean_up  1.3979400 -1.3010300
# 5: 7126 C12                  CCTE_Shafer_MEA_dev_network_spike_peak_dn  0.9965717 -1.0000000
# 6: 7126 C12                  CCTE_Shafer_MEA_dev_network_spike_peak_up -2.0000000 -1.0000000
# 7:  7126 C9 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_dn -2.0000674 -1.0000000
# 8:  7126 C9 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_up  1.6989700 -1.0000000
# 9: 7126 D10        CCTE_Shafer_MEA_dev_correlation_coefficient_mean_dn -1.9956786 -0.9956786
# 10: 7126 D10        CCTE_Shafer_MEA_dev_correlation_coefficient_mean_up  1.7032914 -0.9956786
# 11: 7126 D11                          CCTE_Shafer_MEA_dev_burst_rate_dn -1.9956786 -0.9956786
# 12: 7126 D11                          CCTE_Shafer_MEA_dev_burst_rate_up -0.5218257 -0.9956786
# 13:  7126 G2          CCTE_Shafer_MEA_dev_network_spike_duration_std_dn -1.0145010 -0.9956786
# 14:  7126 G2          CCTE_Shafer_MEA_dev_network_spike_duration_std_up -0.9414168 -0.9956786
# 15:  9163 A2 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_dn -1.8774734 -1.9586073
# 16:  9163 A2 CCTE_Shafer_MEA_dev_per_network_spike_spike_number_mean_up -2.4942797 -1.9586073
# 17:  9163 B8                 CCTE_Shafer_MEA_dev_burst_duration_mean_dn -3.0000222 -2.0000000
# 18:  9163 B8                 CCTE_Shafer_MEA_dev_burst_duration_mean_up  0.6989700 -2.0000000

# Check some plots - is  the "dn" in the right here, or flukey?

plotting.data <- use.dat[spid == '7126 B10' & aenm == 'CCTE_Shafer_MEA_dev_per_network_spike_spike_percent_dn']
row <- list(conc = plotting.data[, concentration_unlogged],
            resp = plotting.data[, response],
            bmed = plotting.data[, unique(bmed)],
            cutoff = plotting.data[, unique(coff)],
            onesd = plotting.data[, unique(osd)],
            name = plotting.data[, unique(spid)],
            assay = plotting.data[, unique(aenm)])
res <- concRespCore(row,fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3",
                                      "exp4", "exp5"),conthits = T, do.plot=T)

# Just see soem good plots to get confidence :)

# Any others to check out before next section?


# I think this is a moment when I am overanalyzing a bit
# In the vast majority of cases, the difference in the ac50's seems to be within +/-10% 

## How often is modl_ga < lowest conc tested? ------------------------------

# Note: I probably can't rely on the modl.ga.lowconc flag, because that is restricted to cases
# where the hitc == 1. These methods are not configured fro tcplfit2, with continuous hit calls

# Any other flags?


# Takeaways ---------------------------------------------------------------

# - Some DIV12 endpoints have resp range 0 for some spid. But not worth debugging right now, so just ignore
# - Sometimes, the gnls model will have issues and there won't be an ac50_loss value. Btu I think that's okay
#   In cases where that is the most sensitive model, I think the curve will be a bit wonky regardless,
#   So I'm guessing that if we had an ac50_loss, it woudl not be a believable super-potent value. Plus, we 
#   have lots of other endpoints to look for legitimate activity as well.
# - When you do get an error regarding "end poitns not of opposite sign", know that everything but the ac50_loss for the gnls 
#   Will still be calculate. So you don't have to worry about anythign getting skipped over.
# - Up vs dn...


