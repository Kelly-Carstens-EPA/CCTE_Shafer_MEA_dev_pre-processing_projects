# ------------------------------------------------------------------------ #
# Checking that none of hte cultures look reeeeeally bad
# 
# May 17, 2022
# carpenter.amy@epa.gov
# ------------------------------------------------------------------------ # 

library(data.table)
library(ggplot2)
library(openxlsx)


# Load DNT NTP 2021 data --------------------------------------------------

load('DNT_NTP2021/output/DNT_NTP2021_preliminary_longfile.RData')


# Confirm no super terrible cultures --------------------------------------

# usually set wllq to 0 where median of controls on DIV 12 is < 10 spikes per min or < 2 active electrodes
remove_apid <- dat[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn), .(bval = median(rval, na.rm = T)), by = .(apid)][bval < 10/60, unique(apid)]
remove_apid <- union(remove_apid,
                     dat[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn), .(bval = median(rval, na.rm = T)), by = .(apid)][bval < 2, unique(apid)])
remove_apid
# empty , good


# Load mea nfa data to date -----------------------------------------------

load('lvl0_snapshots/mea_nfa_lvl0_2021-05-05.RData')

# Merge together
setdiff(names(mea_nfa_lvl0), names(dat))
dat[, acnm := acsn]
dat[, spid := treatment]
dat[, DNT_NTP2021 := 1]
mc0 <- rbind(mea_nfa_lvl0, dat, fill = T)
rm(mea_nfa_lvl0, dat)
mc0[is.na(DNT_NTP2021), DNT_NTP2021 := 0]


# View controls over time -------------------------------------------------

mc0[, culture := sub('_.*$','',apid)]
mc0[, culture_date := as.Date(culture, format = '%Y%m%d')]
mc0[, bval := median(rval[wllq == 1 & wllt == 'n'], na.rm = T), by = .(apid, acnm)]


## Mean firing rate --------------------------------------------------------

plotdat <- mc0[grepl("firing_rate_mean_DIV12",acnm) & wllt == 'n']

p <- ggplot(plotdat[wllq == 1], aes(x = culture_date, y = rval)) +
  geom_jitter(aes(color = as.factor(DNT_NTP2021)), height = 0, width = 0.2, pch = 1)+
  geom_point(aes(y = bval))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)-2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)+2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  ggtitle('All MEA NFA data to date\nMean Firing Rate on DIV 12 in Controls\nPlate-wise median shown in black\nMean +/- 2SD of plate-wise medians shown in horizontal lines')+
  theme(legend.position = 'top')
p
ggsave(plot = p, filename = 'DNT_NTP2021/check_for_chem_to_rescreen/mean_firing_rate_controls_over_time.png',
       width = 10, height = 6)

## nAE ---------------------------------------------------------------------

plotdat <- mc0[grepl("active_electrodes_number_DIV12",acnm) & wllt == 'n']
p <- ggplot(plotdat, aes(x = culture_date, y = rval)) +
  geom_jitter(aes(color = as.factor(DNT_NTP2021)), height = 0, width = 0.2, pch = 1)+
  geom_point(aes(y = bval))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)-2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)+2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  ggtitle('All MEA NFA data to date\n# Active Electrodes on DIV 12 in Controls\nPlate-wise median shown in black\nMean +/- 2SD of plate-wise medians shown in horizontal lines')+
  theme(legend.position = 'top')
ggsave(plot = p, filename = 'DNT_NTP2021/check_for_chem_to_rescreen/nAE_controls_over_time.png',
       width = 10, height = 6)

## nAE AUC ---------------------------------------------------------------------

plotdat <- mc0[grepl("active_electrodes_number$",acnm) & wllt == 'n']
p <- ggplot(plotdat, aes(x = culture_date, y = rval)) +
  geom_jitter(aes(color = as.factor(DNT_NTP2021)), height = 0, width = 0.2, pch = 1)+
  geom_point(aes(y = bval))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)-2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)+2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  ggtitle('All MEA NFA data to date\n# Active Electrodes AUC in Controls\nPlate-wise median shown in black\nMean +/- 2SD of plate-wise medians shown in horizontal lines')+
  theme(legend.position = 'top')
ggsave(plot = p, filename = 'DNT_NTP2021/check_for_chem_to_rescreen/nAE_AUC_controls_over_time.png',
       width = 10, height = 6)


## network spike number --------------------------------------------------------
plotdat <- mc0[grepl("network_spike_number$",acnm) & wllt == 'n']

p <- ggplot(plotdat[wllq == 1], aes(x = culture_date, y = rval)) +
  geom_jitter(aes(color = as.factor(DNT_NTP2021)), height = 0, width = 0.2, pch = 1)+
  geom_point(aes(y = bval))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)-2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)+2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  ggtitle('All MEA NFA data to date\nNetwork Spike Number AUC in Controls\nPlate-wise median shown in black\nMean +/- 2SD of plate-wise medians shown in horizontal lines')+
  theme(legend.position = 'top')
p
ggsave(plot = p, filename = 'DNT_NTP2021/check_for_chem_to_rescreen/networ_spike_number_AUC_controls_over_time.png',
       width = 10, height = 6)


## Mutual information --------------------------------------------------------
plotdat <- mc0[grepl("mutual_information_norm$",acnm) & wllt == 'n']

p <- ggplot(plotdat[wllq == 1], aes(x = culture_date, y = rval)) +
  geom_jitter(aes(color = as.factor(DNT_NTP2021)), height = 0, width = 0.2, pch = 1)+
  geom_point(aes(y = bval))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T))+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)-2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  geom_hline(yintercept = mean(plotdat[wllq == 1, bval], na.rm = T)+2*sd(plotdat[wllq == 1, bval], na.rm = T), lty = 'dashed')+
  ggtitle('All MEA NFA data to date\nMutual Information AUC in Controls\nPlate-wise median shown in black\nMean +/- 2SD of plate-wise medians shown in horizontal lines')+
  theme(legend.position = 'top')
p
ggsave(plot = p, filename = 'DNT_NTP2021/check_for_chem_to_rescreen/mutual_information_normalized_controls_over_time.png',
       width = 10, height = 6)



# Would be nice: flag everythign that is below X for a given thing

bval.tb <- mc0[, unique(.SD), .SDcols = c('bval','acnm','DNT_NTP2021','apid','culture_date')]
bval.tb[, bval_perc := rank(bval, ties.method = 'random')/.N, by = .(acnm)]

bval.tb[DNT_NTP2021 == 1, summary(bval_perc)]

# Which endpoints/plates are really low?
bval.tb[!grepl('DIV',acnm) | grepl('DIV12',acnm) & DNT_NTP2021 == 1 & bval_perc < 0.1]

# Which endpoints/plates are really high?

save.bval.tb <- bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & DNT_NTP2021 == 1][order(acnm, apid), .(acnm, culture_date, apid, bval, bval_perc)]
wb <- createWorkbook()
addWorksheet(wb, 'bval %ile')
writeData(wb, sheet = 1, save.bval.tb)
saveWorkbook(wb, file = 'DNT_NTP2021/check_for_chem_to_rescreen/DNT_NTP2021_bval_percentile_table_2022-05-20.xlsx')


# Checkout where beyond 2SD -----------------------------------------------

bval.tb[, mean_bval := mean(bval, na.rm = T), by = .(acnm)]
bval.tb[, sd_bval := sd(bval, na.rm = T), by = .(acnm)]
bval.tb[, normal_bval_lb := mean_bval - 2*sd_bval]
bval.tb[, normal_bval_ub := mean_bval + 2*sd_bval]

bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval < normal_bval_lb, .(length(unique(apid))), by = .(DNT_NTP2021)]
# DNT_NTP2021 V1
# 1:           0 34
# 2:           1  7

# Where above normal? Though I thin kthis is less concerning
bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval > normal_bval_ub, .(length(unique(apid))), by = .(DNT_NTP2021)]
# DNT_NTP2021  V1
# 1:           0 115
# 2:           1  41

bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval < normal_bval_lb & DNT_NTP2021 == 1, .N, by = .(culture_date, apid)][order(N)]
#    culture_date               apid  N
# 1:   2021-12-08 20211208_MW78-6305  1
# 2:   2021-11-10 20211110_MW78-6209  3
# 3:   2021-11-10 20211110_MW78-6213  4
# 4:   2021-11-10 20211110_MW78-6212  5
# 5:   2021-11-10 20211110_MW78-6211  7
# 6:   2021-11-10 20211110_MW78-6208  8
# 7:   2021-11-10 20211110_MW78-6210 10
# Where just 1-3 endpoints are a bit low  probably less concnering
# but let's see specific endpoints
bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval < normal_bval_lb & DNT_NTP2021 == 1, .(culture_date, apid, acnm)][order(apid)]

# Endpoints that correspond to general activity
bval.tb[(grepl('active_electrodes_number',acnm) | grepl('firing_rate_mean',acnm)) & !grepl('DIV[579]',acnm) & bval < normal_bval_lb & DNT_NTP2021 == 1, .(culture_date, apid, acnm)][order(apid)]
#    culture_date               apid                                               acnm
# 1:   2021-11-10 20211110_MW78-6208       CCTE_Shafer_MEA_dev_active_electrodes_number
# 2:   2021-11-10 20211110_MW78-6208 CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12
# 3:   2021-11-10 20211110_MW78-6210       CCTE_Shafer_MEA_dev_active_electrodes_number
# 4:   2021-11-10 20211110_MW78-6210 CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12
# 5:   2021-11-10 20211110_MW78-6211       CCTE_Shafer_MEA_dev_active_electrodes_number
check.plates <- bval.tb[(grepl('active_electrodes_number',acnm) | grepl('firing_rate_mean',acnm)) & !grepl('DIV[579]',acnm) & bval < normal_bval_lb & DNT_NTP2021 == 1, unique(apid)]
check.spid <- mc0[apid %in% check.plates & wllt == 't', unique(spid)]
length(check.spid) # 12... oh right, probs some plates are from the same group, so have same chem


# Load current info re chem to rescreen -----------------------------------

chem.info <- as.data.table(read.xlsx('DNT_NTP2021/check_for_chem_to_rescreen/DNT_NTP2021_MEA_NFA_tcplfit2_summary_to_aid_rescreen_determinations_2022-05-17.xlsx', sheet= 2))

chem.info[, rescreen := 0]
chem.info[num_ac50_below_minconc_and_hit >= 3 & num_ac50_below_minconc_and_hit / num_hitcall_above_0.5 >= 3/8, rescreen := 1]
chem.info[num_noisy_endpoints >= 10, rescreen := 1]
chem.info[, .N, by = .(rescreen)]
# 13/113 to rescreen

chem.info[, num_noisy_endpoints_rank := rank(-1*num_noisy_endpoints, ties.method = 'min')]
chem.info[, summary(num_noisy_endpoints_rank)]

# Where 
setdiff(check.spid, chem.info$spid) # empty
chem.info[spid %in% check.spid]
#        spid num_assay_components num_hitcall_above_0.5 num_ac50_below_minconc_and_hit num_noisy_endpoints                                         wllq_notes rescreen num_noisy_endpoints_rank
# 1: 7126 C11                   19                    18                             16                   0 7126 C11 will be repeated on a lower concentration        1                       27
# 2: 7126 C12                   19                     6                              3                   7                                                           1                        5
# 3:  7126 C7                   19                     8                              3                   7                                                           1                        5
# 4:  7126 C9                   19                     6                              3                   7                                                           1                        5
# 5:  7126 C1                   19                    17                              0                  15                                                           1                        1
# 6:  7126 C4                   19                     8                              0                  13                                                           1                        2
# 7:  7126 C3                   19                     5                              0                  10                                                           1                        3
# 8:  7126 C6                   19                    18                              0                   8                                                           0                        4
# 9:  7126 C2                   19                    14                              0                   7                                                           0                        5
# 10:  7126 C5                   19                    18                              0                   7                                                           0                        5
# 11: 7126 C10                   19                    17                              0                   4                                                           0                       10
# 12:  7126 C8                   19                    18                              0                   4                                                           0                       10
# So currently 7/12 of the spids are already slotted to be rested for various reasons
# The other 5 - these are some of the top spids with the most noisy endpoints

# How many other spid have this many noisy endpoints?
chem.info[order(-num_noisy_endpoints), .(spid, num_noisy_endpoints, spid %in% check.spid)][num_noisy_endpoints > 0]
# There is only 1 other spid htat has a comparable number of noisy endpoints
# (9163 B6, with 4 noisy endpoints. This spid also has 14/19 predicted hits!)

# On that point - these 5 additional spid to rescreen all have 14-18 hits...

# So the activity towards the top is generally believable

# Looking at other spids... many are a bit noisy
chem.info[num_noisy_endpoints == 0]

load(file.path('DNT_NTP2021/check_for_chem_to_rescreen/data/DNT_NTP2021_tcplfit2_exploratory_ac50_estimates_2022-05-17.RData'))


# Or make it simple - just pull all curves for substances that are flagged for any of these reasons
N <- nrow(chem.info[num_noisy_endpoints == 0 & num_ac50_below_minconc_and_hit == 0])
set.seed(23)
plot.spids <- chem.info[num_noisy_endpoints == 0][sample(N, 10), unique(spid)]
length(plot.spids) # 40

# Adding some additional columsn for concRespPlot
mc5[, assay := aenm]
mc5[, name := spid]
mc5[, .N, by = .(rmse)][N > 1] # only 0 is ever repeated...
# and these cases don't matter, bc they won't be near the 95th percentile
mc5[, rmse_percentile := rank(rmse, ties.method = 'random')/.N, by = .(aenm)]

library(tcplfit2)

pdf(file = 'DNT_NTP2021/check_for_chem_to_rescreen/figs/DNT_NTP2021_tcplfit2_plots_chem_that_look_good.pdf',
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


# How to RMSE %iles for these spid compare to others?
mc5[, spid_from_low_plates := as.numeric(spid %in% check.spid)]

ggplot(mc5, aes(x = rmse_percentile)) +
  geom_histogram(aes(fill = as.factor(spid_from_low_plates)), position = 'identity', alpha = 0.5)


# TBH, I'm feeling quite skeptical of this percentile business

# The situation:
# - Based on convo with Tim, There are 13 substances that we should rescreen
# - I was considering recommending that we also rescreen all substances tested on plates with very low actiivty compared to the rest
# of the mea nfa data, particularly the MFR and the nAE.
#  Tim suggested that I look at the curves for any substances tsted on these plates that we aren't already rescreening
# Just considering plates with low activity for nAE or MFR AUC/DIV12 endpoints, there are 3 plates havign tested 12 spid
# 7 of these we already plan to retest.
# So I'm deciding if I should recommend retesting the other 5 as well.
# On one hand, i want to recommend that we do retest these 5, because they comprise all but 1 of the remaining spids that I have flag having 
# 4 or more noisy endpoints.
# On the other hand, noise happens. Just looking comparing curves, I'm having trouble assessing whether the curves for these spid are
# consistently more noisy than for curves for other spids.

# I think I could go either way.
# BUT, I do have a fairly stringent cutoff for being a noisy endpoint (RMSE > coff AND 95% RMSE for that endpoint)
# So I think I'm going to recommend that we retest these 5 substances. Will email this to Seline and Tim
# If Tim disagrees, then we can discuss!

# to do:
# - Send list to Seline of samples to repeat (include reasons of why repeat)
# - write up a short justification for these samples 
# - Is this final script sufficient to describe our methods for what we decided to rescreen? If not, add additional documentation