# ------------------------------------------------------------------------ #
# Checking that none of hte cultures look reeeeeally bad
# 
# May 17, 2022
# carpenter.amy@epa.gov
# ------------------------------------------------------------------------ # 

library(data.table)
library(ggplot2)
library(openxlsx)

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




# Start with table  -------------------------------------------------------

chem.info <- as.data.table(read.xlsx('DNT_NTP2021/check_for_chem_to_rescreen/DNT_NTP2021_MEA_NFA_tcplfit2_summary_to_aid_rescreen_determinations_2022-05-17.xlsx', sheet= 2))
# Created in "DNT_NTP2021_determine_samples_to_rescreen_2022-05-17.R"

# But that script just created this table to aid decisions
# Now we are actually going to make decisions


# Apply criteria discussed with Tim ---------------------------------------

# Intialize columns
chem.info[, rescreen := 0]
chem.info[, rescreen_note := '']

# Where 3 or more endpoints have an estimated AC50 < min conc tested and hitcall > 50% AND the predicted AC50 is < min conc tested for at least 3/8 (37.5%) of the predicted hits,
# rescreen those susbstances at lower concentrations
chem.info[num_ac50_below_minconc_and_hit >= 3 & num_ac50_below_minconc_and_hit / num_hitcall_above_0.5 >= 3/8, 
          `:=`(rescreen = 1,
               rescreen_note = paste0(rescreen_note, 'rescreen at lower concentrations;'))]

# Where 10 or more of the endpoints are flagged as being noisy
# (based on RMSE > cutoff and RMSE > 95%ile for that endpoint), rescreen
chem.info[num_noisy_endpoints >= 10, 
          `:=`(rescreen = 1,
               rescreen_note = paste0(rescreen_note, 'several endpoints appear noisy;'))]

# View results
chem.info[, .N, by = .(rescreen, rescreen_note)]
#    rescreen                     rescreen_note   N
# 1:        1 rescreen at lower concentrations;  10
# 2:        0                                   100
# 3:        1   several endpoints appear noisy;   3
# So we definitely want to rescreen at least 13 samples



# Confirm no apid have exceptionally low activity --------------------------------------

load('DNT_NTP2021/output/DNT_NTP2021_preliminary_longfile.RData')

# Using thresholds currently set in save lvl0 snapshot function (that I made for the MEA NFA)

# usually set wllq to 0 where median of controls on DIV 12 is < 10 spikes per min or < 2 active electrodes
remove_apid <- dat[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn), .(bval = median(rval, na.rm = T)), by = .(apid)][bval < 10/60, unique(apid)]
remove_apid <- union(remove_apid,
                     dat[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn), .(bval = median(rval, na.rm = T)), by = .(apid)][bval < 2, unique(apid)])
remove_apid
# empty , good



# Check if any apid have relatively low activity --------------------------

## Create 1 mc0 with all mea nfa data to date -----------------------------------------------

load('lvl0_snapshots/mea_nfa_lvl0_2021-05-05.RData')

# Merge together
setdiff(names(mea_nfa_lvl0), names(dat))
dat[, acnm := acsn]
dat[, spid := treatment]
dat[, DNT_NTP2021 := 1]
mc0 <- rbind(mea_nfa_lvl0, dat, fill = T)
rm(mea_nfa_lvl0, dat)
mc0[is.na(DNT_NTP2021), DNT_NTP2021 := 0]

mc0[, culture := sub('_.*$','',apid)]
mc0[, culture_date := as.Date(culture, format = '%Y%m%d')]
mc0[, bval := median(rval[wllq == 1 & wllt == 'n'], na.rm = T), by = .(apid, acnm)]


## Identify any apid with relatively low bvals ------------------------------------------------

bval.tb <- mc0[, unique(.SD), .SDcols = c('bval','acnm','DNT_NTP2021','apid','culture_date')]

# Calcuate typical range of bvals
bval.tb[, mean_bval := mean(bval, na.rm = T), by = .(acnm)]
bval.tb[, sd_bval := sd(bval, na.rm = T), by = .(acnm)]
bval.tb[, normal_bval_lb := mean_bval - 2*sd_bval]
bval.tb[, normal_bval_ub := mean_bval + 2*sd_bval]

# Where is bval unusually low?
bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval < normal_bval_lb, .(length(unique(apid))), by = .(DNT_NTP2021)]
# DNT_NTP2021 V1
# 1:           0 34
# 2:           1  7
# some of DNT NTP2021's plates are affected, though there are plenty of instances 
# from other data sets as well

# Where above normal? Though I thin kthis is less concerning though
bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval > normal_bval_ub, .(length(unique(apid))), by = .(DNT_NTP2021)]
# DNT_NTP2021  V1
# 1:           0 115
# 2:           1  41

# Which apid are affected? (Just looking at AUC and DIV12 endpoints)
bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval < normal_bval_lb & DNT_NTP2021 == 1, .N, by = .(culture_date, apid)][order(N)]
#    culture_date               apid  N
# 1:   2021-12-08 20211208_MW78-6305  1
# 2:   2021-11-10 20211110_MW78-6209  3
# 3:   2021-11-10 20211110_MW78-6213  4
# 4:   2021-11-10 20211110_MW78-6212  5
# 5:   2021-11-10 20211110_MW78-6211  7
# 6:   2021-11-10 20211110_MW78-6208  8
# 7:   2021-11-10 20211110_MW78-6210 10
# Where just 1-3 endpoints are a bit low is probably less concnering
# but let's see specific endpoints
bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval < normal_bval_lb & DNT_NTP2021 == 1, .(culture_date, apid, acnm)][order(apid)]

# Let's focus on the endpoints that are typically used to assess the general activity

bval.tb[acnm %in% c('CCTE_Shafer_MEA_dev_active_electrodes_number',
                    'CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12',
                    'CCTE_Shafer_MEA_dev_firing_rate_mean',
                    'CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12') & bval < normal_bval_lb & DNT_NTP2021 == 1, .(culture_date, apid, acnm)][order(apid)]
#    culture_date               apid                                               acnm
# 1:   2021-11-10 20211110_MW78-6208       CCTE_Shafer_MEA_dev_active_electrodes_number
# 2:   2021-11-10 20211110_MW78-6208 CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12
# 3:   2021-11-10 20211110_MW78-6210       CCTE_Shafer_MEA_dev_active_electrodes_number
# 4:   2021-11-10 20211110_MW78-6210 CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12
# 5:   2021-11-10 20211110_MW78-6211       CCTE_Shafer_MEA_dev_active_electrodes_number
# Cool, the 3 plates with 7 or more affected endpoints from above are identified with this method
plates.possibly.rescreen <- bval.tb[(grepl('active_electrodes_number',acnm) | grepl('firing_rate_mean',acnm)) & !grepl('DIV[579]',acnm) & bval < normal_bval_lb & DNT_NTP2021 == 1, unique(apid)]
plates.possibly.rescreen
# "20211110_MW78-6208" "20211110_MW78-6210" "20211110_MW78-6211"
spid.possibly.rescreen<- mc0[apid %in% plates.possibly.rescreen & wllt == 't', unique(spid)]
length(spid.possibly.rescreen) # 12... oh right, probs some plates are from the same group, so have same chem

# Side note:
# of the 7 apid identified above, all but the first one with just 1 low endpoint have tested the same spid as the 3 apid selected here
mc0[spid %in% spid.possibly.rescreen, .N, by = .(apid)]
# apid    N
# 1: 20211110_MW78-6208 3654
# 2: 20211110_MW78-6209 3654
# 3: 20211110_MW78-6210 3654
# 4: 20211110_MW78-6211 3654
# 5: 20211110_MW78-6212 3654
# 6: 20211110_MW78-6213 3654


# Assess spid on plates to possibly rescreen ------------------------------

setdiff(spid.possibly.rescreen, chem.info$spid) # empty
chem.info[spid %in% spid.possibly.rescreen]
# spid num_assay_components num_hitcall_above_0.5 num_ac50_below_minconc_and_hit num_noisy_endpoints                                         wllq_notes                     rescreen_note rescreen
# 1: 7126 C11                   19                    18                             16                   0 7126 C11 will be repeated on a lower concentration rescreen at lower concentrations;        1
# 2: 7126 C12                   19                     6                              3                   7                                                    rescreen at lower concentrations;        1
# 3:  7126 C7                   19                     8                              3                   7                                                    rescreen at lower concentrations;        1
# 4:  7126 C9                   19                     6                              3                   7                                                    rescreen at lower concentrations;        1
# 5:  7126 C1                   19                    17                              0                  15                                                      several endpoints appear noisy;        1
# 6:  7126 C4                   19                     8                              0                  13                                                      several endpoints appear noisy;        1
# 7:  7126 C3                   19                     5                              0                  10                                                      several endpoints appear noisy;        1
# 8:  7126 C6                   19                    18                              0                   8                                                                                             0
# 9:  7126 C2                   19                    14                              0                   7                                                                                             0
# 10:  7126 C5                   19                    18                              0                   7                                                                                             0
# 11: 7126 C10                   19                    17                              0                   4                                                                                             0
# 12:  7126 C8                   19                    18                              0                   4                                                                                             0
# So currently 7/12 of the spids are already slotted to be rested for various reasons
# The other 5 - these are some of the top spids with the most noisy endpoints

# How many other spid have this many noisy endpoints?
chem.info[order(-num_noisy_endpoints), .(spid, num_noisy_endpoints, spid %in% spid.possibly.rescreen)][num_noisy_endpoints > 0]
# There is only 1 other spid htat has a comparable number of noisy endpoints
# (9163 B6, with 4 noisy endpoints. This spid also has 14/19 predicted hits!)

# On that point - these 5 additional spid to rescreen all have 14-18 hits...

# So the activity towards the top is generally believable

# Let's comparse some dose response curves visually


# Compare dose-response with less noisy samples ---------------------------

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


# How do RMSE %iles for these spid compare to others?
mc5[, spid_from_low_plates := as.numeric(spid %in% spid.possibly.rescreen)]

ggplot(mc5, aes(x = rmse_percentile)) +
  geom_histogram(aes(fill = as.factor(spid_from_low_plates)), position = 'identity', alpha = 0.5)


# something else to consider - part of the chemical effect might be to increase the noise.

# If I'm going to make this recommendation based on the noise, should I include 9163 B6 as well?

plot.spids <- '9163 B6'
pdf(file = 'DNT_NTP2021/check_for_chem_to_rescreen/figs/DNT_NTP2021_tcplfit2_9163_B6.pdf',
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


# I believe this was the culprit plate:  "20220330_MW78-7118"
# Looks like everything above the second conc was cytotoxic
# I'm guessing that was just a fluke
# This is probably the main source of the noise

# So I think the key question is - would a researcher looking at this have enough
# evidence to support a decision regarding the toxicity of this substance, given 1 of the 3 replicates being cytotoxic (and I'm guessing mistakenly?)

# Maybe run this by Tim?

# Basically, it feels weird to recommend rescreening the extra 5 based on the combined factor of the plate being off AND 
# several endpoints being noisy
# When there is another sample that has a similar level of noise that is on another plate.
# Well, we you say it that way, it sounds reasonably justified.

# Let's go with it


# Recommend retest for noisy samples on low activity plates  --------------

plates.with.controls.low.activity <- bval.tb[acnm %in% c('CCTE_Shafer_MEA_dev_active_electrodes_number',
                                                'CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12',
                                                'CCTE_Shafer_MEA_dev_firing_rate_mean',
                                                'CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12') & bval < normal_bval_lb & DNT_NTP2021 == 1, unique(apid)]
spid.on.plates.with.low.acitvity <- mc0[apid %in% plates.with.controls.low.activity & wllt == 't', unique(spid)]

chem.info[spid %in% spid.on.plates.with.low.acitvity & rescreen != 1]
# spid num_assay_components num_hitcall_above_0.5 num_ac50_below_minconc_and_hit num_noisy_endpoints wllq_notes rescreen_note rescreen
# 1:  7126 C6                   19                    18                              0                   8                                 0
# 2:  7126 C2                   19                    14                              0                   7                                 0
# 3:  7126 C5                   19                    18                              0                   7                                 0
# 4: 7126 C10                   19                    17                              0                   4                                 0
# 5:  7126 C8                   19                    18                              0                   4                                 0

chem.info[spid %in% spid.on.plates.with.low.acitvity & num_noisy_endpoints >= 4, 
          `:=`(rescreen = 1,
               rescreen_note = paste0(rescreen_note, 'Sample has >= 4 noisy endpoints and platewise median of controls < 2SD below mean of all NFA data for key general activity endpoints;'))]

definitions.tb <- data.table(column = c('spid' ,'num_assay_components' ,'num_hitcall_above_0.5' ,'num_ac50_below_minconc_and_hit' ,'num_noisy_endpoints' ,'wllq_notes','rescreen','rescreen_note'),
                             definition = c('Sample ID','Total number of assay component (2 cytotoxicity, plus 17 AUC endpoints fit bidirectionally)',
                                            'Number of assay components with tcplfit2 hit call >= 50% (tcplfit2 assigns continuous hit call estimates)',
                                            'Number of assay components with hit call >= 50% and AC50 < lowest conc tested',
                                            'Number of assay components with winning model RMSE above the cutoff and the 95%-ile for the given assay component',
                                            'Well quality notes added by lab technicians',
                                            'Amy\'s recommendation to rescreen (1 = yes, 0 = no)',
                                            'Motivation to potentially rescreen'))


wb <- createWorkbook()
addWorksheet(wb, sheet = 'definitions')
writeData(wb, sheet = 1, definitions.tb)
addWorksheet(wb, sheet = 'tcplfit2 summary')
writeData(wb, sheet = 2, chem.info)
saveWorkbook(wb, file = 'DNT_NTP2021/check_for_chem_to_rescreen/DNT_NTP2021_MEA_NFA_rescreen_recommendations_2022-05-24.xlsx',
             overwrite = T)
