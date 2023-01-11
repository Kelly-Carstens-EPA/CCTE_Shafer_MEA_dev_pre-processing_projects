# ------------------------------------------------------------------------ #
# using tcplfit2 results to determine which treatments appear noisy or very potent
# To select best available culture for each treatment.
# 
# 
# August 1, 2022
# carpenter.amy@epa.gov
# ------------------------------------------------------------------------ # 

library(data.table)
library(ggplot2)
library(tcplfit2)
library(openxlsx)
library(stringi)

# load data ---------------------------------------------------------------

load('DNT_NTP2021/check_for_chem_to_rescreen/data/DNT_NTP2021_tcplfit2_exploratory_ac50_estimates_2022-08-01.RData')
cat(description)
# Preliminary tcplfit2 output and AC50 estimates for DNT NTP 2021 data.
# For AUC up and LDH, AB endpoints only.
# Created with DNT_NTP_2021_run_tcplfit2_2022-08-01.R

load('DNT_NTP2021/output/DNT_NTP2021_longfile.RData')
cat(description)
# DNTP MEA NFA prepared data
# Date Ran: 2022-08-01


# Note where AC50 is below lowest conc tested -------------------------------

# Note that if gnls was the model winner and so the curve has an ac50_loss value,
# the ac50 is always more potent than the ac50_loss
mc5[, .N, by = .(gnls_model_won = fit_method == 'gnls',
                 ac50 < ac50_loss)]
# gnls_model_won ac50    N
# 1:          FALSE   NA 2197
# 2:           TRUE TRUE   26
mc5[, ac50_below_minconc := as.numeric(ac50 < 10^logc_min)]
mc5[, ac50_below_minconc_and_hit := as.numeric(hitcall >= 0.5 & ac50 < 10^logc_min)]

mc5[ac50_below_minconc_and_hit == 1, .N, by = .(spid)]
#       spid N
# 1: 7126 H8 2
# 2: 7126 H9 1
# 3: 7126 G2 4


# Note where noise is relatively high -------------------------------------

# Let's try noting where the noise is in the upper say 95%-ile for a given endpoint AND noise > coff
mc5[, upper_95th_rmse_by_aenm := quantile(rmse, probs = 0.95), by = .(aenm)]
mc5[, exceptional_noise := as.numeric(rmse > coff & rmse > upper_95th_rmse_by_aenm)]


# Save info in table -------------------------------------------

sample.info.tb <- mc5[, .(num_assay_components = .N,
                          num_hitcall_above_0.5 = sum(hitcall >= 0.5),
                          num_ac50_below_minconc_and_hit = sum(ac50_below_minconc_and_hit),
                          num_noisy_endpoints = sum(exceptional_noise)), by = .(spid)]
sample.info.tb <- sample.info.tb[order(-num_ac50_below_minconc_and_hit, -num_noisy_endpoints)]

# Merge in wllq info from Seline's notes
# (only concerned about notes seline added that did not result in outright removal of data points right now)
# wllq.notes.tb <- dat[wllq == 1 | grepl('precipitate',wllq_notes), .(wllq_notes = paste0(sort(unique(wllq_notes)),collapse = ";")), by = .(spid = treatment)]

# sample.info.tb <- merge(sample.info.tb, wllq.notes.tb, by = 'spid', all = T)
# sample.info.tb <- sample.info.tb[order(-num_ac50_below_minconc_and_hit, -num_noisy_endpoints, -wllq_notes)]
sample.info.tb <- sample.info.tb[order(-num_ac50_below_minconc_and_hit, -num_noisy_endpoints)]

View(sample.info.tb)


# Apply criteria discussed with Tim (pre 05/26/202) ---------------------------------------

# Intialize columns
sample.info.tb[, rescreen := 0]
sample.info.tb[, rescreen_note := '']

# Where 3 or more endpoints have an estimated AC50 < min conc tested and hitcall > 50% AND the predicted AC50 is < min conc tested for at least 3/8 (37.5%) of the predicted hits,
# rescreen those susbstances at lower concentrations
sample.info.tb[num_ac50_below_minconc_and_hit >= 3 & num_ac50_below_minconc_and_hit / num_hitcall_above_0.5 >= 3/8, 
               `:=`(rescreen = 1,
                    rescreen_note = paste0(rescreen_note, 'rescreen at lower concentrations;'))]

# Where 10 or more of the endpoints are flagged as being noisy
# (based on RMSE > cutoff and RMSE > 95%ile for that endpoint), rescreen
sample.info.tb[num_noisy_endpoints >= 10, 
               `:=`(rescreen = 1,
                    rescreen_note = paste0(rescreen_note, 'several endpoints appear noisy;'))]

# View results
sample.info.tb[, .N, by = .(rescreen, rescreen_note)]
#    rescreen rescreen_note   N
# 1:        0               117


# Confirm no apid have exceptionally low activity --------------------------------------

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
rm(mea_nfa_lvl0)
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
# 1:           0 39
# 2:           1  6
# some of DNT NTP2021's plates are affected, though there are plenty of instances 
# from other data sets as well

# Where above normal? Though I thin kthis is less concerning though
bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval > normal_bval_ub, .(length(unique(apid))), by = .(DNT_NTP2021)]
# DNT_NTP2021  V1
# 1:           0 120
# 2:           1  44

# Which apid are affected? (Just looking at AUC and DIV12 endpoints)
bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval < normal_bval_lb & DNT_NTP2021 == 1, .N, by = .(culture_date, apid)][order(N)]
#    culture_date               apid N
# 1:   2021-12-08 20211208_MW78-6304 1
# 2:   2021-12-08 20211208_MW78-6305 1
# 3:   2022-04-13 20220413_MW78-7203 1
# 4:   2022-06-01 20220601_MW78-7214 2
# 5:   2022-06-01 20220601_MW78-7216 2
# 6:   2022-06-01 20220601_MW78-7215 4
# Where just 1-3 endpoints are a bit low is probably less concnering
# but let's see specific endpoints
bval.tb[(!grepl('DIV',acnm) | grepl('DIV12',acnm)) & bval < normal_bval_lb & DNT_NTP2021 == 1, .(culture_date, apid, acnm)][order(apid)]
# culture_date               apid                                                   acnm
# 1:   2021-12-08 20211208_MW78-6304      CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12
# 2:   2021-12-08 20211208_MW78-6305      CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12
# 3:   2022-04-13 20220413_MW78-7203      CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12
# 4:   2022-06-01 20220601_MW78-7214   CCTE_Shafer_MEA_dev_bursting_electrodes_number_DIV12
# 5:   2022-06-01 20220601_MW78-7214     CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12
# 6:   2022-06-01 20220601_MW78-7215   CCTE_Shafer_MEA_dev_bursting_electrodes_number_DIV12
# 7:   2022-06-01 20220601_MW78-7215     CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12
# 8:   2022-06-01 20220601_MW78-7215           CCTE_Shafer_MEA_dev_network_spike_peak_DIV12
# 9:   2022-06-01 20220601_MW78-7215 CCTE_Shafer_MEA_dev_correlation_coefficient_mean_DIV12
# 10:   2022-06-01 20220601_MW78-7216   CCTE_Shafer_MEA_dev_bursting_electrodes_number_DIV12
# 11:   2022-06-01 20220601_MW78-7216      CCTE_Shafer_MEA_dev_per_burst_spike_percent_DIV12
# all DIV12 endpoints... I'm less interested in these
# I think it's #goodenough



# Save some plots ---------------------------------------------------------

# I'm curious about the chem that still have some potent hits,
# have a few noisy endpoitns,
# and just want to view a few others

plot_dose_response <- function(plot.spids) {
  
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
  
}

# Adding some additional columsn for concRespPlot
mc5[, assay := aenm]
mc5[, name := spid]


plot.spids <- c(sample.info.tb[num_ac50_below_minconc_and_hit > 0 | num_noisy_endpoints >= 5, spid],
                '7126 D5','7126 H10','7126 C11', # some that were originally found to be potent, then rescreened at lower conc
                '7126 D7','7126 D6', # some that were originally foudn to be very noisy
                'Bisphenol A')
pdf(file = 'DNT_NTP2021/check_for_chem_to_rescreen/figs/DNT_NTP2021_tcplfit2_select_treatments_2022-09-01.pdf',
    width = 8, height = 10)
plot_dose_response(plot.spids)
graphics.off()

# Wait, why are only 4 conc's showing up for 7126 C11?
dat[treatment == '7126 C11' & wllq == 1, .N, by = .(cndx, conc)]
# cndx        conc   N
# 1:    1 0.000050015 260
# 2:    2 0.000150045 259
# 3:    3 0.000500150 260
# 4:    4 0.001500450 260
# 5:    5 0.005001500 260
# 6:    6 0.015004500 260
# 7:    7 0.050015000 260
# we should have 7
# -> ah, I think the conc range on the plots is fixed,
# so some conc's just going below what can be shown
# But still clearlygets a lot of hits!

# Save recommendations ----------------------------------------------------

definitions.tb <- data.table(column = c('spid' ,'num_assay_components' ,'num_hitcall_above_0.5' ,'num_ac50_below_minconc_and_hit' ,'num_noisy_endpoints' ,'wllq_notes','precipitate_noted','rescreen','rescreen_note'),
                             definition = c('Sample ID','Total number of assay component (2 cytotoxicity, plus 17 AUC endpoints fit bidirectionally)',
                                            'Number of assay components with tcplfit2 hit call >= 50% (tcplfit2 assigns continuous hit call estimates)',
                                            'Number of assay components with hit call >= 50% and AC50 < lowest conc tested',
                                            'Number of assay components with winning model RMSE above the cutoff and the 95%-ile for the given assay component',
                                            'Well quality notes added by lab technicians',
                                            'Was precipitate observed and noted in a README file?',
                                            'Amy\'s recommendation to rescreen (1 = yes, 0 = no)',
                                            'Motivation to potentially rescreen'))


wb <- createWorkbook()
addWorksheet(wb, sheet = 'definitions')
writeData(wb, sheet = 1, definitions.tb)
addWorksheet(wb, sheet = 'tcplfit2 summary')
writeData(wb, sheet = 2, sample.info.tb)
saveWorkbook(wb, file = 'DNT_NTP2021/check_for_chem_to_rescreen/DNT_NTP2021_MEA_NFA_rescreen_recommendations_2022-08-01.xlsx',
             overwrite = T)




# Confirm all treatments covered, expected # of cultures ------------------

# Load spidmap for all treatments
spidmap_file <- "Coded Plate Map - Plate No. 4001207126 DNT HEI EPA SHIP CHEM14214 MRI3110.xlsx" # (FOLDER PATH REDACTED)
spid_sheet <- "Chemical List"
spidmap1 <- as.data.table(read.xlsx(spidmap_file, sheet = spid_sheet))
spidmap2 <- as.data.table(read.xlsx('Coded Plate Map - Plate No. 7000439163 DNT HEI EPA SHIP CHEM14214 MRI3110.xlsx', sheet = spid_sheet)) # (FOLDER PATH REDACTED)

all.treatments <- c(spidmap1$`Blind-Code`, spidmap2$`Blind-Code`)
all.treatments <- stri_extract(all.treatments, regex = '[0-9A-Za-z]*') # extra the nongreek symbols

# All treatments present?
setdiff(all.treatments, dat[wllq == 1, DNTP_blind_code])
# "Note" "Vial" ""     "1" 
# (just some footnote stuff from the spidmap)
# So all actual treatments are present!!

# Which treatments have multiple cultures? Is this what I want?
dat[, num_usable_cultures := length(unique(culture_date[wllq == 1])), by = .(treatment)]
dat[num_usable_cultures > 1, .N, by = .(treatment, wllt, num_usable_cultures)]
#        treatment wllt num_usable_cultures     N
# 1:       7126 D4    t                   2  3654
# 2:       7126 D5    t                   2  3654
# 3:       7126 D7    t                   2  3654
# 4:       7126 F1    t                   2  3654
# 5:       7126 F8    t                   2  3654
# 6:      7126 H10    t                   2  3654
# 7: Acetaminophen    t                   2  3654
# 8:   Bisphenol A    t                   2  3654
# 9:          DMSO    n                  12 36018
# 10:         Water    n                   2  1566

check.trts <- dat[num_usable_cultures > 1, .N, by = .(treatment, wllt, num_usable_cultures)][wllt == 't', treatment]
dat[treatment %in% check.trts, .(min_conc = min(conc), max_conc = max(conc)), by = .(treatment, culture_date, wllq_notes)][order(treatment, culture_date)]
# cool, all of these have multiple culture dates because they were retested at a lower concentration
# and right now I'm keeping the data from both cultures, unless there was another wllq note. Sweet!
