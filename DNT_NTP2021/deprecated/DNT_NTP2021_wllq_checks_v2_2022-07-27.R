library(data.table)
library(stringi)
library(ggplot2)
library(openxlsx)

# Load data
dataset_title <- 'DNT_NTP2021'
load(file.path(dataset_title,'output','DNT_NTP2021_longfile.RData'))
cat(description)
# DNTP MEA NFA prepared data
# Date Ran: July 26 2022


# Confirm intended repeats performed --------------------------------------

dat[, repeated_samples := as.numeric(length(unique(culture_date))>1), by = .(treatment, DNTP_blind_code)]

repeats.tb <- as.data.table(read.xlsx('L:/Lab/NHEERL_MEA/Project - DNT_NTP_2021/DNT_NTP2021_MEA_NFA_rescreen_recommendations_2022-05-26.xlsx', sheet = 'tcplfit2 summary'))
repeats.tb[, .N, by = .(rescreen)]
# rescreen  N
# 1:        1 20
# 2:        0 95
# 3: optional  2
setnames(repeats.tb, old = 'spid', new = 'treatment')

# Merge
setdiff(repeats.tb$treatment, dat$treatment) # "Acetamenophin"
repeats.tb[treatment == "Acetamenophin", treatment := "Acetaminophen"]
dat <- merge(dat, repeats.tb[, .(treatment, rescreen, rescreen_note)], by = 'treatment', all.x = 'T')
dat[, .(num_treatments = length(unique(treatment))), by = .(rescreen, repeated_samples)]
#    rescreen repeated_samples num_treatments
# 1:        0                0             91
# 2:        1                1             20
# 3:        0                1              4
# 4: optional                0              2
# 5:     <NA>                1              2

# first 2 rows are in agreement

# 4 samples were repeated that I had not suggested to repeat
dat[rescreen == 0 & repeated_samples == 1, .N, by = .(treatment)]
# treatment     N
# 1:       7126 A3  3654
# 2: Acetaminophen  3654
# 3:   Bisphenol A  3654
# 4:          DMSO 36018
# assay controls make sense
# but what about the top one/
dat[treatment == '7126 A3', .N, by = .(culture, wllq, wllq_notes)]
#     culture     wllq                                                             wllq_notes  source_wllq_notes_by_trt    N
# 1: 20210915 evaluate ; dropped out of solution, precipitates in media (need to be repeated) lab notebook, readme note 1827
# 2: 20220330        1                                                                   ; NA                      <NA> 1827
# ah, okay, makes sense!!

# Looks like the 2 optional-to-repeat samples were not repeated

# The cases for which teh treatments were not in rescreen.tb
dat[is.na(rescreen), .N, by = .(treatment)]
#        treatment    N
# 1:       7126 H7 3654
# 2:         Water 1566
# water makes sense... but what's going on with the other one?
dat[treatment == '7126 H7', .N, by = .(culture_date, wllq, wllq_notes)]
# culture_date wllq                                      wllq_notes    N
# 1:     20220316    0 ; Need to repeat. Got wrong chemical on 3/21/22 1827
# 2:     20220615    1                                            ; NA 1827
# oh.... got it.


# Conclusions;
# all of the samples that I suggested that we repeat in DNT_NTP2021_MEA_NFA_rescreen_recommendations_2022-05-26.xlsx
# were rescreened.
# The 2 smaples that I said were "optional" to repeat were not repeated.
# The 2 assay controls (BPA and Acetaminophen) were repeated.
# 1 sample was repeated because precipitate was observed in the first culture.
# another sample was repeated because the wrong substance was used in the first culture.



# Review substances with precipitate individually -------------------------

# Note that for 20220330_NFA_DNT 2021_Group 19: In lab notebook, was noted that no precipitate was observed
# Just confirmed that all observations off precipitate noted in lab notebook or README notes have been entered in the well_quality_nots_per_culture_treatment_cndx.xlsx

# Clean wllq_notes
dat[, wllq_notes := sub('; ;',';',wllq_notes)]
dat[, wllq_notes := sub('^;[ ]*','',wllq_notes)]
dat[, wllq_notes := sub(';[ ]*$','',wllq_notes)]

# Let's just do this case by case
treatments.sometimes.precipitate <- sort(dat[precipitate_observed == 1, unique(treatment)])
# cat omething to initialize text for review
cat(paste0('# ',treatments.sometimes.precipitate,'\ndat[treatment == "',treatments.sometimes.precipitate,'", .N, by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]'), sep = '\n\n')


# 7126 A11
dat[treatment == "7126 A11", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
#    DNTP_blind_code treatment  culture        wllq              wllq_notes cndxs_affected
# 1:            2223  7126 A11 20210915    evaluate dropped out of solution  1,2,3,4,5,6,7
# So this treatment dropped out of solution, but was not repeated
# will set wllq == 1, but add note to Laura
dat[treatment == "7126 A11", wllq := '1']
dat[treatment == "7126 A11" & grepl('dropped out of solution',wllq_notes), manual_review_note := 'dropped out of solution, but was not repeated']

# 7126 A12
dat[treatment == "7126 A12", .N, by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
#    DNTP_blind_code treatment  culture     wllq                                                                                   wllq_notes    N
# 1:            1852  7126 A12 20210915 evaluate ; dropped out of solution, precipitates in media, need to be repeated on lower concentration 1827
# 2:            1852  7126 A12 20220601        1                                                                                         ; NA 1827
# This sample was repeated, presumably with a different dosing scheme. Will only keep repeats
dat[treatment == "7126 A12" & culture == '20210915', wllq := '0']

# 7126 A3
dat[treatment == "7126 A3", .N, by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# DNTP_blind_code treatment  culture     wllq                                                             wllq_notes    N
# 1:            4285   7126 A3 20210915 evaluate ; dropped out of solution, precipitates in media (need to be repeated) 1827
# 2:            4285   7126 A3 20220330        1                                                                   ; NA 1827
# same as above
dat[treatment == "7126 A3" & culture == '20210915', wllq := '0']

# 7126 B11
dat[treatment == "7126 B11", .N, by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
#    DNTP_blind_code treatment  culture     wllq                                                                                  wllq_notes    N
# 1:            3107  7126 B11 20210929 evaluate   Hot water shut off in building; precipitates in media. Different dilution approached used   42
# 2:            3107  7126 B11 20210929 evaluate Hot water shut off in building; ; precipitates in media. Different dilution approached used 1785
# 3:            3107  7126 B11 20220601        1                                                                                        ; NA 1820
# 4:            3107  7126 B11 20220601        0                                                   LDH data not collected for this plate; NA    7
# same as above
dat[treatment == "7126 B11" & culture == '20210929', wllq := '0']

# 7126 B2
dat[treatment == "7126 B2", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
#   DNTP_blind_code treatment  culture         wllq                                                                                wllq_notes cndxs_affected
# 1:            4446   7126 B2 20210929    evaluate Hot water shut off in building; precipitates in media. Different dilution approached used  1,2,3,4,5,6,7
# only 1 culture tested, so we have to use this data
dat[treatment == "7126 B2" & wllq == 'evaluate', wllq := '1']
dat[treatment == "7126 B2" & grepl('precipitate',wllq_notes), manual_review_note := 'precipitates in media, but was not repeated']

# 7126 C1
dat[treatment == "7126 C1", .N, by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# Looks like the first culture had both precipitate and was noisy, so wllq is already 0
# precipitate not observedin re-run
# no updated to wllq is needed

# 7126 C2
dat[treatment == "7126 C2", .N, by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
dat[treatment == "7126 C2", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# Looks like the first culture had both precipitate (for a few  conc's) but was also noisy, so wllq is already 0
# precipitate not observedin re-run
# no updated to wllq is needed

# 7126 C5
dat[treatment == "7126 C5", .N, by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
dat[treatment == "7126 C5", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# Looks like the first culture had both precipitate (for a few  conc's) but was also noisy, so wllq is already 0
# precipitate not observedin re-run
# no updated to wllq is needed

# 7126 C6
dat[treatment == "7126 C6", .N, by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
dat[treatment == "7126 C6", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# Looks like the first culture had both precipitate (for a few  conc's) but was also noisy, so wllq is already 0
# precipitate not observedin re-run
# no updated to wllq is needed

# 7126 D2
dat[treatment == "7126 D2", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# DNTP_blind_code treatment  culture     wllq                                                                            wllq_notes cndxs_affected
# 1:            2506   7126 D2 20211124        1                                                    Hot water shut off in building; NA      1,2,3,4,5
# 2:            2506   7126 D2 20211124 evaluate Hot water shut off in building; precipitates in media, but not when in 37 C incubator            6,7
# only 1 culture tested, so we have to use this data
dat[treatment == "7126 D2" & wllq == 'evaluate', wllq := '1']
dat[treatment == "7126 D2" & grepl('precipitate',wllq_notes), manual_review_note := 'precipitates in media, but was not repeated']

# 7126 D4
dat[treatment == "7126 D4", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
#    DNTP_blind_code treatment  culture     wllq                                                                                                                       wllq_notes cndxs_affected
# 1:            3990   7126 D4 20211124        1                                                        Hot water shut off in building; will be repeated on a lower concentration      1,2,3,4,5
# 2:            3990   7126 D4 20211124 evaluate Hot water shut off in building; precipitates in media, but not when in 37 C incubator, will be repeated on a lower concentration            6,7
# 3:            3990   7126 D4 20220601        1                                                                                                                               NA  1,2,3,4,5,6,7
# 4:            3990   7126 D4 20220601        0                                                                                        LDH data not collected for this plate; NA  1,2,3,4,5,6,7
# Since i'm not setting the wllq to 0 for any other cases where "precipitates in media, but not when in 37 C incubator" for just some concentrations,
# I'm not going to set wllq to 0 here either.
# Regardless, this treatment was repeated at a lower concentration in the second culture, so we should still be able to get a good dose-response curve in that region.
dat[treatment == "7126 D4", .N, by = .(conc, culture_date)][order(conc)]
dat[treatment == "7126 D4" & wllq == 'evaluate', wllq := '1']

# 7126 D5
dat[treatment == "7126 D5", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
#    DNTP_blind_code treatment  culture     wllq                                                                                                                       wllq_notes cndxs_affected
# 1:            1605   7126 D5 20211124        1                                                        Hot water shut off in building; will be repeated on a lower concentration      1,2,3,4,5
# 2:            1605   7126 D5 20211124 evaluate Hot water shut off in building; precipitates in media, but not when in 37 C incubator, will be repeated on a lower concentration            6,7
# 3:            1605   7126 D5 20220601        1                                                                                                                               NA  1,2,3,4,5,6,7
# 4:            1605   7126 D5 20220601        0                                                                                        LDH data not collected for this plate; NA  1,2,3,4,5,6,7
# same as above
dat[treatment == "7126 D5", .N, by = .(conc, culture_date)][order(conc)]
dat[treatment == "7126 D5" & wllq == 'evaluate', wllq := '1']

# 7126 D6
dat[treatment == "7126 D6", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
#    DNTP_blind_code treatment  culture     wllq                                                                            wllq_notes cndxs_affected
# 1:            1776   7126 D6 20211124        1                                                    Hot water shut off in building; NA      1,2,3,4,5
# 2:            1776   7126 D6 20211124 evaluate Hot water shut off in building; precipitates in media, but not when in 37 C incubator            6,7
# only 1 culture tested, so we have to use this data
dat[treatment == "7126 D6" & wllq == 'evaluate', wllq := '1']
dat[treatment == "7126 D6" & grepl('precipitate',wllq_notes), manual_review_note := 'precipitates in media, but was not repeated']

# 7126 D9
dat[treatment == "7126 D9", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
#    DNTP_blind_code treatment  culture     wllq                                                                                                                       wllq_notes cndxs_affected
# 1:            3829   7126 D9 20211124        0                                                    CTB incubation time is less than one hour; Hot water shut off in building; NA      1,2,3,4,5
# 2:            3829   7126 D9 20211124        1                                                                                               Hot water shut off in building; NA      1,2,3,4,5
# 3:            3829   7126 D9 20211124        0 CTB incubation time is less than one hour; Hot water shut off in building; precipitates in media, but not when in 37 C incubator            6,7
# 4:            3829   7126 D9 20211124 evaluate                                            Hot water shut off in building; precipitates in media, but not when in 37 C incubator            6,7
# only 1 culture tested, have to keep
dat[treatment == "7126 D9" & wllq == 'evaluate', wllq := '1']
dat[treatment == "7126 D9" & grepl('precipitate',wllq_notes), manual_review_note := 'precipitates in media, but was not repeated']


# 7126 E1
dat[treatment == "7126 E1", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# DNTP_blind_code treatment  culture     wllq                                                                                             wllq_notes cndxs_affected
# 1:            4863   7126 E1 20211208        1                                                                     Hot water shut off in building; NA      1,2,3,4,5
# 2:            4863   7126 E1 20211208 evaluate Hot water shut off in building; precipitates in dosing plate but dissolved in well (10 in 490uL media)            6,7
# only 1 culture tested, have to keep
dat[treatment == "7126 E1" & wllq == 'evaluate', wllq := '1']
dat[treatment == "7126 E1" & grepl('precipitate',wllq_notes), manual_review_note := 'precipitates in media, but was not repeated']

# 7126 E2
dat[treatment == "7126 E2", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
#    DNTP_blind_code treatment  culture     wllq                                                                                             wllq_notes cndxs_affected
# 1:            4825   7126 E2 20211208        1                                                                     Hot water shut off in building; NA      1,2,3,4,5
# 2:            4825   7126 E2 20211208 evaluate Hot water shut off in building; precipitates in dosing plate but dissolved in well (10 in 490uL media)            6,7
# only 1 culture tested, have to keep
dat[treatment == "7126 E2" & wllq == 'evaluate', wllq := '1']
dat[treatment == "7126 E2" & grepl('precipitate',wllq_notes), manual_review_note := 'precipitates in media, but was not repeated']

# 7126 E3
dat[treatment == "7126 E3", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# DNTP_blind_code treatment  culture     wllq                                                                                                                                               wllq_notes cndxs_affected
# 1:            4894   7126 E3 20211208        1                                                                         Hot water shut off in building; may need to be repeated on a lower concentration      1,2,3,4,5
# 2:            4894   7126 E3 20211208 evaluate Hot water shut off in building; may need to be repeated on a lower concentration, precipitates in dosing plate but dissolved in well (10 in 490uL media)            6,7
# only 1 culture tested, have to keep
dat[treatment == "7126 E3" & wllq == 'evaluate', wllq := '1']
dat[treatment == "7126 E3" & grepl('precipitate',wllq_notes), manual_review_note := 'precipitates in media, but was not repeated']

# 7126 E7
dat[treatment == "7126 E7", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# DNTP_blind_code treatment  culture     wllq                                                                                             wllq_notes cndxs_affected
# 1:            4269   7126 E7 20211208        1                                                                     Hot water shut off in building; NA    1,2,3,4,5,6
# 2:            4269   7126 E7 20211208 evaluate Hot water shut off in building; precipitates in dosing plate but dissolved in well (10 in 490uL media)              7
# only 1 culture tested, have to keep
dat[treatment == "7126 E7" & wllq == 'evaluate', wllq := '1']
dat[treatment == "7126 E7" & grepl('precipitate',wllq_notes), manual_review_note := 'precipitates in media, but was not repeated']

# 7126 E8
dat[treatment == "7126 E8", .(cndxs_affected = paste0(sort(unique(cndx)),collapse=",")), by = .(DNTP_blind_code, treatment, culture, wllq, wllq_notes)]
# DNTP_blind_code treatment  culture     wllq                                                                                             wllq_notes cndxs_affected
# 1:            4105   7126 E8 20211208        1                                                                     Hot water shut off in building; NA    1,2,3,4,5,6
# 2:            4105   7126 E8 20211208 evaluate Hot water shut off in building; precipitates in dosing plate but dissolved in well (10 in 490uL media)              7
# only 1 culture tested, have to keep
dat[treatment == "7126 E8" & wllq == 'evaluate', wllq := '1']
dat[treatment == "7126 E8" & grepl('precipitate',wllq_notes), manual_review_note := 'precipitates in media, but was not repeated']


# Confirm every chemical has a usable sample
all.treatments <- dat[wllt == 't', unique(treatment)]
dat[wllq == 1, setdiff(all.treatments, treatment)]
# empty, cool



# Other wllq issues -------------------------------------------------------


## Confirm well with cell debris -------------------------------------------

# Background:
# Lab notebook indicates "Plate 1 78-7220 LDH A1, B2, D4 cell debris"
# While README note indicated that following wells shoudl be discarded: ""78-7220 A1, 78-7220 B1, 78-7220 D4"
# I defaulted set wllq to 0 for well B2 instead of B1
# confirming that looks correct

plot.culture <- dat[grepl('MW78-7220',apid), unique(culture_date)]
plotdat <- dat[culture_date %in% plot.culture & grepl("LDH",acsn)]
plotdat$wllq <- as.character(plotdat$wllq)
plotdat[grepl('MW78-7220',apid) & rowi == 2 & coli == 1, wllq := '78-7220 B1']
ggplot(plotdat, aes(x = rval, y = treatment)) +
  geom_jitter(aes(color = cndx, size = wllq), pch = 1, height = 0.15, width = 0)+
  scale_size_manual(values= c('1' = 2,
                              '0' = 4,
                              '78-7220 B1' = 6))+
  ggtitle(paste0('LDH rvals for culture ',plot.culture))

# Well 78-7220 B1 looks completely normal compared to both the other replicates for 
# I'm don't think I need to change teh wllq for that well.


# Note regarding temporary misdose of Bisphenal A in 75-9317 --------------

# From the lab notebook: "Plate 75-9317, DIV5 Dosed 7126 H11 into row E [meant to be Bisphenol A], aspirated and add fresh media, dosed with Bisphenol A."
# When a momentary misdose has happened previously for other project, it did not noticably affect the results.
# I am double checking now.
plot.culture <- dat[grepl('75\\-9317',apid), unique(culture_date)]
plotdat <- dat[culture_date %in% plot.culture & grepl("LDH",acsn)]
plotdat$wllq <- as.character(plotdat$wllq)
plotdat[, .N, by = .(wllq)]
# all 1
ggplot(plotdat, aes(x = rval, y = treatment)) +
  geom_jitter(aes(color = cndx, pch = apid), width = 0, height = 0.15)+
  scale_shape_manual(values = c("20211027_MW75-9315" =  1, # circle
                                "20211027_MW75-9316" =8, # asterisk
                                "20211027_MW75-9317" = 6 # upside down triangle
                                ))
# Points from 20211027_MW75-9317 don't appear at all atypical compared to points from other plates for Bisphenol A
# no changes to wllq are warranted.


# Any other cases to evaluate? --------------------------------------------

dat[wllq == 'evaluate', .N]
# empty, cool!
