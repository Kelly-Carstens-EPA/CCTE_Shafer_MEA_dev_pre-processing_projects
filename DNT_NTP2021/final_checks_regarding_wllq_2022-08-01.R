# ------------------------------------------------------------------------ #
# Just some final checks to confirm that my wllq logic aligns with the data
# August 1, 2022
# ------------------------------------------------------------------------ #

library(data.table)
library(openxslx)

# Load data
load('DNT_NTP2021/output/DNT_NTP2021_longfile.RData')
cat(description)
# DNTP MEA NFA prepared data
# Date Ran: 2022-08-01


# Confirm counts of observations of precipitate
dat[, culture_rep_id := frank(culture_date, ties.method = 'dense'), by = .(DNTP_blind_code)]
pcpt.trts <- dat[precipitate_observed == 1, unique(treatment)]
dat[treatment %in% pcpt.trts, .(max_conc = max(conc),
                                max_wllq = max(wllq)), by = .(DNTP_blind_code, treatment, culture_rep_id)][order(treatment)]
#    DNTP_blind_code treatment culture_rep_id max_conc max_wllq
# 1:            2223  7126 A11              1   99.900        1
# 2:            1852  7126 A12              1  100.000        0
# 3:            1852  7126 A12              2    0.100        1
# 4:            4285   7126 A3              1   50.000        0
# 5:            4285   7126 A3              2   50.000        1
# 6:            3107  7126 B11              1  101.000        0
# 7:            3107  7126 B11              2    0.101        1
# 8:            4446   7126 B2              1  100.000        1
# 9:            4863   7126 E1              1  100.000        1
# 10:            4825   7126 E2              1  100.000        1
# 11:            4894   7126 E3              1  100.000        1
# 12:            4269   7126 E7              1  100.000        1
# 13:            4105   7126 E8              1  100.000        1


dat[treatment %in% pcpt.trts, .(max_conc = max(conc),
                                max_wllq = max(wllq)), by = .(DNTP_blind_code, treatment, culture_rep_id, wllq_notes)][order(treatment)]



# 3 Any othe rwllq decisions not addressed?
View(dat[wllq == 0, .N, by = .(wllq_notes)][order(wllq_notes)])

dat[wllq_notes == 'Hot water shut off in building' & wllq == 0, .N, by = .(treatment, DNTP_blind_code, culture_date)]
# treatment DNTP_blind_code culture_date   N
# 1:   7126 D2            2506     20211124 522
# 2:   7126 D4            3990     20211124 522
# 3:   7126 D5            1605     20211124 522
# 4:   7126 D6            1776     20211124 522
# 5:   7126 D9            3829     20211124 520
# I'm not sure what's going for for htese
dat[wllq_notes == 'Hot water shut off in building' & wllq == 0, .N, by = .(wllq_notes_merged)]
# wllq_notes_merged    N
# 1:                                              Hot water shut off in building; precipitates in media, but not when in 37 C incubator   34
# 2:                                            Hot water shut off in building; ; precipitates in media, but not when in 37 C incubator 1530
# 3:   Hot water shut off in building; precipitates in media, but not when in 37 C incubator, will be repeated on a lower concentration   24
# 4: Hot water shut off in building; ; precipitates in media, but not when in 37 C incubator, will be repeated on a lower concentration 1020
# okay, so all were observed to precipitate... but i removed this note bc Tim said not relevant.

dat[wllq_notes == 'Hot water shut off in building' & wllq == 0, .N, by = .(acsn)]
# all endpoints... is this 1 particular plate?
dat[wllq_notes == 'Hot water shut off in building' & wllq == 0, .N, by = .(apid)]
# apid   N
# 1: 20211124_MW78-6214 696
# 2: 20211124_MW78-6215 696
# 3: 20211124_MW78-6216 696
# 4: 20211124_MW78-6218 174
# 5: 20211124_MW78-6219 174
# 6: 20211124_MW78-6217 172
# nope, 6 plates.

# any indication in the previuos notes?
dat[wllq_notes == 'Hot water shut off in building' & wllq == 0, .N, by = .(wllq_notes_by_trt, wllq_notes_by_well)]
# nope, same

dat[wllq_notes == 'Hot water shut off in building' & wllq == 0, .N, by = .(wllq_merged, wllq_by_trt, wllq_by_well)]
# wllq_merged wllq_by_trt wllq_by_well    N
# 1:    evaluate    evaluate            1 2608
# so the wllq must have been set to 0 in the run me script some where along
# ah, got it! set wllq to 1 instead of '1', I think that did it

# Any other cases of concern?
View(dat[wllq == 0, .N, by = .(wllq_notes)][order(wllq_notes)])
View(dat[wllq == 1, .N, by = .(wllq_notes)][order(wllq_notes)])


# Re-check after made updates to run_me, re-ran, got new data -------------

rm(list = ls())

# Load data
load('DNT_NTP2021/output/DNT_NTP2021_longfile.RData')
cat(description)
# DNTP MEA NFA prepared data
# Date Ran: 2022-08-01

# Confirm counts of observations of precipitate
dat[, culture_rep_id := frank(culture_date, ties.method = 'dense'), by = .(DNTP_blind_code)]
pcpt.trts <- dat[precipitate_observed == 1, unique(treatment)]
dat[treatment %in% pcpt.trts, .(max_conc = max(conc),
                                max_wllq = max(wllq)), by = .(DNTP_blind_code, treatment, culture_rep_id)][order(treatment)]
# seems to be same as before'
pcpt.trts <- dat[precipitate_observed == 1, unique(treatment)]
dat[treatment %in% pcpt.trts, .(max_conc = max(conc),
                                max_wllq = max(wllq)), by = .(DNTP_blind_code, treatment, culture_rep_id, wllq_notes)][order(treatment)]
# yep, this lines up with table!

# 3 Any othe rwllq decisions not addressed?
View(dat[wllq == 0, .N, by = .(wllq_notes)][order(wllq_notes)])
# all of these makes sense, and i think I've either explained these or they are self-explanatory

View(dat[wllq == 1, .N, by = .(wllq_notes)][order(wllq_notes)])

# This all looks okay!

# Alignment with rescreen reocmmendation table...
rec <- as.data.table(read.xlsx('DNT_NTP2021/check_for_chem_to_rescreen/DNT_NTP2021_MEA_NFA_rescreen_recommendations_2022-05-26.xlsx', sheet = 'tcplfit2 summary'))
rec[spid %in% pcpt.trts, .(spid, rescreen, rescreen_note)][order(spid)]
# spid rescreen                                                                                                                                                     rescreen_note
# 1: 7126 A11        0                                                                                                                                                                  
# 2: 7126 A12        1                                                                                                                                 rescreen at lower concentrations;
# 3:  7126 A3        0                                                                                                                                                                  
# 4: 7126 B11        1                                                                                                                                 rescreen at lower concentrations;
# 5:  7126 B2 optional Chemical observed to precipitate. If a different dilution scheme would help, could try that. Apparent cytotoxicity may be partially due to precipitate formation.
# 6:  7126 E1        0                                                                                                                                                                  
# 7:  7126 E2        0                                                                                                                                                                  
# 8:  7126 E3        0                                                                                                                                                                  
# 9:  7126 E7        0                                                                                                                                                                  
# 10:  7126 E8        0                                                                                                                                                                  
# oaky, so a lot ofthe rescreening had nothing to do with the rescreen recommendaitons workflow