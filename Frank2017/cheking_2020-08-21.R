# confirming what is and is not included in the published data
# for the Frank 86-compound data set
# 08/21/2020
# so that I know what to include in tcpl
library(readxl)
library(data.table)
dat <- as.data.table(read_excel("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/Published Downloaded Data Reference/Copy of kfx169_SuppTables.xlsx", sheet = "Tab7-Compound_RawValues"))
aucdat <- as.data.table(read_excel("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/Published Downloaded Data Reference/Copy of kfx169_SuppTables.xlsx", sheet = "Tab 8-Compound_AUCValues"))

# GENERAL CHECKS ----------------------------------------------------------
dat[, length(unique(date))] # 17
nrow(dat[, .N, by = c("date","Plate.SN")]) # 52 plates. 17 * 3 = 51...
dat[, length(unique(Plate.SN)), by = "date"]
dat[date == "20150805", unique(Plate.SN)] # "MW1040-11" "MW1040-12"
# I thought only AB and LDH data had to be removed... will ask
dat[date == "20151125", unique(Plate.SN)] # "MW1086-39" "MW1086-41"
# will ask about this one too

# Checking if every well has DIV 5,7,9,12 ---------------------------------
# are there 4 DIVs per well?
dat[, .(DIVs = paste0(sort(unique(DIV)),collapse=",")), by = c("date","Plate.SN","well")][DIVs != "5,7,9,12"]
# looks like all wells from 20141029  MW1008-41 are missing a DIV
dat[Plate.SN == "MW1008-41" & date == "20141029", unique(DIV)] # 12  7  9
# the DIV 5 spike list file was just in a different place/named differently, so it was not included

# 20141015 MW1007-107   A3 only has 1 DIV. Oh...
dat[Plate.SN == "MW1007-107" & well == "A3", unique(DIV)] # 12
# wow, that was not a great move. Now the AUC value will be calculated from 0 at DIV 2 to DIV 12. 
# options:
# - just don't include this well at all, manually set wllq==0
# - Leave it as only DIV 12, then burst parameter to AUC will set wllq==0 bc it won't estimate more than 1 DIV
# - Do include all DIV for this well. Might filter it out later if I do general checks, but won't filter it out manually.
aucdat[plate.SN == "MW1007-107" & well == "A3"]
aucdat[treatment == "Saccharin" & dose == 0.1]

# 20150128 MW1007-109   F8 only has 3 DIV.
dat[date == "20150128" & Plate.SN == "MW1007-109" & well == "F8", unique(DIV)] # 5 7 9
# Looking at the summary data... MFR is 0 at DIV 12, but that is also the case in teh other 2 replciates!
dat[trt == "Spiroxamine" & dose == 30 & DIV == 12, .(Plate.SN, date, trt, dose, DIV, meanfiringrate, burst.per.min, r, mi)]
# Plate.SN     date         trt dose DIV meanfiringrate burst.per.min r mi
# 1:  MW1008-1 20150128 Spiroxamine   30  12              0             0 0  0
# 2: MW1008-40 20150128 Spiroxamine   30  12              0             0 0  0
# There are NO notes in the lab notebook of concern.
# I don't see any reason why this well was removed. I am going to include it
dat[, .N, by = c("Plate.SN","well")]


# Any wells missing entirely? ---------------------------------------------------
dat[, .N, by = c("date","Plate.SN")][ N != 4*48] # 4*48 = 192
# date   Plate.SN   N
# 1: 20141015 MW1007-107 185 # 3 DIV were excluded for A3, A5 was excluded entirely, as noted -> 192 - (3 + 4) = 185
# 2: 20141015 MW1007-108 184 # all DIV were exlcued for A3 and A5, as noted -> 192 - (4+4) = 184
# 3: 20141029  MW1008-41 144 # every well is missing DIV 5 as noted above -> 192 - (48) = 144
# 4: 20150128 MW1007-109 191 # 1 DIV missing for F8, as noted -> 192 - 1 = 192
# 5: 20151125  MW1086-39  96
# 6: 20151125  MW1086-41  96
dat[date == "20151125" & Plate.SN == "MW1086-39", unique(well)]
# [1] "A1" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8" "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8"
dat[date == "20151125" & Plate.SN == "MW1086-39", unique(trt)] # "Tetrabromobisphenol A" "TPP"                   "TDCPP" 
# okay, so only 3 treatment included on from this plate.
dat[date == "20151125" & Plate.SN == "MW1086-41", unique(trt)] # same
# the third plate was not included at all from this culture.
# other compounds tested here: Emamectin benzoate from 0.0001 - 0.1,
# Saccharin 0.03 - 30, 
# Spiroxamine 0.0003 - 0.3
dat[trt %in% c("Saccharin","Spiroxamine","Emamectin benzoate"), .(paste0(sort(unique(dose)),collapse=",")), by = c("date","trt","Plate.SN")]
# I'm pretty sure I had a conversation with Kathleen about this. Will check.
# Nothign else to change for my analysis

# repeat for aucdat
aucdat[, .N, by = c("date","plate.SN")][ N != 48]
# date   plate.SN  N
# 1: 20141015 MW1007-107 47
# 2: 20141015 MW1007-108 46
# 3: 20151125  MW1086-39 24
# 4: 20151125  MW1086-41 24
# yep, totally lines up with above


# What all is present/missing for LDH ----------------------------------------------------
ldh <- as.data.table(read_excel("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/Published Downloaded Data Reference/Copy of kfx169_SuppTables.xlsx", sheet = "Tab 11-LDH_Data"))
# since the dates aren't listed, I will just have to go by if a compound included at all, and at what dose
ldh[, .N, by = c("compound","dose")][N != 1] #64 dose/trt pairs have 2 replicates
ldh_trt_reps <- ldh[, .N, by = c("compound","dose")][N != 1, unique(compound)] # 8 compounds, at 8 "conc's"
auc_trt_reps <- aucdat[dose != 0, .N, by = c("treatment","dose")][N != 3, unique(treatment)] # 19 treatments
setdiff(ldh_trt_reps, auc_trt_reps) # NA. I'm not counting number of reps, but 
cdat <- merge(ldh[, .N, by = c("compound","dose")], aucdat[, .N, by = c("treatment","dose")], by.x = c("dose","compound"), by.y = c("dose","treatment"),
      suffixes = c(".ldh",".auc"))
cdat[N.auc != 3*N.ldh][order(compound), .(doses = paste0(sort(unique(dose)),collapse=","), N.ldh = unique(N.ldh), N.auc = unique(N.auc)), by = c("compound")]
#                  compound                    doses N.ldh N.auc
# 1:         Acetaminophen 0,0.03,0.1,0.3,1,3,10,30     4    14
# 2:           Bisphenol A 0,0.03,0.1,0.3,1,3,10,30     1     6
# 3:               Cocaine 0,0.03,0.1,0.3,1,3,10,30     1     6
# 4:  Cytosine Arabinoside 0,0.03,0.1,0.3,1,3,10,30     2     9
# 5:                  DEHP 0,0.03,0.1,0.3,1,3,10,30     1     6
# 6:          Deltamethrin 0,0.03,0.1,0.3,1,3,10,30     1     6
# 7:       Hexachlorophene 0,0.03,0.1,0.3,1,3,10,30     1     2
# 8:           Hydroxyurea 0,0.03,0.1,0.3,1,3,10,30     1     2
# 9:          Methotrexate 0,0.03,0.1,0.3,1,3,10,30     1     2
# 10:             Saccharin                    0.1,1     1     2
# 11:             Saccharin                    0.1,1     1     1
# 12:                 TDCPP 0,0.03,0.1,0.3,1,3,10,30     1     5
# 13:                   TPP 0,0.03,0.1,0.3,1,3,10,30     1     5
# 14:          Tebuconazole 0,0.03,0.1,0.3,1,3,10,30     1     2
# 15: Tetrabromobisphenol A 0,0.03,0.1,0.3,1,3,10,30     2     5
# 16:           Thalidomide 0,0.03,0.1,0.3,1,3,10,30     1     2
aucdat[treatment == "Acetaminophen", unique(date)] # 5 unique culture dates 20141112 20140827 20141203 20150819 20150805
# LDH dat would have had only 4 cultures
# 20150805 -> That is where 2 of the LDH plates were off. Will confirm what to do here.
ldh[compound  =="Acetaminophen"]

aucdat[treatment %in% c("Bisphenol A","Cocaine","DEHP","Deltamethrin"), unique(date)] # 20141112 20140827 which one exlcuded from LDH?
# Confirmed in summary data file, LDH data present matches 20141112 culture
aucdat[treatment %in% c("Cytosine Arabinoside"), unique(date)] # cytosien arab tested in the 2 cultures above plus also tested on 20150909, which is included in both AUC and LDH

# TDCPP and TPP
aucdat[treatment %in% c("TDCPP","TPP"), unique(date)] # 20151125 (1 LDH plate is off in notebook) 20150128. Confirm by summary sheet, 20151125 was excluded
# what about the other compounds tested on 20151125?
aucdat[date == "20151125", unique(treatment)] # "TDCPP"                 "Tetrabromobisphenol A" "TPP". The 3 others - Spirox, Em Benzoate, and Saccharin not included
aucdat[treatment == "Tetrabromobisphenol A", unique(date)] # 20151125 20141203
# just confirmed that TBBA has 2 full cultures in ldh 
aucdat[treatment == "Tetrabromobisphenol A", unique(plate.SN), by = "date"] # only 2 plates from 20151125 included. That's right - Plate 1088-3 was not included for all AUC data. 
# But the note was for LDH! Thsi is just showing to me that what is included in the LDH data is probably not hte best measure.

# Saccharin
cdat[compound == "Saccharin"] # So none of the Saccharin wells were excluded

# "Hexachlorophene","Hydroxyurea","Tebuconazole","Thalidomide","Methotrexate'
aucdat[treatment %in% c("Hexachlorophene","Hydroxyurea","Tebuconazole","Thalidomide","Methotrexate"), unique(plate.SN), by = "date"] # just 2 plates for 1 date
ldh[(is.na(LDH1) | is.na(LDH2) | is.na(LDH3)) & !is.na(compound)] # empty. So, all 3 col's are always filled.
# so, the LDH includes all plates from 20150805, even though 2 of the 3 plates say LDH data is unuseable!
# I guess they did this because these compounds were not repeated anywhere else. But the data is unuseable!

# now I still need to check where there are less than 3 LDH col's if an individual plate was removed...

# What all is present/missing for CTB? ---------------------------------------------------
ab <- as.data.table(read_excel("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/Published Downloaded Data Reference/Copy of kfx169_SuppTables.xlsx", sheet = "Tab 10-Alamar Blue Data"))
cdat <- merge(ab[, .N, by = c("compound","dose")], aucdat[, .N, by = c("treatment","dose")], by.x = c("dose","compound"), by.y = c("dose","treatment"),
              suffixes = c(".ab",".auc"))
cdat[N.auc != 3*N.ab][order(compound), .(doses = paste0(sort(unique(dose)),collapse=","), N.ab = unique(N.ab), N.auc = unique(N.auc)), by = c("compound")]
# compound                    doses N.ab N.auc
# 1:         Acetaminophen 0,0.03,0.1,0.3,1,3,10,30    5    14
# 2:       Hexachlorophene 0,0.03,0.1,0.3,1,3,10,30    1     2
# 3:           Hydroxyurea 0,0.03,0.1,0.3,1,3,10,30    1     2
# 4:          Methotrexate 0,0.03,0.1,0.3,1,3,10,30    1     2
# 5:             Saccharin                    0.1,1    1     2
# 6:             Saccharin                    0.1,1    1     1
# 7:                 TDCPP 0,0.03,0.1,0.3,1,3,10,30    1     5
# 8:                   TPP 0,0.03,0.1,0.3,1,3,10,30    1     5
# 9:          Tebuconazole 0,0.03,0.1,0.3,1,3,10,30    1     2
# 10: Tetrabromobisphenol A 0,0.03,0.1,0.3,1,3,10,30    1     5
# 11:           Thalidomide 0,0.03,0.1,0.3,1,3,10,30    1     2

ab[(is.na(AB1) | is.na(AB2) | is.na(AB3)) & !is.na(compound)] # empty. So all 3 plate replicates are always filled

# Acetaminophen - so all 3 plates were used from all 5 cultures, even though AB data is unusable for 1 plate in 20150805
# "Hexachlorophene","Hydroxyurea","Tebuconazole","Thalidomide","Methotrexate" - as noted above, AUC dat does not include data from 1 plate in 20150805
# Saccharin - same story

# TDCPP and TPP - same story
# TBBA - only 1 culutre this time. 
ab[compound == "Tetrabromobisphenol A"]
# Confrimed AB values do not match 20151125, so these must be from 20141203



# overall, I don't trust what was and was not included in the LDH and AB data as a guide for what I should include
# It seems that they were constrained by the need to always have 3 plates and at least 1 culture for every compound,
# mroe than driven by actually data usability


# Initial General Overview of Wllq Thresholds --------------------------------
dat[DIV ==12 & meanfiringrate < 0.1, .N] # 250!
dat[DIV ==12 & meanfiringrate == 0, .N] # 243!
dat[DIV ==12 & meanfiringrate == 0, .N, by = c("date","Plate.SN")][order(N)]
dat[DIV ==12 & meanfiringrate == 0, .N, by = c("date","trt")][order(N)]

# focus on control wells
dat[DIV ==12 & dose == 0 & meanfiringrate == 0,] # only 1 case
dat[DIV ==12 & dose == 0 & meanfiringrate < 0.1,]

aucdat[dose == 0 & meanfiringrate_AUC < 1]
stripchart(meanfiringrate_AUC ~ date + plate.SN, aucdat, vertical = T, pch = 1, las = 2, cex.axis = 0.5)
stripchart(meanfiringrate_AUC ~ date, aucdat, vertical = T, pch = 1, las = 2, cex.axis = 0.8, main = "Published AUC values by culture") # woah, that's interesting
stripchart(meanfiringrate_AUC ~ date, aucdat[dose == 0], vertical = T, pch = 1, las = 2, cex.axis = 0.8) # huh, that really does not look right...

stripchart(meanfiringrate_AUC ~ date, aucdat[dose == 0], vertical = T, pch = 1, las = 2, cex.axis = 0.8, ylim = c(0,60)) # huh, that really does not look right...

# perhaps these AUC values are normalized? And the control were just reeeeally low on these 3 plates?
stripchart(meanfiringrate ~ date, dat[DIV == 12 & dose == 0], vertical = T, pch = 1, las = 2) # no, the DIV 12 values are just way higher in these 3 plates as well
stripchart(meanfiringrate ~ date, dat[DIV == 5 & dose == 0], vertical = T, pch = 1) # no, the DIV 12 values are just way higher in these 3 plates as well
stripchart(meanfiringrate ~ date, dat[DIV == 7 & dose == 0], vertical = T, pch = 1) # no, the DIV 12 values are just way higher in these 3 plates as well
stripchart(meanfiringrate ~ date, dat[DIV == 9 & dose == 0], vertical = T, pch = 1) # no, the DIV 12 values are just way higher in these 3 plates as well

# okay, let's just check if there are notes in the lab notebook about not grounding electrodes are something...
stripchart(meanfiringrate ~ date, dat[DIV == 12], vertical = T, pch = 1, las = 2)
stripchart(burst.per.min_AUC ~ date, aucdat, vertical = T, pch = 1, las = 2, cex.axis = 0.8) # these values look completely normal!
stripchart(ns.n_AUC ~ date, aucdat, vertical = T, pch = 1, las = 2, cex.axis = 0.8) # again, normal
stripchart(nAE_AUC ~ date, aucdat, vertical = T, pch = 1, las = 2, cex.axis = 0.8) # normal.
# Wow.
# I guess I will just make a note to check the values from these 3 cultures once I re-process the data
# It's possible there was just a fluke in the script or data handling.
# or, what if someone transformed these values to by spikes per min instead of per sec?
# ya, I think that would make sense based on the magnitude of these values
# ya, no way there would be 500 spikes per second averaged over a well!

# cultures of concern:
# 20150805, 20160511, 20160601


# NOrmalize by culture?
aucdat[dose == 0, median(meanfiringrate_AUC), by = c("date","plate.SN")][, summary(V1)]
aucdat[dose == 0, median(meanfiringrate_AUC), by = c("date","plate.SN")][order(V1)]
stripchart(aucdat[!(date %in% c("20150805", "20160511", "20160601")) & dose == 0, median(meanfiringrate_AUC), by = c("date","plate.SN")][, V1], vertical = T,
           method = "jitter", pch = 1)
boxplot(aucdat[!(date %in% c("20150805", "20160511", "20160601")) & dose == 0, median(meanfiringrate_AUC), by = c("date","plate.SN")][, V1])

# hubba bubba. 
# I will investigate normalizing by culture median. But not going to stress it
stripchart(aucdat[!(date %in% c("20150805", "20160511", "20160601")) & dose == 0, median(meanfiringrate_AUC), by = c("date")][, V1], vertical = T,
           method = "jitter", pch = 1)

# OTHER CHECKS -----------------------------------
(trts <- aucdat[date == "20150805", unique(treatment)])
aucdat[treatment %in% trts, unique(plate.SN), by = c("date","treatment")]

# Endpoint concern checks
dat[, ns.mean.spikes.in.ns := as.numeric(ns.mean.spikes.in.ns)]
stripchart(ns.percent.of.spikes.in.ns ~ dose, dat, vertical = T, pch = 1, method = "jitter")
stripchart(ns.mean.spikes.in.ns ~ dose, dat, vertical = T, pch = 1, method = "jitter")
stripchart(ns.percent.of.spikes.in.ns ~ DIV, dat, vertical = T, pch = 1, method = "jitter") # okay so there is def a shift here
stripchart(ns.mean.spikes.in.ns ~ DIV, dat, vertical = T, pch = 1, method = "jitter") # okay so there is def a shift here
boxplot(ns.mean.spikes.in.ns ~ DIV, dat[dose == 0]) # okay so there is def a shift here
# huh. why is DIV 9 lower?

stripchart(ns.mean.spikes.in.ns ~ DIV + dose, dat, vertical = T, pch = 1, method = "jitter") 
# wow, these are much bigger values than I would have expected.
# Wow. Perhaps using a large duration for the network spikes would just add mroe noise?

# final plots
stripchart(ns.percent.of.spikes.in.ns ~ DIV, dat[dose == 0], vertical = T, pch = 1, method = "jitter", main = "All Frank 2017 published data")
stripchart(ns.mean.spikes.in.ns ~ DIV, dat[dose == 0], vertical = T, pch = 1, method = "jitter", main = "All Frank 2017 published data")
stripchart(ns.n ~ DIV, dat[dose == 0], vertical = T, pch = 1, method = "jitter", main = "All Frank 2017 published data")
stripchart(ns.durn.m ~ DIV, dat[dose == 0], vertical = T, pch = 1, method = "jitter", main = "All Frank 2017 published data")

dat[, dummy_check := 0.05*meanfiringrate*nAE]
plot(dummy_check ~ ns.mean.spikes.in.ns, dat)
# okay, good, dummy_check is not a good approximation for ns.meanspikes.in.ns