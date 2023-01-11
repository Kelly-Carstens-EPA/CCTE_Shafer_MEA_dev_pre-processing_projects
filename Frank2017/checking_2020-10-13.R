# 10/13/2020
# checking out the data
library(data.table)
library(devtools)
install_github("sje30/sjemea")

library(xlsx)
library(readxl)
dat <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/Published Downloaded Data Reference/Tab8_AUC Compound Values.csv"))

dat[, .N, by = .(date)][order(date)]
# date   N
# 1: 20140827 144
# 2: 20140910 144
# 3: 20140924 144
# 4: 20141015 141
# 5: 20141029 144
# 6: 20141112 144
# 7: 20141203 144
# 8: 20141231 144
# 9: 20150128 288
# 10: 20150401 144
# 11: 20150805  96
# 12: 20150819 144
# 13: 20150909 144
# 14: 20151014 144
# 15: 20151125  48
# 16: 20160511 144
# 17: 20160601 144

# 17 cultures included
dat[, .(length(unique(plate.SN))), by = .(date)][order(date)]

# which plates included in each culture?
dat[, .(plates = paste0(sort(sub("MW","",unique(plate.SN))),collapse=", ")), by = .(date)][order(date)]
# date                                               plates
# 1: 20140827                           1007-100, 1007-91, 1007-98
# 2: 20140910                            1008-39, 1008-40, 1008-68
# 3: 20140924                            1007-70, 1007-81, 1007-84
# 4: 20141015                          1007-107, 1007-108, 1008-39
# 5: 20141029                             1008-41, 1008-42, 1036-8
# 6: 20141112                           1041-25,  1041-28, 1041-17
# 7: 20141203                            1042-42, 1042-43, 1042-44
# 8: 20141231                            1045-02, 1045-06, 1045-09
# 9: 20150128 1007-109, 1008-1, 1008-37, 1008-40, 1008-41, 1008-42
# 10: 20150401                            1053-26, 1053-31, 1053-33
# 11: 20150805                                     1040-11, 1040-12
# 12: 20150819                               1044-2, 1044-6, 1044-7
# 13: 20150909                            1047-22, 1047-29, 1047-30
# 14: 20151014                            1076-42, 1076-43, 1076-44
# 15: 20151125                                     1086-39, 1086-41
# 16: 20160511                            1112-16, 1112-17, 1112-18
# 17: 20160601                            1062-29, 1062-43, 1062-44

# how many data points included for each plate? (which are not 48?)
dat[, .N, by = .(date, plate.SN)][N != 48]
# date   plate.SN  N
# 1: 20141015 MW1007-107 47
# 2: 20141015 MW1007-108 46
# 3: 20151125  MW1086-39 24
# 4: 20151125  MW1086-41 24

dat[plate.SN == "MW1007-107", sort(unique(well)), by = sub("[[:digit:]]","",well)] # A5 is missing
dat[plate.SN == "MW1007-108", sort(unique(well)), by = sub("[[:digit:]]","",well)] # A3 and A5 are missing
dat[plate.SN == "MW1086-39", sort(unique(well))] # Rows B, C, and D are misssing
dat[plate.SN == "MW1086-41", sort(unique(well))] # Rows B, C, and D are misssing
dat[date == "20151125", unique(treatment)]
# "TDCPP"                 "Tetrabromobisphenol A" "TPP" 


# Any invidiual DIV not included, for good reason?
div_dat <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Frank2017/Published Downloaded Data Reference/Tab7-Compound_RawValues.csv"))
div_dat[, .(length(unique(well))), by = .(date, Plate.SN)][V1 != 48]
# same as above

div_dat[, .(paste0(sort(unique(DIV)),collapse=",")), by = .(date, Plate.SN, well)][V1 != "5,7,9,12"]
# date   Plate.SN well     V1
# 1: 20141015 MW1007-107   A3     12
# 50: 20150128 MW1007-109   F8  5,7,9
# plus DIV 5 for all wells in 20141029 MW1008-41
div_dat[Plate.SN == "MW1007-109", .N, by = "DIV"]
# DIV  N
# 1:  12 47
# 2:   5 48
# 3:   7 48
# 4:   9 48

div_dat[Plate.SN == "MW1007-107" & well == "A3", .(date, Plate.SN, DIV, well, trt, dose, MFR = meanfiringrate*60, nAE)]
# date   Plate.SN DIV well       trt dose      MFR nAE
# 1: 20141015 MW1007-107  12   A3 Saccharin  0.1 57.65677   1
# Goal:
# Determien if I agree with all of these inclusions/exclusions
# or justify myself it not
# else ask Kathleen!

dat[dose == 0, summary(meanfiringrate_AUC)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   10.65   14.60  128.86   20.51 1501.98 
dat[dose == 0, IQR(meanfiringrate_AUC)]
div_dat[dose == 0 & DIV == 12 & meanfiringrate == 0]
dat[dose == 0 & meanfiringrate_AUC < 1.5, .(date, plate.SN, treatment, dose, meanfiringrate_AUC, nAE_AUC)]

dat[plate.SN == "MW1036-8" & dose == 0, range(meanfiringrate_AUC)]
# [1] 16.744 24.583
# seems very acceptable, totally within the IQR