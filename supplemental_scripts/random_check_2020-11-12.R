# random checks before sending mea_nfa_lvl0_2020-11-12.RData

# 11/10/2020 - viewing compounds tested on 20160921
check_spids <- alldat[grepl("20160921",apid) & wllt == "t", c(spid)]
alldat[spid %in% check_spids, .(paste0(sort(unique(apid)),collapse=",")), by = .(spid)]
# half were re-tested, the other half were not.

check_spids <- unique(check_spids)
alldat[, bval := median(rval[wllt == "n" & wllq == 1], na.rm = T), by = .(apid,acsn)]
alldat[, resp.test := (rval - bval)/(0 - bval)]
alldat[, bmad := mad(resp.test[wllq == 1 & wllt == "n"]), by = .(acsn)]
test <- alldat[spid %in% check_spids]
acsni <- "CCTE_Shafer_MEA_dev_firing_rate_mean"
spidi <- check_spids[1]

plotdat <- test[wllq == 1 & acsn == acsni & spid == spidi]
plot(plotdat[, .(log10(conc), resp.test)])
abline(h = 3*unique(plotdat$bmad))

# 11/12/2020
# checking out the spike list time for these files
library(rhdf5)
setwd('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl')
# checkout the culture with teh HUGE inter-networ spike intervals
h5files <- list.files(file.path('DNTGF2019/h5Files'), pattern = "20190807", full.names = T)
for (h5file in h5files) {
  cat(basename(h5file),"\t")
  h5dat <- h5read(h5file, name = "/spikes")
  cat(range(h5dat),"\n")
}
# NFA_20190807_MW69-3715_05_00(000).h5 	0.00392 900.0035 
# NFA_20190807_MW69-3715_07_00(001).h5 	5.02392 904.9901 
# NFA_20190807_MW69-3715_09_00(000).h5 	0.00032 899.9814 
# NFA_20190807_MW69-3715_12_00(000).h5 	0 899.9947 
# NFA_20190807_MW69-3802_05_00(000).h5 	0.34552 900.258 
# NFA_20190807_MW69-3802_07_00(000).h5 	0.00528 899.9264 
# NFA_20190807_MW69-3802_09_00(000).h5 	0.00048 899.9982 
# NFA_20190807_MW69-3802_12_00(000).h5 	0.00104 899.9942 
# NFA_20190807_MW69-3803_05_00(000).h5 	0.2452 900.0757 
# NFA_20190807_MW69-3803_07_00(000).h5 	-1.47574e+15 -1.47574e+15 
# NFA_20190807_MW69-3803_09_00(000).h5 	0.00848 900.0058 
# NFA_20190807_MW69-3803_12_00(000).h5 	0.00128 900.0009 
# NFA_20190807_MW69-3804_05_00(000).h5 	0.08648 900.0143 
# NFA_20190807_MW69-3804_07_00(000).h5 	5.02096 905.0037 
# NFA_20190807_MW69-3804_09_00(000).h5 	0.01456 900.0062 
# NFA_20190807_MW69-3804_12_00(000).h5 	0.00016 899.9994 
# NFA_20190807_MW69-3805_05_00(000).h5 	0.1652 900.0121 
# NFA_20190807_MW69-3805_07_00(000)(001).h5 	6.51504 901.4618 
# NFA_20190807_MW69-3805_09_00(000).h5 	0.00576 900.0001 
# NFA_20190807_MW69-3805_12_00(000).h5 	0.01168 900.0116 
# NFA_20190807_MW69-3806_05_00(000).h5 	0.12224 900.0932 
# NFA_20190807_MW69-3806_07_00(000).h5 	0.03184 900.013 
# NFA_20190807_MW69-3806_09_00(000).h5 	0.01072 900.01 
# NFA_20190807_MW69-3806_12_00(000).h5 	0.0024 900.002
# okay, it looks like eeverything covered 900 seconds!

# verifying spid assignmnet
load("lvl0_snapshots/mea_nfa_lvl0_extra_cols_2020-11-11.RData")
trts <- mea_nfa_lvl0[, .(length(unique(spid))), by = .(treatment)][V1 > 1, unique(treatment)]
mea_nfa_lvl0[treatment %in% trts, .(unique(dataset)), by = .(spid, treatment)][order(treatment)]

trts <- mea_nfa_lvl0[, .(length(unique(dataset))), by = .(treatment)][V1 > 1, unique(treatment)]
mea_nfa_lvl0[treatment %in% trts, .(unique(dataset)), by = .(spid, treatment)][order(treatment)]
# look alright

mea_nfa_lvl0[spid == "EX000411", unique(dataset)] # "Brown2014" "Frank2017", cool
mea_nfa_lvl0[mea_treatment_name == "Bis1", unique(dataset)] # Frank2017...
mea_nfa_lvl0[dataset == "Brown2014",.(unique(treatment)), by = .(spid)] # right, I forgot that I renamed Bis 1 for dat tested here

mea_nfa_lvl0[wllt == 't', .(number_of_spids = length(unique(spid))), by = .(dataset)]
