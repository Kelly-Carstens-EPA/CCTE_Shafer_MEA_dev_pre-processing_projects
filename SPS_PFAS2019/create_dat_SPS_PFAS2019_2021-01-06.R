# Recreating the data file 'dat_SPS_PFAS2019_2021-01-06.RData'
# Because I can't find the script where I originally made it
# May 18, 2021

rm(list = ls())
setwd('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl')
load('SPS_PFAS2019/output_2021-01-06/SPS_PFAS2019_longfile.RData')
dat <- dat[!grepl('DIV[579]$',acsn)]
setnames(dat, old = 'acsn', new = 'acnm')
dat[, dataset := 'SPS_PFAS2019']
dat[is.na(rval), wllq := 0]

save(dat, file = file.path("SPS_PFAS2019", "output_2021-01-06", "dat_SPS_PFAS2019_2021-01-06.RData"))

# confirm this matches dat used for SPS hit call analysis
check.dat <- dat
load('L:/Lab/NHEERL_MEA/Project PFAS 2019/MEA NFA/SPS Hit Call Analysis/dat_SPS_PFAS2019_2021-01-06.RData')
all.equal(check.dat, dat)
# TRUE


