# determining if the differences in parameter values between old and new data for DNT 
# can be explained only by spike list file chopping changes
# or if I made an unknown detrimental change to the parameter vlaue calculation
rm(list=ls())

# source most recent function
source('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R/create_burst_ont_Data.R')
source('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R/local.corr.all.ont.ae.filter.R')

# get the original h5files
h5files_org <- list.files("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/h5Files", pattern = "\\.h5", full.names = T)
csv.filename.ABEfilt <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/comparing_previous_values/org_h5file_values"

library(sjemea)
library(rhdf5)
library(meadq)
# taken from meadq data/chgv_parameters.R
# not sure how these threshold are usually imported... probably hidden in some function call
data('chgv_parameters')
# elec.min.rate <- 0
# elec.max.rate <- 1000
# well.min.rate <- 0
# 
# mi.par <- list(beg.isi =    0.1,
#                end.isi =    0.25,
#                min.ibi =    0.8,
#                min.durn =   0.05,
#                min.spikes = 5)
# 
# 
# ns.T <- 0.05    	#time in seconds
# ns.N <- 4         #how many coincident electrodes?
# sur<-100 # num. ms before and after spike to check I think, used in ms

create_burst_ont_Data(h5files_org)
