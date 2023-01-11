# bin number wrong 900.000000 18000
# fairly confident it is from 20180815 MW1207-26
sf12 <- "L:/Lab/NHEERL_MEA/Project PFAS 2018/20180815 Culture PFAS Group_2-1/Original Files (Spike Counts)/ON_20180815_MW1207-26_12_00(000)_spike_list.csv"
sf12 <- read.csv(sf12,header=T,colClasses=c("NULL", "NULL", NA, NA, NA))
head(sf12) # hmm, starts at 0...
tail(sf12) # just empty rows...
nrow(sf12[sf12$Time..s. != "",]) # 1250546
max(sf12$Time..s.)
sf12$Time..s. <- as.numeric(as.character(sf12$Time..s.))
head(sf12)
str(sf12)
max(sf12$Time..s., na.rm = T) # 900.2486
range(sf12$Time..s., na.rm= T) # 0.00008 900.24856. wow, super normal
rm(sf12)
h5Files <- list.files(path = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/h5Files", pattern = "20180815_MW1207-26",
                      full.names = T)

# no errors with:
s[[1]]$file
# [1] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/h5Files/ON_20180815_MW1207-26_05_00(000).h5"

cur.file <- 3
s[[cur.file]]$file
# "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/h5Files/ON_20180815_MW1207-26_09_00(000).h5"
nspikes.old <- calculate.network.spikes(s[[cur.file]])
# bin number wrong 900.000000 18000
# 900.000000 - corresponds to the time of the current spike
# we want to put the current spike in a bin
# count has 18000 bins ceiling(time of last spike in well - time of first spike/ 0.05) = 1800
# but, the bins are indexed from 0 - 17999. 
# If the spike was at 899.99sec, then it would go in count[17999] (the 1800th bin)
# but intead, this spike should go in 18001st bin, which doesn't exist
# so, this is just a crazy but rare fluke that there was a spike right at the last second. :)

# th3 min/max warning
file <- "L:/Lab/NHEERL_MEA/Project PFAS 2018/20180815 Culture PFAS Group_2-1/Original Files (Spike Counts)/ON_20180815_MW1207-26_05_00(000)_spike_list.csv"

f <- file
spikes.sep <- lapply(f, spkList2list) # apply the spike list function
str(spikes.sep)
# List of 1
# $ :List of 201
# ..$        : num(0) 
# ..$ #00FF00: num(0) 
#   ..$ A1_11  : num 705
# ..$ A1_13  : num [1:2] 592 868
# ..$ A1_32  : num [1:2] 260 566
# ..$ A1_42  : num [1:313] 1.43 2.65 6.28 9.76 16.47 ...
# ..$ A1_43  : num [1:73] 2.95 6.44 56.81 91.95 94.94 ...
# ..$ A2_11  : num [1:5] 473 638 795 801 876
# ..$ A2_14  : num [1:9] 21.6 22.6 62.8 148.7 171 ...
# ..$ A2_31  : num [1:629] 0.33 3.11 3.16 5.42 6.01 ...
# ..$ A2_33  : num [1:498] 0.313 1.906 4.176 9.408 12.028 ...
# ..$ A2_44  : num [1:11] 21.9 65.2 228.3 241.8 310.1 ...
# ..$ A3     : num(0) 
# ..$ A3_11  : num [1:1115] 0.847 1.403 2.69 3.594 4.632 ...
# ..$ A3_14  : num [1:120] 11.9 17.1 26.7 38.5 46 ...
# ..$ A3_31  : num [1:752] 0.582 4.011 4.066 7.303 7.395 ...
# oh dear... so some non-electrodes are still included!
# perhaps I need to do a better job of truncated the unneeded portions..
# actually, the none-electrode wells are getting removed
# But because data.raw$elect is a factor, when you split by the elect, 
# the default is to keep even teh factor groups that are not present
# hence the groups liek "", #00FF00, A3 being present
# this is where warning comes:
summary.table <- t(sapply(spikes.sep, sjemea::axion.spikesum2) )
# There were 28 warnings (use warnings() to see them)

# from sjemea axiont_functions.R
axion.spikesum2 <- function(spikes) {
  ## Generate a simple summary of the spikes list.
  ## This version returns a vector, rather than a string.  This is more
  ## useful for building a data frame of values.
  len <- length(spikes)
  all.range <- sapply(spikes, range)
  nspikes <- sum(sapply(spikes, length))
  min <- min(all.range[1,])
  max <- max(all.range[2,])
  str <- sprintf("summary: %d electrodes %d spikes, min %.4f max %.4f",
                 len, nspikes, min, max)
  ##str
  c(nelectrodes=len, nspikes=nspikes, time.min=min, time.max=max)
}

# okay, so this occurs because the Electrode col is read as a factor
# options:
spikes<-split(data.raw2$timestamps, data.raw2$elect, drop = TRUE) # to drop unused factor levels

# okay, but where I havn't checked this before... is it a big issue?

spikes <- spikes.sep[[1]]
names <- names(spikes)
nspikes <- sapply(spikes, length)
head(nspikes)
#   #00FF00   A1_11   A1_13   A1_32   A1_42 
# 0       0       1       2       2     313
data.frame(elec=rep(names, times=nspikes), time=unlist(spikes))
# so nope, this error stopped at 

# thoughts on file start time:
# I really should check that the total time from the last spike to the first spike is less than 900sec
# because, what if there is a delay or offset?
# how often would that happen?
