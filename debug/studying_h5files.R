# learning about how the h5files are strucutred
# 09/02/2020

# inspired bc something weird happened with this file for DIV 7 and 9. Had to remake DIV 9. Tried to delete and remake DIV 7, but I couldn't delete bc said it was open in another program.
# After I remade DIV 9, it said this file was edited as well at that time!!
library(rhdf5)
dat <- h5read("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/RejectedCultures/h5Files/ON_20160316_MW1112-13_07_00(000).h5", name = "/")
str(dat)
which(dat$spikes > 899)
dat$spikes[1700:1710] # [1] 886.00552 888.53552 892.35168 892.86992 899.02696 899.30496   0.97136  15.04888  21.03584  24.00776  24.44336
# wow!! It does look like something got appended here

# okay, let's compare to spike list file.
length(dat$spikes) # 402477
# There are this many rows in the spike list file: 402574. So subtract header row, then sub 96 rows past 900 sec. That lines up
# So I don't think that extra rows were appended here
# Now I'm just concerend about the ordering.
length(dat$channels) # 698. This is probs just every elect that sipkes at least once
length(dat$names) # 698, hmm...
length(dat$sCount)
# [1] 698
# hmm... I wonder if the spikes are ordered by the electrode in names/channels.
dat$sCount[1] # 132
dat$channels[1] # "A1_11"
# So perhaps the first 132 spikes listed in spikes belongs to A1_11
dat$spikes[c(1,131:134)]
# [1]  21.54888 876.07920 876.23408   2.77664  21.39992 # huzzah, this makes sense!
# okay, so I think this is probably right.
# My only concern is that this file was open/modified after th eDIV 9/12 files.
# So I think I want to re-run all of these 1 more time.
rm(dat)

dat <- h5read("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/RejectedCultures/h5Files/ON_20160316_MW1112-13_12_00(000).h5", name = "/")
which(dat$spikes > 899.9)
dat$spikes[1700:1710]
dat$spikes[370934:370936] # wait a sec....
length(dat$spikes)
# wait... is dat$spikes not ordered?
is.ordered(dat$spikes) # FALSE, not surprising
rm(dat)

# an untainted file
dat <- h5read("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/RejectedCultures/h5Files/ON_20181205_MW1235-12_09_00(000).h5", name = "/")
which(dat$spikes > 899.9)
is.ordered(dat$spikes) # FALSE
rm(dat)