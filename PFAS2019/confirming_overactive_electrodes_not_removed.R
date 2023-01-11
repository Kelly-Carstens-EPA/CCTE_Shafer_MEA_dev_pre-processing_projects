# checking if overactive electrodes have been removed in any case
# 3/19/2021
library(data.table)
setwd('L:/Lab/NHEERL_MEA/Project PFAS 2019/MEA NFA')
test.dat <- as.data.table(read.csv('20210127 Culture Chlropyrafos oxon/75-5620/Spike List Files/NFA_20210127_MW75-5620_07_00(000)_spike_list.csv'))
str(test.dat)
# Exract list of overactive electrodes from 'Overactive electrodes.txt'
oa.elecs <- c('A3_22','A3_31','A7_11','B6_32','C3_41')

# Are these still present in spike list file?
setdiff(oa.elecs, test.dat$Electrode)
# [1] "A3_22" "A7_11" "B6_32"
# Hmm... so 3 of the 5 overatice electrodes are present... this seems odd.
# perhaps this list of OE's is from a previous culture?
# -> yep, just confirmed that 'Overactive electrodes.txt' is the same as the one form the previous culture
file.info('20210127 Culture Chlropyrafos oxon/75-5620/Overactive electrodes.txt')
#                                                                      size isdir mode               mtime               ctime               atime
# 20210127 Culture Chlropyrafos oxon/75-5620/Overactive electrodes.txt  103 FALSE  666 2019-11-25 10:01:05 2021-01-27 11:31:56 2021-01-27 11:31:56
# exe
# 20210127 Culture Chlropyrafos oxon/75-5620/Overactive electrodes.txt  no
# Yeah, this was lost modified in 2019
# other times probs correspond to when moved to new folder
# all times are before the date this spike list file was recording (2/5/2021)
# so def not relevant

# There is an identical file of overactive electrodes in 20210127 G2 75-5616 - again, I'm assumign this is not relevant.


# For the SPS - where these overactive elctrodes removed in the previous culture?
rm(test.dat)
test.dat <- as.data.table(read.csv('20201209 Culture SPS G4/72-8207/Spike List Files/NFA_20201209_MW72-8207_07_00(000)(000)_spike_list.csv'))
setdiff(oa.elecs, test.dat$Electrode)
# "A3_22" "A3_31"
# oh duh!!
# this file is from 2019 - not 2020. So this 'Overactive electrodes.txt' file doesn't correspond to any cultures in the SPS for PFAS either!
# I think we are good on this issue. It was decided not to removed overactive electrodes, adn Seline knows that. I don't think they would change that without me knowing