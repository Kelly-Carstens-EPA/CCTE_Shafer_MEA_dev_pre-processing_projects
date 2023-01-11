# 11/11/2020
# considering if I should remove some "Really bad" plates
# e.g. in 20160921
# Would like to have some kind of consistent rule though

# Getting a sense of how many control wells fall below the thresholds we woudl expect
alldat[grepl("firing_rate_mean_DIV12",acsn) & wllt == "n" & wllq == 1, .N, by = rval >= 50/60]
# rval    N
# 1:  TRUE 1376
# 2: FALSE  148

# how many plates have 3 or more wells with wllt == "n" below 50 spikes per min?
alldat[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn) & rval < 50/60, .N, by = .(apid)][N >= 3]
# wow, 20 plates
# one plate even has 5 pts below the threshold!
alldat[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn) & apid == '20181017_MW1207-44', .(rowi, rval*60)]
# rowi       V2
# 1:    1 47.43832
# 2:    2 42.89544
# 3:    3 35.42794
# 4:    4 65.11826
# 5:    5 36.15573
# 6:    6 24.95345
# yes, these are low.... But I think the plate from 20160921 stands out much more

# I think what matters most is teh bval, not how many wells are above or below the threshold
# Since bval is what is used to normalize

# for how many apid is the median below 50 spikes per minute? And by how much?
alldat[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn), .(bval = median(rval)), by = .(apid)][bval < 50/60, .(apid, bval*60)]
# apid       V2
# 1: 20190807_MW69-3804 40.43596
# 2: 20190807_MW69-3805 38.27709
# 3: 20191113_MW70-2519 48.97869
# 4: 20180905_MW1207-31 47.36600
# 5: 20181017_MW1207-44 39.52558
# 6:  20181024_MW1208-5 44.37660
# 7: 20160921_MW1159-40 35.68658
# 8: 20160921_MW1159-43 37.45733
# 9: 20160921_MW1160-23  5.73338
# 10:  20170222_MW1146-5 49.31465
# 11: 20170628_MW1146-22 45.05874

# So all of these could be argued to be low...
# But then we would get into all kinds of other issues with potentially having to re-test compounds,
# then is it better to have any data at this point then not great data...
# But there is 1 plate that clearly stands out.
alldat[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn) & apid == '20160921_MW1160-23', .(rowi, rval*60)]
# rowi        V2
# 1:    1   0.00000
# 2:    2   0.00000
# 3:    3  11.46676
# 4:    4   0.00000
# 5:    5  56.92047
# 6:    6 154.40960
# I think that is woudl be reasonable for me to remove this plate.
# The controls clearly did not develop.


# let's check out the AE endpoint as well
alldat[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn), .(bval = median(rval)), by = .(apid)][bval < 10, .(apid, bval)]
# apid V2
# 1: 20180912_MW1207-38 95
# 2: 20181114_MW1234-51 90
# 3: 20160921_MW1159-40 85
# 4: 20160921_MW1160-23  5

# again, this 1 plate stands out
alldat[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn) & apid == '20160921_MW1160-23', .(rowi, rval)]
# rowi rval
# 1:    1    0
# 2:    2    0
# 3:    3    1
# 4:    4    0
# 5:    5   15
# 6:    6   16

# okay, so I would like to remove 20160921_MW1160-23,
# and leave in all others

# curious how the bmad will change?
alldat[, bval := median(rval[wllt == 'n' & wllq == 1], na.rm = T), by = .(apid, acsn)]
alldat[, resp.test := (rval - bval)/bval]
alldat[, bmad := median(resp.test[wllt == 'n' & wllq == 1], na.rm = T), by = acsn]
alldat[grepl("firing_rate_mean_DIV12",acsn), unique(bmad)*3]
# o0.. I did something wrong
# but it doesn't really matter