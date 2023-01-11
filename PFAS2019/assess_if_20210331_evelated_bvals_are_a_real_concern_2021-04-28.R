# 4/28/2021
# I noticed that the bval's in several of the last plates for PFAS2019 seem elevated,
# particularly relative ot the rest of the treated wells on plate
# Several of these plates only have 5 control wells, becuase one of ones that would be a control is just Media instead


# checking out all control wells
setwd('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl')
load('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_2021-04-28.RData')

# Look at controsl revlative to a tiivty in test wells
# sort by date as well

# mfr
plotdat <- mea_nfa_lvl0[grepl('firing_rate_mean$',acnm)]
plotdat[, .N, by = .(spid)]
plotdat$apid <- factor(plotdat$apid, levels = sort(unique(plotdat$apid)), ordered = T)
png(filename = 'PFAS2019/mean_firing_rate_auc_all_time_2021-04-28.png', height = 600, width = 1000)
par(mar = c(9,3,2,2))
stripchart(rval ~ apid, plotdat[wllq == 1 & wllt == 't'], vertical = T, pch = 1, method = 'jitter', las = 2, col = 'gray80', cex.axis = 0.7)
stripchart(rval ~ apid, plotdat[wllq == 1 & wllt == 'n'], vertical = T, pch = 19, method = 'jitter', las = 2, add = T)
stripchart(bval ~ apid, plotdat[wllq == 1 & wllt == 'n', .(bval = median(rval)), by = .(apid)], vertical = T, pch = 19, las = 2, add = T, col = 'seagreen3')
legend(x = 'topleft', legend = c('median of control wells','all control wells','all test wells'),
       pch = c(19,19,1), col = c('seagreen3','black','gray80'))
title(main = 'Mean Firing Rate AUC by apid (where wllq==1)')
graphics.off()

mfr.tb <- plotdat[, .(range_of_rval = max(rval[wllq == 1 & wllt == 'n']) - min(rval[wllq == 1 & wllt == 'n'])), by = .(apid)]
mfr.tb[, summary(range_of_rval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -Inf    5.18    7.04    -Inf    9.23   25.65
mfr.tb <- mfr.tb[order(range_of_rval)]
mfr.tb[, rank := .I]
nrow(mfr.tb) # 277
0.75*277 # 207
mfr.tb[grepl('20210331',apid)]
# apid range_of_rval rank
# 1: 20210331_MW75-8019       4.46871   51
# 2: 20210331_MW75-8020       5.61934   88
# 3: 20210331_MW75-8102       9.23338  208
# 4: 20210331_MW75-8104      12.23617  250
# 5: 20210331_MW75-8103      15.32259  269
# 6: 20210331_MW75-8101      19.76666  276
# okay, so some of these plates have ranges that are in the 4th quartile
# 2 are among the top
# but still, is this reason for concern?

# I don't think so

# CONCLUSION:
# I think this is very believable noise, and I dont' think the noise is at a level to merit removal

plotdat[wllq == 1 & wllt == 'n', .(num_control_wells_per_apid = .N), by = .(apid)][, .N, by = .(num_control_wells_per_apid)]
# num_control_wells_per_apid   N
# 1:                         12   4
# 2:                          7   1
# 3:                          8   1
# 4:                          6 262
# 5:                          4   1
# 6:                          5   4
# okay, so there are VERY few plates with less than 6 controls per plate