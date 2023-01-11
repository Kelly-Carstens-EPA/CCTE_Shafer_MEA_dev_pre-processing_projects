# quick detective work...
load('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/lvl0_snapshots/mea_nfa_lvl0_2020-11-12.RData')
load('L:/Lab/NHEERL_MEA/tcpl_nheerl_mea_dev/plots_12_NOV_2020/ccte_mea_dev_data_12nov2020.RData')
all.equal(mea_nfa_lvl0, mc0) # diff col's
rm(list = c('mc5_mc6','mc3'))
usecols <- intersect(names(mea_nfa_lvl0), names(mc0))
all.equal(mea_nfa_lvl0[,..usecols], mc0[,..usecols], check.attributes = F, ignore.row.order = T)
# TRUE!

# let's just check the concentrations
conc.tb.tcpl <- mc0[, .N, by = .(spid, conc, apid, rowi, coli, wllt)]
conc.tb.local <- mea_nfa_lvl0[, .N, by = .(spid, conc, apid, rowi, coli, wllt)]
comp.conc <- merge(conc.tb.tcpl, conc.tb.local, by = c('spid','wllt','apid','rowi','coli'), all = T)
comp.conc[conc.x != conc.y, .N, by = .(spid)] # affects 30 spids. But only for some points...
mea_nfa_lvl0[, .(length(unique(acnm)))] # 36
comp.conc[signif(conc.x,3) != signif(conc.y,3), .N, by = .(spid)]
# this is empty!
# so the differences in conc are all very slight -> perhaps this corresponds to just data storage accuracy limitations?

# wait a sec... what if the conc's in my source data are different for different endpoints?
conc.tb.tcpl <- mc0[, .N, by = .(spid, conc, apid, rowi, coli, wllt, acnm)]
conc.tb.local <- mea_nfa_lvl0[, .N, by = .(spid, conc, apid, rowi, coli, wllt, acnm)]
comp.conc <- merge(conc.tb.tcpl, conc.tb.local, by = c('spid','wllt','apid','rowi','coli','acnm'), all = T)
comp.conc[conc.x != conc.y, .N, by = .(spid)]
# empty! ah ha!
# So -> Katie did not change the conc's/do any concentratin corection

# so I must have variable conc's within my data...
mea_nfa_lvl0[, .(length(unique(conc))), by = .(apid, rowi, coli)][V1 > 1]
# yep, over 500 cases!!

mea_nfa_lvl0[, .(length(unique(signif(conc,3)))), by = .(apid, rowi, coli)][V1 > 1]
# this is empty

# okay, I think it's coming back to me - 
# some conc's were different several decimal places out
# but since all the same when round, I wasn't concered about it
# probs a difference in .xlsx for cyto endpoints and .csv for mea endpoints
# i'm okay wiht leaving this as is for now, but will triage for updating in teh next release