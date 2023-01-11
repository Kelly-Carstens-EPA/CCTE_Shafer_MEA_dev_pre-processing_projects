# general checks before I save the files

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
source(file.path(root_output_dir,"supplemental_scripts","get_latest_dat.R"))

alldat <- get_latest_dat()

# see where a compound tested in multipel cultures
alldat[, culture := sub("_.*$","",apid)]
repeated_spids <- alldat[wllt == "t", .(length(unique(culture))), by = .(spid)][V1 != 1, spid]
alldat[spid %in% repeated_spids, .(length(unique(spid))), by = .(dataset)]
alldat[spid %in% repeated_spids, .(length(unique(culture))), by = .(treatment)]

# to really see duplicated compounds,
# map to CASRN or dtxsid, then see where multiple spid map to same casn
duplicated_spids <- alldat[, .(length(unique(treatment))), by = .(spid)][V1 != 1, unique(spid)]
unique(alldat[spid %in% duplicated_spids, .(spid, apid, treatment)])

# 10/16/2020
# checking out the range of all cytotoxicity values
plotdat <- alldat[grepl("LDH",acsn)]
plotdat$apid <- factor(plotdat$apid, levels = sort(unique(plotdat$apid)), ordered = T)
par(oma = c(2,0,0,0))
stripchart(rval ~ apid, plotdat[wllq == 1], vertical = T, method = "jitter", pch = 1, las = 2, cex.axis = 0.7)
stripchart(rval ~ apid, plotdat[wllq == 0], vertical = T, method = "jitter", pch = 1, col = "red", add = T)
stripchart(rval ~ apid, plotdat[J(qry)], vertical = T, method = "jitter", pch = 19, col = rgb(0,0,0.8,0.5), add = T)

# question mark chem
qry <- data.table(apid = c("20181017_MW1208-4","20181017_MW1207-43","20181114_MW1234-49","20181114_MW1234-49"), treatment = c("1475824","1475815","Loperamide","1475813"), conc = c(0.3,3,3,0.03))
setkey(plotdat, apid, treatment, conc)
qry <- data.table(apid = c("20181017_MW1208-4","20181017_MW1207-43","20181114_MW1234-49","20181114_MW1234-25"), 
                  treatment = c("1475824","1475815","4-(4-Chlorophenyl)-4-hydroxy-N,N-dimethyl-alpha,alpha-diphenylpiperidine-1-butyramide monohydrochloride","1475813"),
                  conc = c(0.3,3,3,0.03))

plotdat[wllq == 1, summary(rval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.5134  0.8775  0.8604  1.1747  3.4000 
plotdat[J(qry), .(apid, treatment, rowi, coli, rval)]

# let me just view Lop real quick...
stripchart(rval ~ signif(log10(conc),3), plotdat[dataset == "PFAS2018" & treatment == "4-(4-Chlorophenyl)-4-hydroxy-N,N-dimethyl-alpha,alpha-diphenylpiperidine-1-butyramide monohydrochloride"],
           vertical = T, pch = 1, main = paste0(unique(plotdat$acsn)," PFAS2018 Loperamide Dose-response"))
abline(h = plotdat[apid == "20181114_MW1234-49" & wllt == "n" & wllq == 1, median(rval)])
# this will still have a dn dose-response curve!!

stripchart(rval ~ signif(log10(conc),3), plotdat[dataset == "PFAS2018" & treatment == "1475813"],
           vertical = T, pch = 1, main = paste0(unique(plotdat$acsn)," PFAS2018 1475813 Dose-response"))
abline(h = plotdat[apid == "20181114_MW1234-25" & wllt == "n" & wllq == 1, median(rval)])

stripchart(rval ~ signif(log10(conc),3), plotdat[dataset == "PFAS2018" & treatment == "1475815"],
           vertical = T, pch = 1, main = paste0(unique(plotdat$acsn)," PFAS2018 1475815 Dose-response"))
abline(h = plotdat[apid == "20181017_MW1207-43" & wllt == "n" & wllq == 1, median(rval)])

stripchart(rval ~ signif(log10(conc),3), plotdat[dataset == "PFAS2018" & treatment == "1475824"],
           vertical = T, pch = 1, main = paste0(unique(plotdat$acsn)," PFAS2018 1475824 Dose-response"))
abline(h = plotdat[apid == "20181017_MW1208-4" & wllt == "n" & wllq == 1, median(rval)])

# I could either remove these points

# these are points that we are definitely keeping
stripchart(rval ~ signif(log10(conc),3), plotdat[dataset == "PFAS2018" & treatment == "1483029"],
           vertical = T, pch = 1)
abline(h = plotdat[apid == "20180912_MW1207-37" & wllt == "n" & wllq == 1, median(rval)])

stripchart(rval ~ signif(log10(conc),3), plotdat[dataset == "PFAS2018" & treatment == "1483037"],
           vertical = T, pch = 1)
abline(h = plotdat[apid == "20180912_MW1207-38" & wllt == "n" & wllq == 1, median(rval)])

stripchart(rval ~ signif(log10(conc),3), plotdat[dataset == "PFAS2018" & treatment == "1475858"],
           vertical = T, pch = 1)
abline(h = plotdat[apid == "20180912_MW1207-41" & wllt == "n" & wllq == 1, median(rval)])
# huh?

# chem's are wrong in source file!!
alldat[, date:= sub("_.*$","",apid)]
alldat[dataset == "PFAS2018", .(paste0(sort(unique(treatment)),collapse=",")),by=.(date)][order(date)]
