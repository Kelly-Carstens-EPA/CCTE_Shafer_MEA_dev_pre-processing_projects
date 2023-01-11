# 10/29/2020
library(data.table)

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
source(file.path(root_output_dir,"supplemental_scripts","get_latest_dat.R"))

alldat <- get_latest_dat()
# Loading longfile.Rdata for...
# Brown2014 
# DNTGF2019 
# Frank2017 
# NTP91 
# OPP2015 
# PFAS2018 
# ToxCast2016

# let's determine what looks good regarding Sodium orthovanadte
view_replicates_by_dose <- function(alldat, use_treatment = NULL, use_spid = NULL, acsni = "CCTE_Shafer_MEA_dev_AB") {
  plotdat <- alldat[spid %in% use_spid | treatment %in% use_treatment]
  plotdat[, conc_index := signif(log10(conc),3)]
  plotdat$conc_index <- factor(plotdat$conc_index, levels = sort(unique(plotdat$conc_index)), ordered = T)
  stripchart(rval ~ conc_index, plotdat[acsn == acsni & wllq == 1], vertical = T, pch = "", xaxt = "n", xlab = "conc (uM)")
  cultures <- sort(unique(sub("_.*$","",plotdat$apid)))
  for (culture in cultures) {
    stripchart(rval ~ conc_index, plotdat[acsn == acsni & wllq == 1 & grepl(culture, apid)], vertical = T, pch = 19, xaxt = "n", xlab = "conc (uM)", 
               col = which(cultures == culture), add = T)
  }
  abline(h = alldat[wllt == "n" & acsn == acsni & wllq == 1, median(rval)])
  text(x = length(unique(signif(plotdat$conc))), y = alldat[wllt == "n" & acsn == acsni & wllq == 1, median(rval)], labels = "median of all controls", cex = 0.7, adj = c(1,-0.2))
  title(main = paste0(unique(plotdat$treatment)[1]," ", acsni," by dose\nfrom ",paste0(unique(plotdat$dataset),collapse=", ")))
  axis(side = 1, at = seq(1, length(unique(signif(plotdat$conc)))), labels = sort(unique(signif(plotdat$conc,3))))
  legend(title = "cultures", x = "topright", legend = c(cultures), col = rank(cultures), pch = 19, bg = "transparent", cex = 0.8)
  rm(list= c("plotdat"))
}

graphics.off()
pdf(file.path(root_output_dir,"plots",paste0("sodium_orthovanadate_culture_replciates_mfr_auc.pdf")))
view_replicates_by_dose(alldat, use_spid = "EX000499")
view_replicates_by_dose(alldat, use_spid = "EX000499", acsni = "CCTE_Shafer_MEA_dev_mutual_information_norm")
graphics.off()
alldat[spid == "EX000499" & grepl("firing_rate_mean$",acsn), .(.N, concs = paste0(sort(unique(conc)),collapse=",")), by = .(sub("_.*$","",apid))]
# sub  N                    concs
# 1: 20140205  6      0.03,0.1,0.3,1,3,10
# 2: 20140212  6        0.1,0.3,1,3,10,30
# 3: 20140402  6        0.1,0.3,1,3,10,30
# 4: 20140423  6        0.1,0.3,1,3,10,30
# 5: 20140716 14 0.01,0.03,0.1,0.3,1,3,10
# 6: 20140730 14 0.01,0.03,0.1,0.3,1,3,10
# 7: 20141203 21   0.03,0.1,0.3,1,3,10,30
view_replicates_by_dose(alldat, use_spid = "EX000475")

# PLOTS ---------------------------------------------------------
# defaults:
# $mar
# [1] 5.1 4.1 4.1 2.1
plot_by_apid <- function(plotdat, threshold = NULL) {
  acsni <- unique(plotdat$acsn)
  plot_desc <- acsni
  if(!grepl("DIV",acsni)) plot_desc <- paste0(plot_desc," AUC")
  
  par(oma = c(2,0,1,0), mar = c(4,4.1,1,2.1))
  
  plotdat$apid <- factor(plotdat$apid, levels = sort(c(unique(plotdat$apid))), ordered = T)
  
  # plotdat$apid <- factor(plotdat$apid, levels = sort(unique(plotdat$apid)), ordered = T)
  stripchart(rval ~ apid, plotdat, vertical = T, pch = "", las = 2, cex = 1, cex.axis = 0.85, xaxt = "n", ylab = sub("CCTE_Shafer_MEA_dev_","",plot_desc), xlab = "apid by culture")
  stripchart(rval ~ apid, plotdat[wllq == 0], vertical = T, pch = 19, las = 2, col = rgb(1,0,0,alpha=0.5), 
             cex = 1, cex.axis = 0.85, add = T)
  stripchart(rval ~ apid, plotdat[wllq == 1], vertical = T, pch = 19, las = 2, col = rgb(0,0,1,alpha=0.5), 
             cex = 1, cex.axis = 0.85, add = T)
  
  # add the median from each plate
  stripchart(bval ~ apid, plotdat[wllq == 1, .(bval = median(rval)), by = .(apid)], vertical = T, pch = 19, las = 2, col = rgb(0.1,0.1,0.1,alpha=0.75), 
             cex = 1, cex.axis = 0.85, add = T)
  
  # add horizontal threshold line, if present
  abline(h = threshold, lty = "dashed", lwd = 2)
  
  # show the separations in culture date
  apid_char <- sort(unique(as.character(plotdat$apid)))
  cultures <- unique(sub("_.*$","",apid_char))
  cultures <- sapply(cultures, function(x) sum(grepl(x,apid_char)))
  abline(v = cumsum(cultures) + 0.5, col = "gray80")
  # text(x = cumsum(cultures) - 0.5*cultures, y = (max(60*plotdat$rval) - min(60*plotdat$rval))*.9 + min(60*plotdat$rval), 
  #      labels = names(cultures), srt = 90)
  axis(side = 1, at = cumsum(cultures) - 0.5*cultures, labels = names(cultures), las = 2, tick = FALSE, cex = 0.75)
  
  # titles and legend
  if(length(setdiff(c("n"),unique(plotdat$wllt)))==0) plot_desc <- paste0(plot_desc, " Control Wells")
  title(main = plot_desc)
  legend(x = "topright", legend = c("control wllq==1","control wllq=0","apid control median"), col = c(rgb(0,0,1,alpha=0.5),rgb(1,0,0,alpha=0.5),rgb(0.1,0.1,0.1,alpha=0.75)),
         cex = 0.65, pch = 19, bg = "transparent")
}


plot_by_apid <- function(plotdat, threshold = NULL) {
  acsni <- unique(plotdat$acsn)
  plot_desc <- acsni
  if(!grepl("DIV",acsni)) plot_desc <- paste0(plot_desc," AUC")
  
  par(oma = c(2,0,1,0), mar = c(4,4.1,1,2.1))
  
  cultures <- unique(sub("_.*$","",plotdat$apid))
  spacer_lvls <- sort(paste0(cultures, rep(c("_A","_B","_Y","_Z"),times = rep(length(cultures),4))))
  plotdat$apid <- factor(plotdat$apid, levels = sort(c(unique(plotdat$apid),spacer_lvls)), ordered = T)
  
  # plotdat$apid <- factor(plotdat$apid, levels = sort(unique(plotdat$apid)), ordered = T)
  stripchart(rval ~ apid, plotdat, vertical = T, pch = "", las = 2, cex = 1, cex.axis = 0.85, xaxt = "n", ylab = sub("CCTE_Shafer_MEA_dev_","",plot_desc))
  stripchart(rval ~ apid, plotdat[wllq == 0], vertical = T, pch = 19, las = 2, col = rgb(1,0,0,alpha=0.3), 
             cex = 1, cex.axis = 0.85, add = T)
  stripchart(rval ~ apid, plotdat[wllq == 1], vertical = T, pch = 19, las = 2, col = rgb(0,0,1,alpha=0.3), 
             cex = 1, cex.axis = 0.85, add = T)
  
  # add the median from each plate
  stripchart(bval ~ apid, plotdat[wllt == "n" & wllq == 1, .(bval = median(rval)), by = .(apid)], vertical = T, pch = 19, las = 2, col = rgb(0.1,0.1,0.1,alpha=0.75), 
             cex = 1, cex.axis = 0.85, add = T)
  
  # add horizontal threshold line, if present
  abline(h = threshold, lty = "dashed", lwd = 2)
  
  # show the separations in culture date
  apid_char <- attr(plotdat$apid, which = "levels")
  cultures <- unique(sub("_.*$","",apid_char))
  cultures <- sapply(cultures, function(x) sum(grepl(x,apid_char)))
  abline(v = cumsum(cultures) + 0.5, col = "gray80")
  # text(x = cumsum(cultures) - 0.5*cultures, y = (max(60*plotdat$rval) - min(60*plotdat$rval))*.9 + min(60*plotdat$rval), 
  #      labels = names(cultures), srt = 90)
  axis(side = 1, at = cumsum(cultures) - 0.5*cultures, labels = names(cultures), las = 2, tick = FALSE, cex = 0.6)
  
  # titles and legend
  if(length(setdiff(c("n"),unique(plotdat$wllt)))==0) plot_desc <- paste0(plot_desc, " Control Wells")
  title(main = plot_desc)
  legend(x = "topright", legend = c("control wllq==1","control wllq=0","apid control median"), col = c(rgb(0,0,1,alpha=0.3),rgb(1,0,0,alpha=0.3),rgb(0.1,0.1,0.1,alpha=0.75)),
         cex = 0.65, pch = 19, bg = "transparent")
}
plotdat <- alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12" & wllt == "n"]
plotdat[, rval := rval*60]
plot_by_apid(plotdat, threshold = 50)
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12" & wllt == "n"], threshold = 10)
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_burst_rate" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_network_spike_number" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_network_spike_number_DIV12" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_network_spike_peak" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_mutual_information_norm" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_network_spike_number" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_correlation_coefficient_mean" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_LDH" & wllt == "n"])
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_AB" & wllt == "n"])


# boxplot of all points, show all apid medians
boxplot_by_culture <- function(plotdat, threshold = NULL) {
  acsni <- unique(plotdat$acsn)
  plot_desc <- acsni
  if(!grepl("DIV",acsni)) plot_desc <- paste0(plot_desc," AUC")
  
  par(oma = c(2,0,1,0), mar = c(4,4.1,1,2.1))
  
  plotdat[, culture := sub("_.*$","",apid)]
  plotdat$culture <- factor(plotdat$culture, levels = sort(c(unique(plotdat$culture))), ordered = T)
  
  stripchart(rval ~ culture, plotdat[wllq == 1], vertical = T, pch = "", las = 2, 
             cex = 1, cex.axis = 0.85, method = "jitter", xlab = "culture date")
  stripchart(bval ~ culture, plotdat[wllt == "n" & wllq == 1, .(bval = median(rval)), by = .(apid, culture)], vertical = T, pch = 19, las = 2, col = rgb(0.1,0.1,0.1,alpha=0.75), 
             cex = 1, cex.axis = 0.85, method = "jitter", add = T)
  boxplot(rval ~ culture, plotdat[wllq == 1], col = rgb(.7,.7,.7,.2), add = T, xaxt = "n", yaxt = "n", outline = F)
  
  # add horizontal threshold line, if present
  abline(h = threshold, lty = "dashed", lwd = 2)
  
  # titles and legend
  if(length(setdiff(c("n"),unique(plotdat$wllt)))==0) plot_desc <- paste0(plot_desc, " Control Wells")
  title(main = plot_desc)
  legend(x = "topright", legend = c("control wllq==1","control wllq=0","apid control median"), col = c(rgb(0,0,1,alpha=0.5),rgb(1,0,0,alpha=0.5),rgb(0.1,0.1,0.1,alpha=0.75)),
         cex = 0.65, pch = 19, bg = "transparent")
}
plotdat <- alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12" & wllt == "n"]
plotdat[, rval := rval*60]
boxplot_by_culture(plotdat, threshold = 50)
boxplot_by_culture(alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean" & wllt == "n"])

boxplot_by_culture(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12" & wllt == "n"], threshold = 10)
boxplot_by_culture(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number" & wllt == "n"])
boxplot_by_culture(alldat[acsn == "CCTE_Shafer_MEA_dev_network_spike_peak" & wllt == "n"])


# Compounds with variable solvent controls ---------------------------------------------------
# (will return this this once I know how exactly how the control treatment spids will be assigned)

# group by corresponding solvent control (i.e., was a compound dissolved in different things in diff cultures?) Should this data be combined, or kept separate?
trt_apid_rowi_summayr <- alldat[wllt == "t", .N, by = .(treatment, spid, apid, rowi, dataset)]
control_summary <- alldat[wllt == "n", .N, by = .(treatment, spid, apid, rowi, dataset)]
solvent_summary <- merge(trt_apid_rowi_summayr, control_summary, by = c("apid","rowi","dataset"), suffixes = c(".trt",".cntrl"), all = T)
solvent_summary[, .(length(unique(spid.cntrl))), by = .(spid.trt)][V1 != 1]
# spid.trt V1
# 1: EX000404  2
# 2: EX000411  2
solvent_summary[spid.trt %in% c("EX000404","EX000411")][order(spid.trt, dataset, apid)]
alldat[treatment == "Loperamide", unique(spid)]
# [1] "EX000411"

col_map <- data.table(dataset = unique(alldat$dataset), 
                      col = unlist(lapply(c("black","blue","green","red","orange","purple","cyan"), function(col) rgb(t(col2rgb(col))/255, alpha = 0.5))))
view_replicates_by_dose <- function(alldat, use_treatment = NULL, use_spid = NULL, acsni = "CCTE_Shafer_MEA_dev_firing_rate_mean") {
  plotdat <- alldat[spid %in% use_spid | treatment %in% use_treatment]
  datasets <- unique(plotdat$dataset)
  plotdat[, conc_index := signif(log10(conc),3)]
  plotdat$conc_index <- factor(plotdat$conc_index, levels = sort(unique(plotdat$conc_index)), ordered = T)
  stripchart(rval ~ conc_index, plotdat[acsn == acsni & wllq == 1], vertical = T, pch = "", xaxt = "n", xlab = "conc (uM)")
  for (dataseti in datasets) {
    stripchart(rval ~ conc_index, plotdat[acsn == acsni & wllq == 1 & dataset == dataseti], vertical = T, pch = 19, xaxt = "n", xlab = "conc (uM)", 
               col = col_map[dataset == dataseti, col], add = T)
  }
  abline(h = alldat[wllt == "n" & acsn == acsni & wllq == 1, median(rval)])
  text(x = length(unique(signif(plotdat$conc))), y = alldat[wllt == "n" & acsn == acsni & wllq == 1, median(rval)], labels = "median of all controls", cex = 0.7, adj = c(1,-0.2))
  title(main = paste0(unique(plotdat$treatment)[1]," MFR AUC by dose\nfrom ",paste0(unique(plotdat$dataset),collapse=", ")))
  axis(side = 1, at = seq(1, length(unique(signif(plotdat$conc)))), labels = sort(unique(signif(plotdat$conc,3))))
  legend(x = "topright", legend = c(datasets), col = col_map[match(datasets, dataset), col], pch = 19, bg = "transparent", cex = 0.8)
  rm(list= c("plotdat"))
}

# Acetaminophen
alldat[treatment == "Acetaminophen", .N, by = "dataset"]
adat <- alldat[treatment == "Acetaminophen"]
stripchart(rval ~ signif(log10(conc),3), adat[grepl("firing_rate_mean$",acsn) & wllq == 1], vertical = T, pch = 1, xaxt = "n", xlab = "conc (uM)")
abline(h = alldat[wllt == "n" & grepl("firing_rate_mean$",acsn) & wllq == 1, median(rval)])
text(x = 1, y = alldat[wllt == "n" & grepl("firing_rate_mean$",acsn) & wllq == 1, median(rval)], labels = "median of all controls", cex = 0.7, adj = c(0,-0.2))
title(main = paste0("Acetaminophen MFR AUC by dose from ",paste0(unique(adat$dataset),collapse=", ")))
axis(side = 1, at = seq(1, length(unique(signif(adat$conc)))), labels = sort(unique(signif(adat$conc,3))))
# as far as the plot/hit call, I think it would be fine to include or not include all of the replicates

# Loperamide
ldat <- alldat[spid %in% c("EX000411", "EX000492")]
stripchart(rval ~ signif(log10(conc),3), ldat[grepl("firing_rate_mean$",acsn) & wllq == 1], vertical = T, pch = 19, xaxt = "n", xlab = "conc (uM)", col = rgb(0.5,0.5,0.5,0.5))
abline(h = alldat[wllt == "n" & grepl("firing_rate_mean$",acsn) & wllq == 1, median(rval)])
text(x = length(unique(signif(ldat$conc))), y = alldat[wllt == "n" & grepl("firing_rate_mean$",acsn) & wllq == 1, median(rval)], labels = "median of all controls", cex = 0.7, adj = c(1,-0.2))
title(main = paste0("Loperamide MFR AUC by dose\nfrom ",paste0(unique(ldat$dataset),collapse=", ")))
axis(side = 1, at = seq(1, length(unique(signif(ldat$conc)))), labels = sort(unique(signif(ldat$conc,3))))
# as far as the plot/hit call, I think it would be fine to include or not include all of the replicates
rm(list= c("ldat","adat"))


# Confirm SPIDs that are used in multiple datasets --------------------------------------------
# (confirm lot number, etc., matches for each case)
alldat[wllt == "t", .(num_datasets = length(unique(dataset)), trt = paste0(unique(treatment)[1],collapse=","), datasets = paste0(sort(unique(dataset)),collapse=",")), by = .(spid)][num_datasets != 1]
#        spid num_datasets                                   trt                               datasets
# 1: EX000404            3                         Acetaminophen           Brown2014,Frank2017,PFAS2018
# 2: EX000475            2                 Bisindolylmaleimide I                    Brown2014,Frank2017
# 3: EX000487            2                         L-Domoic acid                    Brown2014,Frank2017
# 4: EX000411            4                            Loperamide Brown2014,DNTGF2019,Frank2017,PFAS2018
# 5: EX000498            2                            Mevastatin                    Brown2014,Frank2017
# 6: EX000499            2                  Sodium orthovanadate                    Brown2014,Frank2017
# 7: EX000408            2                            Glyphosate                    DNTGF2019,Frank2017
# 8: EX000361            2 Tris (2-chloroethyl) phosphate (TCEP)                        Frank2017,NTP91
# 9: EX000420            2                           Bisphenol A                     Frank2017,PFAS2018
# 10: EX000362            2                           Valinomycin                      NTP91,ToxCast2016
# Glyphosate and BPA are my only concerns... will run this by Tim


# any SPID repeated many times (likely assay controls) - decide if keep all data -------------------------
alldat[wllq == 1 & wllt == "t", .(num_apid = length(unique(apid)), trt = unique(treatment)[1]), by = .(spid)][num_apid > 6][order(-num_apid)]
# spid num_apid                    trt
# 1: EX000411       22             Loperamide
# 2: EX000404       21          Acetaminophen
# 3: EX000374        9 L-Glufosinate Ammonium
# 4: EX000408        9             Glyphosate
# 5: EX000413        9   Cytosine Arabinoside
# 6: EX000420        9            Bisphenol A
# 7: EX000475        7  Bisindolylmaleimide I
# 8: EX000487        7          L-Domoic acid
# 9: EX000498        7             Mevastatin
# 10: EX000499        7   Sodium orthovanadate

# L-Glufosinate
view_replicates_by_dose(alldat, use_spid = "EX000374")
# ya, not worth stressing over. We'll keep all of these

# Glyphosate
view_replicates_by_dose(alldat, use_spid = "EX000408")

alldat[spid == "EX000413", unique(dataset)] # "Frank2017". This is fine to include all I think

# bPA - looks okay acutally. Def not the same, but okay
view_replicates_by_dose(alldat, use_spid = "EX000420")
view_replicates_by_dose(alldat, use_spid = "EX000475")
view_replicates_by_dose(alldat, use_spid = "EX000487")
view_replicates_by_dose(alldat, use_spid = "EX000411")
view_replicates_by_dose(alldat, use_spid = "EX000404")
# I think that all of these are fine, just run it by Tim
graphics.off()
pdf(file = file.path(root_output_dir, paste0("assay_controls_mfr_auc_replicates_oct19_2020.pdf")))
for (spidi in alldat[wllq == 1 & wllt == "t", .(num_apid = length(unique(apid)), trt = unique(treatment)[1]), by = .(spid)][num_apid > 6][order(-num_apid), spid]) {
  view_replicates_by_dose(alldat, use_spid = spidi)
}
graphics.off()
