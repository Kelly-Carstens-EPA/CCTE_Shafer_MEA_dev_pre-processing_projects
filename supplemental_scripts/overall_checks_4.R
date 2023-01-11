# 10/30/2020
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


# PLOTS by APID ---------------------------------------------------------
# want to view the controls median versus apid median

plot_by_apid <- function(plotdat, threshold = NULL, default_cex = 1) {
  acsni <- unique(plotdat$acsn)
  plot_desc <- acsni
  if(!grepl("(DIV)|(AB)|(LDH)",acsni)) plot_desc <- paste0(plot_desc," AUC")
  
  par(oma = c(2,0,1,0), mar = c(4,4.1,1,2.1))
  
  cultures <- unique(sub("_.*$","",plotdat$apid))
  spacer_lvls <- sort(paste0(cultures, rep(c("_A","_B","_Y","_Z"),times = rep(length(cultures),4))))
  plotdat$apid <- factor(plotdat$apid, levels = sort(c(unique(plotdat$apid),spacer_lvls)), ordered = T)

  stripchart(rval ~ apid, plotdat, vertical = T, pch = "", las = 2, cex = default_cex, xaxt = "n", ylab = sub("CCTE_Shafer_MEA_dev_","",plot_desc))
  stripchart(rval ~ apid, plotdat[wllq == 0], vertical = T, pch = 19, las = 2, col = rgb(1,0,0,alpha=0.3), 
             cex = default_cex, add = T)
  stripchart(rval ~ apid, plotdat[wllq == 1], vertical = T, pch = 19, las = 2, col = rgb(0,0,1,alpha=0.3), 
             cex = default_cex, add = T)
  
  # add the median from each plate
  stripchart(bval ~ apid, plotdat[wllt == "n" & wllq == 1, .(bval = median(rval)), by = .(apid)], vertical = T, pch = 19, las = 2, col = rgb(0.1,0.1,0.1,alpha=0.75), 
             cex = default_cex, add = T)
  
  # add horizontal threshold line, if present
  abline(h = threshold, lty = "dashed", lwd = 2)
  
  # show the separations in culture date
  apid_char <- attr(plotdat$apid, which = "levels")
  cultures <- unique(sub("_.*$","",apid_char))
  cultures <- sapply(cultures, function(x) sum(grepl(x,apid_char)))
  abline(v = cumsum(cultures) + 0.5, col = "gray80")
  
  # adding line by apid...
  # abline(v = which(!grepl("_[ABYZ]",apid_char)), col = "gray85")
  
  # text(x = cumsum(cultures) - 0.5*cultures, y = (max(60*plotdat$rval) - min(60*plotdat$rval))*.9 + min(60*plotdat$rval), 
  #      labels = names(cultures), srt = 90)
  axis(side = 1, at = cumsum(cultures) - 0.5*cultures, labels = names(cultures), las = 2, tick = FALSE, cex.axis = default_cex*0.8, hadj = 0.65)
  
  # titles and legend
  if(length(setdiff(c("n"),unique(plotdat$wllt)))==0) plot_desc <- paste0(plot_desc, " Control Wells")
  title(main = plot_desc)
  legend(x = "topright", legend = c("control wllq==1","control wllq=0","apid control median"), col = c(rgb(0,0,1,alpha=0.3),rgb(1,0,0,alpha=0.3),rgb(0.1,0.1,0.1,alpha=0.75)),
         cex = default_cex, pch = 19, bg = "transparent")
}

# view a few
plotdat <- alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12" & wllt == "n"]
plotdat[, rval := rval*60]
plot_by_apid(plotdat, threshold = 50, default_cex)
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12" & wllt == "n"], threshold = 10, default_cex)
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean" & wllt == "n"])
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

# save in pdf
acsns <- unique(alldat$acsn)
use_acsns <- sort(acsns[!grepl("_DIV",acsns)])

graphics.off()
pdf(file.path(root_output_dir,"plots",paste0("plateSN_controls_grouped_by_culture_DIV12.pdf")), width = 11, height = 8.5)
default_cex <- 0.75
plotdat <- alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12" & wllt == "n"]
plotdat[, rval := rval*60]
plot_by_apid(plotdat, threshold = 50, default_cex)
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12" & wllt == "n"], threshold = 10, default_cex)
for (acsni in use_acsns[!grepl("(AB)|(LDH)|(firing_rate_mean)|(active_electrodes_number)",use_acsns)]) {
  plot_by_apid(alldat[acsn == paste0(acsni,"_DIV12") & wllt == "n"], default_cex = default_cex)
}
graphics.off()

# now plot the AUC acsn's
graphics.off()
pdf(file.path(root_output_dir,"plots",paste0("plateSN_controls_grouped_by_culture_AUC.pdf")), width = 11, height = 8.5)
default_cex <- 0.75
for (acsni in use_acsns) {
  plot_by_apid(alldat[acsn == acsni & wllt == "n"], default_cex = default_cex)
}
graphics.off()


# boxplot of all points, show all apid medians -------------------------------------------
boxplot_by_culture <- function(plotdat, threshold = NULL, default_cex = 1) {
  acsni <- unique(plotdat$acsn)
  plot_desc <- acsni
  if(!grepl("DIV",acsni)) plot_desc <- paste0(plot_desc," AUC")
  
  par(oma = c(2,0,1,0), mar = c(4,4.1,1,2.1))
  
  plotdat[, culture := sub("_.*$","",apid)]
  plotdat$culture <- factor(plotdat$culture, levels = sort(c(unique(plotdat$culture))), ordered = T)

  plotdat[wllq == 1, bval := median(rval), by = .(apid, culture)]
  stripchart(rval ~ culture, plotdat[wllq == 1 & rval != bval], vertical = T, pch = 19, las = 2, col = rgb(0.8,.8,.8,.8),
             cex = default_cex, method = "jitter")  
  # stripchart(rval ~ culture, plotdat[wllq == 1], vertical = T, pch = "", las = 2, 
  #            cex = default_cex, method = "jitter")
  stripchart(bval ~ culture, plotdat[wllt == "n" & wllq == 1, .(bval = median(rval)), by = .(apid, culture)], vertical = T, pch = 19, las = 2, col = rgb(0.1,0.1,0.1,alpha=0.75), 
             cex = default_cex, method = "jitter", add = T)
  boxplot(rval ~ culture, plotdat[wllq == 1], col = rgb(.7,.7,.7,.2), add = T, xaxt = "n", yaxt = "n", outline = F)
  
  # add horizontal threshold line, if present
  abline(h = threshold, lty = "dashed", lwd = 2)
  
  # titles and legend
  if(length(setdiff(c("n"),unique(plotdat$wllt)))==0) plot_desc <- paste0(plot_desc, " Control Wells")
  title(main = plot_desc)
  legend(x = "topright", legend = c("control wllq==1","plate control median"), col = c(rgb(0.8,.8,.8,.8),rgb(0.1,0.1,0.1,alpha=0.75)),
         cex = default_cex, pch = 19, bg = "transparent")
}

plotdat <- alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12" & wllt == "n"]
plotdat[, rval := rval*60]
boxplot_by_culture(plotdat, threshold = 50)
boxplot_by_culture(alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean" & wllt == "n"])
boxplot_by_culture(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12" & wllt == "n"], threshold = 10)
boxplot_by_culture(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number" & wllt == "n"])
boxplot_by_culture(alldat[acsn == "CCTE_Shafer_MEA_dev_network_spike_peak" & wllt == "n"])

# save as pdfs
graphics.off()
pdf(file.path(root_output_dir,"plots",paste0("plateSN_controls_boxplots_by_culture_DIV12.pdf")), width = 11, height = 8.5)
default_cex <- 0.75
plotdat <- alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12" & wllt == "n"]
plotdat[, rval := rval*60]
boxplot_by_culture(plotdat, threshold = 50, default_cex)
boxplot_by_culture(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12" & wllt == "n"], threshold = 10, default_cex)
for (acsni in use_acsns[!grepl("(AB)|(LDH)|(firing_rate_mean)|(active_electrodes_number)",use_acsns)]) {
  boxplot_by_culture(alldat[acsn == paste0(acsni,"_DIV12") & wllt == "n"], default_cex = default_cex)
}
graphics.off()


graphics.off()
pdf(file.path(root_output_dir,"plots",paste0("plateSN_controls_boxplots_by_culture_AUC.pdf")), width = 11, height = 8.5)
default_cex <- 0.75
for (acsni in use_acsns) {
  boxplot_by_culture(alldat[acsn == acsni & wllt == "n"], default_cex = default_cex)
}
graphics.off()