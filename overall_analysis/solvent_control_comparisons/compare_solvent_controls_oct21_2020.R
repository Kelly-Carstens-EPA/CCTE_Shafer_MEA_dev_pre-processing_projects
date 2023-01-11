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
alldat[treatment == "Loperamide", spid := "EX000411"] # changing this in Brown2014, because casrn actually corresponds to Lop hydrochloride


# Compare Solvent Controls -----------------------------------------
alldat[wllt == "n", .N, by = .(treatment, conc)]
# treatment    conc      N
# 1:         DMSO 0.00100 116625
# 2:        Water 0.00100   9633
# 3:         DMSO 0.00146   5742
# 4: DMSO/Ethanol 0.00100    261
# 5:      Ethanol 0.00100    261

alldat[wllt == "n" & conc == 0.00146, unique(dataset)] # "DNTGF2019"
alldat[wllt == "n" & treatment == "Water", .N/87, by = .(dataset)]
# dataset       V1
# 1: Brown2014 23.72414
# 2: DNTGF2019 42.00000
# 3: Frank2017 45.00000
alldat[grepl("20150819",apid) & treatment == "Water", unique(rowi)] # 1

alldat[wllt == "n" & treatment == "Water", .N/87, by = .(apid)]

compare_controls <- function(alldat, acsni, use_dataset = NULL) {
  cat(acsni,"\n")
  plotdat <- alldat[wllt == "n" & wllq == 1 & acsn == acsni]
  if(!is.null(use_dataset)) plotdat <- plotdat[dataset == use_dataset]
  plotdat[, trt_conc := paste0(treatment, "\n",signif(conc,2)*100,"%")]
  stripchart(rval ~ trt_conc, plotdat, vertical = T, pch = 1, method = "jitter", col = "gray50")
  boxplot(rval ~ trt_conc, plotdat, col = "transparent", add = T, boxwex = 0.3, outline = FALSE)
  title(main = paste(acsni, "Control Well Comparison",use_dataset,sep = " "))
  
  cat("DMSO 0.1% vs DMSO 0.15%\n")
  print(t.test(plotdat[trt_conc == "DMSO\n0.1%",rval], 
               plotdat[trt_conc == "DMSO\n0.15%",rval],
               alternative = "two.sided", paired = FALSE))
  
  cat("\nDMSO 0.1% vs Water 0.1%\n")
  print(t.test(plotdat[trt_conc == "DMSO\n0.1%",rval], 
               plotdat[trt_conc == "Water\n0.1%",rval],
               alternative = "two.sided", paired = FALSE))
}

compare_controls(alldat, "CCTE_Shafer_MEA_dev_firing_rate_mean")
compare_controls(alldat, "CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12")
compare_controls(alldat, "CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12")
compare_controls(alldat, "CCTE_Shafer_MEA_dev_active_electrodes_number")
compare_controls(alldat, "CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12")
compare_controls(alldat, "CCTE_Shafer_MEA_dev_network_spike_peak")
compare_controls(alldat, "CCTE_Shafer_MEA_dev_network_spike_peak_DIV12")
compare_controls(alldat, "CCTE_Shafer_MEA_dev_LDH") # significan tdiff
compare_controls(alldat, "CCTE_Shafer_MEA_dev_AB") # noticeable diff

plotdat[trt_conc == "DMSO\n0.1%", IQR(rval)]

?t.test
alldat[, trt_conc := paste0(treatment, "\n",signif(conc,2)*100,"%")]
standard_dmso <- alldat[wllq == 1 & acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean" & trt_conc == "DMSO\n0.1%",rval]
higher_conc_dmso <- alldat[wllq == 1 & acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean" & trt_conc == "DMSO\n0.15%",rval]
water <- alldat[wllq == 1 & acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean" & trt_conc == "Water\n0.1%",rval]
t.test(standard_dmso,
       higher_conc_dmso,
       alternative = "two.sided", paired = FALSE)
# Welch Two Sample t-test
# 
# data:  standard_dmso and higher_conc_dmso
# t = 3.8159, df = 73.163, p-value = 0.0002814
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.029408 3.280156
# sample estimates:
#   mean of x mean of y 
# 9.957069  7.802287
t.test(standard_dmso,
       water,
       alternative = "two.sided", paired = FALSE)
# Welch Two Sample t-test
# 
# data:  standard_dmso and water
# t = -2.1207, df = 131.4, p-value = 0.03582
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.89971521 -0.06606819
# sample estimates:
#   mean of x mean of y 
# 9.957069 10.939961

# same them all
graphics.off()
pdf(file = file.path(plot_output_dir,paste0("mea_nfa_solvent_controls_comparison.pdf")))
plot_acsns <- unique(alldat$acsn)[!grepl("DIV[579]",unique(alldat$acsn))]
for (acsni in plot_acsns) {
  compare_controls(alldat, acsni)
}
graphics.off()

# there does seem to be a diff in the 0.15% DMSO wells...
# is the diff more because of the culture/time, or becaue it is truly different?
compare_controls(alldat, "CCTE_Shafer_MEA_dev_active_electrodes_number")
compare_controls(alldat, "CCTE_Shafer_MEA_dev_active_electrodes_number", use_dataset = "DNTGF2019")
# ah, so the Water wells might be signficantly different

# Question - do the DMSO 0.15% wells vary from the 0.1% DMSO wells by more than the inter-plate or inter-culture variability?
#  are teh 0.15% DMSO wells consistently the lowest in each apid?
plotdat <- alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number" & wllq == 1 & wllt == "n" & dataset == "DNTGF2019"]
plotdat$apid <- factor(sub("_","\n",plotdat$apid), levels = sort(unique(sub("_","\n",plotdat$apid))), ordered = T)
stripchart(rval ~ apid, plotdat[treatment == "DMSO"], vertical = T, pch = "", las = 2, cex.axis = 0.8)
stripchart(rval ~ apid, plotdat[treatment == "DMSO" & conc == 0.001], vertical = T, pch = 19, col = rgb(0.5,0.5,0.5,alpha=0.5), add = T)
stripchart(rval ~ apid, plotdat[treatment == "DMSO" & conc == 0.00146], vertical = T, pch = 19, col = rgb(1,0,0,alpha=0.5),add = T)
stripchart(rval ~ apid, plotdat[treatment == "Water"], vertical = T, pch = 19, col = rgb(0,0,1,alpha=0.5),add = T)

stripchart(rval ~ dataset, alldat[treatment == "DMSO" & conc == 0.001 & wllt == "n" & wllq == 1 & acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number"], vertical = T, method = "jitter", pch = 1, col = "gray50")
boxplot(rval ~ dataset, alldat[treatment == "DMSO" & conc == 0.001 & wllt == "n" & wllq == 1 & acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number"], add = T, col = "transparent", boxwex = 0.3)


# 10/30/2020 -------------------------------
# checkout the 
acsni <- "CCTE_Shafer_MEA_dev_active_electrodes_number"
acsni <- "CCTE_Shafer_MEA_dev_firing_rate_mean"
check_apids <- alldat[wllt == "n", .(length(unique(treatment)), length(unique(conc))), by = "apid"][V1 > 1 | V2 > 1, unique(apid)]
plotdat <- alldat[apid %in% check_apids & wllt == "n" & acsn == acsni]
plotdat$apid <- factor(plotdat$apid, levels = sort(unique(plotdat$apid)), ordered = T)
par(mar = c(7, 4.1, 4.1, 2.1))
stripchart(rval ~ apid, plotdat[wllq == 1], vertical = T, pch = "", las = 2, cex.axis = 0.8)
stripchart(rval ~ apid, plotdat[wllq == 1 & treatment == "DMSO" & conc == 0.001], vertical = T, pch = 19, col = rgb(0.5,0.5,0.5,alpha=0.5), add = T)
stripchart(rval ~ apid, plotdat[wllq == 1 & treatment == "DMSO" & conc == 0.00146], vertical = T, pch = 19, col = rgb(1,0,0,alpha=0.5), add = T, cex = 0.8)
stripchart(rval ~ apid, plotdat[wllq == 1 & treatment == "Water"], vertical = T, pch = 19, col = rgb(0,0,1,alpha=0.5), add = T, cex = 0.8)
stripchart(rval ~ apid, plotdat[wllq == 1 & treatment %in% c("DMSO/Ethanol","Ethanol")], vertical = T, pch = 19, col = rgb(0,.8,.2,alpha=0.5), add = T, cex = 0.8)
legend(x = "topright", legend = c("DMSO 0.1%","DMSO 0.146%","Water 0.1%","Ethanol or DMSO/Ethanol"), 
       col = c(rgb(0.5,0.5,0.5,alpha=0.5),rgb(1,0,0,alpha=0.5),rgb(0,0,1,alpha=0.5),rgb(0,.8,.2,alpha=0.5)),
       pch = 19, bg = "transparent", cex = 0.6)
title(main = paste0("Apid with Multiple Solvent Types\n",acsni))

# let's see if this normalized by apid
plotdat[, norm_rval := (rval - median(rval))/median(rval), by = "apid"]
stripchart(norm_rval ~ apid, plotdat[wllq == 1], vertical = T, pch = "", las = 2, cex.axis = 0.8)
abline(h = 1)
stripchart(norm_rval ~ apid, plotdat[wllq == 1 & treatment == "DMSO" & conc == 0.001], vertical = T, pch = 19, col = rgb(0.5,0.5,0.5,alpha=0.5), add = T)
stripchart(norm_rval ~ apid, plotdat[wllq == 1 & treatment == "DMSO" & conc == 0.00146], vertical = T, pch = 19, col = rgb(1,0,0,alpha=0.5),add = T)
stripchart(norm_rval ~ apid, plotdat[wllq == 1 & treatment == "Water"], vertical = T, pch = 19, col = rgb(0,0,1,alpha=0.5),add = T)
stripchart(norm_rval ~ apid, plotdat[wllq == 1 & treatment %in% c("DMSO/Ethanol","Ethanol")], vertical = T, pch = 19, col = rgb(0,.8,.2,alpha=0.5),add = T)
legend(x = "topright", legend = c("DMSO 0.1%","DMSO 0.146%","Water 0.1%","Ethanol or DMSO/Ethanol"), 
       col = c(rgb(0.5,0.5,0.5,alpha=0.5),rgb(1,0,0,alpha=0.5),rgb(0,0,1,alpha=0.5),rgb(0,.8,.2,alpha=0.5)),
       pch = 19, bg = "transparent", cex = 0.6)
title(main = paste0("Apid with Multiple Solvent Types\nNormalized ",acsni))

# save plots
plot_output_dir <- file.path(root_output_dir,"overall_analysis","solvent_control_comparisons","plots")
graphics.off()
pdf(file.path(plot_output_dir,paste0("apid_with_multiple_solvent_types_AUC_",as.character.Date(Sys.Date()),".pdf")), width = 11.5, height = 8)
par(mar = c(7, 4.1, 4.1, 2.1))
acsns <- sort(unique(alldat$acsn))
acsns <- acsns[!grepl("DIV",acsns)]
for (acsni in acsns) {
  check_apids <- alldat[wllt == "n", .(length(unique(treatment)), length(unique(conc))), by = "apid"][V1 > 1 | V2 > 1, unique(apid)]
  plotdat <- alldat[apid %in% check_apids & wllt == "n" & acsn == acsni]
  plotdat$apid <- factor(plotdat$apid, levels = sort(unique(plotdat$apid)), ordered = T)
  stripchart(rval ~ apid, plotdat[wllq == 1], vertical = T, pch = "", las = 2, cex.axis = 0.5)
  stripchart(rval ~ apid, plotdat[wllq == 1 & treatment == "DMSO" & conc == 0.001], vertical = T, pch = 19, col = rgb(0.5,0.5,0.5,alpha=0.5), add = T, cex = 0.8)
  stripchart(rval ~ apid, plotdat[wllq == 1 & treatment == "DMSO" & conc == 0.00146], vertical = T, pch = 19, col = rgb(1,0,0,alpha=0.5),add = T, cex = 0.8)
  stripchart(rval ~ apid, plotdat[wllq == 1 & treatment == "Water"], vertical = T, pch = 19, col = rgb(0,0,1,alpha=0.5),add = T, cex = 0.8)
  stripchart(rval ~ apid, plotdat[wllq == 1 & treatment %in% c("DMSO/Ethanol","Ethanol")], vertical = T, pch = 19, col = rgb(0,.8,.2,alpha=0.5),add = T, cex = 0.8)
  legend(x = "topright", legend = c("DMSO 0.1%","DMSO 0.146%","Water 0.1%","Ethanol or DMSO/Ethanol"), 
         col = c(rgb(0.5,0.5,0.5,alpha=0.5),rgb(1,0,0,alpha=0.5),rgb(0,0,1,alpha=0.5),rgb(0,.8,.2,alpha=0.5)),
         pch = 19, bg = "transparent", cex = 0.6)
  title(main = paste0("Apid with Multiple Solvent Types\n",acsni,ifelse(!grepl("(DIV)|(AB)|(LDH)",acsni)," AUC","")))
}
graphics.off()

graphics.off()
pdf(file.path(plot_output_dir,paste0("apid_with_multiple_solvent_types_DIV12_",as.character.Date(Sys.Date()),".pdf")), width = 11.5, height = 8)
par(mar = c(7, 4.1, 4.1, 2.1))
acsns <- sort(unique(alldat$acsn))
acsns <- grep("DIV12",acsns,val=T)
for (acsni in acsns) {
  check_apids <- alldat[wllt == "n", .(length(unique(treatment)), length(unique(conc))), by = "apid"][V1 > 1 | V2 > 1, unique(apid)]
  plotdat <- alldat[apid %in% check_apids & wllt == "n" & acsn == acsni]
  plotdat$apid <- factor(plotdat$apid, levels = sort(unique(plotdat$apid)), ordered = T)
  stripchart(rval ~ apid, plotdat[wllq == 1], vertical = T, pch = "", las = 2, cex.axis = 0.5)
  stripchart(rval ~ apid, plotdat[wllq == 1 & treatment == "DMSO" & conc == 0.001], vertical = T, pch = 19, col = rgb(0.5,0.5,0.5,alpha=0.5), add = T, cex = 0.8)
  stripchart(rval ~ apid, plotdat[wllq == 1 & treatment == "DMSO" & conc == 0.00146], vertical = T, pch = 19, col = rgb(1,0,0,alpha=0.5),add = T, cex = 0.8)
  stripchart(rval ~ apid, plotdat[wllq == 1 & treatment == "Water"], vertical = T, pch = 19, col = rgb(0,0,1,alpha=0.5),add = T, cex = 0.8)
  stripchart(rval ~ apid, plotdat[wllq == 1 & treatment %in% c("DMSO/Ethanol","Ethanol")], vertical = T, pch = 19, col = rgb(0,.8,.2,alpha=0.5),add = T, cex = 0.8)
  legend(x = "topright", legend = c("DMSO 0.1%","DMSO 0.146%","Water 0.1%","Ethanol or DMSO/Ethanol"), 
         col = c(rgb(0.5,0.5,0.5,alpha=0.5),rgb(1,0,0,alpha=0.5),rgb(0,0,1,alpha=0.5),rgb(0,.8,.2,alpha=0.5)),
         pch = 19, bg = "transparent", cex = 0.6)
  title(main = paste0("Apid with Multiple Solvent Types\n",acsni,ifelse(!grepl("(DIV)|(AB)|(LDH)",acsni)," AUC","")))
}
graphics.off()


# view the different control types alongside cndx 1, 2, etc?
acsni <- "CCTE_Shafer_MEA_dev_firing_rate_mean"
plotdat <- alldat[acsn == acsni & wllq == 1]
plotdat[, trt_conc := ifelse(wllt == "n",paste0(treatment,"_",signif(as.numeric(conc),2)),signif(as.numeric(conc),1))]
trt_concs <- unique(plotdat$trt_conc)
plotdat$trt_conc <- factor(plotdat$trt_conc, levels = c(sort(trt_concs[is.na(as.numeric(trt_concs))]), sort(as.numeric(trt_concs[!is.na(as.numeric(trt_concs))]))), ordered = T)
boxplot(rval ~ trt_conc, plotdat, las = 2)
# once again, I think it is difficult to differentiate between the differences due to culture date
# and the actual distinction caused by the variable control solvent


# what if I compare the normalized control well values??
compare_controls_norm <- function(alldat, acsni, use_dataset = NULL) {
  cat(acsni,"\n")
  plotdat <- alldat[wllt == "n" & wllq == 1 & acsn == acsni]
  if(!is.null(use_dataset)) plotdat <- plotdat[dataset == use_dataset]
  plotdat[, trt_conc := paste0(treatment, "\n",signif(conc,2)*100,"%")]
  plotdat[, resp := (rval - median(rval))/median(rval), by = "apid"]
  stripchart(resp ~ trt_conc, plotdat, vertical = T, pch = 1, method = "jitter", col = "gray50")
  boxplot(resp ~ trt_conc, plotdat, col = "transparent", add = T, boxwex = 0.3, outline = FALSE)
  title(main = paste("Normalized ",acsni, "\nControl Well Comparison",use_dataset,sep = " "))
}

graphics.off()
pdf(file.path(plot_output_dir,paste0("mea_nfa_solvent_controls_comparison_normalized_",as.character.Date(Sys.Date()),".pdf")), width = 11.5, height = 8)
acsns <- sort(unique(alldat$acsn))
acsns <- acsns[!grepl("DIV",acsns)]
for (acsni in acsns) {
  compare_controls_norm(alldat, acsni)
}
graphics.off()

