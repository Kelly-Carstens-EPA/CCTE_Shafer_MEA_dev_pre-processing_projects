dataset_checks <- function(dat) {
  
  # this section is to confirm that the data has been processed correctly
  cat("\nFinal Checks\n")
  cat("Number of cultures dates:",dat[, length(unique(sub("_.+$","",apid)))])
  cat("\nRange of culture dates:", dat[, range(sub("_.+$","",apid))] )
  cat("\nNumber of plates tested:",dat[, length(unique(apid))])
  cat("\nNumber of compounds tested:",dat[wllt == "t", length(unique(spid))])
  cat("\nAny NA rvals? ")
  print(dat[is.na(rval), .N, by = "wllq"])
  cat("\nWllq breakdown for all points:\n")
  print(dat[, .N, by = "wllq"]) # note if wllq is NA anywhere
  cat("Number of unique acsn's present:",length(unique(dat$acsn)),"\n")
  check.points <- dcast(dat[, .N, by = c("acsn","apid")], apid ~ acsn, value.var = "N", fill = 0)
  setnames(check.points, old = names(check.points), new = sub("CCTE_Shafer_MEA_dev_","",names(check.points)))
  cat(paste0("The following plates don't have the expected number of points (48):\n"))
  standard_cols <- setdiff(names(check.points), c("apid"))
  pts_flag <- FALSE
  for (apidi in unique(check.points$apid)) {
    if (check.points[apid == apidi, any(.SD != 48), .SDcols = c(standard_cols)]) {
      pts_flag <- TRUE
      MEA_pts <- check.points[apid == apidi, .(sort(unique(.SD))), .SDcols = setdiff(standard_cols, c("AB","LDH"))]
      print(check.points[apid == apidi, .(apid, AB, LDH, MEA_pts = paste0(sort(unique(unlist(MEA_pts))),collapse=","))])
    }
  }
  if(!pts_flag) {
    cat("(all plates have the expected number of points for each assay component)\n")
  }
  
  # check number of controls per plate
  cat("\nApid/acsn pairs without 6 control wells:\n")
  print(dat[wllt == "n", .N, by = c("acsn","apid")][N != 6])
  
  # range by acsn
  cat("\nRange of rval's by acsn:\n")
  print(dat[wllq == 1, .(min = format(min(rval,na.rm=T),digits=2,scientific = F), 
                   median = format(median(rval,na.rm=T),digits=2,scientific = F),
                   max = format(max(rval,na.rm=T),digits=2,scientific = F),
                   num_NA = sum(is.na(rval))), by = "acsn"][order(acsn)])
  
  # PLOTS to visually confirm results
  
  # view all by plate
  stripchart(rval ~ sub("_","\n",apid), dat[wllq == 1 & wllt == "t" & acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean"], vertical = T, pch = 1, method = "jitter", las = 2, cex.axis = 0.75,
             col = "blue", main = paste0(dataset_title," NFA Mean Firing Rate AUC by Plate"))
  stripchart(rval ~ sub("_","\n",apid), dat[wllq == 1 & wllt == "n" & acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean"], vertical = T, pch = 19, method = "jitter", las = 2, cex.axis = 0.75,
             add = T)
  legend(x = "topright", legend = c("control","all treated"), col = c("black","blue"), pch = c(19,1), bg = "transparent")
  
  stripchart(rval ~ sub("_","\n",apid), dat[wllq == 1 & wllt == "t" & acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number"], vertical = T, pch = 1, method = "jitter", las = 2, cex.axis = 0.75,
             col = "blue", main = paste0(dataset_title," NFA # Active Electrodes AUC by Plate"))
  stripchart(rval ~ sub("_","\n",apid), dat[wllq == 1 & wllt == "n" & acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number"], vertical = T, pch = 19, method = "jitter", las = 2, cex.axis = 0.75,
             add = T)
  legend(x = "topright", legend = c("control","all treated"), col = c("black","blue"), pch = c(19,1), bg = "transparent")
  
  # define 'plotdat' - of the AUC MFR, with specialized conc group labels
  plotdat <- dat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean"]
  plotdat[, conc_grp := ifelse(wllt == "n",paste0(treatment,"\n",conc),signif(conc,1))]
  conc_grps <- unique(plotdat$conc_grp)
  plotdat$conc_grp <- factor(plotdat$conc_grp, levels = c(grep("\n",conc_grps,val = T),sort(unique(as.numeric(conc_grps[!grepl("\n",conc_grps)])))), ordered = T)
  
  # view all compounds together by dose
  stripchart(rval ~ conc_grp, plotdat[wllq == 1], vertical = T, pch = 1, method = "jitter", las = 2,
             main = paste0("Mean Firing Rate AUC by dose for all compounds in ",dataset_title), ylab = "CCTE_Shafer_MEA_dev_firing_rate_mean (AUC)", xlab = "conc")
  if (plotdat[, any(wllq==0)])
    stripchart(rval ~ signif(conc,1), dat[wllq == 0 & acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean"], vertical = T, pch = 1, method = "jitter",
               add = T, col = "red")
  legend(x = "topright", legend = c("wllq==1","wllq==0"), col = c("black","red"), pch = c(1,1), bg = "transparent")
  
  # find a compound that is likely to be a positive and plot dose response
  plot_spid <- dat[conc == max(conc) & acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean", .(med_rval = median(rval)), by = "spid"][med_rval == min(med_rval), spid[1]]
  plot_plates <- control_dat <- dat[spid == plot_spid, unique(apid)]
  stripchart(rval ~ conc_grp, plotdat[apid %in% plot_plates & (spid == plot_spid | wllt == "n") & wllq == 1], vertical = T, pch = 19, las = 2,
             col = rgb(0.1,0.1,0.1,0.5),
             ylim = range(dat[wllq == 1 & acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean",rval]), ylab = "CCTE_Shafer_MEA_dev_firing_rate_mean (AUC)",
             xlab = "conc", main = paste0(dat[spid == plot_spid,unique(treatment)]," Mean Firing Rate AUC Dose Response"))
  if (plotdat[apid %in% plot_plates & (spid == plot_spid | wllt == "n"), any(wllq==0)])
    stripchart(rval ~ conc_grp, plotdat[apid %in% plot_plates & (spid == plot_spid | wllt == "n") & wllq == 0], vertical = T, pch = 19, las = 2,
               add = TRUE, col = rgb(0.9,0,0,0.5))
  legend(x = "topright", legend = c("wllq==1","wllq==0"), col = c(rgb(0.1,0.1,0.1,0.5),rgb(0.9,0,0,0.5)), pch = c(19,19), bg = "transparent")
  
  # Cytotox
  stripchart(rval ~ signif(conc,1), dat[wllq == 1 & grepl("AB",acsn)], las = 2,
             vertical = TRUE, pch = 1, method = "jitter", xlab = "conc", main = paste0("AB Blank-Corrected Values for ",dataset_title,"\nwhere wllq == 1"))
  if (nrow(dat[wllq == 1 & grepl("LDH",acsn)]) > 0) {
    stripchart(rval ~ signif(conc,1), dat[wllq == 1 & grepl("LDH",acsn)], las = 2,
               vertical = TRUE, pch = 1, method = "jitter", xlab = "conc", main = paste0("LDH Blank-Corrected Values for ",dataset_title,"\nwhere wllq == 1"))
  }
 
}