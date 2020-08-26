dataset_checks <- function(dat) {
  
  # this section is to confirm that the data has been processed correctly
  cat("\nFinal Checks\n")
  cat("Number of cultures dates:",dat[, length(unique(sub("_.+$","",apid)))])
  cat("\nRange of culture dates:", dat[, range(sub("_.+$","",apid))] )
  cat("\nNumber of plates tested:",dat[, length(unique(apid))])
  cat("\nNumber of compounds tested:",dat[wllt == "t", length(unique(spid))])
  cat("\nWllq breakdown:\n")
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
  print(dat[wllq == 1, .(min = format(min(rval),digits=2,scientific = F), 
                   median = format(median(rval),digits=2,scientific = F),
                   max = format(max(rval),digits=2,scientific = F)), by = "acsn"])
  
  # PLOTS to visually confirm results
  
  # view all by plate
  stripchart(rval ~ sub("_","\n",apid), dat[wllq == 1 & wllt == "t" & grepl("firing_rate_mean",acsn)], vertical = T, pch = 1, method = "jitter", las = 2, cex.axis = 0.75,
             col = "blue", main = paste0(dataset_title," NFA Mean Firing Rate AUC by Plate"))
  stripchart(rval ~ sub("_","\n",apid), dat[wllq == 1 & wllt == "n" & grepl("firing_rate_mean",acsn)], vertical = T, pch = 19, method = "jitter", las = 2, cex.axis = 0.75,
             add = T)
  legend(x = "topright", legend = c("control","all treated"), col = c("black","blue"), pch = c(19,1), bg = "transparent")
  
  stripchart(rval ~ sub("_","\n",apid), dat[wllq == 1 & wllt == "t" & grepl("dev_active_electrodes_number",acsn)], vertical = T, pch = 1, method = "jitter", las = 2, cex.axis = 0.75,
             col = "blue", main = paste0(dataset_title," NFA # Active Electrodes AUC by Plate"))
  stripchart(rval ~ sub("_","\n",apid), dat[wllq == 1 & wllt == "n" & grepl("dev_active_electrodes_number",acsn)], vertical = T, pch = 19, method = "jitter", las = 2, cex.axis = 0.75,
             add = T)
  legend(x = "topright", legend = c("control","all treated"), col = c("black","blue"), pch = c(19,1), bg = "transparent")
  
  # view all compounds together by dose
  stripchart(rval ~ signif(conc,1), dat[wllq == 1 & grepl("firing_rate_mean",acsn)], vertical = T, pch = 1, method = "jitter", las = 2,
             main = paste0("Mean Firing Rate AUC for all compounds in ",dataset_title), ylab = "CCTE_Shafer_MEA_dev_firing_rate_mean (AUC)", xlab = "conc")
  stripchart(rval ~ signif(conc,1), dat[wllq == 0 & grepl("firing_rate_mean",acsn)], vertical = T, pch = 1, method = "jitter",
             add = T, col = "red")
  legend(x = "topright", legend = c("wllq==1","wllq==0"), col = c("black","red"), pch = c(1,1), bg = "transparent")
  
  # find a compound that is likely to be a positive and plot dose response
  plot_spid <- dat[conc == max(conc) & grepl("firing_rate_mean",acsn), .(med_rval = median(rval)), by = "spid"][min(med_rval), spid]
  plot_plates <- control_dat <- dat[spid == plot_spid, unique(apid)]
  stripchart(rval ~ conc, dat[apid %in% plot_plates & (spid == plot_spid | wllt == "n") & wllq == 1 & grepl("firing_rate_mean",acsn)], vertical = T, pch = 1, method = "jitter", las = 2,
             ylim = range(dat[wllq == 1 & grepl("firing_rate_mean",acsn),rval]), ylab = "CCTE_Shafer_MEA_dev_firing_rate_mean (AUC)",
             xlab = "conc", main = paste0(plot_spid," Mean Firing Rate AUC Dose Response"))
  stripchart(rval ~ conc, dat[apid %in% plot_plates & (spid == plot_spid | wllt == "n") & wllq == 0 & grepl("firing_rate_mean",acsn)], vertical = T, pch = 1, method = "jitter", las = 2,
             add = TRUE, col = "red")
  legend(x = "topright", legend = c("wllq==1","wllq==0"), col = c("black","red"), pch = c(1,1), bg = "transparent")
  
  # Cytotox
  stripchart(rval ~ conc, dat[wllq == 1 & grepl("AB",acsn)],
             vertical = TRUE, pch = 1, method = "jitter", xlab = "conc", main = paste0("AB Blank-Corrected Values for ",dataset_title,"\nwhere wllq == 1"))
  stripchart(rval ~ conc, dat[wllq == 1 & grepl("LDH",acsn)],
             vertical = TRUE, pch = 1, method = "jitter", xlab = "conc", main = paste0("LDH Blank-Corrected Values for ",dataset_title,"\nwhere wllq == 1"))
  
}