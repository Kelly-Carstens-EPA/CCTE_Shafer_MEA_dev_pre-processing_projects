# check out number of 

tcdat <- fread("L:/Lab/NHEERL_MEA/PIP3 - Project/Data/ToxCast tcpl prep/Intermediate Output/MEA_mc0_TC.csv")

tcdat[, unique(apid)]

plates <- c("MW1147-28", "MW1147-29", "MW1147-30")
tcdat[apid %in% plates, unique(treatment)]

id.cols <- c("date","Plate.SN","well","trt","dose","units","DIV","file.name")
endpoint_cols <- setdiff(names(tcdat), id.cols)

plot_endpoint_info <- function(endpoint) {
  # plot.new()
  treatments <- c("1,1,2,2-Tetrahydroperfluoro-1-decanol", "1H,1H,2H,2H-Perfluorooctyl iodide",
                  "Perfluoroundecanoic acid","Rotenone", "Disulfiram", "Valinomycin")
  yrange <- tcdat[acsn == endpoint, range(rval)]
  par(mfrow = c(2,3), oma = c(0,0,2,0), mar = c(4, 4, 1.5, 2), xpd = TRUE)
  for (trt in treatments) {
    # cytotox
    plot(tcdat[apid %in% plates & grepl(trt,treatment) & acsn == endpoint, .(log(conc), rval)],
         xlab = "log(conc)", ylab = paste0(sub("NHEERL_MEA_dev_","",endpoint), " AUC"), pch = 1, ylim = yrange)
    title(main = trt)
  }
  mtext(text = paste0(endpoint, " AUC"),
        col = c(1),side = 3, outer = TRUE, cex = 1)
}

plot_endpoint_info("NHEERL_MEA_dev_LDH")
plot_endpoint_info("NHEERL_MEA_dev_AB")
plot_endpoint_info("NHEERL_MEA_dev_active_electrodes_number")
plot_endpoint_info("NHEERL_MEA_dev_firing_rate_mean")
plot_endpoint_info("NHEERL_MEA_dev_bursting_electrodes_number")

all_acsn <- unique(tcdat$acsn)
graphics.off()
pdf(file = file.path('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/compare_activity_3_pfas.pdf'), height = 6, width = 10, pointsize = 8.5)
for (comp in all_acsn) {
  plot_endpoint_info(comp)
}
graphics.off()
