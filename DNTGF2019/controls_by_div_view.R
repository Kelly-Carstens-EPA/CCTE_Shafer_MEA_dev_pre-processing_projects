# getting a general sense of the control wells
plotdat <- read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/output/DNTGF2019_parameters_by_DIV.csv")
setDT(plotdat)
plotdat <- plotdat[dose == 0]
plotdat <- plotdat[order(DIV)]
plotdat[, full_id := paste0(date,"/n",Plate.SN," ", well)]

acsns <- c("burst.per.min","per.spikes.in.burst")
graphics.off()
par(mfrow = c(2,3))
for (acsni in acsns) {
  for (culture in unique(plotdat$date)) {
    plot(plotdat[, c("DIV", acsni), with = F], pch = "", xlim = c(5, 13))
    for (full_idi in plotdat[date == culture, unique(full_id)]) {
      points(plotdat[full_id == full_idi, c("DIV", acsni), with = F], type = "o", pch = 19, col = rgb(.4,.4,.4,.5))
      title(main = paste0(acsni, "\n",culture," Control Wells by DIV"))
    }
  }
}

acsns <- names(plotdat)[9:25]
# by plate
graphics.off()
pdf(file = file.path("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/plots","controls_by_DIV.pdf"))
par(mfrow = c(2,3))
for (acsni in acsns) {
  for (plate in unique(plotdat$Plate.SN)) {
    plot(plotdat[, c("DIV", acsni), with = F], pch = "", xlim = c(5, 13))
    for (full_idi in plotdat[Plate.SN == plate, unique(full_id)]) {
      points(plotdat[full_id == full_idi, c("DIV", acsni), with = F], type = "o", pch = 19, col = rgb(.4,.4,.4,.5))
      title(main = paste0(acsni, "\n",plotdat[Plate.SN == plate, unique(date)], " ",plate," Controls by DIV"))
    }
  }
}
graphics.off()

rm(plotdat)
