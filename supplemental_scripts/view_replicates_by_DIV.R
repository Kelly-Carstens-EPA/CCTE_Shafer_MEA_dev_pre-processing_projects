# quick function to compare replicates over DIV

view_replicates_by_DIV <- function(dat, compoundi, dosei, acsns = c("meanfiringrate","nAE"), title_msg = "") {
  
  require(data.table)
  # (possibly add option to input a well from any plate, then determien teh corresponding dose and treatment here)
  setDT(dat)
  
  # get treated data
  plotdat <- dat[treatment == compoundi & dose == dosei]
  plotdat[, full_id := paste0(date,"\n",Plate.SN," ", well)]
  plotdat <- plotdat[order(DIV)]
  if (length(unique(plotdat$date))==1) {
    # remove date identifier bc will just take up extra space on graph
    plotdat[, full_id := sub("^[[:digit:]]*\\\n","",full_id)]
  }
  
  # get control data
  setkey(dat, date, Plate.SN, dose)
  plate_info <- dat[treatment == compoundi & dose == dosei, .N, by = .(date, Plate.SN)]
  control_dat <- dat[J(plate_info$date,plate_info$Plate.SN, 0)]
  control_dat[, full_id := paste0(date,"\n",Plate.SN," ", well)]
  control_dat <- control_dat[order(DIV)]

  for (acsni in acsns) {
    ylim <- range(c(control_dat[, c(acsni), with = F], plotdat[, c(acsni), with = F]), na.rm = T)
    plot(control_dat[, c("DIV", acsni), with = F], pch = "", xlim = c(5, 14.5), ylim = ylim)
    
    # plot controls
    for (full_idi in unique(control_dat$full_id)) {
      points(control_dat[full_id == full_idi, c("DIV", acsni), with = F], type = "o", pch = 19, col = rgb(.8,.8,.8,.5))
    }
    
    # plot the treated replicates
    for (full_idi in unique(plotdat$full_id)) {
      points(plotdat[full_id == full_idi, c("DIV", acsni), with = F], type = "o", pch = 19, lwd = 1.2)
    }
    text_info <- plotdat[DIV == 12, unique(get(acsni)), by = .(full_id)]
    text(x = 12, y = text_info$V1, labels = text_info$full_id, pos = 4, offset = 0.3, cex = 0.7)
    title(main = paste0(acsni," Replicates by DIV\n",compoundi, " ",dosei,"uM ",title_msg))
    legend(x = "topright", legend = c("controls",paste0(compoundi," ",dosei,"uM")), pch = c(19,19), 
           col = c(rgb(.8,.8,.8,.5), "black"), bg = "transparent", cex = 0.8)
  }

  rm(list = c("plotdat","control_dat"))
}


# dat <- read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/output/DNTGF2019_parameters_by_DIV.csv")
# compoundi <- "78"
# dosei <- 30
# graphics.off()
# view_replicates_by_DIV(dat, compoundi, dosei, acsns = c("meanfiringrate","nAE","burst.per.min","ns.n","r","mi"))
# 
# view_replicates_by_DIV(dat, compoundi, dosei, acsns = "per.spikes.in.burst")
# view_replicates_by_DIV(dat, 77, dosei, acsns = "per.spikes.in.burst")
# 
# view_replicates_by_DIV(dat, 78, 30, acsns = names(dat)[9:25])
# view_replicates_by_DIV(dat, 78, 3)
# view_replicates_by_DIV(dat, 78, 10)
# view_replicates_by_DIV(dat, 78, 1)

# decprecated version
# view_replicates_by_DIV <- function(dat, compoundi, dosei, acsns = c("meanfiringrate","nAE")) {
#   require(data.table)
#   # possibly add option to input a well from any plate, then determien teh corresponding dose and treatment here
#   setDT(dat)
#   plotdat <- dat[trt == compoundi & dose == dosei]
#   plotdat[, full_id := paste(date, Plate.SN, well, sep="\n")]
#   plotdat <- plotdat[order(DIV)]
#   for (acsni in acsns) {
#     # include control wells in the ylim, to control the visual difference in replciates
#     ylim <- range(dat[date %in% plotdat$date & ((trt == compoundi & dose == dosei) | dose == 0), c(acsni), with = F], na.rm = T)
#     if(any(is.na(ylim))|any(is.infinite(ylim))) browser()
#     plot(plotdat[, c("DIV", acsni), with = F], pch = "", xlim = c(5, 14), ylim = ylim)
#     for (full_idi in unique(plotdat$full_id)) {
#       points(plotdat[full_id == full_idi, c("DIV", acsni), with = F], type = "o", pch = 19)
#     }
#     text_info <- plotdat[DIV == 12, unique(get(acsni)), by = .(full_id)]
#     text(x = 12, y = text_info$V1, labels = text_info$full_id, pos = 4, offset = 0.3, cex = 0.7)
#     title(main = paste0("Replicates by DIV\n",compoundi, " ",dosei,"uM ",acsni))
#   }
#   rm(plotdat)
# }
