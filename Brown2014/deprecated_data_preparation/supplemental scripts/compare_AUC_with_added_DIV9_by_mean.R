# Comparing results from plate with missing DIV 9

# --------------------------------- Generate Figures to see values with added DIV 9
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/")
library(data.table)
platei <- "MW1007-104" # fortunately, this is the only use of these plate in the culture

# this prepared data had no DIV 9 added
auc1 <- fread("Intermediate_output/not_using_comparison_only/Brown2016_AUC_no_added_DIV9_for_comparison_only.csv")
# with mean approximated DIV 9
# auc2 <- fread("Intermediate_output/Brown2016_AUC.csv")
auc2 <- fread("Intermediate_output/not_using_comparison_only/Brown2016_AUC_DIV9_mean.csv")

# get data for plate with interpolating values, add in as a new plate
add.dat <- auc2[plate.SN == platei]
add.dat[, plate.SN := paste0(sub("MW","",plate.SN),"new")]
auc1 <- rbind(auc1, add.dat)

id.cols <- c("date","plate.SN","well","treatment","dose","units","file.name")
endpoint_cols <- setdiff(names(auc1), id.cols)

# create date_plate column to distinguish
auc1[, date_plate := paste0(date, "\n", plate.SN)]
auc1[date_plate == "20140730\nMW1007-104", date_plate := "original\n1007-104"]
auc1[date_plate == "20140730\n1007-104new", date_plate := "new\n1007-104"]

# boxplots for control wells
graphics.off()
pdf(file = file.path(paste0('figs/',platei,'_comparison_control_wells_with_updates_bymean.pdf')), height = 6, width = 10, pointsize = 5)
par(mfrow = c(3,3), oma = c(0,0,6,0), mar = c(4, 4, 1.5, 2), xpd = TRUE)
for (endpoint in endpoint_cols) {
  setnames(x = auc1, old = endpoint, new = "current_endpoint")
  boxplot(current_endpoint ~ date_plate,auc1[dose == 0], ylab = endpoint)
  mtext(text = paste0("Comparison of ",platei," versus the rest of plates in data set\nControl wells only"),
        col = c(1),side = 3, outer = TRUE, cex = 1)
  setnames(x = auc1, old = "current_endpoint", new = endpoint)
}
graphics.off()

# boxplots for all wells
graphics.off()
pdf(file = file.path(paste0('figs/',platei,'_comparison_all_wells_with_updates_bymean.pdf')), height = 6, width = 10, pointsize = 5)
par(mfrow = c(3,3), oma = c(0,0,6,0), mar = c(4, 4, 1.5, 2), xpd = TRUE)
for (endpoint in endpoint_cols) {
  setnames(x = auc1, old = endpoint, new = "current_endpoint")
  boxplot(current_endpoint ~ date_plate,auc1, ylab = endpoint)
  mtext(text = paste0("Comparison of ",platei," versus the rest of plates in data set\nAll wells"),
        col = c(1),side = 3, outer = TRUE, cex = 1)
  setnames(x = auc1, old = "current_endpoint", new = endpoint)
}
graphics.off()


# ----------------------------looking a few specific endpoint per DIV
divdat <- fread("Intermediate_Output/Brown2016_all_prepared_data_added_DIV9.csv")

# need to fix columns that are named as "DIV12"
divdat[, "DIV"] <- as.numeric(sub(pattern="(DIV)0*","",divdat$DIV))

endi <- "ns.percent.of.spikes.in.ns"
source('supplemental scripts/plot-functions.R')
allplates <- unique(divdat$Plate.SN)
# compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN != platei], endpoint = endi, plot_type = "b")
# compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == allplates[1]], endpoint = endi, plot_type = "b", offsetDIV = F)
# 
# # inter-network spike interval
# compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == allplates[1]], endpoint = "ns.mean.insis", plot_type = "b", offsetDIV = T)
# 
# # let's get a closer look at the network spikes
# compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == allplates[1]], endpoint = "ns.n", plot_type = "b", offsetDIV = T)
# 
# # mean ns durn
# compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == allplates[1]], endpoint = "ns.durn.m", plot_type = "b", offsetDIV = T)

# let's check out some burst-related things
# compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == "MW1234-49"], endpoint = "mean.isis", plot_type = "b", offsetDIV = F)
# compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == "MW1234-47"], endpoint = "burst.per.min", plot_type = "b", offsetDIV = F)
# 
# compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == "MW1234-25"], endpoint = "r", plot_type = "b", offsetDIV = F)

# graphs to show Tim, to get a general picture of what is an issue here
graphics.off()
pdf(file = file.path(paste0('figs/',platei,'_comparison_4_endpoints_by_div_with_updates_bymean.pdf')), height = 6, width = 10, pointsize = 6)
par(mfrow = c(2,2))
compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == "MW1007-38"], endpoint = "meanfiringrate", 
                          plot_type = "b", offsetDIV = F, single_plot = T)
compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == "MW1007-38"], endpoint = "ns.percent.of.spikes.in.ns", 
                          plot_type = "b", offsetDIV = F, single_plot = T)
compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == "MW1007-38"], endpoint = "ns.durn.m", 
                          plot_type = "b", offsetDIV = F, single_plot = T)
compareControlsByEndpoint(run1.data = divdat[Plate.SN == platei], run2.data = divdat[Plate.SN == "MW1007-38"], endpoint = "r", 
                          plot_type = "b", offsetDIV = F, single_plot = T)
graphics.off()
