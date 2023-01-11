# compare resultsof cytotox_prep06.R to the cytotox data in the final ish paper set up
library(data.table)

new.cyto <- fread("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/Intermediate_output/Brown2016_cytotoxicity_test2.csv")

new.cyto[treatment == "Bisindolymaleimide 1", treatment := "Bis 1"]
new.cyto[, apid := sub("Summary.xlsx","",apid)]

# calculated the percent of control values
new.cyto[conc == 0, control_mean := mean(rval), by = c("date","apid")]
new.cyto[, control_mean_all := unique(control_mean[!is.na(control_mean)]), by = c("date","apid")]
new.cyto[, poc := rval / control_mean_all]

# now remove chem we don't care about
new.cyto <- new.cyto[!(treatment %in% c("Glyphosate ","Van"))]

# make into wide table
new.cyto[, date_plate := paste0(date,"_",apid)]
wide.dat <- dcast(new.cyto, treatment + conc ~ date_plate, value.var = "poc", fun.aggregate = mean)

datacols <- unique(new.cyto$date_plate)
for (i in 1:nrow(wide.dat)) {
  row.dat <- unlist(wide.dat[i,..datacols ], use.names=F)
  wide.dat[i, Mean := mean(row.dat[!is.nan(row.dat)], na.rm=T)]
  wide.dat[i, StDev := sd(row.dat, na.rm=T)]
}

fwrite(wide.dat, file = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/Intermediate_output/compare_cyto_poc_data3.csv")
