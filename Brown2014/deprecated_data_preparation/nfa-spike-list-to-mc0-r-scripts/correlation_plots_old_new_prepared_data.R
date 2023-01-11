# correlation plots of old versus new data 

# possible source of differences
# weirdness with h5 file, well info got duplicated?
# update to local correlation
library(data.table)
org.dat <- fread("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/source_prepared_data/Final_Data_Set_SA1_DNT_Paper1 (2)(updated).csv")
new.dat <- fread("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/prepared_data/Brown2016_all_prepared_data.csv")
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016")
# preliminary comparisons
nrow(org.dat)
nrow(new.dat) # new dat has 98 more rows... oh yay, I didn't exclude the cytotox!
new.dat[trt != "Glyphosate",.N] # hmm, now this is only 992
new.dat[trt != "Glyphosate" | dose == 0, .N] # 1006 Ah, this matches
new.dat2 <- new.dat[trt != "Glyphosate" | dose == 0]

id.cols <- c("date","Plate.SN","well","trt","dose","units","DIV","file.name")
endpoint_cols <- setdiff(names(org.dat), id.cols)

# merge data by well, plate, trt, etc
new.dat$DIV <- as.numeric(new.dat$DIV)
wdat <- merge(org.dat, new.dat, by = id.cols, suffixes = c(".org",".new"))

graphics.off()
pdf(file = "figs/correlation_plot_newly_calculated_vs_original_parameter_values_Brown2016.pdf")
par(mfrow = c(3,3), mar = c(4, 4, 3, 2))
for (endpoint in endpoint_cols) {
  plotcols <- c(paste0(endpoint, ".org"), paste0(endpoint, ".new"))
  plot(wdat[, ..plotcols], xlab = paste0("original ",endpoint),
       ylab = paste0("new ",endpoint))
  abline(a = 0, b = 1)
  num_diff <- length(which(wdat[, plotcols[1], with = F] != wdat[, plotcols[2], with = FALSE]))
  title(main = paste0("number diferent: ",num_diff))
  # mtext(text = paste0("Correlation Plots of newly calculated vs. original activity values for Brown et al 2016 data"), side = 3, outer = T)
}
graphics.off()

# okay, so, we are seeing several differences... where are these differences coming from?
# like, it should be identical!

# ugh, I mean, really the only thing that changed is the correlation coefficient. And i changed how that was calculated, so I'm good with that

# want to check for NA's
for (endpoint in endpoint_cols) {
  setnames(wdat, old = c(paste0(endpoint, ".org"), paste0(endpoint, ".new")), new = c("eorg", "enew"))
  na_in_new_only <- wdat[is.na(eorg) & !is.na(enew), .N]
  na_in_org_only <- wdat[!is.na(eorg) & is.na(enew), .N]
  print(paste0(endpoint," has ", na_in_org_only, " values that are NA in original but not in new."))
  print(paste0(endpoint," has ", na_in_new_only, " values that are NA in new but not in original."))
  setnames(wdat, new = c(paste0(endpoint, ".org"), paste0(endpoint, ".new")), old = c("eorg", "enew"))
}
