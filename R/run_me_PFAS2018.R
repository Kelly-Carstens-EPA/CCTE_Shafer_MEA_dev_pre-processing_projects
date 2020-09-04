###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "PFAS2018" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probs want to be true when you first run
save_notes_graphs <- FALSE # can say no if you are in debugging phase, but do do a final run with saving notes adn graphs

default_ControlTreatmentName = "DMSO" # all compounds other than those listed below should have this vehicle control
# Enter the names of the compounds as they appear in the MEA data that have a vehicle control other than the default
different_vehicleControlCompounds = c() # e.g. c("Sodium Orthovanadate", "Amphetamine")
# Enter the names of the vehicle controls as they correspond to the compounds in the previous list
different_vehicleControls = c() # e.g. c("Water", "Water")

# might update this, not sure
# trt_col <- "Chemical ID...2"
# stock_conc_col <- "Conc"
# spid_col <- "NCCT ID...3"

spidmap_file <- "L:/Lab/NHEERL_MEA/Project PFAS 2018/EPA_9238_EPA-Shafer_75_20180511_key_MW Waste Calculations.xlsx"
spid_sheet <- "Worksheet1 (2)"

scripts.dir <- "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/nfa-spike-list-to-mc0-r-scripts/R"
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl" # where the dataset_title folder will be created
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(openxlsx)

# create a summary log file and store the 
if(save_notes_graphs) {
  sink(file = file.path(root_output_dir, dataset_title, paste0(dataset_title,"_run_log_",as.character.Date(Sys.Date()),".txt")))
  cat("Output from the script run_me_",dataset_title,".R\n",sep="")
  cat("Date:",as.character.Date(Sys.Date()),"\n")
  cat("USER INPUT settings:\n")
  print(sapply(ls(), get, envir = .GlobalEnv))
  graphics.off()
  pdf(file = file.path(root_output_dir, dataset_title, paste0(dataset_title,"_summary_plots_",as.character.Date(Sys.Date()),".pdf")))
}

# source the ultimate function!
source(file.path(scripts.dir, 'source_steps.R'))

# prepare spidmap
spidmap <- as.data.table(read.xlsx(spidmap_file, sheet = spid_sheet))
head(spidmap)
unique(spidmap$Unit) # all mM
setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap[, treatment := as.character(treatment)]

# run the final function
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
source(file.path(scripts.dir, 'confirm_concs.R'))
tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title, spidmap, default_ControlTreatmentName, remake_all,
                 different_vehicleControlCompounds = different_vehicleControlCompounds, different_vehicleControls = different_vehicleControls)

# final data checks
# - number of plates, number of cultures
# - wllq breakdown
# - number of data points per plate
# - number of controls per plate?
# - basic checks/data counting
# - general dose-response visualization, for all endpoints, including cytotox

dat <- read.csv(file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.csv")))
setDT(dat)
cat("\nFinal Checks\n")
cat("Number of cultures dates:",dat[, length(unique(sub("_.+$","",apid)))],"\n")
cat("\nRange of culture dates:", dat[, range(sub("_.+$","",apid))] )
cat("\nNumber of plates tested:",dat[, length(unique(apid))])
cat("\nNumber of compounds tested:",dat[wllt == "t", length(unique(spid))])
cat("Wllq breakdown:\n")
print(dat[, .N, by = "wllq"]) # note if wllq is NA anywhere
cat("Number of unique acsn's present:",length(unique(dat$acsn)),"\n")
# dat[, .N, by = c("acsn","apid")][N != 48] # this doesn't work, bc missing apid/acsn pairs are just misssing. Need fill=0
check.points <- dcast(dat[, .N, by = c("acsn","apid")], apid ~ acsn, value.var = "N", fill = 0)
setnames(check.points, old = names(check.points), new = sub("CCTE_Shafer_MEA_dev_","",names(check.points)))
cat(paste0("\nThe following plates don't have the expected number of points (48):\n"))
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
cat("Apid/acsn pairs without 6 control wells:\n")
print(dat[wllt == "n", .N, by = c("acsn","apid")][N != 6])

# PLOTS to visually confirm results
# think - what do I want to see to gut-check that everything went smoothly? what I am concerned about?
# confirm concs for each compound, if match exp summary file?

# a dose-response AUC plot?
# by DIV plots?

# which to plot? All controls? (will do with all data as well)

# view all control wells some how... then overlay treated wells?
stripchart(rval ~ apid, dat[wllt == "n" & grepl("firing_rate_mean",acsn)], vertical = T, pch = 1, method = "jitter", las = 2)
unique(dat$spid) # could prompt user to select a few known positives or negatives to check
pspid <- "EPATL0167A01"
stripchart(rval ~ conc, dat[spid == pspid & grepl("firing_rate_mean",acsn)], vertical = T, pch = 1, method = "jitter", las = 2)

stripchart(rval ~ signif(conc,1), dat[wllq == 1 & grepl("firing_rate_mean",acsn)], vertical = T, pch = 1, method = "jitter", las = 2)

# range by acsn
dat[wllq == 1, paste0(signif(range(rval),2),collapse = ", "), by = "acsn"]

# good to check controls
stripchart(meanfiringrate ~ date, aucdat[dose == 0], vertical = T, pch = 1, las = 2, cex.axis = 0.8)

# MEA ACUTE PLOTTING
# # MEA points from -100 to 300
# # by acnm
# default_oma <- par("oma")
# par(oma = c(default_oma[1]+5, default_oma[2:4]))
# boxplot(rval ~ sub("CCTE_Shafer_MEA_acute_","",acnm), dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm)], main = paste0("All MEA components for ",dataset_title,"\nwhere wllq=1 (rval's above 300 not shown)"), 
#         ylim = c(-100, 300), las = 2, cex.axis = 0.6, xlab = "")
# par(oma = default_oma)
# 
# # by wllt/conc
# dat4[, wllt_conc := ifelse(wllt == "t", paste0(signif(conc,digits=1)), wllt)]
# dat4$wllt_conc <- factor(dat4$wllt_conc, 
#                          levels = c(dat4[wllt!="t",sort(unique(wllt))], paste0(dat4[wllt=="t",sort(unique(signif(conc,digits=1)))])), ordered = T)
# stripchart(rval ~ wllt_conc, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval < 300], 
#            vertical = T, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", ylab = "rval (percent change in activity)", col = "lightblue", 
#            main = paste0("All MEA Components by conc for ",dataset_title,"\nwhere wllq=1 and rval < 300"))
# boxplot(rval ~ wllt_conc, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval < 300], outline = F, col = "transparent", boxwex = 0.5, add = T)
# abline(h = 0, lty = "dashed")
# 
# # View the extent of extreme outliers (usually due to very small baseline value)
# stripchart(rval ~ wllt_conc, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval >= 300], 
#            vertical = T, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", col = "blue", 
#            main = paste0("Outlier MEA Points in ",dataset_title,"\nwhere wllq=1 and rval >= 300"))
# cat("\nSummary of MEA rval's above 300% change by acnm (for wllt 't' or 'n'):\n")
# print(dat4[wllt %in% c("t","n") & wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval >= 300, .(wllts = paste0(sort(unique(wllt)),collapse=","), .N), by = c("acnm")][order(-N)])
# 
# # View Cytotox components
# stripchart(rval ~ wllt_conc, dat4[wllq == 1 & grepl("AB",acnm)], 
#            vertical = TRUE, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", main = paste0("AB Blank-Corrected Values for ",dataset_title,"\nwhere wllq == 1"))
# stripchart(rval ~ wllt_conc, dat4[wllq == 1 & grepl("LDH",acnm)], 
#            vertical = TRUE, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", main = paste0("LDH Blank-Corrected Values for ",dataset_title,"\nwhere wllq == 1"))
# dat4[, wllt_conc := NULL]


if(save_notes_graphs) {
  sink()
  graphics.off()
}

