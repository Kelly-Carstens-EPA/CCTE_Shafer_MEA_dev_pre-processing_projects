rm(list=ls())
graphics.off()
###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "DNTGF2019" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probs want to be true when you first run
save_notes_graphs <- TRUE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed

default_ControlTreatmentName <- "DMSO" # usually DMSO. all compounds other than those listed below should have this vehicle control

spidmap_file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/All Assays_list_toxcast_OECD 20190524.xlsx"
spid_sheet <- "NFA Groups"

scripts.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R"
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
  cat("Date Ran:",as.character.Date(Sys.Date()),"\n")
  cat(R.version.string,"\n")
  cat("USER INPUT settings:\n")
  print(sapply(ls(), get, envir = .GlobalEnv))
  graphics.off()
  pdf(file = file.path(root_output_dir, dataset_title, paste0(dataset_title,"_summary_plots_",as.character.Date(Sys.Date()),".pdf")))
}

# run the main steps
source(file.path(scripts.dir, 'source_steps.R'))

# run tcpl_MEA_dev_AUC
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
dat <- tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title)


# change untreated wells to Control Treatment ------------------------------------
dat[wllt == "n", treatment := default_ControlTreatmentName]
# update other control wells as needed, e.g.
dat <- update_control_well_treatment(dat, control_compound = "Water",culture_date = "20190904", plates = paste0("MW69-381",7:9), control_rowi = which(LETTERS[1:6] %in% c("E","F")))
dat <- update_control_well_treatment(dat, control_compound = "Water",culture_date = c("20190710","20190724"), control_rowi = 1:6)
dat[wllt == "n", .N, by = "treatment"]


# Assign SPIDs ------------------------------------------------------------------
spidmap <- as.data.table(read.xlsx(spidmap_file, sheet = spid_sheet))
head(spidmap)
unique(spidmap$Unit) # all mM? - yes
setnames(spidmap, old = c(2,5,3), new = c("treatment","stock_conc","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap[, spid := paste0("EPAPLT0",spid)]
spidmap[, expected_stock_conc := 20] # initialize expected_stock_conc. Usually this is 20mM. Change as needed.
# update expected_stock_conc for individual compouunds where needed 
# for example, 
# spidmap[treatment %in% c("2,2',4,4',5,5'-Hexabromodiphenyl ether","Dibenz(a,h)anthracene"), expected_stock_conc := 10.0]
spidmap[, treatment := as.character(treatment)]
spidmap[, stock_conc := as.numeric(stock_conc)]
head(spidmap[, .(treatment, spid, stock_conc, expected_stock_conc)])

spidmap2 <- as.data.table(read.xlsx("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/glufosinates_spidmap.xlsx", sheet = 1))
setnames(spidmap2, old = c("Compound.name.in.MEA.data","SPID"), new = c("treatment","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap2[, stock_conc := NA_real_] # will get actual stock conc's from invitrodb
spidmap2[, expected_stock_conc := 20] # initialize expected_stock_conc. Usually this is 20mM. Change as needed.
spidmap2[, treatment := as.character(treatment)]
spidmap2[, stock_conc := as.numeric(stock_conc)]
usecols <- c("spid","treatment","stock_conc","expected_stock_conc")
spidmap <- rbind(spidmap[, ..usecols], spidmap2[, ..usecols])
spidmap

# rename any compounds, if needed
dat[treatment == "Glufo", treatment := "L-Glufosinate Ammonium"]

# assign spids
dat <- check_and_assign_spids(dat, spidmap)


# Confirm Conc's ----------------------------------------------------------------
# confirm that the conc's collected from master chem lists and Calc files match
# and that the correct concentration-corrections has been done for each compound

# check if there are multiple conc's assigned to the same well (usually occurs if there are differences between master chem file and calc file)
# Note: in TCPL mc1, the conc's are set to dat[ , conc := signif(conc, 3)]. So it's okay for us to round here.
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
# if any, standardize those before continuing.
problem_comps <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1, unique(treatment)]
problem_comps
# [1] "100" "18"  "34"  "37"  "46"  "49"  "8"   "82"  "88"  "97" 
dat[treatment %in% problem_comps, .(paste0(sort(unique(signif(conc,3))),collapse=",")), by = .(treatment)]

# get a summary of the treatments by srcf
summary_dat <- dat[treatment %in% problem_comps, .(conc_shown = unique(conc)), by = .(apid, rowi, coli, treatment, srcf)]
summary_dat[, conc_round := signif(conc_shown, 1)]
summary_dat[, conc_source := ifelse(grepl("AUC",srcf),"AUC","Calc")]
summary_wide <- dcast(summary_dat, apid + treatment + rowi + coli ~ conc_source, value.var = "conc_shown")

# going to check out these treatments 1 by 1
summary_wide[treatment == "100"][order(AUC)] # it is clear that the issue is just due to rounding from the AUC data
# I wouldlike to just take the Calc values. see if same for other comopunds

summary_wide[treatment == "18"][order(AUC), .(AUC, round(Calc, 2))] # yep, these are all equal
summary_wide[treatment == "34"][order(AUC), .(AUC, round(Calc, 2))] # one difference remains: 9.64  9.63
summary_wide[treatment == "37"][order(AUC), .(AUC, round(Calc, 2))] # yep, these are all equal
# let's just check all at once for this
summary_wide[round(Calc, 2) != AUC]
# apid treatment rowi coli   AUC       Calc
# 1: 20190904_MW69-3817        34    3    4  0.30  0.2890500
# 2: 20190904_MW69-3817        34    3    7  9.64  9.6350000
# 3: 20190904_MW69-3818        34    1    4  0.30  0.2890500
# 4: 20190904_MW69-3818        34    1    7  9.64  9.6350000
# 5: 20190904_MW69-3819        34    2    4  0.30  0.2890500
# 6: 20190904_MW69-3819        34    2    7  9.64  9.6350000
# 7: 20190918_MW70-2406        46    4    4  0.30  0.2870715
# 8: 20190918_MW70-2406        46    4    5  1.00  0.9569050
# 9: 20190918_MW70-2406        46    4    6  3.00  2.8707150
# 10: 20190918_MW70-2406        46    4    7 10.00  9.5690500
# 11: 20190918_MW70-2406        46    4    8 30.00 28.7071500
# for these 2 compounds, what is "correct"?
spidmap[treatment == "34", signif(stock_conc/expected_stock_conc*10,3)] # 9.63
# so for 34, again I want to just use the values from the Calc file.
spidmap[treatment == "46", signif(stock_conc/expected_stock_conc*10,3)] # 10
# for this compound, I want to use the conc's listed from the AUC data instead

summary_wide[, use_conc := Calc]
summary_wide[treatment == "46", use_conc := AUC]
summary_wide[, use_conc := as.numeric(use_conc)]

dat <- merge(dat, summary_wide[, .(apid, treatment, rowi, coli, use_conc)], all.x = T, by = c("apid","treatment","rowi","coli"))
dat[!is.na(use_conc), conc := use_conc]
dat[, use_conc := NULL]

# actually, for 34, I need to just update this manually
dat[treatment == "34", unique(conc)]
dat[treatment == "34", conc_test := signif(conc, 1)] # I can do this becuase teh stock conc is close enough to 20, so will round correctly
dat[treatment == "34", .(unique(conc_test)), by = "conc"]
# conc    V1
# 1:  0.028905  0.03
# 2:  0.096350  0.10
# 3:  0.289050  0.30
# 4:  0.963500  1.00
# 5:  2.890500  3.00
# 6:  9.635000 10.00
# 7: 28.905000 30.00
dat[treatment == "34", conc := signif(conc, 1)]
dat[, conc_test := NULL]

# confirming this is empty now
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]

# finally, run this:
source(file.path(scripts.dir, 'confirm_concs.R'))
dat <- confirm_concs(dat, spidmap, expected_target_concs = c(0.03,0.1,0.3,1,3,10,30))
rm(list=c("spidmap","spidmap2","summary_dat","summary_wide"))


# FINAL DATA CHECKS
# this section is to confirm that the data has been processed correctly
source(file.path(scripts.dir, 'dataset_checks.R'))
dataset_checks(dat)

# Any other plots or things to check?
# correlation plot with previously pipelined data
# dat <- read.csv(file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.csv")))
# setDT(dat)
odat <- read.csv("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/DNT2019_all_MEA_mc0_withspids.csv")
odat_gf <- read.csv("L:/Lab/NHEERL_MEA/Project - Glufosinate/Network Formation Assay/Spike List to mc0/Final mc0 files/glufosinates_mc0.csv")
odat <- rbind(odat, odat_gf)
rm(odat_gf)

dat[, plate.id := sub("^[[:digit:]]{8}_","",apid)]
dat[, acsn_bk := sub("CCTE_Shafer","NHEERL",acsn)]
dat[acsn_bk == "NHEERL_MEA_dev_inter_network_spike_interval_mean", acsn_bk := "NHEERL_MEA_dev_per_network_spike_interspike_interval_mean"]
dat[, rowi := as.integer(rowi)]
dat[, coli := as.integer(coli)]
dat2 <- merge(dat, odat, by.x = c("plate.id","acsn_bk","rowi","coli"), by.y = c("apid","acsn","rowi","coli"),
              suffixes = c(".new",".org"))
nrow(dat2) == nrow(dat) # TRUE
dat2[spid.new != spid.org, .N, by = .(spid.new, spid.org)]
# spid.new spid.org  N
# 1:    Water     DMSO 76
# 2:     DMSO    Water 19
dat2[spid.new == "DMSO" & spid.org == "Water", .N, by = .(plate.id, rowi, coli)]
# plate.id rowi coli  N
# 1: MW69-3818    4    2 19
dat2[spid.org == "DMSO" & spid.new == "Water", .N, by = .(plate.id, rowi, coli)]
# plate.id rowi coli  N
# 1: MW69-3817    5    2 19
# 2: MW69-3818    5    2 19
# 3: MW69-3818    6    2 19
# 4: MW69-3819    6    2 19
# ya know, I think I did something funky here with the row removal/keeping some control wells
# I know that the control well spid's I am usign now are based on what is recording in the Calc files and lab notebooks
# so I am sticking with that
dat2[wllt.new != wllt.org] # empty
dat2[conc.new != conc.org] # many rows
dat2[conc.new != conc.org, .N, by = "treatment"] # ah, this looks like the same "problem conc's" that I corrected here
dat2[signif(conc.new, 1) != signif(conc.org, 1), .N, by = "treatment"] # phew, this is empty. So nothing is drastically off

# okay before I view the plots, reasons why values might look different:
# - differences in spike list file chopping
# - possible former mistakes in spike list file selection (e.g. getting the"modified" file instead)
# - other unknown things which could be not good

for (acsni in unique(dat2$acsn_bk)) {
  plot(dat2[acsn_bk == acsni, .(rval.org, rval.new)], main = paste0(acsni,"\nNew vs original AUC rval correlation"))
  abline(0,1)
}

# some of the "off" points
dat2[grepl("burst_duration_mean",acsn_bk) & abs(rval.org - rval.new) > 50]
dat2[grepl("bursting_electrodes_number",acsn_bk) & abs(rval.org - rval.new) > 10, .(rval.new, rval.org, apid, srcf.org)]
dat2[grepl("peak",acsn_bk) & abs(rval.org - rval.new) > 10, .(rval.new, rval.org, apid, srcf.org)]
dat2[grepl("network_spike_peak",acsn_bk) & abs(rval.org - rval.new) > 15]
dat2[grepl("mutual_information",acsn_bk) & rval.org == 0.0 & rval.new != rval.org, .N, by = c("treatment","conc.new")]
# huh, seems to mostly affect a specific handful of treatments...
dat2[grepl("mutual_information",acsn_bk) & rval.org == 0.0 & rval.new != rval.org, .N, by = "apid"]
# apid  N
# 1: 20190807_MW69-3803  1
# 2: 20191030_MW70-2508 47
# 3: 20191030_MW70-2509 48
# 4: 20191030_MW70-2510 48
# I don't know...

# created this prepared data with the old h5files, but new functions
pfiles <-list.files(file.path(root_output_dir,dataset_title,"comparing_previous_values"),full.names = T, pattern = "\\.csv")
dat3 <- data.table()
for (pfile in pfiles) {
  dati <- read.csv(pfile)
  dat3 <- rbind(dat3, dati)
  rm(dati)
}

# loading the original pdat, with the previous scripts
pdat_org <- read.csv("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/prepared_data/prepared_data_DNT_2019_All_combined.csv")
setDT(pdat_org)
pdat_org <- pdat_org[, .SD, .SDcols = names(pdat_org)[!grepl("^cv",names(pdat_org))]]
nrow(pdat_org)
nrow(dat3) # equal
all.equal(pdat_org, dat3, ignore.row.order = TRUE)
# [1] "Datasets has different indexes. 'target' has no index. 'current': well__Plate.SN."
attr(dat3, which = "index") <- NULL
all.equal(pdat_org, dat3, ignore.row.order = TRUE)
# TRUE!!
# so, that means that all differences in values are really just due to differences in the spike list file chopping.
# still, whyare some endpoints so 'volatile'/will change so much with a slight change in recording section used?
rm(list = c("dat3","pdat_org"))


# let's see if the change in rval is less than the median IQR of DMSO wells by plate for each endpoint
acsni <- "NHEERL_MEA_dev_bursting_electrodes_number"
for (acsni in unique(dat2$acsn_bk)) {
  nwell_median_iqr <- dat2[acsn_bk == acsni & wllt.new == "n" & wllt.org == "n", .(IQR(rval.new)), by = .(plate.id)][, median(V1)]
  print(acsni)
  print(dat2[acsn_bk == acsni & abs(rval.org - rval.new) > nwell_median_iqr, .N])
}

myfun <- function(acsni) {
  nwell_median_iqr <- dat2[acsn_bk == acsni & wllt.new == "n" & wllt.org == "n", .(IQR(rval.new)), by = .(plate.id)][, median(V1)]
  val <- dat2[acsn_bk == acsni & abs(rval.org - rval.new) > nwell_median_iqr, .N]
  val
}
concerning_pt_count <- sapply(unique(dat2$acsn_bk), myfun)
graphics.off()
pdf(file = file.path(root_output_dir,dataset_title, "plots","new_rval_vs_dec2019.pdf"))
for (acsni in unique(dat2$acsn_bk)) {
  plot(dat2[acsn_bk == acsni, .(rval.org, rval.new)], main = paste0(acsni,"\nNew vs original AUC rval correlation. N outliers=",concerning_pt_count[acsni]))
  abline(0,1)
  nwell_median_iqr <- dat2[acsn_bk == acsni & wllt.new == "n" & wllt.org == "n", .(IQR(rval.new)), by = .(plate.id)][, median(V1)]
  abline(a = c(-1)*nwell_median_iqr, b = c(1), col = "red")
  abline(a = c(1)*nwell_median_iqr, b = c(1), col = "red")
}
graphics.off()
dat2[grepl("burst_duration_mean",acsn_bk), summary(rval.new)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    2.76    4.87    8.67    7.67  221.59 
dat2[grepl("burst_duration_mean",acsn_bk), summary(rval.org)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    2.76    4.85    8.64    7.68  221.57 
# this is good, at least
# it is concerning that some of these values are way different
# esp teh mean burst durataion - are we getting some insanely long bursts now?
boxplot(dat2[acsn_bk == "NHEERL_MEA_dev_burst_duration_mean", .(rval.org, rval.new)]) # looks liek the diff's are in teh outliers

# i'm just concerned that there might be something really off with these wells...
# but something also tells me that it's fine.

rm(dat2)
rm(odat)
dat[, c("acsn_bk","plate.id") := NULL]


# save dat and graphs
write.csv(dat, file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.csv")), row.names = F)
rm(dat)

if(save_notes_graphs) {
  sink() # close the txt log file
  graphics.off() # clear the plot history
}

cat("\nDone!\n")

# DATA CHECKS:

# # quick check of how the data looked when it was pipelined previuolsy
# dat <- read.csv("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/DNT2019_all_MEA_mc0_withspids.csv")
# setDT(dat)
# dat[, .N, by = "apid"] # 36 plates = 3*12 groups
# # all plates have 912 points = 48 wells * 19 endpoints
# # except for 
# # 16: MW69-3817 760
# # 17: MW69-3818 760
# # 18: MW69-3819 760
# dat[apid %in% c("MW69-3817", "MW69-3818", "MW69-3819"), unique(spid)] # only 5 spids
# These are the plates where Glufo was tested, so this would be where I removed those data rows
# since I prepared the GF data separately. Now I am doing it together.
# # so even though it looks like the Calc data was not included for Group 12, 
# # It appears that plates 70-2520, 70-2601, 70-2602 have all 912 plates. So that data must have been added.
# dat[, .N, by = "wllq"] # all have wllq == 1
# # wllq     N
# # 1:    1 32376

# # quick check of modified vs original spike list ifles
# spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20190904 Culture DNT Group 3/MW 69-3813/Spike List Files/Modified Spike lists/NFA_20190904_MW69-3813_05_00(000)_spike_list.csv",
#              select = 3:5)
# spk[V4 == "A1_21"]
# spk[V4 %in% c("A1_21","A1_33","B5_11","C2_43","F8_22")] # empty
# head(spk)
# # V3        V4            V5
# # 1: Time (s) Electrode Amplitude(mV)
# # 2:                                 
# #   3:                                 
# #   4:                                 
# #   5:                                 
# #   6:  0.03008     E5_31         0.051
# # def has rows removed
# rm(spk)
# 
# spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20190904 Culture DNT Group 3/MW 69-3813/Spike List Files/Original Spike lists/NFA_20190904_MW69-3813_05_00(000)_spike_list.csv",
#              select = 3:5)
# spk[Electrode == "A1_21"] # several rows
# spk[Electrode %in% c("A1_21","A1_33","B5_11","C2_43","F8_22")] # many rows
# head(spk)
# # okay, so def use the original spike list file here.
# rm(spk)
# 
# # What did Melissa use?
# spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/All Spike List Files/NFA_20190904_MW69-3813_05_00(000)_spike_list.csv",
#              select = 3:5)
# spk[Electrode == "A1_21"] # several rows
# spk[Electrode %in% c("A1_21","A1_33","B5_11","C2_43","F8_22")] # many rows
# head(spk)
# # sweet! So it looks liek she used the "original" files, which is good, so that results won't change
# rm(spk)
# 
# # what about for e.g. Group1? No outlier elec's missing. Still use originals?
# spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20190807 Culture DNT Group 1/MW 69-3715/Spike List Files/NFA_20190807_MW69-3715_05_00(000)_spike_list.csv",
#              select = 3:5)
# unique(spk$Electrode)
# 
# spk_mod <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20190807 Culture DNT Group 1/MW 69-3715/Spike List Files/Modified Spike List Files/NFA_20190807_MW69-3715_05_00(000)_spike_list.csv",
#              select = 3:5)
# setdiff(unique(spk$Electrode), unique(spk_mod$V4)) # "E8_11" "F1_42" "F1_32" "D7_31"
# # huh, interesting.
# # yup, this totally matches what is here: 
# # L:\Lab\NHEERL_MEA\Project - DNT 2019\Project DNT 2019 NFA MEA\20190807 Culture DNT Group 1\20190807 Culture DNT Group 1 Electrode Outliers.xlsx
# spk[Electrode == "E8_11"] # huh, so this is probably where the last seconds at 907 got chopped off in the modified version
# # Time (s) Electrode Amplitude(mV)
# # 1:   0.00392     E8_11         0.026
# # 2:   0.14056     E8_11         0.023
# # 3:   0.26600     E8_11         0.031
# # 4:   0.38480     E8_11         0.029
# # 5:   0.49792     E8_11         0.025
# # ---                                  
# #   6537: 906.57560     E8_11         0.025
# # 6538: 906.73000     E8_11         0.025
# # 6539: 906.86616     E8_11         0.027
# # 6540: 907.00096     E8_11         0.031
# # 6541: 907.15344     E8_11         0.030
# spk_mod[V4 == "E8_11"] # empty
# # Cool! So once again, use the non-modified version

# # DIV 12 is missing from "Original Spike List files" folder. Can I use the one in the modified folder?
# spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20191016 Culture DNT Group 7/70-2414/Spike List Files/Modified/NFA_20191016_MW70-2414_12_00(000)(000)_spike_list.csv",
#              select = 1:5)
# head(spk)
# spk[V4 %in% c("A5_31","C1_21","C1_41","E1_23","E6_33")] #empty data table
# spk[1:10]
# #             Original File Time                       10/28/2019 9:55
# # 7:          Sampling Frequency                              12.5 kHz
# # 8:               Voltage Scale        -5.48486178148311E-08 V/sample
# # 9:       Experiment Start Time                       10/28/2019 9:26
# rm(spk)
# 
# # The other DIV 12 file, let's see if this is BIC or what
# spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20191016 Culture DNT Group 7/70-2414/Spike List Files/Modified/NFA_20191016_MW70-2414_12_00(000)(001)_spike_list.csv",
#              select = 1:5)
# head(spk)
# # Investigator                  V2 Time (s) Electrode Amplitude(mV)
# # 1:       Recording Name            20191016 697.2514     E1_13         0.013
# # 2:          Description                     697.2538     A5_34         0.046
# # 3:                                          697.2546     D6_42         0.030
# # 4: Maestro Pro Settings                     697.2549     B2_32         0.024
# # 5:   Original File Time 10/28/2019 09:55:21 697.2604     A8_43         0.014
# # 6:   Sampling Frequency            12.5 kHz 697.2636     A2_23         0.019
# # oof, this is the one where timing is off
# spk[Electrode %in% c("A5_31","C1_21","C1_41","E1_23","E6_33")] # many rows!
# spk[1:10]
# # 5:    Original File Time            10/28/2019 09:55:21
# # 6:    Sampling Frequency                       12.5 kHz
# # 7:         Voltage Scale -5.48486178148311E-08 V/sample
# # 8: Experiment Start Time            10/28/2019 09:26:37
# # same as above. So doubtful that either of this is after BIC added
# # And I don't think I have seen any BIC recordings in this data set at all
# rm(spk)
# 
# # the file that Melissa used
# spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/All Spike List Files/NFA_20191016_MW70-2414_12_00(000)(000)_spike_list.csv",
#              select = 1:5)
# head(spk)
# spk[V4 %in% c("A5_31","C1_21","C1_41","E1_23","E6_33")] # many rows!
# # V1             V2        V3    V4    V5
# # 1:              Plate Type Classic MEA 48    0.0756 E6_33 0.022
# # 2:        High Pass Filter    Butterworth   0.08936 A5_31 0.021
# # 3:               Threshold              6   0.11024 E1_23  0.02
# # 4: Threshold Crossing Only           TRUE   0.11112 C1_21 0.049
# # 5: Coincident Event Filter Default (Well)   0.11336 C1_21  0.01
# ---                                                             
# spk[1:10]
# #             Original File Time                       10/28/2019 9:55
# # 7:          Sampling Frequency                              12.5 kHz
# # 8:               Voltage Scale        -5.48486178148311E-08 V/sample
# # 9:       Experiment Start Time                       10/28/2019 9:26
# rm(spk)
# # V1              V2       V3        V4            V5
# # 1:         Investigator                 Time (s) Electrode Amplitude(mV)
# # 2:       Recording Name        20191016        0     E1_13         0.013
# # 3:          Description                  0.00248     A5_34         0.046
# # 4:                                        0.0032     D6_42          0.03
# # 5: Maestro Pro Settings                  0.00352     B2_32         0.024
# # 6:   Original File Time 10/28/2019 9:55  0.00904     A8_43         0.014
# # Ah! Did melissa just subtract to get to 0?
# 
# # verifying replicates for a particular wells
# source(file.path(root_output_dir,"supplemental_scripts","view_replicates_by_DIV.R"))
# dat <- read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/output/DNTGF2019_parameters_by_DIV.csv")
# compoundi <- "78"
# for (dosei in c(30,10,3,1)) {
#   graphics.off()
#   pdf(file = file.path(root_output_dir, dataset_title, "plots",paste0(dataset_title,"_",compoundi,"_", dosei,"uM_by_DIV.pdf")))
#   view_replicates_by_DIV(dat, compoundi, dosei, acsns = c("meanfiringrate","nAE","burst.per.min","ns.n","r","mi"), title_msg = "(MW70???2509 initially misdosed with 77 DIV 5)")
#   graphics.off()
# }
# 
# graphics.off()
# compoundi <- 77
# dosei <- 30
# pdf(file = file.path(root_output_dir, dataset_title, "plots",paste0(dataset_title,"_",compoundi,"_", dosei,"uM_by_DIV.pdf")))
# view_replicates_by_DIV(dat, compoundi, dosei, acsns = c("meanfiringrate","nAE","burst.per.min","ns.n","r","mi"), title_msg = "(comparing with 78 plots)")
# graphics.off()
# 
# dosei <- 10
# pdf(file = file.path(root_output_dir, dataset_title, "plots",paste0(dataset_title,"_",compoundi,"_", dosei,"uM_by_DIV.pdf")))
# view_replicates_by_DIV(dat, compoundi, dosei, acsns = c("meanfiringrate","nAE","burst.per.min","ns.n","r","mi"), title_msg = "(comparing with 78 plots)")
# graphics.off()
