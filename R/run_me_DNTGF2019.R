rm(list=ls())
graphics.off()
###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "DNTGF2019" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probs want to be true when you first run
save_notes_graphs <- FALSE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed

default_ControlTreatmentName = "DMSO" # usually DMSO. all compounds other than those listed below should have this vehicle control
# Enter the names of the compounds as they appear in the MEA data that have a vehicle control other than the default
different_vehicleControlCompounds = c() # e.g. c("Sodium Orthovanadate", "Amphetamine")
# Enter the names of the vehicle controls as they correspond to the compounds in the previous list
different_vehicleControls = c() # e.g. c("Water", "Water")

spidmap_file <- ""
spid_sheet <- ""

scripts.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R"
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl" # where the dataset_title folder will be created
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(readxl)

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

# run the main steps
source(file.path(scripts.dir, 'source_steps.R'))

# prepare spidmap
spidmap <- as.data.table(read_excel(spidmap_file, sheet = spid_sheet))
head(spidmap)
unique(spidmap$Concentration_Unit) # all mM?
setnames(spidmap, old = c(trt_col, conc_col, spid_col), new = c("treatment","stock_conc","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap[, treatment := as.character(treatment)]
head(spidmap[, .(treatment, spid, stock_conc)])

# # rename any compounds, if needed
# auc <- fread(file.path(root_output_dir,dataset_title, "output", paste0(dataset_title, "_AUC.csv")))
# cyto <- fread(file.path(root_output_dir,dataset_title, "output", paste0(dataset_title, "_cytotox.csv")))
# auc[treatment == "Dibenz[a,c] anthracene", treatment := "Dibenz[a,c]anthracene"]
# cyto[treatment == "Dibenz[a,c] anthracene", treatment := "Dibenz[a,c]anthracene"]
# write.csv(auc, file.path(root_output_dir,dataset_title, "output", paste0(dataset_title, "_AUC.csv")), row.names = FALSE)
# write.csv(cyto, file.path(root_output_dir,dataset_title, "output", paste0(dataset_title, "_cytotox.csv")), row.names = FALSE)
# rm(list = c("auc","cyto"))

# run tcpl_MEA_dev_AUC
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
source(file.path(scripts.dir, 'confirm_concs.R'))
tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title, spidmap, default_ControlTreatmentName,
                 different_vehicleControlCompounds = different_vehicleControlCompounds, different_vehicleControls = different_vehicleControls)

# FINAL DATA CHECKS
# this section is to confirm that the data has been processed correctly
dat <- read.csv(file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.csv")))
setDT(dat)
source(file.path(scripts.dir, 'dataset_checks.R'))
dataset_checks(dat)

# Any other plots or things to check?

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

# DIV 12 is missing from "Original Spike List files" folder. Can I use the one in the modified folder?
spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20191016 Culture DNT Group 7/70-2414/Spike List Files/Modified/NFA_20191016_MW70-2414_12_00(000)(000)_spike_list.csv",
             select = 1:5)
head(spk)
spk[V4 %in% c("A5_31","C1_21","C1_41","E1_23","E6_33")] #empty data table
spk[1:10]
#             Original File Time                       10/28/2019 9:55
# 7:          Sampling Frequency                              12.5 kHz
# 8:               Voltage Scale        -5.48486178148311E-08 V/sample
# 9:       Experiment Start Time                       10/28/2019 9:26
rm(spk)

# The other DIV 12 file, let's see if this is BIC or what
spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/20191016 Culture DNT Group 7/70-2414/Spike List Files/Modified/NFA_20191016_MW70-2414_12_00(000)(001)_spike_list.csv",
             select = 1:5)
head(spk)
# Investigator                  V2 Time (s) Electrode Amplitude(mV)
# 1:       Recording Name            20191016 697.2514     E1_13         0.013
# 2:          Description                     697.2538     A5_34         0.046
# 3:                                          697.2546     D6_42         0.030
# 4: Maestro Pro Settings                     697.2549     B2_32         0.024
# 5:   Original File Time 10/28/2019 09:55:21 697.2604     A8_43         0.014
# 6:   Sampling Frequency            12.5 kHz 697.2636     A2_23         0.019
# oof, this is the one where timing is off
spk[Electrode %in% c("A5_31","C1_21","C1_41","E1_23","E6_33")] # many rows!
spk[1:10]
# 5:    Original File Time            10/28/2019 09:55:21
# 6:    Sampling Frequency                       12.5 kHz
# 7:         Voltage Scale -5.48486178148311E-08 V/sample
# 8: Experiment Start Time            10/28/2019 09:26:37
# same as above. So doubtful that either of this is after BIC added
# And I don't think I have seen any BIC recordings in this data set at all
rm(spk)

# the file that Melissa used
spk <- fread("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/All Spike List Files/NFA_20191016_MW70-2414_12_00(000)(000)_spike_list.csv",
             select = 1:5)
head(spk)
spk[V4 %in% c("A5_31","C1_21","C1_41","E1_23","E6_33")] # many rows!
# V1             V2        V3    V4    V5
# 1:              Plate Type Classic MEA 48    0.0756 E6_33 0.022
# 2:        High Pass Filter    Butterworth   0.08936 A5_31 0.021
# 3:               Threshold              6   0.11024 E1_23  0.02
# 4: Threshold Crossing Only           TRUE   0.11112 C1_21 0.049
# 5: Coincident Event Filter Default (Well)   0.11336 C1_21  0.01
---                                                             
spk[1:10]
#             Original File Time                       10/28/2019 9:55
# 7:          Sampling Frequency                              12.5 kHz
# 8:               Voltage Scale        -5.48486178148311E-08 V/sample
# 9:       Experiment Start Time                       10/28/2019 9:26
rm(spk)
# V1              V2       V3        V4            V5
# 1:         Investigator                 Time (s) Electrode Amplitude(mV)
# 2:       Recording Name        20191016        0     E1_13         0.013
# 3:          Description                  0.00248     A5_34         0.046
# 4:                                        0.0032     D6_42          0.03
# 5: Maestro Pro Settings                  0.00352     B2_32         0.024
# 6:   Original File Time 10/28/2019 9:55  0.00904     A8_43         0.014
# Ah! Did melissa just subtract to get to 0?