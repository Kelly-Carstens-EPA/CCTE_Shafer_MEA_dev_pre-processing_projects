rm(list=ls())
###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "RejectedCultures" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
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

# overwriting this to search for files myself
# cultures_table <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/RejectedCultures/rejected_cultures_list.csv"))
# file_names <- c()
# for (culture in cultures_table$Culture) {
#   cat("\n",culture,"\n", sep = "")
#   culture.dir <- list.files(file.path(cultures_table[Culture == culture, Main_dir]), pattern = as.character(culture), full.names = T, include.dirs = T)
#   plate.dirs <- grep("([0-9]){2,4}-([0-9]){1,4}$", list.dirs(culture.dir, recursive = F), val = T)
#   if (length(plate.dirs)==0) {
#     group.dirs <- grep("[Gg]roup",list.dirs(culture.dir, recursive = F), val = T)
#     group.dirs <- group.dirs[grepl("[Gg]roup",basename(group.dirs))]
#     plate.dirs <- unlist(lapply(group.dirs, function(x) grep("([0-9]){2,4}-([0-9]){1,4}$", list.dirs(x, recursive = F), val = T)))
#   }
#   
#   # get calc files
#   calc_files <- list.files(culture.dir, pattern = "Calculations", full.names = T)
#   if (length(calc_files) > ceiling(length(plate.dirs)/3)) {
#     calc_files <- choose.files(default = calc_files[1], caption = paste0("Select Calc files for ",culture))
#   }
#   for (plate.dir in plate.dirs) {
#     cat(basename(plate.dir)," ")
#     s_files <- list.files(path = plate.dir, pattern = "_spike_list", recursive = T, full.names = T)
#     if (length(s_files) != 4) {
#       cat(basename(s_files), sep ="\n")
#       s_files <- choose.files(default = s_files[1], caption = "Select spike list files")
#     }
#     m_files <- list.files(path = file.path(plate.dir,"csv Files"), pattern = "_MaestroExperimentLog_Ontogeny")
#     summary_files <- list.files(path = plate.dir, pattern = "Summary", recursive = T)
#   }
#   file_names <- c(file_names, unlist(sapply(Filter(function(x) length(get(x)) > 0, c("calc_files","s_files","m_files","summary_files")), get)))
# }
# length(file_names) # 40
# nrow(cultures_table) # 14
# source('gather_files_functions.R')
# # then write it

# ya know, let's just use the interface that I already made...

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